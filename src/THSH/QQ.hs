{-|
Module      : THSH.QQ
Description : Template Haskell qausi-quote machinary, heavily using code from PyF package.
Copyright   : (c) Miao ZhiCheng, 2024
License     : MIT
Maintainer  : zhicheng.miao@gmail.com
Stability   : experimental
Portability : POSIX
-}


{-# LANGUAGE TemplateHaskellQuotes #-}

module THSH.QQ
  ( thsh
  ) where

-- ghc modules
import           GHC                        (SrcLoc, mkSrcLoc, mkSrcSpan)
import qualified Language.Haskell.TH        as TH
import           Language.Haskell.TH.Quote  (QuasiQuoter (..))
import           Language.Haskell.TH.Syntax (Code, Exp (..), Q (..))
-- base module
import           Data.List                  (intercalate)
import           Data.Maybe                 (catMaybes, listToMaybe)
import           Data.String                (fromString)
-- transformers module
import           Control.Monad.Trans.Reader (runReader)
-- parsec module
import qualified Text.Parsec                as Ps
import qualified Text.Parsec.Error          as PsError
import qualified Text.Parsec.Pos            as PsPos
--
import           THSH.Funclet               (AnyFunclet (..))
import qualified THSH.Internal.PyFInternals as PyF
import           THSH.Internal.THUtils      (reportErrorAt)
import           THSH.Script                (Script (..), genFuncletPipeCode)


-- | The quasi quoter for Template Haskell shell scripts.
thsh :: QuasiQuoter
thsh = QuasiQuoter
    { quoteExp = toExp,
      quotePat = mkErr "pattern",
      quoteType = mkErr "type",
      quoteDec = mkErr "declaration"
    }
  where
    mkErr :: String -> a
    mkErr name = error ("thsh : This QuasiQuoter can not be used as a " ++ name ++ "!")

    toExp :: String -> Q Exp
    toExp s = do
      loc <- TH.location
      exts <- TH.extsEnabled
      let context = PyF.ParsingContext (Just ('«', '»')) exts

      -- Setup the parser so it matchs the real original position in the source
      -- code.
      let filename = TH.loc_filename loc
      let initPos = Ps.setSourceColumn (Ps.setSourceLine (PsPos.initialPos filename) (fst $ TH.loc_start loc)) (snd $ TH.loc_start loc)
      case runReader (Ps.runParserT (Ps.setPosition initPos >> PyF.parseGenericFormatString) () filename s) context of
        -- returns a dummy exp, so TH continues its life. This TH code won't be
        -- executed anyway, there is an error
        Left err -> reportParserErrorAt err >> [|()|]
        Right items -> do
          -- stop at the first item that contains an error
          mapM id (map PyF.checkOneItem items) >>= pure . listToMaybe . catMaybes >>= \case
            Nothing -> TH.unTypeCode (mkScript items)
            Just (srcSpan, msg) -> reportErrorAt srcSpan msg >> [|()|]

{- ========== mkScript ========== -}

mkScript :: [PyF.Item] -> Code Q Script
mkScript items = [|| MkScript $$(TH.unsafeCodeCoerce source) $$(TH.unsafeCodeCoerce funclets) ||]
  where items'   = fmap matchItem items
        funclets = foldl appendQ <$> [| [] |] <*>
                   mapM snd (filter ((== True) . fst) items')
        source   = snd $ foldl (\(c, rs) (isFunclet, frag) -> if isFunclet
                                 then (c + 1, appendQ <$> rs <*> [| genFuncletPipeCode c |])
                                 else (c, appendQ <$> rs <*> frag)
                               ) (0 :: Int, [| [] |]) items'

-- | call `<>` between two 'Exp'
appendQ :: Exp -> Exp -> Exp
appendQ s0 s1 = InfixE (Just s0) (VarE '(<>)) (Just s1)

-- | Match an item to a funclet expression (True) or a formatted String expression (False)
matchItem :: PyF.Item -> (Bool, Q Exp)
matchItem (PyF.Raw x)                   = (False, [| x |])
matchItem (PyF.Replacement (_, expr) y) =
  let isFunclet = case expr of
                    AppE (VarE a) _ -> if | TH.nameBase a == "sh" -> True
                                          | TH.nameBase a == "fn" -> True
                                          | otherwise             -> False
                    _               -> False
  in (isFunclet, if isFunclet then [| [MkAnyFunclet $(pure expr)] |]
                 else [| $(PyF.getFormatExpr y) $(pure expr) |])

{- ========== reportParserErrorAt ========== -}

reportParserErrorAt :: Ps.ParseError -> Q ()
reportParserErrorAt err = reportErrorAt srcSpan msg
  where
    msg = intercalate "\n" $ formatErrorMessages err

    srcSpan = mkSrcSpan loc loc'

    loc = srcLocFromParserError (Ps.errorPos err)
    loc' = srcLocFromParserError (Ps.incSourceColumn (Ps.errorPos err) 1)

formatErrorMessages :: Ps.ParseError -> [String]
formatErrorMessages err
  -- If there is an explicit error message from parsec, use only that
  | not $ null messages = map PsError.messageString messages
  -- Otherwise, uses parsec formatting
  | otherwise = [PsError.showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (PsError.errorMessages err)]
  where
    (_sysUnExpect, msgs1) = span (PsError.SysUnExpect "" ==) (PsError.errorMessages err)
    (_unExpect, msgs2) = span (PsError.UnExpect "" ==) msgs1
    (_expect, messages) = span (PsError.Expect "" ==) msgs2

srcLocFromParserError :: Ps.SourcePos -> SrcLoc
srcLocFromParserError sourceLoc = srcLoc
  where
    line = Ps.sourceLine sourceLoc
    column = Ps.sourceColumn sourceLoc
    name = Ps.sourceName sourceLoc
    srcLoc = mkSrcLoc (fromString name) line column
