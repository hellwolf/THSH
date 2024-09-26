{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module THSH.Internal.PyFInternals
  ( checkOneItem
  , getFormatExpr
  , ParsingContext (..)
  , Item (..)
  , parseGenericFormatString
  ) where

-- ghc modules
import           GHC                        (SrcSpan)
import           GHC.TypeError              (ErrorMessage (Text), TypeError)
import           Language.Haskell.TH.Syntax (Exp (..), Lit (..), Q (..))
-- baes module
import           Data.Kind                  (Type)
import           Data.Maybe                 (catMaybes, fromMaybe)
import           Data.Proxy                 (Proxy (Proxy))
-- PyF module (Note: we take the risks of using internal functions)
import           PyF.Class                  (PyFCategory (..), PyFClassify, PyFToString (..), PyfFormatFractional (..),
                                             PyfFormatIntegral (..))
import           PyF.Formatters             (AnyAlign (..))
import qualified PyF.Formatters             as Formatters
import           PyF.Internal.PythonSyntax  (AlternateForm (..), ExprOrValue (..), FormatMode (..), Item (..),
                                             Padding (..), ParsingContext (..), Precision (..), TypeFormat (..),
                                             parseGenericFormatString, pattern DefaultFormatMode)
--
import           THSH.Internal.HsExprUtils  (RdrName, findFreeVariables)
import           THSH.Internal.THUtils      (freeVariableByNameExists)


{- ========== checkOneItem ========== -}

checkOneItem :: Item -> Q (Maybe (SrcSpan, String))
checkOneItem (Raw _) = pure Nothing
checkOneItem (Replacement (hsExpr, _) formatMode) = do
  let allNames = findFreeVariables hsExpr <> findFreeVariablesInFormatMode formatMode
  res <- mapM freeVariableByNameExists allNames
  let resFinal = catMaybes res

  case resFinal of
    []                   -> pure Nothing
    ((err, srcSpan) : _) -> pure $ Just (srcSpan, err)

findFreeVariablesInFormatMode :: Maybe FormatMode -> [(SrcSpan, RdrName)]
findFreeVariablesInFormatMode Nothing = []
findFreeVariablesInFormatMode (Just (FormatMode padding tf _ )) = findFreeVariables tf <> case padding of
  PaddingDefault -> []
  Padding eoi _  -> findFreeVariables eoi

{- ========== getFormatExpr ========== -}

getFormatExpr :: Maybe FormatMode -> Q Exp
getFormatExpr mode = let mode' = fromMaybe DefaultFormatMode mode in padAndFormat mode'

padAndFormat :: FormatMode -> Q Exp
padAndFormat (FormatMode padding tf grouping) = case tf of
  -- Integrals
  BinaryF alt s -> [|formatAnyIntegral $(withAlt alt Formatters.Binary) s $(newPaddingQ padding) $(toGrp grouping 4)|]
  CharacterF -> [|formatAnyIntegral Formatters.Character Formatters.Minus $(newPaddingQ padding) Nothing|]
  DecimalF s -> [|formatAnyIntegral Formatters.Decimal s $(newPaddingQ padding) $(toGrp grouping 3)|]
  HexF alt s -> [|formatAnyIntegral $(withAlt alt Formatters.Hexa) s $(newPaddingQ padding) $(toGrp grouping 4)|]
  OctalF alt s -> [|formatAnyIntegral $(withAlt alt Formatters.Octal) s $(newPaddingQ padding) $(toGrp grouping 4)|]
  HexCapsF alt s -> [|formatAnyIntegral (Formatters.Upper $(withAlt alt Formatters.Hexa)) s $(newPaddingQ padding) $(toGrp grouping 4)|]
  -- Floating
  ExponentialF prec alt s -> [|formatAnyFractional $(withAlt alt Formatters.Exponent) s $(newPaddingQ padding) $(toGrp grouping 3) $(splicePrecision defaultFloatPrecision prec)|]
  ExponentialCapsF prec alt s -> [|formatAnyFractional (Formatters.Upper $(withAlt alt Formatters.Exponent)) s $(newPaddingQ padding) $(toGrp grouping 3) $(splicePrecision defaultFloatPrecision prec)|]
  GeneralF prec alt s -> [|formatAnyFractional $(withAlt alt Formatters.Generic) s $(newPaddingQ padding) $(toGrp grouping 3) $(splicePrecision defaultFloatPrecision prec)|]
  GeneralCapsF prec alt s -> [|formatAnyFractional (Formatters.Upper $(withAlt alt Formatters.Generic)) s $(newPaddingQ padding) $(toGrp grouping 3) $(splicePrecision defaultFloatPrecision prec)|]
  FixedF prec alt s -> [|formatAnyFractional $(withAlt alt Formatters.Fixed) s $(newPaddingQ padding) $(toGrp grouping 3) $(splicePrecision defaultFloatPrecision prec)|]
  FixedCapsF prec alt s -> [|formatAnyFractional (Formatters.Upper $(withAlt alt Formatters.Fixed)) s $(newPaddingQ padding) $(toGrp grouping 3) $(splicePrecision defaultFloatPrecision prec)|]
  PercentF prec alt s -> [|formatAnyFractional $(withAlt alt Formatters.Percent) s $(newPaddingQ padding) $(toGrp grouping 3) $(splicePrecision defaultFloatPrecision prec)|]
  -- Default / String
  DefaultF prec s -> [|formatAny s $(paddingToPaddingK padding) $(toGrp grouping 3) $(splicePrecision Nothing prec)|]
  StringF prec -> [|Formatters.formatString (newPaddingKForString $(paddingToPaddingK padding)) $(splicePrecision Nothing prec) . pyfToString|]

-- | Default precision for floating point
defaultFloatPrecision :: Maybe Int
defaultFloatPrecision = Just 6

withAlt :: AlternateForm -> Formatters.Format t t' t'' -> Q Exp
withAlt NormalForm e    = [|e|]
withAlt AlternateForm e = [|Formatters.Alternate e|]

-- | Precision to maybe
splicePrecision :: Maybe Int -> Precision -> Q Exp
splicePrecision def PrecisionDefault = [|def :: Maybe Int|]
splicePrecision _ (Precision p)      = [|Just $(exprToInt p)|]

toGrp :: Maybe Char -> Int -> Q Exp
toGrp mb a = [|grp|]
  where
    grp = (a,) <$> mb

newPaddingQ :: Padding -> Q Exp
newPaddingQ padding = case padding of
  PaddingDefault -> [|Nothing :: Maybe (Int, AnyAlign, Char)|]
  (Padding i al) -> case al of
    Nothing           -> [|Just ($(exprToInt i), AnyAlign Formatters.AlignRight, ' ')|] -- Right align and space is default for any object, except string
    Just (Nothing, a) -> [|Just ($(exprToInt i), a, ' ')|]
    Just (Just c, a)  -> [|Just ($(exprToInt i), a, c)|]

exprToInt :: ExprOrValue Int -> Q Exp
-- Note: this is a literal provided integral. We use explicit case to ::Int so it won't warn about defaulting
exprToInt (Value i)            = [|$(pure $ LitE (IntegerL (fromIntegral i))) :: Int|]
exprToInt (HaskellExpr (_, e)) = [|$(pure e)|]

paddingToPaddingK :: Padding -> Q Exp
paddingToPaddingK p = case p of
  PaddingDefault                   -> [|PaddingDefaultK|]
  Padding i Nothing                -> [|PaddingK ($(exprToInt i)) Nothing :: PaddingK 'Formatters.AlignAll Int|]
  Padding i (Just (c, AnyAlign a)) -> [|PaddingK $(exprToInt i) (Just (c, a))|]

paddingKToPadding :: PaddingK k i -> Maybe (i, AnyAlign, Char)
paddingKToPadding p = case p of
  PaddingDefaultK -> Nothing
  (PaddingK i al) -> case al of
    Nothing           -> Just (i, AnyAlign Formatters.AlignRight, ' ') -- Right align and space is default for any object, except string
    Just (Nothing, a) -> Just (i, AnyAlign a, ' ')
    Just (Just c, a)  -> Just (i, AnyAlign a, c)

formatAnyIntegral :: forall i paddingWidth t t'. Integral paddingWidth => PyfFormatIntegral i => Formatters.Format t t' 'Formatters.Integral -> Formatters.SignMode -> Maybe (paddingWidth, AnyAlign, Char) -> Maybe (Int, Char) -> i -> String
formatAnyIntegral f s Nothing grouping i = pyfFormatIntegral @i @paddingWidth f s Nothing grouping i
formatAnyIntegral f s (Just (padSize, AnyAlign alignMode, c)) grouping i = pyfFormatIntegral f s (Just (padSize, alignMode, c)) grouping i

formatAnyFractional :: forall paddingWidth precision i t t'. (Integral paddingWidth, Integral precision, PyfFormatFractional i) => Formatters.Format t t' 'Formatters.Fractional -> Formatters.SignMode -> Maybe (paddingWidth, AnyAlign, Char) -> Maybe (Int, Char) -> Maybe precision -> i -> String
formatAnyFractional f s Nothing grouping p i = pyfFormatFractional @i @paddingWidth @precision f s Nothing grouping p i
formatAnyFractional f s (Just (padSize, AnyAlign alignMode, c)) grouping p i = pyfFormatFractional f s (Just (padSize, alignMode, c)) grouping p i

class FormatAny i k where
  formatAny :: forall paddingWidth precision. (Integral paddingWidth, Integral precision) => Formatters.SignMode -> PaddingK k paddingWidth -> Maybe (Int, Char) -> Maybe precision -> i -> String

instance (FormatAny2 (PyFClassify t) t k) => FormatAny t k where
  formatAny = formatAny2 (Proxy :: Proxy (PyFClassify t))

class FormatAny2 (c :: PyFCategory) (i :: Type) (k :: Formatters.AlignForString) where
  formatAny2 :: forall paddingWidth precision. (Integral paddingWidth, Integral precision) => Proxy c -> Formatters.SignMode -> PaddingK k paddingWidth -> Maybe (Int, Char) -> Maybe precision -> i -> String

instance (Show t, Integral t) => FormatAny2 'PyFIntegral t k where
  formatAny2 _ s a p _precision = formatAnyIntegral Formatters.Decimal s (paddingKToPadding a) p

instance (PyfFormatFractional t) => FormatAny2 'PyFFractional t k where
  formatAny2 _ s a = formatAnyFractional Formatters.Generic s (paddingKToPadding a)

data PaddingK k i where
  PaddingDefaultK :: PaddingK 'Formatters.AlignAll Int
  PaddingK :: i -> Maybe (Maybe Char, Formatters.AlignMode k) -> PaddingK k i

newPaddingKForString :: Integral i => PaddingK 'Formatters.AlignAll i -> Maybe (Int, Formatters.AlignMode 'Formatters.AlignAll, Char)
newPaddingKForString padding = case padding of
  PaddingDefaultK           -> Nothing
  PaddingK i Nothing        -> Just (fromIntegral i, Formatters.AlignLeft, ' ') -- default align left and fill with space for string
  PaddingK i (Just (mc, a)) -> Just (fromIntegral i, a, fromMaybe ' ' mc)

-- TODO: _s(ign) and _grouping should trigger errors
instance (PyFToString t) => FormatAny2 'PyFString t 'Formatters.AlignAll where
  formatAny2 _ _s a _grouping precision t = Formatters.formatString (newPaddingKForString a) precision (pyfToString t)

instance TypeError ('Text "String type is incompatible with inside padding (=).") => FormatAny2 'PyFString t 'Formatters.AlignNumber where
  formatAny2 = error "Unreachable"
