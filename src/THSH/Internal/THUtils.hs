{-# LANGUAGE CPP #-}

module THSH.Internal.THUtils
  ( reportErrorAt
  , lookupName
  ) where

import           GHC                        (SrcSpan, moduleNameString)
import           GHC.Tc.Errors.Types        (TcRnMessage (TcRnUnknownMessage))
import           GHC.Tc.Types               (TcM)
import           GHC.Tc.Utils.Monad         (addErrAt)
import           GHC.Types.Error            (NoDiagnosticOpts (NoDiagnosticOpts), UnknownDiagnostic (UnknownDiagnostic))
import           GHC.Types.Name             (occNameString)
import           GHC.Types.Name.Reader      (RdrName (..))
import           GHC.Utils.Error            (mkPlainError, noHints)
import           GHC.Utils.Outputable       (text)
import qualified Language.Haskell.TH        as TH
import           Language.Haskell.TH.Syntax (Q (Q))
--
import           Data.Maybe                 (isJust)
import           Unsafe.Coerce              (unsafeCoerce)


-- | This function is similar to TH reportError, however it also provide
-- correct SrcSpan, so error are localised at the correct position in the TH
-- splice instead of being at the beginning.
--
-- From: PyF.Internal.QQ
reportErrorAt :: SrcSpan -> String -> Q ()
reportErrorAt loc msg = unsafeRunTcM $ addErrAt loc msg'
  where
#if MIN_VERSION_ghc(9,7,0)
    msg' = TcRnUnknownMessage (UnknownDiagnostic (const NoDiagnosticOpts) (mkPlainError noHints (text msg)))
#elif MIN_VERSION_ghc(9,6,0)
    msg' = TcRnUnknownMessage (UnknownDiagnostic $ mkPlainError noHints $
                         text msg)
#elif MIN_VERSION_ghc(9,3,0)
    msg' = TcRnUnknownMessage (GhcPsMessage $ PsUnknownMessage $ mkPlainError noHints $
                         text msg)
#else
    msg' = fromString msg
#endif

-- Stolen from: https://www.tweag.io/blog/2021-01-07-haskell-dark-arts-part-i/
-- This allows to hack inside the the GHC api and use function not exported by template haskell.
-- This may not be always safe, see https://github.com/guibou/PyF/issues/115,
-- hence keep that for "failing path" (i.e. error reporting), but not on
-- codepath which are executed otherwise.
-- From: PyF.Internal.QQ
unsafeRunTcM :: TcM a -> Q a
unsafeRunTcM m = Q (unsafeCoerce m)


lookupName :: RdrName -> Q Bool
lookupName n = case n of
  (Unqual o)   -> isJust <$> TH.lookupValueName (occNameString o)
  (Qual m o)   -> isJust <$> TH.lookupValueName (moduleNameString m <> "." <> occNameString o)
  -- No idea how to lookup for theses names, so consider that they exists
  (Orig _m _o) -> pure True
  (Exact _)    -> pure True
