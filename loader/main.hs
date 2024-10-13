{-# LANGUAGE QuasiQuotes #-}
module Main where

import           Data.Function         ((&))
import           Data.List             (intercalate)
import           System.Console.GetOpt (ArgDescr (..), ArgOrder (Permute), OptDescr (..), getOpt, usageInfo)
import           System.Environment    (getArgs, lookupEnv)
import           System.Exit           (ExitCode (..), exitWith)
import           System.IO             (IOMode (..), hGetLine, hIsEOF, hPutStr, hPutStrLn, stderr, withFile)
import           Text.Read             (readMaybe)
-- split
import           Data.List.Split       (splitOn)
-- extra
import           Data.List.Extra       (trimEnd)
-- filepath
import           System.FilePath       (dropFileName, splitExtension, takeFileName, (</>))
-- process
import           System.Process        (proc, waitForProcess, withCreateProcess)
-- PyF
import           PyF                   (fmt)


oops :: String -> IO a
oops msg = hPutStrLn stderr msg >> exitWith (ExitFailure 1)

main :: IO ExitCode
main = do
  (options, args) <- getArgs >>= parseArgs

  -- digest options and arguments
  let verbosity = getVerbosity options
  (scriptFile:argv) <- if length args < 1 then oops "A script file is required."
                       else pure args

  thshFile <- genTHSHFile options scriptFile

  withCreateProcess
    (proc "cabal" ([ "run", "-v" ++ show verbosity, thshFile, "--"] <> argv))
    (\_ _ _ ph -> waitForProcess ph)

{- INTERNAL FUNCTIONS -}

helpHeader :: String
helpHeader = "Usage: thsh script [OPTION...] [--] [args...]"

data Flag = Verbosity   !Int    -- valid values: 0..3; invalid values -> -1
          | UseLanguageEdition String
          | DisableBlockArguments
          | PrintHelp
            deriving (Show, Eq)
optsDescr :: [OptDescr Flag]
optsDescr = [ Option ['v'] ["verbose"]  (ReqArg verbp "VERBOSITY")
              "Control verbosity (n is 0--3, default is 1)."
            , Option ['l'] ["language"] (ReqArg UseLanguageEdition "LANG")
              "Language edition used, default is GHC2021."
            , Option [] ["disable-block-arguments"] (NoArg DisableBlockArguments)
              "Disable block arguments extensions, ffs."
            , Option ['h'] ["help"]     (NoArg  PrintHelp)
              "Print this help message."
            ]
  where verbp p = Verbosity $ case readMaybe p of Just a | a >= 0 && a <=3 -> a | otherwise -> -1; Nothing -> -1
getVerbosity :: [Flag] -> Int
getVerbosity (Verbosity a:_) = a
getVerbosity (_:os)          = getVerbosity os
getVerbosity []              = 1
getLanguageEdition :: [Flag] -> String
getLanguageEdition (UseLanguageEdition a:_) = a
getLanguageEdition (_:os)                   = getLanguageEdition os
getLanguageEdition []                       = "GHC2021"

parseArgs :: [String] -> IO ([Flag], [String])
parseArgs rawArgs = case getOpt Permute optsDescr rawArgs of
  (options, args, []) -> do
    if PrintHelp `elem` options
      then putStr usageInfoText >> exitWith ExitSuccess
      else if getVerbosity options == -1 then oops ("Error: invalid verbosity number.\n" ++ usageInfoText)
      else pure (reverse options, args) -- reverse the options so that it takes latter options as the effective ones
  (_ ,_ , errs)      -> oops (concat errs ++ usageInfoText)
  where usageInfoText = usageInfo helpHeader optsDescr

-- | Read and split script file into (strippedScript, cabalMetadata, projectMetadata)
readScriptFile :: FilePath -> IO (String, String, String)
readScriptFile filePath = withFile filePath ReadMode $ go ("", "", "") (0 :: Int)
  where go results mode = \hdl ->
          hIsEOF hdl >>= \case
          True -> pure results -- short circuit when eof
          False -> hGetLine hdl >>= pure . trimEnd >>= \line -> case () of
            _ | line == "{- cabal:" ->
                  if mode == 0 then go results 1 hdl
                  else oops "Error: unexpected cabal metadata section start"
              | line == "{- project: " ->
                  if mode == 0 then go results 2 hdl
                  else oops "Error: unexpected project metadata section start"
              | line == "-}"  ->
                  if mode /= 0 then go results 0 hdl
                  else oops "Error: unexpected metadata section close"
              | otherwise -> go results mode hdl >>= pure . add_line mode line
        add_line 0 line (c0, c1, c2) = (line ++ "\n" ++ c0, c1, c2)
        add_line 1 line (c0, c1, c2) = (c0, line ++ "\n" ++ c1, c2)
        add_line 2 line (c0, c1, c2) = (c0, c1, line ++ "\n" ++ c2)
        add_line _ _ _               = error "bad mode"

genTHSHFile :: [Flag] -> FilePath -> IO FilePath
genTHSHFile options scriptFile = do
  withFile thshFile WriteMode
    (\hdl -> do
        contents <- readScriptFile scriptFile
        hPutStr hdl =<< genTHSH options contents
        pure thshFile
    ) where thshFile = splitExtension scriptFile
                       & \(a, b) -> dropFileName a </> "." ++ takeFileName a ++ ".thsh" ++ b

genTHSH :: [Flag] -> (String, String, String) -> IO String
genTHSH options (strippedScript, cabalMeta, projectMeta) =
  lookupEnv "THSH_EXTRA_PACKAGE_DBS"
  >>= (\case Just env -> pure $ Just $ splitOn ":" env
             Nothing  -> pure Nothing
      )
  >>= \extraPackageDBs -> let extraPackageDBsLine = case extraPackageDBs of
                                Nothing  -> ""
                                Just dbs -> "package-dbs: " ++ intercalate ",\n  " dbs
                              langEdition = getLanguageEdition options
                              extraLangOptions =
                                if DisableBlockArguments `elem` options then [] else ["BlockArguments"]
                                <> [ "LambdaCase" ]
                          in pure [fmt|\
{{- cabal:
build-depends: base, THSH, PyF
default-language: { langEdition }
-- ! BEGIN USER CABAL METADATA
{ cabalMeta }
-- ! END USER CABAL METADATA
-}}
{{- project:
{ extraPackageDBsLine }
-- ! BEGIN USER PROJECT METADATA
{ projectMeta }
-- ! END USER PROJECT METADATA
-}}
{{-# LANGUAGE QuasiQuotes #-}}
{{-# LANGUAGE { intercalate ", " extraLangOptions } #-}}

import THSH
import System.Exit

-- ! BEGIN USER STRIPPED SCRIPT
{strippedScript}
-- ! END USER STRIPPED SCRIPT

main :: IO ExitCode
main = do
  runFuncletWithStdHandles __main__
|]
