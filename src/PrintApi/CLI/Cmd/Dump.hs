module PrintApi.CLI.Cmd.Dump where

import Control.Monad.IO.Class
import Data.Function (on)
import Data.List qualified as List
import GHC
import GHC.Compat
import GHC.Core.Class (classMinimalDef)
import GHC.Core.InstEnv (instEnvElts, instanceHead)
import GHC.Data.FastString (fsLit)
import GHC.Driver.Env (hscEPS, hsc_units)
import GHC.Driver.Ppr (showSDocForUser)
import GHC.Types.Name (nameOccName, stableNameCmp)
import GHC.Types.TyThing (tyThingParent_maybe)
import GHC.Types.TyThing.Ppr (pprTyThing)
import GHC.Unit.External (eps_inst_env)
import GHC.Unit.Info (PackageName (..), UnitInfo, unitExposedModules, unitId)
import GHC.Unit.State (lookupPackageName, lookupUnitId)
import GHC.Unit.Types (UnitId)
import GHC.Utils.Outputable
import System.IO qualified as System
import System.OsPath (OsPath)
import System.OsPath qualified as OsPath
import Prelude hiding ((<>))

import PrintApi.IgnoredDeclarations

run
  :: FilePath
  -> Maybe OsPath
  -> String
  -> IO ()
run root mModuleIgnoreList packageName = do
  userIgnoredModules <- case mModuleIgnoreList of
    Nothing -> pure []
    Just ignoreListPath -> do
      ignoreListFilePath <- liftIO $ OsPath.decodeFS ignoreListPath
      modules <- lines <$> liftIO (System.readFile ignoreListFilePath)
      pure $ List.map mkModuleName modules
  rendered <- computePackageAPI root userIgnoredModules packageName
  liftIO $ putStrLn rendered

computePackageAPI
  :: FilePath
  -> [ModuleName]
  -> String
  -> IO String
computePackageAPI root userIgnoredModules packageName = runGhc (Just root) $ do
  let args :: [Located String] =
        map
          noLoc
          [ "-package=" ++ packageName
          , "-dppr-cols=1000"
          , "-fprint-explicit-runtime-reps"
          , "-fprint-explicit-foralls"
          ]
  dflags <- do
    dflags <- getSessionDynFlags
    logger <- getLogger
    (dflags', _fileish_args, _dynamicFlagWarnings) <-
      GHC.parseDynamicFlags logger dflags args
    pure dflags'

  _ <- setProgramDynFlags dflags
  unit_state <- hsc_units <$> getSession
  unit_id <- case lookupPackageName unit_state (PackageName $ fsLit packageName) of
    Just unit_id -> pure unit_id
    Nothing -> fail "failed to find package"
  unit_info <- case lookupUnitId unit_state unit_id of
    Just unit_info -> pure unit_info
    Nothing -> fail "unknown package"

  decls_doc <- reportUnitDecls userIgnoredModules unit_info
  insts_doc <- reportInstances

  name_ppr_ctx <- GHC.getNamePprCtx
  pure $ showSDocForUser dflags unit_state name_ppr_ctx (vcat [decls_doc, insts_doc])

ignoredTyThing :: TyThing -> Bool
ignoredTyThing _ = False

reportUnitDecls :: [ModuleName] -> UnitInfo -> Ghc SDoc
reportUnitDecls userIgnoredModules unit_info = do
  let exposed :: [ModuleName]
      exposed = map fst (unitExposedModules unit_info)
  vcat <$> mapM (reportModuleDecls userIgnoredModules $ unitId unit_info) exposed

reportModuleDecls :: [ModuleName] -> UnitId -> ModuleName -> Ghc SDoc
reportModuleDecls userIgnoredModules unit_id modl_nm
  | modl_nm `elem` (userIgnoredModules ++ ignoredModules) = do
      pure $ vcat [mod_header, text "-- ignored", text ""]
  | otherwise = do
      modl <- GHC.lookupQualifiedModule (OtherPkg unit_id) modl_nm
      mb_mod_info <- GHC.getModuleInfo modl
      mod_info <- case mb_mod_info of
        Nothing -> fail "Failed to find module"
        Just mod_info -> pure mod_info

      Just name_ppr_ctx <- mkNamePprCtxForModule mod_info
      let names = GHC.modInfoExports mod_info
      let sorted_names = List.sortBy (compare `on` nameOccName) names
      things <- mapM GHC.lookupName sorted_names
      let contents =
            vcat $
              [ pprTyThing ss thing $$ extras
              | Just thing <- things
              , case tyThingParent_maybe thing of
                  Just parent
                    | isExported mod_info (getOccName parent) -> False
                  _ -> True
              , not $ ignoredTyThing thing
              , let ss = mkShowSub mod_info
              , let extras = case thing of
                      ATyCon tycon
                        | Just cls <- tyConClass_maybe tycon ->
                            nest 2 (text "{-# MINIMAL" <+> ppr (classMinimalDef cls) <+> text "#-}")
                      _ -> empty
              ]

      pure $
        withUserStyle name_ppr_ctx AllTheWay $
          hang mod_header 2 contents
            <> text ""
  where
    mod_header =
      vcat
        [ text ""
        , text "module" <+> ppr modl_nm <+> text "where"
        , text ""
        ]

reportInstances :: Ghc SDoc
reportInstances = do
  hsc_env <- getSession
  eps <- liftIO $ hscEPS hsc_env
  let instances = eps_inst_env eps
  pure $
    vcat $
      [ text ""
      , text ""
      , text "-- Instances:"
      ]
        ++ [ ppr inst
           | inst <- List.sortBy compareInstances (instEnvElts instances)
           , not $ ignoredInstance inst
           ]

compareInstances :: ClsInst -> ClsInst -> Ordering
compareInstances inst1 inst2 =
  mconcat
    [ stableNameCmp (getName cls1) (getName cls2)
    ]
  where
    (_, cls1, _tys1) = instanceHead inst1
    (_, cls2, _tys2) = instanceHead inst2
