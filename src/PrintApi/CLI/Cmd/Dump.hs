-- |
--  Module      : PrintApi.CLI.Cmd.Dump
--  Copyright   : © Hécate, 2024
--  License     : MIT
--  Maintainer  : hecate@glitchbra.in
--  Visibility  : Public
--
--  The processing of package information
module PrintApi.CLI.Cmd.Dump where

import Control.Monad.IO.Class
import Data.Function ( (&), on )
import Data.List qualified as List
import Data.List.Extra qualified as List
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
import qualified Data.Maybe as Maybe

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

printPackageDocumentation 
  :: FilePath
  -> String
  -> IO String
printPackageDocumentation root packageName = runGhc (Just root) $ do
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
  unitId <- case lookupPackageName unit_state (PackageName $ fsLit packageName) of
    Just unitId -> pure unitId
    Nothing -> fail "failed to find package"
  unitInfo <- case lookupUnitId unit_state unitId of
    Just unitInfo -> pure unitInfo
    Nothing -> fail "unknown package"
  moduleDocumentation <- reportPackageDocumentation unitInfo
  name_ppr_ctx <- GHC.getNamePprCtx
  pure $ List.trim $ showSDocForUser dflags unit_state name_ppr_ctx moduleDocumentation

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
  unitId <- case lookupPackageName unit_state (PackageName $ fsLit packageName) of
    Just unitId -> pure unitId
    Nothing -> fail "failed to find package"
  unitInfo <- case lookupUnitId unit_state unitId of
    Just unitInfo -> pure unitInfo
    Nothing -> fail "unknown package"

  decls_doc <- reportUnitDecls userIgnoredModules unitInfo
  insts_doc <- reportInstances

  name_ppr_ctx <- GHC.getNamePprCtx
  pure $ List.trim $ showSDocForUser dflags unit_state name_ppr_ctx (vcat [decls_doc, insts_doc])

ignoredTyThing :: TyThing -> Bool
ignoredTyThing _ = False

reportUnitDecls :: [ModuleName] -> UnitInfo -> Ghc SDoc
reportUnitDecls userIgnoredModules unitInfo = do
  let exposed :: [ModuleName]
      exposed = map fst (unitExposedModules unitInfo)
  vcat <$> mapM (reportModuleDecls userIgnoredModules $ unitId unitInfo) exposed

reportPackageDocumentation :: UnitInfo -> Ghc SDoc
reportPackageDocumentation unitInfo = do
  let exposed :: [ModuleName]
      exposed = map fst (unitExposedModules unitInfo)
  vcat <$> mapM (reportModuleDocumentation (unitId unitInfo)) exposed

reportModuleDocumentation :: UnitId -> ModuleName -> Ghc SDoc
reportModuleDocumentation unitId moduleName = do
  modl <- GHC.lookupQualifiedModule (OtherPkg unitId) moduleName
  mb_mod_info <- GHC.getModuleInfo modl
  mod_info <- case mb_mod_info of
    Nothing -> fail "Failed to find module"
    Just mod_info -> pure mod_info
  Just name_ppr_ctx <- mkNamePprCtxForModule mod_info
  let mod_header =
        vcat
          [ text ""
          , text "module" <+> ppr moduleName <+> text "where"
          , text ""
          ]
  let mDocs =
         mod_info
         & modInfoIface
         & Maybe.fromJust
         & mi_docs
  case mDocs of
    Nothing -> do 
      liftIO $ putStrLn "No documentation for the interface"
      pure empty
    Just docs -> do 
      let contents = getVisibility docs
      pure $
        withUserStyle name_ppr_ctx AllTheWay $
          hang mod_header 2 contents
            <> text ""

reportModuleDecls :: [ModuleName] -> UnitId -> ModuleName -> Ghc SDoc
reportModuleDecls userIgnoredModules unitId moduleName
  | moduleName `elem` (userIgnoredModules ++ ignoredModules) = do
      pure $ vcat [mod_header, text "-- ignored", text ""]
  | otherwise = do
      modl <- GHC.lookupQualifiedModule (OtherPkg unitId) moduleName
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
        , text "module" <+> ppr moduleName <+> text "where"
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


getVisibility :: Docs -> SDoc
getVisibility moduleDocs = 
  let mModuleHeader = moduleDocs.docs_mod_hdr
  in case mModuleHeader of
        Nothing -> "Visibility: Public"
        Just hsDoc -> pprHsDocString hsDoc.hsDocString
