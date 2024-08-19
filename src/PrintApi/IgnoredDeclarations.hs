module PrintApi.IgnoredDeclarations
  ( ignoredOccNames
  , ignoredOccName
  , ignoredTyCon
  , ignoredModules
  , ignoredInstance
  , isExported
  , showOcc
  ) where

import Data.List (isPrefixOf)
import Data.List qualified as List
import GHC (ModuleInfo, modInfoExports)
import GHC.Core.InstEnv (ClsInst, instanceHead)
import GHC.Core.TyCon (TyCon)
import GHC.Core.Type (Type, tyConsOfType)
import GHC.Types.Name
  ( Name
  , OccName
  , getName
  , getOccName
  , mkDataOcc
  , mkVarOcc
  , nameModule_maybe
  , nameOccName
  , occNameString
  )
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import GHC.Unit.Types (moduleName)
import Language.Haskell.Syntax.Module.Name (ModuleName, mkModuleName)

ignoredOccNames :: [OccName]
ignoredOccNames =
  map mkDataOcc cTypeCons
    ++ map mkVarOcc integerConversionIds
  where
    -- Data constructors from Foreign.C.Types whose RHSs are inherently platform-dependent
    cTypeCons =
      [ "CBool"
      , "CChar"
      , "CClock"
      , "CDouble"
      , "CFile"
      , "CFloat"
      , "CFpos"
      , "CInt"
      , "CIntMax"
      , "CIntPtr"
      , "CJmpBuf"
      , "CLLong"
      , "CLong"
      , "CPtrdiff"
      , "CSChar"
      , "CSUSeconds"
      , "CShort"
      , "CSigAtomic"
      , "CSize"
      , "CTime"
      , "CUChar"
      , "CUInt"
      , "CUIntMax"
      , "CUIntPtr"
      , "CULLong"
      , "CULong"
      , "CUSeconds"
      , "CUShort"
      , "CWchar"
      ]

    -- Conversion functions in GHC.Integer which are only exposed on 32-bit
    -- platforms
    integerConversionIds =
      [ "int64ToInteger"
      , "integerToInt64"
      , "integerToWord64"
      , "word64ToInteger"
      ]

ignoredOccName :: OccName -> Bool
ignoredOccName occ = occ `elem` ignoredOccNames

ignoredName :: Name -> Bool
ignoredName nm
  | ignoredOccName (getOccName nm) =
      True
  | Just md <- nameModule_maybe nm
  , moduleName md `elem` ignoredModules =
      True
  | otherwise =
      False

ignoredTyCon :: TyCon -> Bool
ignoredTyCon = ignoredName . getName

ignoredType :: Type -> Bool
ignoredType = any ignoredTyCon . nonDetEltsUniqSet . tyConsOfType

ignoredModules :: [ModuleName]
ignoredModules =
  List.map
    mkModuleName
    (unstableModules ++ platformDependentModules)
  where
    unstableModules =
      [ "GHC.Prim"
      , "GHC.Conc.POSIX"
      , "GHC.Conc.IO"
      ]
    platformDependentModules =
      [ "System.Posix.Types"
      , "Foreign.C.Types"
      ]

-- | Ignore instances whose heads mention ignored types.
ignoredInstance :: ClsInst -> Bool
ignoredInstance inst
  | ignoredName $ getName cls =
      True
  | any ignoredType tys =
      True
  | ctupleInstance $ getName cls =
      True
  | otherwise =
      False
  where
    (_, cls, tys) = instanceHead inst

ctupleInstance :: Name -> Bool
ctupleInstance name =
  isPrefixOf "CTuple" nameS
    || nameS == "CUnit"
    || nameS == "CSolo"
  where
    nameS = occNameString (getOccName name)

exportedOccs :: ModuleInfo -> [OccName]
exportedOccs mod_info = map nameOccName (modInfoExports mod_info)

isExported :: ModuleInfo -> OccName -> Bool
isExported mod_info occ = occ `elem` exportedOccs mod_info

showOcc :: ModuleInfo -> OccName -> Bool
showOcc mod_info occ =
  isExported mod_info occ
    && not (ignoredOccName occ)
