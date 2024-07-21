-- GHC 9.10.1 Compatibility
module GHC.Compat where

import GHC (ModuleInfo, modInfoExports)
import GHC.Iface.Syntax (AltPpr (..), ShowForAllFlag (..), ShowHowMuch (..), ShowSub (..))
import GHC.Types.Name (OccName, nameOccName)

import DumpDecls.IgnoredDeclarations

mkShowSub :: ModuleInfo -> ShowSub
mkShowSub mod_info =
  let ss_how_much = ShowSome (Just (showOcc mod_info)) (AltPpr Nothing)
   in ShowSub
        { ss_how_much = ss_how_much
        , ss_forall = ShowForAllMust
        }
