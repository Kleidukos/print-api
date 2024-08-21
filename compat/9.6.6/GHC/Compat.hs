-- GHC 9.6.6 Compatibility
module GHC.Compat where

import GHC (ModuleInfo)
import GHC.Iface.Syntax (AltPpr (..), ShowForAllFlag (..), ShowHowMuch (..), ShowSub (..))

mkShowSub :: ModuleInfo -> ShowSub
mkShowSub _mod_info =
  let ss_how_much = ShowSome [] (AltPpr Nothing)
   in ShowSub
        { ss_how_much = ss_how_much
        , ss_forall = ShowForAllMust
        }
