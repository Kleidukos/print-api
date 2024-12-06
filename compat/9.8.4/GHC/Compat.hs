-- GHC 9.10.1 Compatibility
module GHC.Compat where

import GHC (ModuleInfo)
import GHC.Iface.Syntax (AltPpr (..), ShowForAllFlag (..), ShowHowMuch (..), ShowSub (..))
import PrintApi.IgnoredDeclarations ()

mkShowSub :: ModuleInfo -> ShowSub
mkShowSub _ =
  let ss_how_much = ShowSome [] (AltPpr Nothing)
   in ShowSub
        { ss_how_much = ss_how_much
        , ss_forall = ShowForAllMust
        }
