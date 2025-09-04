-- GHC 9.12.2 compatibility
module GHC.Compat
  ( GHC.mkNamePprCtxForModule
  , mkShowSub
  ) where

import GHC (ModuleInfo)
import GHC qualified
import GHC.Iface.Syntax (AltPpr (..), ShowForAllFlag (..), ShowHowMuch (..), ShowSub (..))

import PrintApi.IgnoredDeclarations

mkShowSub :: ModuleInfo -> ShowSub
mkShowSub mod_info =
  let ss_how_much = ShowSome (Just (showOcc mod_info)) (AltPpr Nothing)
   in ShowSub
        { ss_how_much = ss_how_much
        , ss_forall = ShowForAllMust
        }
