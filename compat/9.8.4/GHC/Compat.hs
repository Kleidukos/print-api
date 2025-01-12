-- GHC 9.8.4 compatibility
module GHC.Compat
  ( mkNamePprCtxForModule
  , mkShowSub
  ) where

import Data.Maybe (fromJust)
import GHC (Ghc, Module, ModuleInfo, NamePprCtx)
import GHC qualified
import GHC.Iface.Syntax (AltPpr (..), ShowForAllFlag (..), ShowHowMuch (..), ShowSub (..))

mkNamePprCtxForModule :: Module -> ModuleInfo -> Ghc NamePprCtx
mkNamePprCtxForModule _ mod_info = fromJust <$> GHC.mkNamePprCtxForModule mod_info

mkShowSub :: ModuleInfo -> ShowSub
mkShowSub _ =
  let ss_how_much = ShowSome [] (AltPpr Nothing)
   in ShowSub
        { ss_how_much = ss_how_much
        , ss_forall = ShowForAllMust
        }
