{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | CallStack with a nicer 'Show' instance
--
-- Use of this module is intended to /replace/ import of @GHC.Stack@
module System.FS.CallStack (
    prettyCallStack
    -- * opaque
  , PrettyCallStack
    -- * Re-exports
  , HasCallStack
  ) where

import           GHC.Stack (CallStack, HasCallStack)
import qualified GHC.Stack as GHC

{-------------------------------------------------------------------------------
  Auxiliary: CallStack with different Show instance
-------------------------------------------------------------------------------}

-- | CallStack with 'Show' instance using 'prettyCallStack'
newtype PrettyCallStack = PrettyCallStack CallStack

instance Show PrettyCallStack where
  show (PrettyCallStack cs) = GHC.prettyCallStack cs

prettyCallStack :: HasCallStack => PrettyCallStack
prettyCallStack = PrettyCallStack GHC.callStack
