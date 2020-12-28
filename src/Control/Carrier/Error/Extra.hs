{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase     #-}

module Control.Carrier.Error.Extra
  ( rethrowing
  , catching
  ) where


import Control.Algebra
    ( Has )
import Control.Carrier.Error.Either
    ( ErrorC, runError )
import Control.Effect.Throw
    ( Throw, throwError )


-- | Remaps an error thrown by an action.
rethrowing
  :: forall e1 e2 m a sig.
     ( Has (Throw e2) sig m )
  => (e1 -> e2)
  -> ErrorC e1 m a
  -> m a
rethrowing f action = runError action >>= \case
  Left err -> throwError (f err)
  Right a  -> return a


-- | Handles an error thrown by an action.
catching
  :: Monad m
  => ErrorC e m a
  -> (e -> m a)
  -> m a
catching action handler = runError action >>= \case
  Left e  -> handler e
  Right a -> return a
