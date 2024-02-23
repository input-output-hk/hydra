-- | A subscription mechanism to decouple the producer and consumer of some data.
module Hydra.API.Subscription where

import Hydra.Prelude hiding (TVar, readTVar, seq)

import Control.Concurrent.Class.MonadSTM (dupTChan, newBroadcastTChanIO, readTChan, writeTChan)

type Subscription m a = STM m a

type SubscribeTo m a = m (Subscription m a)

type UpdateSubscriptions m a = a -> STM m ()

-- | Setup a subscription mechanism, that can be used to subscribe to some data
-- that is provided in a different place.
setupSubscriptions :: MonadSTM m => m (SubscribeTo m a, UpdateSubscriptions m a)
setupSubscriptions = do
  ch <- newBroadcastTChanIO
  pure (subscribe ch, writeTChan ch)
 where
  subscribe ch = do
    sub <- atomically (dupTChan ch)
    pure $ readTChan sub

-- | Subscribe to updates and wait for some specific value.
waitFor :: MonadSTM m => Subscription m a -> (a -> Maybe b) -> m b
waitFor getNext f = do
  atomically $ untilJustM (f <$> getNext)

-- | Keep running an operation until it becomes a 'Just', then return the value
--   inside the 'Just' as the result of the overall loop.
-- TODO: vendored from 'extra'
untilJustM :: Monad m => m (Maybe a) -> m a
untilJustM act = do
  res <- act
  case res of
    Just r -> pure r
    Nothing -> untilJustM act
