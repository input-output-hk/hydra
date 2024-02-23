module Hydra.API.SubscriptionSpec where

import Hydra.Prelude
import Test.Hydra.Prelude hiding (shouldReturn)

import Test.QuickCheck (Positive (..))
import Test.Util (shouldReturn, shouldRunInSim)
import Control.Monad.Class.MonadAsync (wait, replicateConcurrently)
import Hydra.API.Subscription (setupSubscriptions)

spec :: Spec
spec =
  describe "setupSubscriptions" $
    prop "invokes all subscribers on new output" $ \(value :: Int) (Positive nSubscribers) -> do
      shouldRunInSim $ do
        (subscribe, updateSubscriptions) <- setupSubscriptions

        let subscribers = replicateConcurrently nSubscribers $ do
              getNext <- subscribe
              atomically getNext `shouldReturn` value

        -- Expect subscribers should block on no data being available
        timeout 1 subscribers `shouldReturn` Nothing

        -- Expect unblocking upon providing new data
        failAfter 2 $ do
          withAsync subscribers $ \asyncWaiters -> do
            -- NOTE: There is a small acceptable race condition between
            -- starting to listen for values and being notified through
            -- updateSubscriptions.
            threadDelay 0.001
            atomically (updateSubscriptions value)
            wait asyncWaiters `shouldReturn` replicate nSubscribers ()
