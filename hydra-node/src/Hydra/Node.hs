{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Top-level module to run a single Hydra node.
--
-- Checkout [Hydra
-- Documentation](https://hydra.family/head-protocol/core-concepts/architecture)
-- for some details about the overall architecture of the `Node`.
module Hydra.Node where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  MonadLabelledSTM,
  labelTVarIO,
  newTVarIO,
  stateTVar,
 )
import Control.Monad.Trans.Writer (execWriter, tell)
import Hydra.API.Server (Server, sendOutput)
import Hydra.Cardano.Api (AsType (AsPaymentKey, AsSigningKey, AsVerificationKey), getVerificationKey)
import Hydra.Chain (
  Chain (..),
  ChainStateHistory,
  ChainStateType,
  HeadParameters (..),
  IsChainState,
  PostTxError,
 )
import Hydra.Chain.Direct.Tx (verificationKeyToOnChainId)
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.Crypto (AsType (AsHydraKey))
import Hydra.HeadLogic (
  Effect (..),
  Environment (..),
  Event (..),
  HeadState (..),
  IdleState (..),
  Outcome (..),
  aggregateState,
  collectEffects,
  defaultTTL,
  recoverChainStateHistory,
  recoverState,
 )
import Hydra.HeadLogic qualified as Logic
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State (getHeadParameters)
import Hydra.Ledger (IsTx (), Ledger)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message)
import Hydra.Node.EventQueue (EventQueue (..), Queued (..))
import Hydra.Node.ParameterMismatch (ParamMismatch (..), ParameterMismatch (..))
import Hydra.Options (ChainConfig (..), DirectChainConfig (..), RunOptions (..), defaultContestationPeriod)
import Hydra.Party (Party (..), deriveParty)
import Hydra.Persistence (PersistenceIncremental (..), NewPersistenceIncremental(..), EventSource(..), EventSink(..), putEventToSinks, putEventsToSinks, NewPersistenceIncremental)

-- * Environment Handling

-- | Intialize the 'Environment' from command line options.
initEnvironment :: RunOptions -> IO Environment
initEnvironment options = do
  sk <- readFileTextEnvelopeThrow (AsSigningKey AsHydraKey) hydraSigningKey
  otherParties <- mapM loadParty hydraVerificationKeys
  participants <- getParticipants
  pure $
    Environment
      { party = deriveParty sk
      , signingKey = sk
      , otherParties
      , participants
      , contestationPeriod
      }
 where
  -- XXX: This is mostly a cardano-specific initialization step of loading
  -- --cardano-verification-key options and deriving 'OnChainId's from it. We should be able to call out to the various chain layer
  getParticipants =
    case chainConfig of
      Offline{} -> pure []
      Direct
        DirectChainConfig
          { cardanoVerificationKeys
          , cardanoSigningKey
          } -> do
          ownSigningKey <- readFileTextEnvelopeThrow (AsSigningKey AsPaymentKey) cardanoSigningKey
          otherVerificationKeys <- mapM (readFileTextEnvelopeThrow (AsVerificationKey AsPaymentKey)) cardanoVerificationKeys
          pure $ verificationKeyToOnChainId <$> (getVerificationKey ownSigningKey : otherVerificationKeys)

  contestationPeriod = case chainConfig of
    Offline{} -> defaultContestationPeriod
    Direct DirectChainConfig{contestationPeriod = cp} -> cp

  loadParty p =
    Party <$> readFileTextEnvelopeThrow (AsVerificationKey AsHydraKey) p

  RunOptions
    { hydraSigningKey
    , hydraVerificationKeys
    , chainConfig
    } = options

-- | Checks that command line options match a given 'HeadState'. This function
-- takes 'Environment' because it is derived from 'RunOptions' via
-- 'initEnvironment'.
--
-- Throws: 'ParameterMismatch' when state not matching the environment.
checkHeadState ::
  MonadThrow m =>
  Tracer m (HydraNodeLog tx) ->
  Environment ->
  HeadState tx ->
  m ()
checkHeadState tracer env headState = do
  unless (null paramsMismatch) $ do
    traceWith tracer (Misconfiguration paramsMismatch)
    throwIO $ ParameterMismatch paramsMismatch
 where
  paramsMismatch =
    maybe [] validateParameters $ getHeadParameters headState

  validateParameters HeadParameters{contestationPeriod = loadedCp, parties} =
    execWriter $ do
      when (loadedCp /= configuredCp) $
        tell [ContestationPeriodMismatch{loadedCp, configuredCp}]

      let loadedParties = sort parties
          configuredParties = sort (party : otherParties)
      when (loadedParties /= configuredParties) $
        tell [PartiesMismatch{loadedParties, configuredParties}]

  Environment{contestationPeriod = configuredCp, otherParties, party} = env

-- ** Create and run a hydra node

-- | Main handle of a hydra node where all layers are tied together.
data HydraNode tx m = HydraNode
  { eq :: EventQueue m (Event tx)
  , hn :: Network m (Message tx)
  , nodeState :: NodeState tx m
  , oc :: Chain tx m
  , server :: Server tx m
  , ledger :: Ledger tx
  , env :: Environment
  , persistence :: NewPersistenceIncremental (StateChanged tx) m
  -- , latestStateChangeId :: TVar m Word64
  -- , eventSource :: EventSource (StateChanged tx) m
  -- , eventSinks :: NonEmpty (EventSink (StateChanged tx) m)
  --FIXME(Elaine): bundle eventSource,Sinks, latestStateChangeId into a single type for convenience?
  -- they should still definitely be separable too
  }

data HydraNodeLog tx
  = BeginEvent {by :: Party, eventId :: Word64, event :: Event tx}
  | EndEvent {by :: Party, eventId :: Word64}
  | BeginEffect {by :: Party, eventId :: Word64, effectId :: Word32, effect :: Effect tx}
  | EndEffect {by :: Party, eventId :: Word64, effectId :: Word32}
  | LogicOutcome {by :: Party, outcome :: Outcome tx}
  | LoadedState {numberOfEvents :: Word64}
  | Misconfiguration {misconfigurationErrors :: [ParamMismatch]}
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (HydraNodeLog tx)
deriving stock instance IsChainState tx => Show (HydraNodeLog tx)
deriving anyclass instance IsChainState tx => ToJSON (HydraNodeLog tx)
deriving anyclass instance IsChainState tx => FromJSON (HydraNodeLog tx)

instance (IsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (HydraNodeLog tx) where
  arbitrary = genericArbitrary
  shrink = genericShrink

runHydraNode ::
  ( MonadCatch m
  , MonadAsync m
  , IsChainState tx
  ) =>
  Tracer m (HydraNodeLog tx) ->
  HydraNode tx m ->
  m ()
runHydraNode tracer node =
  -- NOTE(SN): here we could introduce concurrent head processing, e.g. with
  -- something like 'forM_ [0..1] $ async'
  forever $ stepHydraNode tracer node

stepHydraNode ::
  ( MonadCatch m
  , MonadAsync m
  , IsChainState tx
  ) =>
  Tracer m (HydraNodeLog tx) ->
  HydraNode tx m ->
  m ()
stepHydraNode tracer node = do
  e@Queued{eventId, queuedEvent} <- nextEvent eq
  traceWith tracer $ BeginEvent{by = party, eventId, event = queuedEvent}
  outcome <- atomically (processNextEvent node queuedEvent)
  traceWith tracer (LogicOutcome party outcome)
  handleOutcome e outcome
  processEffects node tracer eventId outcome
  traceWith tracer EndEvent{by = party, eventId}
 where
  handleOutcome e = \case
    Error _ -> pure ()
    Wait _reason -> putEventAfter eq waitDelay (decreaseTTL e)
    StateChanged sc -> putEventToSinks eventSinks sc
    Effects _ -> pure ()
    Combined l r -> handleOutcome e l >> handleOutcome e r

  decreaseTTL =
    \case
      -- XXX: this is smelly, handle wait re-enqueing differently
      Queued{eventId, queuedEvent = NetworkEvent ttl aParty msg}
        | ttl > 0 -> Queued{eventId, queuedEvent = NetworkEvent (ttl - 1) aParty msg}
      e -> e

  Environment{party} = env

  HydraNode{eq, env, persistence = NewPersistenceIncremental{eventSinks}} = node

-- | The time to wait between re-enqueuing a 'Wait' outcome from 'HeadLogic'.
waitDelay :: DiffTime
waitDelay = 0.1

-- | Monadic interface around 'Hydra.Logic.update'.
processNextEvent ::
  IsChainState tx =>
  HydraNode tx m ->
  Event tx ->
  STM m (Outcome tx)
processNextEvent HydraNode{nodeState, ledger, env} e =
  modifyHeadState $ \s ->
    let outcome = computeOutcome s e
     in (outcome, aggregateState s outcome)
 where
  NodeState{modifyHeadState} = nodeState

  computeOutcome = Logic.update env ledger

processEffects ::
  ( MonadAsync m
  , MonadCatch m
  , IsChainState tx
  ) =>
  HydraNode tx m ->
  Tracer m (HydraNodeLog tx) ->
  Word64 ->
  Outcome tx ->
  m ()
processEffects HydraNode{hn, oc = Chain{postTx}, server, eq, env = Environment{party}} tracer eventId outcome = do
  mapM_ processEffect $ zip (collectEffects outcome) [0 ..]
 where
  processEffect (effect, effectId) = do
    traceWith tracer $ BeginEffect party eventId effectId effect
    case effect of
      ClientEffect i -> sendOutput server i
      NetworkEffect msg -> broadcast hn msg >> putEvent eq (NetworkEvent defaultTTL party msg)
      OnChainEffect{postChainTx} ->
        postTx postChainTx
          `catch` \(postTxError :: PostTxError tx) ->
            putEvent eq $ PostTxError{postChainTx, postTxError}
    traceWith tracer $ EndEffect party eventId effectId

-- ** Manage state

-- | Handle to access and modify the state in the Hydra Node.
data NodeState tx m = NodeState
  { modifyHeadState :: forall a. (HeadState tx -> (a, HeadState tx)) -> STM m a
  , queryHeadState :: STM m (HeadState tx)
  }

-- | Initialize a new 'NodeState'.
createNodeState :: MonadLabelledSTM m => HeadState tx -> m (NodeState tx m)
createNodeState initialState = do
  tv <- newTVarIO initialState
  labelTVarIO tv "node-state"
  pure
    NodeState
      { modifyHeadState = stateTVar tv
      , queryHeadState = readTVar tv
      }

-- | Load a 'HeadState' from persistence.
loadState ::
  (MonadThrow m, IsChainState tx) =>
  Tracer m (HydraNodeLog tx) ->
  EventSource (StateChanged tx) m ->
  ChainStateType tx ->
  m (HeadState tx, ChainStateHistory tx)
loadState tracer eventSource defaultChainState = do
  events <- getEvents' eventSource
  traceWith tracer LoadedState{numberOfEvents = fromIntegral $ length events}
  let headState = recoverState initialState events
      chainStateHistory = recoverChainStateHistory defaultChainState events
  pure (headState, chainStateHistory)
 where
  initialState = Idle IdleState{chainState = defaultChainState}

loadStateEventSource ::
  (MonadThrow m, MonadIO m, IsChainState tx) =>
  Tracer m (HydraNodeLog tx) ->
  EventSource (StateChanged tx) m ->
  [EventSink (StateChanged tx) m] ->
  ChainStateType tx ->
  m (HeadState tx, ChainStateHistory tx)
loadStateEventSource tracer eventSource eventSinks defaultChainState = do
  events <- getEvents' eventSource
  traceWith tracer LoadedState{numberOfEvents = fromIntegral $ length events}
  let headState = recoverState initialState events
      chainStateHistory = recoverChainStateHistory defaultChainState events
  -- deliver to sinks per spec, deduplication is handled by the sinks
  -- FIXME(Elaine): persistence currently not handling duplication, so this relies on not providing the eventSource's sink as an arg here
  case nonEmpty eventSinks of
    Nothing -> putStrLn "ELAINE: deduplicate events for disk persistence so we can get rid of this kludge"
    Just sinks' -> putEventsToSinks sinks' events
  pure (headState, chainStateHistory)
 where
  initialState = Idle IdleState{chainState = defaultChainState}