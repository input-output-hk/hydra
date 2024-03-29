module Hydra.Chain.Offline where

import Hydra.Prelude

import Cardano.Api.Genesis (shelleyGenesisDefaults)
import Cardano.Api.GenesisParameters (fromShelleyGenesis)
import Cardano.Ledger.Slot (unSlotNo)
import Cardano.Slotting.Time (SystemStart (SystemStart), mkSlotLength)
import Control.Monad.Class.MonadAsync (link)
import Hydra.Cardano.Api (GenesisParameters (..), ShelleyEra, ShelleyGenesis (..), StandardCrypto, Tx)
import Hydra.Chain (
  Chain (..),
  ChainComponent,
  ChainEvent (..),
  ChainStateHistory,
  HeadParameters (..),
  OnChainTx (..),
  PostTxError (..),
  chainSlot,
  chainTime,
  initHistory,
 )
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.HeadId (HeadId (..), HeadSeed (..))
import Hydra.Ledger (ChainSlot (ChainSlot))
import Hydra.Ledger.Cardano.Configuration (readJsonFileThrow)
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime, slotNoToUTCTime)
import Hydra.Options (OfflineChainConfig (..), defaultContestationPeriod)
import Hydra.Party (Party)

-- | Hard-coded 'HeadId' for all offline head instances.
offlineHeadId :: HeadId
offlineHeadId = UnsafeHeadId "offline"

-- | Hard-coded 'HeadSeed' for all offline head instances.
offlineHeadSeed :: HeadSeed
offlineHeadSeed = UnsafeHeadSeed "offline"

-- | Load the given genesis file or use defaults specific to the offline mode.
loadGenesisFile :: Maybe FilePath -> IO (GenesisParameters ShelleyEra)
loadGenesisFile ledgerGenesisFile =
  -- TODO: uses internal cardano-api lib
  fromShelleyGenesis
    <$> case ledgerGenesisFile of
      Nothing -> do
        now <- getCurrentTime
        -- TODO: uses internal cardano-api lib
        pure shelleyGenesisDefaults{sgSystemStart = now}
      Just fp ->
        readJsonFileThrow (parseJSON @(ShelleyGenesis StandardCrypto)) fp

withOfflineChain ::
  OfflineChainConfig ->
  Party ->
  -- | Last known chain state as loaded from persistence.
  ChainStateHistory Tx ->
  ChainComponent Tx IO a
withOfflineChain OfflineChainConfig{ledgerGenesisFile, initialUTxOFile} party chainStateHistory callback action = do
  initializeOfflineHead chainStateHistory initialUTxOFile party callback
  genesis <- loadGenesisFile ledgerGenesisFile
  withAsync (tickForever genesis callback) $ \tickThread -> do
    link tickThread
    action chainHandle
 where
  chainHandle =
    Chain
      { submitTx = const $ pure ()
      , draftCommitTx = \_ _ -> pure $ Left FailedToDraftTxNotInitializing
      , postTx = const $ pure ()
      }

initializeOfflineHead ::
  ChainStateHistory Tx ->
  FilePath ->
  Party ->
  (ChainEvent Tx -> IO ()) ->
  IO ()
initializeOfflineHead chainStateHistory initialUTxOFile ownParty callback = do
  let emptyChainStateHistory = initHistory initialChainState

  -- if we don't have a chainStateHistory to restore from disk from, start a new one
  when (chainStateHistory == emptyChainStateHistory) $ do
    initialUTxO <- readJsonFileThrow parseJSON initialUTxOFile

    callback $
      Observation
        { newChainState = initialChainState
        , observedTx =
            OnInitTx
              { headId = offlineHeadId
              , headSeed = offlineHeadSeed
              , headParameters =
                  HeadParameters
                    { parties = [ownParty]
                    , -- NOTE: This is irrelevant in offline mode.
                      contestationPeriod = defaultContestationPeriod
                    }
              , participants = []
              }
        }
    callback $
      Observation
        { newChainState = initialChainState
        , observedTx =
            OnCommitTx
              { party = ownParty
              , committed = initialUTxO
              , headId = offlineHeadId
              }
        }
    callback $
      Observation
        { newChainState = initialChainState
        , observedTx = OnCollectComTx{headId = offlineHeadId}
        }

tickForever :: GenesisParameters ShelleyEra -> (ChainEvent Tx -> IO ()) -> IO ()
tickForever genesis callback = do
  initialSlot <- slotNoFromUTCTime systemStart slotLength <$> getCurrentTime
  traverse_ nextTick [initialSlot ..]
 where
  nextTick upcomingSlot = do
    let timeToSleepUntil = slotNoToUTCTime systemStart slotLength upcomingSlot
    sleepDelay <- diffUTCTime timeToSleepUntil <$> getCurrentTime
    threadDelay $ realToFrac sleepDelay
    callback $
      Tick
        { chainTime = timeToSleepUntil
        , chainSlot = ChainSlot . fromIntegral $ unSlotNo upcomingSlot
        }
  systemStart = SystemStart protocolParamSystemStart

  slotLength = mkSlotLength protocolParamSlotLength

  GenesisParameters
    { protocolParamSlotLength
    , protocolParamSystemStart
    } = genesis
