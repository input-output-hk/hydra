{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.Close.Healthy where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO qualified as UTxO
import Hydra.Chain.Direct.Contract.Mutation (
  addParticipationTokens,
 )
import Hydra.Chain.Direct.Fixture qualified as Fixture
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry, genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.State (splitUTxO)
import Hydra.Chain.Direct.TimeHandle (PointInTime)
import Hydra.Chain.Direct.Tx (OpenThreadOutput (..), closeTx, mkHeadId, mkHeadOutput)
import Hydra.ContestationPeriod (fromChain)
import Hydra.Contract.HeadState qualified as Head
import Hydra.Crypto (HydraKey, MultiSignature, aggregate, sign)
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Data.Party qualified as OnChain
import Hydra.Ledger (hashUTxO)
import Hydra.Ledger.Cardano (genOneUTxOFor, genVerificationKey)
import Hydra.Ledger.Cardano.Evaluate (genValidityBoundsFromContestationPeriod)
import Hydra.Party (Party, deriveParty, partyToChain)
import Hydra.Plutus.Orphans ()
import Hydra.Snapshot (ConfirmedSnapshot (..))
import Hydra.Snapshot as Snapshot (Snapshot (..), SnapshotNumber, SnapshotVersion)
import PlutusLedgerApi.V2 (BuiltinByteString, toBuiltin)
import Test.Hydra.Fixture (aliceSk, bobSk, carolSk, genForParty)
import Test.QuickCheck (elements)
import Test.QuickCheck.Instances ()

healthySeed :: Int
healthySeed = 42

healthyUTxO :: UTxO
healthyUTxO = genOneUTxOFor somePartyCardanoVerificationKey `generateWith` healthySeed

healthySplitUTxOInHead :: UTxO
healthySplitUTxOToDecommit :: UTxO
(healthySplitUTxOInHead, healthySplitUTxOToDecommit) = splitUTxO healthyUTxO

scriptRegistry :: ScriptRegistry
scriptRegistry = genScriptRegistry `generateWith` 42

-- NOTE: We need to use the contestation period when generating start/end tx
-- validity slots/time since if tx validity bound difference is bigger than
-- contestation period our close validator will fail
healthyCloseLowerBoundSlot :: SlotNo
healthyCloseUpperBoundPointInTime :: PointInTime
(healthyCloseLowerBoundSlot, healthyCloseUpperBoundPointInTime) =
  genValidityBoundsFromContestationPeriod (fromChain healthyContestationPeriod) `generateWith` healthySeed

healthyOpenHeadTxIn :: TxIn
healthyOpenHeadTxIn = generateWith arbitrary healthySeed

healthyOpenHeadTxOut :: TxOutDatum CtxUTxO -> TxOut CtxUTxO
healthyOpenHeadTxOut headTxOutDatum =
  mkHeadOutput Fixture.testNetworkId Fixture.testPolicyId headTxOutDatum
    & addParticipationTokens healthyParticipants

healthyContestationPeriodSeconds :: Integer
healthyContestationPeriodSeconds = 10

healthyContestationPeriod :: OnChain.ContestationPeriod
healthyContestationPeriod = OnChain.contestationPeriodFromDiffTime $ fromInteger healthyContestationPeriodSeconds

healthyContestationDeadline :: UTCTime
healthyContestationDeadline =
  addUTCTime
    (fromInteger healthyContestationPeriodSeconds)
    (snd healthyCloseUpperBoundPointInTime)

healthyCloseUTxOHash :: BuiltinByteString
healthyCloseUTxOHash =
  toBuiltin $ hashUTxO @Tx healthySplitUTxOInHead

healthyParticipants :: [VerificationKey PaymentKey]
healthyParticipants =
  genForParty genVerificationKey <$> healthyParties

somePartyCardanoVerificationKey :: VerificationKey PaymentKey
somePartyCardanoVerificationKey =
  elements healthyParticipants `generateWith` healthySeed

healthySigningKeys :: [SigningKey HydraKey]
healthySigningKeys = [aliceSk, bobSk, carolSk]

healthyParties :: [Party]
healthyParties = deriveParty <$> healthySigningKeys

healthyOnChainParties :: [OnChain.Party]
healthyOnChainParties = partyToChain <$> healthyParties

healthySignature :: Snapshot Tx -> MultiSignature (Snapshot Tx)
healthySignature snapshot = aggregate [sign sk snapshot | sk <- healthySigningKeys]

healthySnapshot :: SnapshotNumber -> SnapshotVersion -> Snapshot Tx
healthySnapshot number version =
  Snapshot
    { headId = mkHeadId Fixture.testPolicyId
    , version
    , number
    , confirmed = []
    , utxo = healthySplitUTxOInHead
    , utxoToDecommit = Just healthySplitUTxOToDecommit
    }

-- FIXME: check all usages of this
healthyOpenDatum :: Snapshot Tx -> Head.State
healthyOpenDatum Snapshot{version} =
  Head.Open
    Head.OpenDatum
      { parties = healthyOnChainParties
      , utxoHash = toBuiltin $ hashUTxO @Tx healthySplitUTxOInHead
      , contestationPeriod = healthyContestationPeriod
      , headId = toPlutusCurrencySymbol Fixture.testPolicyId
      , version = toInteger version
      }

healthyConfirmedSnapshot :: Snapshot Tx -> ConfirmedSnapshot Tx
healthyConfirmedSnapshot snapshot =
  ConfirmedSnapshot
    { snapshot
    , signatures = healthySignature snapshot
    }

healthyConfirmedClosingTx :: Snapshot Tx -> (Tx, UTxO)
healthyConfirmedClosingTx snapshot@Snapshot{version} =
  (tx, lookupUTxO)
 where
  tx :: Tx
  tx =
    closeTx
      scriptRegistry
      somePartyCardanoVerificationKey
      (mkHeadId Fixture.testPolicyId)
      version
      (healthyConfirmedSnapshot snapshot)
      healthyCloseLowerBoundSlot
      healthyCloseUpperBoundPointInTime
      openThreadOutput

  datum :: TxOutDatum CtxUTxO
  datum = toUTxOContext $ mkTxOutDatumInline $ healthyOpenDatum snapshot

  lookupUTxO :: UTxO' (TxOut CtxUTxO)
  lookupUTxO =
    UTxO.singleton (healthyOpenHeadTxIn, healthyOpenHeadTxOut datum)
      <> registryUTxO scriptRegistry

  openThreadOutput :: OpenThreadOutput
  openThreadOutput =
    OpenThreadOutput
      { openThreadUTxO = (healthyOpenHeadTxIn, healthyOpenHeadTxOut datum)
      , openParties = healthyOnChainParties
      , openContestationPeriod = healthyContestationPeriod
      }