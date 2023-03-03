{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Chain.Direct.Contract.Close where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO as UTxO
import Data.Maybe (fromJust)
import Hydra.Chain.Direct.Contract.Gen (genForParty, genHash, genMintedOrBurnedValue)
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  addParticipationTokens,
  changeHeadOutputDatum,
  changeMintedTokens,
  replaceContestationDeadline,
  replaceContesters,
  replaceHeadId,
  replaceParties,
  replacePolicyIdWith,
  replaceSnapshotNumber,
  replaceUtxoHash,
 )
import Hydra.Chain.Direct.Fixture (testNetworkId)
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.TimeHandle (PointInTime)
import Hydra.Chain.Direct.Tx (ClosingSnapshot (..), OpenThreadOutput (..), UTxOHash (UTxOHash), closeTx, mkHeadId, mkHeadOutput)
import Hydra.ContestationPeriod (fromChain)
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.Head (
  HeadError (
    ChangedParameters,
    ClosedWithNonInitialHash,
    ContestersNonEmpty,
    HasBoundedValidityCheckFailed,
    HeadValueIsNotPreserved,
    IncorrectClosedContestationDeadline,
    InfiniteLowerBound,
    InfiniteUpperBound,
    SignatureVerificationFailed,
    SignerIsNotAParticipant
  ),
 )
import qualified Hydra.Contract.HeadState as Head
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Contract.Util (UtilError (MintingOrBurningIsForbidden))
import Hydra.Crypto (HydraKey, MultiSignature, aggregate, sign, toPlutusSignatures)
import Hydra.Data.ContestationPeriod (posixFromUTCTime)
import qualified Hydra.Data.ContestationPeriod as OnChain
import qualified Hydra.Data.Party as OnChain
import Hydra.Ledger (hashUTxO)
import Hydra.Ledger.Cardano (genOneUTxOFor, genValue, genVerificationKey)
import Hydra.Ledger.Cardano.Evaluate (genValidityBoundsFromContestationPeriod)
import Hydra.Party (Party, deriveParty, partyToChain)
import Hydra.Snapshot (Snapshot (..), SnapshotNumber)
import Plutus.Orphans ()
import Plutus.V1.Ledger.Time (DiffMilliSeconds (..), fromMilliSeconds)
import Plutus.V2.Ledger.Api (BuiltinByteString, POSIXTime, PubKeyHash (PubKeyHash), toBuiltin, toData)
import Test.Hydra.Fixture (aliceSk, bobSk, carolSk)
import Test.QuickCheck (arbitrarySizedNatural, choose, elements, listOf1, oneof, suchThat)
import Test.QuickCheck.Instances ()

-- | Healthy close transaction for the generic case were we close a head
--   after one or more snapshot have been agreed upon between the members.
healthyCloseTx :: (Tx, UTxO)
healthyCloseTx =
  (tx, lookupUTxO)
 where
  tx =
    closeTx
      scriptRegistry
      somePartyCardanoVerificationKey
      closingSnapshot
      healthyCloseLowerBoundSlot
      healthyCloseUpperBoundPointInTime
      openThreadOutput
      (mkHeadId Fixture.testPolicyId)

  lookupUTxO =
    UTxO.singleton (healthyOpenHeadTxIn, healthyOpenHeadTxOut)
      <> registryUTxO scriptRegistry

  scriptRegistry = genScriptRegistry `generateWith` 42

  headDatum = fromPlutusData $ toData healthyOpenHeadDatum

  openThreadOutput =
    OpenThreadOutput
      { openThreadUTxO = (healthyOpenHeadTxIn, healthyOpenHeadTxOut, headDatum)
      , openParties = healthyOnChainParties
      , openContestationPeriod = healthyContestationPeriod
      }
  closingSnapshot :: ClosingSnapshot
  closingSnapshot =
    CloseWithConfirmedSnapshot
      { snapshotNumber = healthyCloseSnapshotNumber
      , closeUtxoHash = UTxOHash $ hashUTxO @Tx healthyCloseUTxO
      , signatures = healthySignature healthyCloseSnapshotNumber
      }

-- | Healthy close transaction for the specific case were we close a head
--   with the initial UtxO, that is, no snapshot have been agreed upon and
--   signed by the head members yet.
healthyCloseInitialTx :: (Tx, UTxO)
healthyCloseInitialTx =
  (tx, lookupUTxO)
 where
  tx =
    closeTx
      scriptRegistry
      somePartyCardanoVerificationKey
      closingSnapshot
      healthyCloseLowerBoundSlot
      healthyCloseUpperBoundPointInTime
      openThreadOutput
      (mkHeadId Fixture.testPolicyId)

  lookupUTxO =
    UTxO.singleton (healthyOpenHeadTxIn, healthyOpenHeadTxOut)
      <> registryUTxO scriptRegistry

  scriptRegistry = genScriptRegistry `generateWith` 42

  headDatum = fromPlutusData $ toData healthyOpenHeadDatum

  openThreadOutput =
    OpenThreadOutput
      { openThreadUTxO = (healthyOpenHeadTxIn, healthyOpenHeadTxOut, headDatum)
      , openParties = healthyOnChainParties
      , openContestationPeriod = healthyContestationPeriod
      }
  closingSnapshot :: ClosingSnapshot
  closingSnapshot =
    CloseWithInitialSnapshot
      { openUtxoHash = UTxOHash $ hashUTxO @Tx healthyUTxO
      }

-- NOTE: We need to use the contestation period when generating start/end tx
-- validity slots/time since if tx validity bound difference is bigger than
-- contestation period our close validator will fail
healthyCloseLowerBoundSlot :: SlotNo
healthyCloseUpperBoundPointInTime :: PointInTime
(healthyCloseLowerBoundSlot, healthyCloseUpperBoundPointInTime) =
  genValidityBoundsFromContestationPeriod (fromChain healthyContestationPeriod) `generateWith` 42

healthyOpenHeadTxIn :: TxIn
healthyOpenHeadTxIn = generateWith arbitrary 42

healthyOpenHeadTxOut :: TxOut CtxUTxO
healthyOpenHeadTxOut =
  mkHeadOutput testNetworkId Fixture.testPolicyId headTxOutDatum
    & addParticipationTokens healthyParties
 where
  headTxOutDatum = toUTxOContext (mkTxOutDatum healthyOpenHeadDatum)

healthySnapshot :: Snapshot Tx
healthySnapshot =
  Snapshot
    { number = healthyCloseSnapshotNumber
    , utxo = healthyCloseUTxO
    , confirmed = []
    }

healthyCloseUTxO :: UTxO
healthyCloseUTxO =
  (genOneUTxOFor somePartyCardanoVerificationKey `suchThat` (/= healthyUTxO))
    `generateWith` 42

healthyCloseSnapshotNumber :: SnapshotNumber
healthyCloseSnapshotNumber = 1

healthyOpenHeadDatum :: Head.State
healthyOpenHeadDatum =
  Head.Open
    { parties = healthyOnChainParties
    , utxoHash = toBuiltin $ hashUTxO @Tx healthyUTxO
    , contestationPeriod = healthyContestationPeriod
    , headId = toPlutusCurrencySymbol Fixture.testPolicyId
    }

healthyContestationPeriod :: OnChain.ContestationPeriod
healthyContestationPeriod = OnChain.contestationPeriodFromDiffTime $ fromInteger healthyContestationPeriodSeconds

healthyContestationPeriodSeconds :: Integer
healthyContestationPeriodSeconds = 10

healthyUTxO :: UTxO
healthyUTxO = genOneUTxOFor somePartyCardanoVerificationKey `generateWith` 42

somePartyCardanoVerificationKey :: VerificationKey PaymentKey
somePartyCardanoVerificationKey = flip generateWith 42 $ do
  genForParty genVerificationKey <$> elements healthyParties

healthySigningKeys :: [SigningKey HydraKey]
healthySigningKeys = [aliceSk, bobSk, carolSk]

healthyParties :: [Party]
healthyParties = deriveParty <$> healthySigningKeys

healthyOnChainParties :: [OnChain.Party]
healthyOnChainParties = partyToChain <$> healthyParties

healthySignature :: SnapshotNumber -> MultiSignature (Snapshot Tx)
healthySignature number = aggregate [sign sk snapshot | sk <- healthySigningKeys]
 where
  snapshot = healthySnapshot{number}

healthyContestationDeadline :: UTCTime
healthyContestationDeadline =
  addUTCTime
    (fromInteger healthyContestationPeriodSeconds)
    (snd healthyCloseUpperBoundPointInTime)

healthyClosedUTxOHash :: BuiltinByteString
healthyClosedUTxOHash =
  toBuiltin $ hashUTxO @Tx healthyClosedUTxO

healthyClosedUTxO :: UTxO
healthyClosedUTxO =
  genOneUTxOFor somePartyCardanoVerificationKey `generateWith` 42

data CloseMutation
  = -- | Makes the tx `snapshot signature` invalid by changing the redeemer signature but not the snapshot number in resulting head output.
    -- This ensures the snapshot signature is multisigned by all valid Head participants.
    MutateSignatureButNotSnapshotNumber
  | -- | Makes the tx `snapshot signature` invalid by changing the snapshot number in resulting head output but not the redeemer signature.
    MutateSnapshotNumberButNotSignature
  | -- | Makes the tx `utxo hash` invalid by changing the snapshot number in resulting head output to be a non-initual utxo (when snapshot number <= 0).
    MutateSnapshotNumberToLessThanZero
  | -- | Makes the tx `snapshot signature` invalid by changing the parties in the head input (stored state).
    MutateParties
  | -- | Makes the tx `signer` invalid by using a signer not present in the list of distributed PTs.
    -- This ensures that it's performed by a Head party.
    MutateRequiredSigner
  | -- | Makes the tx `snapshot signature` invalid by changing the utxo hash in resulting head output.
    -- This ensures the output state is consistent with the redeemer.
    MutateCloseUTxOHash
  | -- | Makes the tx resulting `head output` invalid by changing its parties to be different from the head input (stored state).
    MutatePartiesInOutput
  | -- | Makes the tx resulting `head output` invalid by changing its head id to be different from the head input (stored state).
    MutateHeadIdInOutput
  | -- | Makes the tx `validity range` invalid by changing its lower bound to be non finite.
    MutateInfiniteLowerBound
  | -- | Makes the tx `validity range` invalid by changing its upper bound to be non finite.
    MutateInfiniteUpperBound
  | -- | Makes the tx resulting `head output` invalid by changing its contestation deadline to not satisfy `contestationDeadline = upperBound + contestationPeriod`.
    MutateContestationDeadline
  | -- | Makes the tx `validity range` invalid by changing its lower and upper bound to be not bounded as per spec `upperBound - lowerBound <= contestationPeriod`.
    -- This also changes the resulting `head output` contestation deadline to be valid, so it satisfy `contestationDeadline = upperBound + contestationPeriod`.
    MutateValidityInterval
  | -- | Change the head policy id to simulate contestation using a ST and signer from a different head.
    -- The signer shows a correct signature but from a different head.
    -- This will cause the signer to not be present in the participation tokens.
    ContestFromDifferentHead
  | -- | Makes the tx `output minted values` invalid by changing them to include burning/minting of tokens.
    -- Minting or burning of the tokens should not be possible in v_head apart from 'checkAbort' or 'checkFanout'.
    MutateTokenMintingOrBurning
  | -- | Makes the tx resulting `head output` invalid by changing its contesters to be non empty.
    MutateContesters
  | -- | Makes the tx `output values` invalid by changing them arbitrarly to be differnet (not preserved) from the head.
    MutateValueInOutput
  deriving (Generic, Show, Enum, Bounded)

genCloseMutation :: (Tx, UTxO) -> Gen SomeMutation
genCloseMutation (tx, _utxo) =
  oneof
    [ SomeMutation (Just $ toErrorCode SignatureVerificationFailed) MutateSignatureButNotSnapshotNumber . ChangeHeadRedeemer <$> do
        Head.Close . toPlutusSignatures <$> (arbitrary :: Gen (MultiSignature (Snapshot Tx)))
    , SomeMutation (Just $ toErrorCode ClosedWithNonInitialHash) MutateSnapshotNumberToLessThanZero <$> do
        mutatedSnapshotNumber <- arbitrary `suchThat` (<= 0)
        pure $ ChangeOutput 0 $ changeHeadOutputDatum (replaceSnapshotNumber mutatedSnapshotNumber) headTxOut
    , SomeMutation (Just $ toErrorCode SignatureVerificationFailed) MutateSnapshotNumberButNotSignature <$> do
        mutatedSnapshotNumber <- arbitrarySizedNatural `suchThat` (> healthyCloseSnapshotNumber)
        pure $ ChangeOutput 0 $ changeHeadOutputDatum (replaceSnapshotNumber $ toInteger mutatedSnapshotNumber) headTxOut
    , SomeMutation (Just $ toErrorCode SignatureVerificationFailed) MutateParties . ChangeInputHeadDatum <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
        pure $
          Head.Open
            { parties = mutatedParties
            , utxoHash = ""
            , contestationPeriod = healthyContestationPeriod
            , headId = toPlutusCurrencySymbol Fixture.testPolicyId
            }
    , SomeMutation (Just $ toErrorCode ChangedParameters) MutatePartiesInOutput <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
        pure $ ChangeOutput 0 $ changeHeadOutputDatum (replaceParties mutatedParties) headTxOut
    , SomeMutation (Just $ toErrorCode ChangedParameters) MutateHeadIdInOutput <$> do
        otherHeadId <- toPlutusCurrencySymbol . headPolicyId <$> arbitrary `suchThat` (/= Fixture.testSeedInput)
        pure $ ChangeOutput 0 $ changeHeadOutputDatum (replaceHeadId otherHeadId) headTxOut
    , SomeMutation (Just $ toErrorCode SignerIsNotAParticipant) MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey `suchThat` (/= somePartyCardanoVerificationKey)
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation (Just $ toErrorCode SignatureVerificationFailed) MutateCloseUTxOHash . ChangeOutput 0 <$> do
        mutatedUTxOHash <- genHash `suchThat` ((/= healthyClosedUTxOHash) . toBuiltin)
        pure $ changeHeadOutputDatum (replaceUtxoHash $ toBuiltin mutatedUTxOHash) headTxOut
    , SomeMutation (Just $ toErrorCode IncorrectClosedContestationDeadline) MutateContestationDeadline <$> do
        mutatedDeadline <- genMutatedDeadline
        pure $ ChangeOutput 0 $ changeHeadOutputDatum (replaceContestationDeadline mutatedDeadline) headTxOut
    , SomeMutation (Just $ toErrorCode InfiniteLowerBound) MutateInfiniteLowerBound . ChangeValidityLowerBound <$> do
        pure TxValidityNoLowerBound
    , SomeMutation (Just $ toErrorCode InfiniteUpperBound) MutateInfiniteUpperBound . ChangeValidityUpperBound <$> do
        pure TxValidityNoUpperBound
    , SomeMutation (Just $ toErrorCode HasBoundedValidityCheckFailed) MutateValidityInterval <$> do
        (lowerSlotNo, upperSlotNo, adjustedContestationDeadline) <- genOversizedTransactionValidity
        pure $
          Changes
            [ ChangeValidityInterval (TxValidityLowerBound lowerSlotNo, TxValidityUpperBound upperSlotNo)
            , ChangeOutput 0 $ changeHeadOutputDatum (replaceContestationDeadline adjustedContestationDeadline) headTxOut
            ]
    , -- XXX: This is a bit confusing and not giving much value. Maybe we can remove this.
      -- This also seems to be covered by MutateRequiredSigner
      SomeMutation (Just $ toErrorCode SignerIsNotAParticipant) ContestFromDifferentHead <$> do
        otherHeadId <- headPolicyId <$> arbitrary `suchThat` (/= Fixture.testSeedInput)
        pure $
          Changes
            [ ChangeOutput 0 (replacePolicyIdWith Fixture.testPolicyId otherHeadId headTxOut)
            , ChangeInput
                healthyOpenHeadTxIn
                (replacePolicyIdWith Fixture.testPolicyId otherHeadId healthyOpenHeadTxOut)
                ( Just $
                    toScriptData
                      ( Head.Close
                          { signature =
                              toPlutusSignatures $
                                healthySignature healthyCloseSnapshotNumber
                          }
                      )
                )
            ]
    , SomeMutation (Just $ toErrorCode MintingOrBurningIsForbidden) MutateTokenMintingOrBurning
        <$> (changeMintedTokens tx =<< genMintedOrBurnedValue)
    , SomeMutation (Just $ toErrorCode ContestersNonEmpty) MutateContesters . ChangeOutput 0 <$> do
        mutatedContesters <- listOf1 $ PubKeyHash . toBuiltin <$> genHash
        pure $ headTxOut & changeHeadOutputDatum (replaceContesters mutatedContesters)
    , SomeMutation (Just $ toErrorCode HeadValueIsNotPreserved) MutateValueInOutput <$> do
        newValue <- genValue
        pure $ ChangeOutput 0 (headTxOut{txOutValue = newValue})
    ]
 where
  genOversizedTransactionValidity = do
    -- Implicit hypotheses: the slot length is and has always been 1 seconds so we can add slot with seconds
    lowerValidityBound <- arbitrary :: Gen Word64
    upperValidityBound <- choose (lowerValidityBound + fromIntegral healthyContestationPeriodSeconds, maxBound)
    let adjustedContestationDeadline =
          fromMilliSeconds . DiffMilliSeconds $ (healthyContestationPeriodSeconds + fromIntegral upperValidityBound) * 1000
    pure (SlotNo lowerValidityBound, SlotNo upperValidityBound, adjustedContestationDeadline)

  headTxOut = fromJust $ txOuts' tx !!? 0

data CloseInitialMutation
  = MutateCloseContestationDeadline'
  deriving (Generic, Show, Enum, Bounded)

-- | Mutations for the specific case of closing with the intial state.
-- We should probably validate all the mutation to this initial state but at
-- least we keep this regression test as we stumbled upon problems with the following case.
-- The nice thing to do would probably to generate either "normal" healthyCloseTx or
-- or healthyCloseInitialTx and apply all the mutation to it but we did'nt manage to do that
-- rightaway.
genCloseInitialMutation :: (Tx, UTxO) -> Gen SomeMutation
genCloseInitialMutation (tx, _utxo) =
  oneof
    [ SomeMutation (Just $ toErrorCode IncorrectClosedContestationDeadline) MutateCloseContestationDeadline' <$> do
        mutatedDeadline <- genMutatedDeadline
        pure $ ChangeOutput 0 $ changeHeadOutputDatum (replaceContestationDeadline mutatedDeadline) headTxOut
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0

-- | Generate not acceptable, but interesting deadlines.
genMutatedDeadline :: Gen POSIXTime
genMutatedDeadline = do
  oneof
    [ valuesAroundZero
    , valuesAroundDeadline
    ]
 where
  valuesAroundZero = arbitrary `suchThat` (/= deadline)

  valuesAroundDeadline = arbitrary `suchThat` (/= 0) <&> (+ deadline)

  deadline = posixFromUTCTime healthyContestationDeadline
