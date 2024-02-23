{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Chain.Direct.Contract.Increment where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO as UTxO
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Contract.Gen (genForParty)
import Hydra.Chain.Direct.Fixture (testNetworkId, testPolicyId)
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.Tx (
  incrementTx,
  mkHeadId,
  mkHeadOutput,
 )
import Hydra.ContestationPeriod (ContestationPeriod)
import qualified Hydra.Contract.HeadState as Head
import Hydra.Ledger (IsTx (hashUTxO))
import Hydra.Ledger.Cardano (
  adaOnly,
  genTxOut,
  genUTxOAdaOnlyOfSize,
  genVerificationKey,
 )
import Hydra.Party (Party, partyToChain, vkey)
import Hydra.Plutus.Orphans ()
import Hydra.Snapshot (Snapshot (..))
import Hydra.Crypto (MultiSignature (..))
import PlutusTx.Builtins (toBuiltin)
import Test.QuickCheck (elements)
import Test.QuickCheck.Instances ()

healthyIncrementTx :: (Tx, UTxO)
healthyIncrementTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton (headInput, headOutput)
      <> registryUTxO scriptRegistry
      <> healthyUTxOToCommit

  tx =
    incrementTx
      scriptRegistry
      somePartyCardanoVerificationKey
      (mkHeadId testPolicyId)
      parameters
      (headInput, headOutput)
      healthySnapshot
      multisig

  multisig = HydraMultiSignature $ arbitrary `generateWith` 42

  parameters =
    HeadParameters
      { parties = healthyParties
      , contestationPeriod = healthyContestationPeriod
      }

  somePartyCardanoVerificationKey =
    elements healthyParticipants `generateWith` 42

  scriptRegistry = genScriptRegistry `generateWith` 42

  headInput = generateWith arbitrary 42

  headOutput' =
    mkHeadOutput testNetworkId testPolicyId (toUTxOContext $ mkTxOutDatumInline healthyDatum)

  headOutput = modifyTxOutValue (<> participationTokens) headOutput'

  participationTokens =
    valueFromList $
      map
        ( \party ->
            (AssetId testPolicyId (AssetName . serialiseToRawBytes . verificationKeyHash . vkey $ party), 1)
        )
        healthyParties

healthyUTxOToCommit :: UTxO
healthyUTxOToCommit =
  genUTxOAdaOnlyOfSize 2 `generateWith` 42

healthySnapshot :: Snapshot Tx
healthySnapshot =
  Snapshot
    { headId = mkHeadId testPolicyId
    , number = 1
    , utxo = healthyUTxO
    , confirmed = []
    , utxoToCommit = Just healthyUTxOToCommit
    , utxoToDecommit = Nothing
    }

healthyContestationPeriod :: ContestationPeriod
healthyContestationPeriod =
  arbitrary `generateWith` 42

healthyParticipants :: [VerificationKey PaymentKey]
healthyParticipants =
  genForParty genVerificationKey <$> healthyParties

healthyUTxO :: UTxO
healthyUTxO =
  adaOnly
    <$> generateWith
      (UTxO.fromPairs . (: []) <$> ((,) <$> genTxIn <*> genTxOut))
      42

healthyDatum :: Head.State
healthyDatum =
  Head.Open
    { utxoHash = toBuiltin $ hashUTxO @Tx healthyUTxO
    , parties =
        partyToChain <$> healthyParties
    , contestationPeriod = 10
    , headId = toPlutusCurrencySymbol testPolicyId
    }

healthyParties :: [Party]
healthyParties =
  [ generateWith arbitrary i | i <- [1 .. 3]
  ]
