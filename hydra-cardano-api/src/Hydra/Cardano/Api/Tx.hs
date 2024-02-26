module Hydra.Cardano.Api.Tx where

import Hydra.Cardano.Api.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Api (
  EraTx (mkBasicTx),
  inputsTxBodyL,
  mkBasicTxBody,
 )
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Coin (Coin (..))
import Control.Lens ((&), (.~))
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import Data.Set qualified as Set
import Hydra.Cardano.Api.TxIn (mkTxIn, toLedgerTxIn)

-- | Sign transaction using the provided secret key
-- It only works for tx not containing scripts.
-- You can't sign a script utxo with this.
signTx ::
  IsShelleyBasedEra era =>
  SigningKey PaymentKey ->
  Tx era ->
  Tx era
signTx signingKey (Tx body wits) =
  makeSignedTransaction (witness : wits) body
 where
  witness = makeShelleyKeyWitness shelleyBasedEra body (WitnessPaymentKey signingKey)

-- | Create a transaction spending all given `UTxO`.
txSpendingUTxO :: UTxO -> Tx Era
txSpendingUTxO utxo =
  fromLedgerTx $
    mkBasicTx
      ( mkBasicTxBody
          & inputsTxBodyL .~ (toLedgerTxIn `Set.map` inputs)
      )
 where
  inputs = UTxO.inputSet utxo

-- | Get the UTxO that are produced by some transaction.
-- XXX: Defined here to avoid cyclic module dependency
utxoProducedByTx :: Tx Era -> UTxO
utxoProducedByTx tx =
  UTxO.fromPairs $
    zip [0 ..] (txOuts body)
      <&> bimap (mkTxIn tx) toCtxUTxOTxOut
 where
  TxBody body = getTxBody tx

-- | Get explicit fees allocated to a transaction.
txFee' :: Tx era -> Coin
txFee' (getTxBody -> TxBody body) =
  case txFee body of
    TxFeeExplicit _ y -> y

-- * Type Conversions

-- | Convert a cardano-api 'Tx' into a matching cardano-ledger 'Tx'.
toLedgerTx ::
  Tx era ->
  Ledger.Tx (ShelleyLedgerEra era)
toLedgerTx (ShelleyTx _era tx) = tx

-- | Convert a cardano-ledger's 'Tx' in the Babbage era into a cardano-api 'Tx'.
fromLedgerTx ::
  IsShelleyBasedEra era =>
  Ledger.Tx (ShelleyLedgerEra era) ->
  Tx era
fromLedgerTx =
  ShelleyTx shelleyBasedEra
