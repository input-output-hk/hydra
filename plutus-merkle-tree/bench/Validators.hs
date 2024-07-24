{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
-- Plutus core version to compile to. Cardano protocol version 8 is only
-- supporting plutus-core version 1.0.0.
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Validators where

import PlutusTx.Prelude

import Hydra.Plutus.Extras (wrapValidator)
import Plutus.MerkleTree qualified as MT
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 (ScriptContext)
import PlutusTx qualified as Plutus

-- | A validator for measuring cost of MT membership validation.
merkleTreeMemberValidator :: SerialisedScript
merkleTreeMemberValidator =
  serialiseCompiledCode
    $$( Plutus.compile
          [||
          \ScriptContext{scriptRedeemer} ->
            let (e, root, proof) = unsafeFromBuiltinData $ getRedeemer scriptRedeemer
             in MT.member e root proof
          ||]
      )

-- | A validator for measuring cost of MT construction.
-- data MtBuilderValidator
merkleTreeBuilderValidator :: SerialisedScript
merkleTreeBuilderValidator =
  serialiseCompiledCode
    $$( Plutus.compile
          [||
          \ScriptContext{scriptRedeemer} ->
            let (utxos, root) = unsafeFromBuiltinData $ getRedeemer scriptRedeemer
             in MT.rootHash (MT.fromList utxos) == root
          ||]
      )
