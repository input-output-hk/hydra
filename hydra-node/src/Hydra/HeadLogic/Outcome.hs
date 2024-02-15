{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic.Outcome where

import Hydra.Prelude

import Hydra.API.ServerOutput (ServerOutput)
import Hydra.Chain (ChainStateType, HeadParameters, IsChainState, PostChainTx)
import Hydra.Crypto (MultiSignature, Signature)
import Hydra.HeadId (HeadId, HeadSeed)
import Hydra.HeadLogic.Error (LogicError)
import Hydra.HeadLogic.State (HeadState)
import Hydra.Ledger (ChainSlot, IsTx, TxIdType, UTxOType, ValidationError)
import Hydra.Network.Message (Message)
import Hydra.Party (Party)
import Hydra.Snapshot (Snapshot, SnapshotNumber)

-- | Analogous to events, the pure head logic "core" can have effects emited to
-- the "shell" layers and we distinguish the same: effects onto the client, the
-- network and the chain.
data Effect tx
  = -- | Effect to be handled by the "Hydra.API", results in sending this 'ServerOutput'.
    ClientEffect {serverOutput :: ServerOutput tx}
  | -- | Effect to be handled by a "Hydra.Network", results in a 'Hydra.Network.broadcast'.
    NetworkEffect {message :: Message tx}
  | -- | Effect to be handled by a "Hydra.Chain", results in a 'Hydra.Chain.postTx'.
    OnChainEffect {postChainTx :: PostChainTx tx}
  deriving stock (Generic)

deriving stock instance IsChainState tx => Eq (Effect tx)
deriving stock instance IsChainState tx => Show (Effect tx)
deriving anyclass instance IsChainState tx => ToJSON (Effect tx)
deriving anyclass instance IsChainState tx => FromJSON (Effect tx)

instance
  ( IsTx tx
  , Arbitrary (ChainStateType tx)
  ) =>
  Arbitrary (Effect tx)
  where
  arbitrary = genericArbitrary

-- | Head state changed event. These events represent all the internal state
-- changes, get persisted and processed in an event sourcing manner.
data StateChanged tx
  = HeadInitialized
      { parameters :: HeadParameters
      , chainState :: ChainStateType tx
      , headId :: HeadId
      , headSeed :: HeadSeed
      , stateChangeID :: Word64
      }
  | CommittedUTxO
      { party :: Party
      , committedUTxO :: UTxOType tx
      , chainState :: ChainStateType tx
      , stateChangeID :: Word64
      }
  | HeadAborted {chainState :: ChainStateType tx, stateChangeID :: Word64}
  | HeadOpened {chainState :: ChainStateType tx, initialUTxO :: UTxOType tx, stateChangeID :: Word64}
  | TransactionAppliedToLocalUTxO
      { tx :: tx
      , newLocalUTxO :: UTxOType tx
      , stateChangeID :: Word64
      }
  | SnapshotRequestDecided {snapshotNumber :: SnapshotNumber, stateChangeID :: Word64}
  | -- | A snapshot was requested by some party.
    -- NOTE: We deliberately already include an updated local ledger state to
    -- not need a ledger to interpret this event.
    SnapshotRequested
      { snapshot :: Snapshot tx
      , requestedTxIds :: [TxIdType tx]
      , newLocalUTxO :: UTxOType tx
      , newLocalTxs :: [tx]
      , stateChangeID :: Word64
      }
  | TransactionReceived {tx :: tx, stateChangeID :: Word64}
  | PartySignedSnapshot {snapshot :: Snapshot tx, party :: Party, signature :: Signature (Snapshot tx), stateChangeID :: Word64}
  | SnapshotConfirmed {snapshot :: Snapshot tx, signatures :: MultiSignature (Snapshot tx), stateChangeID :: Word64}
  | HeadClosed {chainState :: ChainStateType tx, contestationDeadline :: UTCTime, stateChangeID :: Word64}
  | HeadIsReadyToFanout {stateChangeID :: Word64}
  | HeadFannedOut {chainState :: ChainStateType tx, stateChangeID :: Word64}
  | ChainRolledBack {chainState :: ChainStateType tx, stateChangeID :: Word64}
  | TickObserved {chainSlot :: ChainSlot, stateChangeID :: Word64}
  deriving stock (Generic)

getStateChangeID :: StateChanged tx -> Word64
getStateChangeID = \case
  HeadInitialized{stateChangeID} -> stateChangeID
  CommittedUTxO{stateChangeID} -> stateChangeID
  HeadAborted{stateChangeID} -> stateChangeID
  HeadOpened{stateChangeID} -> stateChangeID
  TransactionAppliedToLocalUTxO{stateChangeID} -> stateChangeID
  SnapshotRequestDecided{stateChangeID} -> stateChangeID
  SnapshotRequested{stateChangeID} -> stateChangeID
  TransactionReceived{stateChangeID} -> stateChangeID
  PartySignedSnapshot{stateChangeID} -> stateChangeID
  SnapshotConfirmed{stateChangeID} -> stateChangeID
  HeadClosed{stateChangeID} -> stateChangeID
  HeadIsReadyToFanout{stateChangeID} -> stateChangeID
  HeadFannedOut{stateChangeID} -> stateChangeID
  ChainRolledBack{stateChangeID} -> stateChangeID
  TickObserved{stateChangeID} -> stateChangeID

--FIXME(Elaine): these stateChangeID fields were added in an attempt to make every StateChanged keep track of its ID
-- it's not clear how to handle the state for this. but for now the field is kept so that the type of putEvent' can be kept simple, and shouldn't do harm



instance (IsTx tx, Arbitrary (HeadState tx), Arbitrary (ChainStateType tx)) => Arbitrary (StateChanged tx) where
  arbitrary = genericArbitrary

deriving stock instance (IsTx tx, Eq (HeadState tx), Eq (ChainStateType tx)) => Eq (StateChanged tx)
deriving stock instance (IsTx tx, Show (HeadState tx), Show (ChainStateType tx)) => Show (StateChanged tx)
deriving anyclass instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (StateChanged tx)
deriving anyclass instance (IsTx tx, FromJSON (HeadState tx), FromJSON (ChainStateType tx)) => FromJSON (StateChanged tx)

data Outcome tx
  = Effects {effects :: [Effect tx]}
  | StateChanged {event :: StateChanged tx}
  | Wait {reason :: WaitReason tx}
  | Error {error :: LogicError tx}
  | Combined {left :: Outcome tx, right :: Outcome tx}
  deriving stock (Generic)

instance Semigroup (Outcome tx) where
  (<>) = Combined

deriving stock instance IsChainState tx => Eq (Outcome tx)
deriving stock instance IsChainState tx => Show (Outcome tx)
deriving anyclass instance IsChainState tx => ToJSON (Outcome tx)
deriving anyclass instance IsChainState tx => FromJSON (Outcome tx)

instance (IsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (Outcome tx) where
  arbitrary = genericArbitrary
  shrink = \case
    Combined l r -> [l, r] <> [Combined l' r' | l' <- shrink l, r' <- shrink r]
    other -> genericShrink other

collectEffects :: Outcome tx -> [Effect tx]
collectEffects = \case
  Error _ -> []
  Wait _ -> []
  StateChanged _ -> []
  Effects effs -> effs
  Combined l r -> collectEffects l <> collectEffects r

collectWaits :: Outcome tx -> [WaitReason tx]
collectWaits = \case
  Error _ -> []
  Wait w -> [w]
  StateChanged _ -> []
  Effects _ -> []
  Combined l r -> collectWaits l <> collectWaits r

data WaitReason tx
  = WaitOnNotApplicableTx {validationError :: ValidationError}
  | WaitOnSnapshotNumber {waitingFor :: SnapshotNumber}
  | WaitOnSeenSnapshot
  | WaitOnTxs {waitingForTxIds :: [TxIdType tx]}
  | WaitOnContestationDeadline
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (WaitReason tx)
deriving stock instance IsTx tx => Show (WaitReason tx)
deriving anyclass instance IsTx tx => ToJSON (WaitReason tx)
deriving anyclass instance IsTx tx => FromJSON (WaitReason tx)

instance IsTx tx => Arbitrary (WaitReason tx) where
  arbitrary = genericArbitrary
