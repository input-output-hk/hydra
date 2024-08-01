# Protocol

Additional implementation-specific documentation for the Hydra Head protocol and extensions like incremental decommits.

### Incremental Commits
```mermaid
sequenceDiagram
    Note over Alice: Alice shows a tx spending on L1 of what she wants to commit to L2
    Alice->>+Node A: POST /commit (commitTx)

    Note over Node A: TODO input validation?

    Note over Node A: get approval through a signed snapshot
    par
        Node A->>Node A: ReqInc commitTx
    and
        Node A->>Node B: ReqInc commitTx
    end

    Node A -->> Alice: CommitRequested

    par Alice isLeader
        Node A->>Node A: ReqSn commitTx
    and
        Node A->>Node B: ReqSn commitTx
    end

    Node A->>Node A: sig = sign snapshot incl. inputs(commitTx)

    par broadcast
        Node A->>Node A: AckSn sig
    and
        Node A->>Node B: AckSn sig
    end
    Node B->>Node A: AckSn sig

    Node A ->>Node A: construct incrementTx using multi-signed snapshot and commitTx (blueprint)

    Node A-->>-Alice: incrementTx

    Alice ->> Alice: sign incrementTx
    Alice ->> Chain: submit incrementTx


    Chain ->> Node B: OnIncrementTx utxo
    Chain ->>+ Node A: OnIncrementTx utxo
    Node A ->> Node A: localUtxo = localUtxo + utxo
    Node A ->>- Alice: CommitFinalized

    Note over Alice: Alice can spend on L2 what she committed from L1
```


### Incremental Decommits

```mermaid
sequenceDiagram
    Alice->>+Node A: POST /decommit (decTx)
    Node A-->>-Alice: OK

    Node A->>Node A: canApply decTx

    par broadcast
        Node A->>Node A: ReqDec decTx
    and
        Node A->>Node B: ReqDec decTx
    end

    Node A -->> Alice: DecommitRequested

    par Alice isLeader
        Node A->>Node A: ReqSn decTx
    and
        Node A->>Node B: ReqSn decTx
    end

    Node A->>Node A: canApply decTx, decUTxO =  outputs(decTx)
    Node A->>Node A: sig = sign snapshot incl. decUTxO

    par broadcast
        Node A->>Node A: AckSn sig
    and
        Node A->>Node B: AckSn sig
    end

    Node B->>Node A: AckSn sig

    Node A -->> Alice: SnapshotConfirmed
    Node A -->> Alice: DecommitApproved

    Node A ->> Chain: DecrementTx snapshot sig
    Chain ->> Node A: OnDecrementTx
    Node A -->> Alice: DecommitFinalized
```
