{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Alonzo.Core (EraTxAuxData (hashTxAuxData))
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..))
import Cardano.Ledger.Api (
  AlonzoPlutusPurpose (AlonzoSpending),
  Metadatum,
  auxDataHashTxBodyL,
  auxDataTxL,
  bodyTxL,
  inputsTxBodyL,
  outputsTxBodyL,
  ppProtocolVersionL,
  rdmrsTxWitsL,
  referenceInputsTxBodyL,
  reqSignerHashesTxBodyL,
  unRedeemers,
  validateTxAuxData,
  vldtTxBodyL,
  witsTxL,
  pattern ShelleyTxAuxData,
 )
import Cardano.Ledger.Core (EraTx (getMinFeeTx))
import Cardano.Ledger.Credential (Credential (..))
import Control.Lens ((^.), (^?!))
import Data.Aeson.Lens (key, _JSON)
import Data.Map qualified as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Hydra.Cardano.Api.Pretty (renderTx, renderTxWithUTxO)
import Hydra.Chain (CommitBlueprintTx (..), HeadParameters (..), maximumNumberOfParties)
import Hydra.Chain.Direct.Fixture (
  epochInfo,
  pparams,
  systemStart,
  testNetworkId,
  testPolicyId,
  testSeedInput,
 )
import Hydra.Chain.Direct.Fixture qualified as Fixture
import Hydra.Chain.Direct.Handlers (draftCommitTx_)
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.State (HasKnownUTxO (getKnownUTxO), InitialState (..), genChainStateWithTx, genHydraContext, genStInitial, ownParty, ownVerificationKey)
import Hydra.Chain.Direct.State qualified as Transition
import Hydra.Chain.Direct.Tx (
  HeadObservation (..),
  InitObservation (..),
  abortTx,
  currencySymbolToHeadId,
  headIdToCurrencySymbol,
  headIdToPolicyId,
  headSeedToTxIn,
  initTx,
  mkCommitDatum,
  mkHeadId,
  observeHeadTx,
  observeInitTx,
  onChainIdToAssetName,
  txInToHeadSeed,
  verificationKeyToOnChainId,
 )
import Hydra.Chain.Direct.Wallet (ErrCoverFee (..), coverFee_, getUTxO, newTinyWallet)
import Hydra.Chain.Direct.WalletSpec (mockChainQuery)
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.HeadTokens (headPolicyId, mkHeadTokenScript)
import Hydra.Contract.Initial qualified as Initial
import Hydra.Ledger.Cardano (
  adaOnly,
  addInputs,
  addReferenceInputs,
  addVkInputs,
  emptyTxBody,
  genOneUTxOFor,
  genSigningKey,
  genTxOutWithReferenceScript,
  genUTxO1,
  genUTxOAdaOnlyOfSize,
  genValue,
  genVerificationKey,
  unsafeBuildTransaction,
 )
import Hydra.Ledger.Cardano.Evaluate (EvaluationReport, maxTxExecutionUnits, propTransactionEvaluates)
import Hydra.Logging (nullTracer)
import Hydra.Party (Party)
import PlutusLedgerApi.Test.Examples qualified as Plutus
import Test.Cardano.Ledger.Shelley.Arbitrary (genMetadata')
import Test.Hydra.Fixture (genForParty)
import Test.Hydra.Prelude
import Test.QuickCheck (
  Property,
  checkCoverage,
  choose,
  conjoin,
  counterexample,
  cover,
  elements,
  forAll,
  forAllBlind,
  ioProperty,
  label,
  property,
  vectorOf,
  withMaxSuccess,
  (.&&.),
  (===),
 )
import Test.QuickCheck.Instances.Semigroup ()
import Test.QuickCheck.Monadic (monadicIO)
import Text.Pretty.Simple (pShow)

spec :: Spec
spec =
  parallel $ do
    describe "HeadSeed (cardano)" $
      prop "headSeedToTxIn . txInToHeadSeed === id" $ \txIn -> do
        let headSeed = txInToHeadSeed txIn
        headSeedToTxIn headSeed === Just txIn
          & counterexample (show headSeed)

    describe "HeadId (cardano)" $ do
      prop "headIdToPolicyId . mkHeadId === id" $ \pid -> do
        let headId = mkHeadId pid
        headIdToPolicyId headId === Just pid
          & counterexample (show headId)

      prop "curencySymbolToHeadId . headIdToCurrencySymbol === id" $ \txIn -> monadicIO $ do
        let headId = mkHeadId $ headPolicyId txIn
        let cs = headIdToCurrencySymbol headId
        headId' <- currencySymbolToHeadId cs
        pure $ headId' === headId

    describe "observeHeadTx" $ do
      prop "All valid transitions for all possible states can be observed." $
        checkCoverage $
          forAllBlind genChainStateWithTx $ \(_ctx, st, tx, transition) ->
            genericCoverTable [transition] $
              counterexample (show transition) $
                let utxo = getKnownUTxO st
                 in case observeHeadTx testNetworkId utxo tx of
                      NoHeadTx -> property False
                      Init{} -> transition === Transition.Init
                      Abort{} -> transition === Transition.Abort
                      Commit{} -> transition === Transition.Commit
                      CollectCom{} -> transition === Transition.Collect
                      Close{} -> transition === Transition.Close
                      Contest{} -> transition === Transition.Contest
                      Fanout{} -> transition === Transition.Fanout

    describe "collectComTx" $ do
      prop "cover fee correctly handles redeemers" $
        withMaxSuccess 60 $ \txIn cperiod (party :| parties) walletUTxO -> do
          let allParties = party : parties
              cardanoKeys = genForParty genVerificationKey <$> allParties
          forAll (elements cardanoKeys) $ \signer ->
            forAll genScriptRegistry $ \scriptRegistry ->
              let params = HeadParameters cperiod allParties
                  participants = verificationKeyToOnChainId <$> cardanoKeys
                  tx = initTx testNetworkId txIn participants params
               in case observeInitTx tx of
                    Right InitObservation{initials, initialThreadUTxO} -> do
                      let lookupUTxO =
                            mconcat
                              [ Map.fromList (initialThreadUTxO : initials)
                              , UTxO.toMap (registryUTxO scriptRegistry)
                              ]
                              & Map.mapKeys toLedgerTxIn
                              & Map.map toLedgerTxOut
                       in case abortTx mempty scriptRegistry signer initialThreadUTxO (mkHeadTokenScript testSeedInput) (Map.fromList initials) mempty of
                            Left err ->
                              property False & counterexample ("AbortTx construction failed: " <> show err)
                            Right (toLedgerTx -> txAbort) ->
                              case coverFee_ pparams systemStart epochInfo lookupUTxO walletUTxO txAbort of
                                Left err ->
                                  True
                                    & label
                                      ( case err of
                                          ErrNoFuelUTxOFound{} -> "No fuel UTxO found"
                                          ErrNotEnoughFunds{} -> "Not enough funds"
                                          ErrUnknownInput{} -> "Unknown input"
                                          ErrScriptExecutionFailed{} -> "Script(s) execution failed"
                                          ErrTranslationError{} -> "Transaction context translation error"
                                      )
                                Right ledgerTx ->
                                  let actualExecutionCost = getMinFeeTx pparams ledgerTx
                                      fee = txFee' apiTx
                                      apiTx = fromLedgerTx ledgerTx
                                   in actualExecutionCost > Coin 0 && fee > actualExecutionCost
                                        & label "Ok"
                                        & counterexample ("Execution cost: " <> show actualExecutionCost)
                                        & counterexample ("Fee: " <> show fee)
                                        & counterexample ("Tx: " <> show apiTx)
                                        & counterexample ("Input utxo: " <> show (walletUTxO <> lookupUTxO))
                    Left e ->
                      property False
                        & counterexample "Failed to construct and observe init tx."
                        & counterexample (renderTx tx)
                        & counterexample (show e)

    describe "commitTx" $ do
      prop "genBlueprintTx generates interesting txs" prop_interestingBlueprintTx

      prop "Validate blueprint and commit transactions" $ do
        forAllBlind (genHydraContext maximumNumberOfParties) $ \hctx ->
          forAllBlind (genStInitial hctx) $ \(ctx, stInitial@InitialState{headId}) -> do
            -- forAllBlind genBlueprintTxWithUTxO $ \(lookupUTxO, blueprintTx) ->

            let lookupUTxO = reproJSON ^?! key "utxo" . _JSON
            let blueprintTx = reproJSON ^?! key "blueprintTx" . _JSON

            counterexample ("Blueprint tx: " <> renderTxWithUTxO lookupUTxO blueprintTx) $
              ioProperty $ do
                let sk = genSigningKey `genForParty` ownParty ctx -- NOTE: signature is not not checked
                    vk = ownVerificationKey ctx
                wallet <- newTinyWallet nullTracer Fixture.testNetworkId (vk, sk) (mockChainQuery vk) (pure Fixture.epochInfo)
                walletUTxO <- UTxO.fromPairs . map (bimap fromLedgerTxIn fromLedgerTxOut) . Map.toList <$> atomically (getUTxO wallet)
                let spendableUTxO = getKnownUTxO stInitial <> lookupUTxO <> getKnownUTxO ctx <> walletUTxO
                draftCommitTx_ wallet ctx spendableUTxO headId CommitBlueprintTx{lookupUTxO, blueprintTx} >>= \case
                  Left err -> pure $ property False & counterexample ("Failed to construct commit: " <> toString (pShow err))
                  Right commitTx ->
                    pure $
                      counterexample ("\n\n\nCommit tx: " <> renderTxWithUTxO lookupUTxO commitTx) $ do
                        let blueprintBody = toLedgerTx blueprintTx ^. bodyTxL
                        let commitTxBody = toLedgerTx commitTx ^. bodyTxL

                        conjoin
                          [ -- propTransactionEvaluates (blueprintTx, lookupUTxO)
                            --   & counterexample "Blueprint transaction failed to evaluate"
                            propTransactionEvaluates (commitTx, spendableUTxO)
                              & counterexample "Commit transaction failed to evaluate"
                          , conjoin
                              [ getAuxMetadata blueprintTx `propIsSubmapOf` getAuxMetadata commitTx
                                  & counterexample "Blueprint metadata incomplete"
                              , propHasValidAuxData blueprintTx
                                  & counterexample "Blueprint tx has invalid aux data"
                              , propHasValidAuxData commitTx
                                  & counterexample "Commit tx has invalid aux data"
                              ]
                          , blueprintBody ^. vldtTxBodyL === commitTxBody ^. vldtTxBodyL
                              & counterexample "Validity range mismatch"
                          , (blueprintBody ^. inputsTxBodyL) `propIsSubsetOf` (commitTxBody ^. inputsTxBodyL)
                              & counterexample "Blueprint inputs missing"
                          , property
                              ((`all` (blueprintBody ^. outputsTxBodyL)) (`notElem` (commitTxBody ^. outputsTxBodyL)))
                              & counterexample "Blueprint outputs not discarded"
                          , (blueprintBody ^. reqSignerHashesTxBodyL) `propIsSubsetOf` (commitTxBody ^. reqSignerHashesTxBodyL)
                              & counterexample "Blueprint required signatures missing"
                          , (blueprintBody ^. referenceInputsTxBodyL) `propIsSubsetOf` (commitTxBody ^. referenceInputsTxBodyL)
                              & counterexample "Blueprint reference inputs missing"
                          ]
reproJSON :: Text
reproJSON = "{\"blueprintTx\": {\"cborHex\": \"84a600828258203769dcd954bee36ba0ec94dcf6758c362a069804fdbad82860b1f78d3c171eb2008258207893b1626af5b7cc8d44f20a009fd401d6ced43b7d30477448221d003eec3b600201800200031a02e37044081a02e3284f0e82581cac55de689702d745e77050ce83b77ff9619383bb802e40fb90aa3be4581ce17e5b8c19c89d663c66b3e4943972d7b6dd70a711fade4cf1a6a95aa30380068159114c591149010000333232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323222222333332222232325333058332232323370e6660b4444a6660bc002200426600666e00009200230620014800000520023304e23303132306130620013060305f3061001004001375860bc60be002605c0020042a6660b06602000e60b860b660ba002264a6660b2664466064460066eacc188004004c88cc0c88c00cdd69831000800919b8700148000cc88ccc8c04488dd3198019bab002375600244666024446ea0cdc09bad002375a0020040020040026eac0052f5bded8c02646464a6660b866e1d2000002132533305d3301500c001132533305e3330304a04a660726464646464646464a66082666660446eb8c1acc1a801400c0200092201014000133333022375c60d600a6eb8c1acc1a8010c1ac0100052210145003322332232533306b3371200290000801099b8a333069222533306d00110021330033371400460e400260e20020746660d2444a6660dc66e24009200014bd620998318009980199b81002480080040040ec008cdc08011b8d0014822804cdc5181c80b99b8a303900137666ea0008dd6983500299191919299983519b874800800854cc19d24013470616464725061796d656e744b657948617368556e736166653a206661696c656420746f20676574207061796d656e7420706b6800161375c60dc00260de00460d40026ea8c1acc1b0004c1a800cc8cdc5181b80a981b8009bae30693068002375c60d060be60d202460d060ce00660ca00260c800260ca00460c660c460c801a2666062646466e24008004dd69832183198328011bad30633049306400d23232323371266e0000c004008dd69833191833182680098338081bad306530643066003375a60c860c660ca002018002264a6660be6606e608601a0022930a9982e248105535442443800163062304a3063008153305b4901055354424437001633059491055354424436003232323253330613370e9002001098299919191919002a99983299b87480000084c8c8c8c8c8c8c926533306a001149854cc1a014c58c1b40194ccc1a4cdc3a400000426464646464646464646464646464646493299983b8008a4c2a660ea0c02c60f40066eb8004c1e4004c1dc00cdd7000983b000983a0019bad00130730013071006533306d3370e900000109919191919191919191924ca6660ea0022930a9983982f0b183c0019bae0013077001307500653330713370e900000109919191919191919191919191924ca6660f80022930a9983d0328b183f803299983d99b87480000084c8c8c8c8c8c8c926533308001001149854cc1f81a458c20c040194ccc1fccdc3a4000004264646464646464932999842008008a4c2a66104020da2c610e0200ca6661060266e1d2000002132325333085013370e6e340052038132324994ccc2140400452615330830106e1630880100315330820105816375c002610e020022a6661060266e1d2002002132325333085013370e6e340052038132324994ccc2140400452615330830106e1630880100315330820105916375c002610e020022a66100020d82c6110020046106020026ea8004c20c0400454ccc1fccdc3a4004004264646464646464646464932999843808008a4c2a6610a020e02c6114020066eb4004c22404004c21c0400cdd68009843008009842008019bad001308301001153307c06816308401002307f001375400260fe0022a6660f666e1d20020021324994ccc1e8004526153307806316153307806416308001002307b001375400260f600260f200ca6660ea66e1d20000021323253330773370e6e340052038132324994ccc1dc004526153307506016307a003153307404a16375c00260f20022a6660ea66e1d20020021323253330773370e6e340052038132324994ccc1dc004526153307506016307a003153307404b16375c00260f20022a660e40bc2c60f400460ea0026ea8004c1d400454cc1b816858c1d8008c1c4004dd500098388008a9983502b0b183900118368009baa001306d001153306605216306e0023069001375400260d20022a6660ca66e1d20020021324994ccc190004526153306204d16153306204e16306a0023065001375400260ca00220c460cc00460c20026ea8004c184c160c18800454cc16924010553544244350016330584910553544244340033223304d23303430633064001003375860c460b260c6002606400a00c2a6660b866e1d200200213232533305e33035233223305f22533306200114a02a6660c666ebcc19c00400c5288980118330008011bac306330620030010011533305e33036304200c3062002149854cc16d241065354424431300016153305b4910553544244390016375860c260ae60c4016609060c200c264a6660ba66644464646464666444660024600600220069408c020004004cc88cc0048c888c00800cc00c00448940048c8c8c8c80154ccc19ccdc3a40000042649329998330008a4c2a660c809e2c2a6660ce66e1d20020021324994ccc198004526153306404f16153330673370e900200109924ca6660cc0022930a998320278b0a99983399b87480180084c9265333066001149854cc19013c5854cc19014058c1b0008c19c004dd50009999111983211299983380089128008a99983419baf306a306c00100413005306c00113002306b001001232223002003306a0010010033374a90011982b80082b183298330011bab3064304a3065002232323253330613370e90000010a50153330613370e90030010a5014a260cc00460c20026ea800401c004526153305a490106535442443132001633058491065354424431310033223304d23370e6660aa646eacc190c18cc194004c18cc188c19000403800d2002375860c460c600291010741554354494f4e000063061002305c001375400a2a660ac9210553544244330016305c3051305d002153305549010553544244320016153305549105535442443100163305349105535442443000323232533305a3370e90010010991919982d111299982f00089128008a99982f9801183180089911180100198318008998018011831000919baf3061306200100300137586460c060c200260be60c000c60bc00220b660be00460b40026ea8c16cc168c170008c168c16c004dd700280200180100091119998020019100100100091824980100091191911980080180111129998270008998220018010991919299982919baf002001133047337600046601260b000c60b00066660104400400a60aa0082a6660a466e40dd70011bae0011330470063333008220010033055004005133047003333300822001006005305500430530023052004305300122533304c0021001133330032200130510023050002001223330030020040012223370e6660806eacc138c134c13c00400c00920024890c5354414e44494e475f42494400222223253302930014a0260029448cccccc01c00401000c0088ccdca8030008038a5022223333005004003232303c3371402c66e28dd71827182680099b8a017337146eb8c138004cdc500c19b8a00333714008032609c00220924466600600246600a0024464607666e9520003303e37520026607c6ea40080f4cdc500619b8a002337146601a00a00666e28c04c00c05c41188c8c8c8c94ccc11ccdc3a400400420902646464a66609466e1d20000021323232533304d3370e900100108270991919299982819b87480080084c108c8c8888cc008014010dd7182a8011bae305400a13042323222233003005004375c60aa0046eb8c150028c154008c140004dd51828800982900118268009baa304e0011303c322223001004375c609c008609e00460940026ea8c12cc128010c130008c11c004dd518240009824000919980091119800802803111198008028031119800803003a44101390048810146004881011d004881012a0048810158002233300122533303f0041005100f22533303f004100610072533303e0031007100848901010048810121004881012000488101610048810160002333001714e2880052210d846a5369676e617475726531580048810ba201276761646472657373004881024058004881004881010000237666ea4004888c8c8c94ccc0d0cdc3a4004004200c2600a6070002607200460680026ea80052412c7074727946726f6d28505075624b657948617368293a206d757374206265203238206279746573206c6f6e670049012c7074727946726f6d285053637269707448617368293a206d757374206265203238206279746573206c6f6e6700223375e6e9cc0c8008dd3981900091918181818800981798171818000918141129998158008a5115330063003303000113002302f00122323253300633005300f3030002300f3030001133005300f3030302f001300f3030302f0023030002302f0022232323232533302d53300b5330070021001153300730090021300900113300a00400313300e004003300e3030302f004300d302f302e004302e002302d0022233302800200100314a0466604a0029412891299801998030010008998028010009198118008010a512232323253330253370e90000010991919299981419b87480000085288a50302d002302800137540082a66604a66e1d2002002132323253330283370e900100109919b87375a605a00a0026eb4c0b0004528181680118140009baa004132323253330283370e90020010a5114a0605a00460500026ea8010c0a8008c094004dd500111191919299981219b87480000085288a99981219b87480080084c8c8c94ccc09ccdc3a4000004294054ccc09ccdc3a400800429444cdc41bad302b004375a60560026058004604e0026ea80104c8c8c94ccc09ccdc3a40080042944528181600118138009baa004302900230240013754004466e1cc084dd5000a4004044464660066eb4c088004dd698111810800980b18110009119ba548000cc048cdd2a40006602466e952002330123750004022660240060226602466e952000330123374a9001198091ba8001011330120040110114c0103d87a80004c0103d87980004901317074727946726f6d2850446174615265636f72645b5d293a206c697374206973206c6f6e676572207468616e207a65726f0049013f7265616368656420656e64206f662073756d207768696c65207374696c6c206e6f7420686176696e6720666f756e642074686520636f6e7374727563746f72002301a300200123019300200123018300200123017300200123016300c00122300333006002001300e225333011001101315333012300330150011300430160011013574644460040064601844a66601e0022008264a666022600800226600c00260066028004260066028004602800297ae05740444666600800490001199980280124000eb4dd5800801918011ba900122223300a22533300d00110051533300e3375e6020602400200c2600860286024002260046022002002460166004002460146004002460126012002446600240022a660060042cae708c8c0088cc0080080048c0088cc00800800555cfab9a2250015573aae895d0918011baa0015573c98011e581ceee3a3ac3c5abb48a9144ccad241656418d92dd4671528605b0e8505004c01ffd8799fa1581cc0f8644a01a6bf5db02f4afe30d604975e63dd274f1098a1738e561da1484d6f6e614c69736101d8799fd8799f581c5adb3931b04f04b3cec724bffa6ddfbde4581b88ad3cd036f8739915ffd8799fd8799fd8799f581c74616edff426d4fd7027af0453b6eda6812e6ca8ec7ca182b1f588edffffffff58206b65411ab5550ba69734c59628fcddd906d3062bfa330e9452211d3f1d87883f9f581cac55de689702d745e77050ce83b77ff9619383bb802e40fb90aa3be4581ce17e5b8c19c89d663c66b3e4943972d7b6dd70a711fade4cf1a6a95aff1b0000018f53c3139b1b0000018f54dee3db1b0000018f5515d25b1b0000018f551617bcbb1a002dc6c01a007a12001a000f42401a002dc6c0ff00010581840000d87a80820000f5f6\", \"description\": \"\", \"txId\": \"ace37a2bec41c58f72dc8f5d79801d1405307f942e193c18aa918fffb136ffe3\", \"type\": \"Tx BabbageEra\"}, \"utxo\": {\"3769dcd954bee36ba0ec94dcf6758c362a069804fdbad82860b1f78d3c171eb2#0\": {\"address\": \"addr_test1wpedppdl46j4a95c20wcktcuzcvuttt8v6jpa9nrj6alsksd0mcqw\", \"inlineDatum\": {\"fields\": [], \"constructor\": 1}, \"inlineDatumhash\": \"8392f0c940435c06888f9bdb8c74a95dc69f156367d6a089cf008ae05caae01e\", \"value\": {\"lovelace\": 1099050, \"eee3a3ac3c5abb48a9144ccad241656418d92dd4671528605b0e8505\": {\"5354414e44494e475f424944\": 1}}}, \"7893b1626af5b7cc8d44f20a009fd401d6ced43b7d30477448221d003eec3b60#2\": {\"address\": \"addr_test1vzcnxhr5u6ej3jzecxsef9pgr4g8nf0lxv92p53qluxdmjqtlwpvx\", \"inlineDatum\": null, \"value\": {\"lovelace\": 10000000}}}}"

-- | Check auxiliary data of a transaction against 'pparams' and whether the aux
-- data hash is consistent.
propHasValidAuxData :: Tx -> Property
propHasValidAuxData tx =
  case toLedgerTx tx ^. auxDataTxL of
    SNothing -> property True
    SJust auxData ->
      isValid auxData .&&. hashConsistent auxData
 where
  isValid auxData =
    validateTxAuxData (pparams ^. ppProtocolVersionL) auxData
      & counterexample "Auxiliary data validation failed"

  hashConsistent auxData =
    toLedgerTx tx ^. bodyTxL . auxDataHashTxBodyL === SJust (hashTxAuxData auxData)
      & counterexample "Auxiliary data hash inconsistent"

-- | Check whether one set 'isSubsetOf' of another with nice counter examples.
propIsSubsetOf :: (Show a, Ord a) => Set a -> Set a -> Property
propIsSubsetOf as bs =
  as `Set.isSubsetOf` bs
    & counterexample (show as <> "\n  is not a subset of\n" <> show bs)

-- | Check whether one map 'isSubmapOf' of another with nice counter examples.
propIsSubmapOf :: (Show k, Show v, Ord k, Eq v) => Map k v -> Map k v -> Property
propIsSubmapOf as bs =
  as `Map.isSubmapOf` bs
    & counterexample (show as <> "\n  is not a submap of\n" <> show bs)

genBlueprintTxWithUTxO :: Gen (UTxO, Tx)
genBlueprintTxWithUTxO =
  fmap (second unsafeBuildTransaction) $
    spendingPubKeyOutput (mempty, emptyTxBody)
      >>= spendSomeScriptInputs
      >>= addSomeReferenceInputs
      >>= addValidityRange
      >>= addRandomMetadata
      >>= addCollateralInput
 where
  spendingPubKeyOutput (utxo, txbody) = do
    utxoToSpend <- genUTxOAdaOnlyOfSize =<< choose (0, 3)
    pure
      ( utxo <> utxoToSpend
      , txbody & addVkInputs (toList $ UTxO.inputSet utxoToSpend)
      )

  spendSomeScriptInputs (utxo, txbody) = do
    let alwaysSucceedingScript = PlutusScriptSerialised $ Plutus.alwaysSucceedingNAryFunction 3
    datum <- unsafeHashableScriptData . fromPlutusData <$> arbitrary
    redeemer <- unsafeHashableScriptData . fromPlutusData <$> arbitrary
    let genTxOut = do
          value <- genValue
          let scriptAddress = mkScriptAddress testNetworkId alwaysSucceedingScript
          pure $ TxOut scriptAddress value (TxOutDatumInline datum) ReferenceScriptNone
    utxoToSpend <- genUTxO1 genTxOut
    pure
      ( utxo <> utxoToSpend
      , txbody
          & addInputs
            ( UTxO.pairs $
                ( \_ ->
                    BuildTxWith $
                      ScriptWitness ScriptWitnessForSpending $
                        mkScriptWitness alwaysSucceedingScript (ScriptDatumForTxIn datum) redeemer
                )
                  <$> utxoToSpend
            )
      )

  addSomeReferenceInputs (utxo, txbody) = do
    txout <- genTxOutWithReferenceScript
    txin <- arbitrary
    pure (utxo <> UTxO.singleton (txin, txout), txbody & addReferenceInputs [txin])

  addValidityRange (utxo, txbody) = do
    (start, end) <- arbitrary
    pure
      ( utxo
      , txbody{txValidityLowerBound = start, txValidityUpperBound = end}
      )

  addRandomMetadata (utxo, txbody) = do
    mtdt <- genMetadata
    pure (utxo, txbody{txMetadata = mtdt})

  addCollateralInput (utxo, txbody) = do
    utxoToSpend <- genUTxOAdaOnlyOfSize 1
    pure
      ( utxo <> utxoToSpend
      , txbody{txInsCollateral = TxInsCollateral $ toList (UTxO.inputSet utxoToSpend)}
      )

genMetadata :: Gen TxMetadataInEra
genMetadata = do
  genMetadata' @LedgerEra >>= \(ShelleyTxAuxData m) ->
    pure . TxMetadataInEra . TxMetadata $ fromShelleyMetadata m

getAuxMetadata :: Tx -> Map Word64 Metadatum
getAuxMetadata tx =
  case toLedgerTx tx ^. auxDataTxL of
    SNothing -> mempty
    SJust (AlonzoTxAuxData m _ _) -> m

prop_interestingBlueprintTx :: Property
prop_interestingBlueprintTx = do
  forAll genBlueprintTxWithUTxO $ \(utxo, tx) ->
    checkCoverage
      True
      & cover 1 (spendsFromScript (utxo, tx)) "blueprint spends script UTxO"
      & cover 1 (spendsFromPubKey (utxo, tx)) "blueprint spends pub key UTxO"
      & cover 1 (spendsFromPubKey (utxo, tx) && spendsFromScript (utxo, tx)) "blueprint spends from script AND pub key"
      & cover 1 (hasReferenceInputs tx) "blueprint has reference input"
 where
  hasReferenceInputs tx =
    not . null $ toLedgerTx tx ^. bodyTxL . referenceInputsTxBodyL

  spendsFromPubKey (utxo, tx) =
    any
      ( \txIn -> case UTxO.resolve (fromLedgerTxIn txIn) utxo of
          Just (TxOut (ShelleyAddressInEra (ShelleyAddress _ (KeyHashObj _) _)) _ _ _) -> True
          _ -> False
      )
      $ toLedgerTx tx ^. bodyTxL . inputsTxBodyL

  -- XXX: We do check both, the utxo and redeemers, because we
  -- don't do phase 1 validation of the resulting transactions
  -- and would not detect if redeemers are missing.
  spendsFromScript (utxo, tx) =
    any
      ( \txIn -> case UTxO.resolve (fromLedgerTxIn txIn) utxo of
          Just (TxOut (ShelleyAddressInEra (ShelleyAddress _ (ScriptHashObj _) _)) _ _ _) -> True
          _ -> False
      )
      (toLedgerTx tx ^. bodyTxL . inputsTxBodyL)
      && any
        ( \case
            AlonzoSpending _ -> True
            _ -> False
        )
        ( Map.keysSet
            . unRedeemers
            $ toLedgerTx @Era tx ^. witsTxL . rdmrsTxWitsL
        )

withinTxExecutionBudget :: EvaluationReport -> Property
withinTxExecutionBudget report =
  (totalMem <= maxMem && totalCpu <= maxCpu)
    & counterexample
      ( "Ex. Cost Limits exceeded, mem: "
          <> show totalMem
          <> "/"
          <> show maxMem
          <> ", cpu: "
          <> show totalCpu
          <> "/"
          <> show maxCpu
      )
 where
  budgets = rights $ Map.elems report
  totalMem = sum $ executionMemory <$> budgets
  totalCpu = sum $ executionSteps <$> budgets
  ExecutionUnits
    { executionMemory = maxMem
    , executionSteps = maxCpu
    } = maxTxExecutionUnits

-- | Generate a UTXO representing /commit/ outputs for a given list of `Party`.
-- NOTE: Uses 'testPolicyId' for the datum.
-- NOTE: We don't generate empty commits and it is used only at one place so perhaps move it?
-- FIXME: This function is very complicated and it's hard to understand it after a while
generateCommitUTxOs :: [Party] -> Gen (Map.Map TxIn (TxOut CtxUTxO, UTxO))
generateCommitUTxOs parties = do
  txins <- vectorOf (length parties) (arbitrary @TxIn)
  let vks = (\p -> (genVerificationKey `genForParty` p, p)) <$> parties
  committedUTxO <-
    vectorOf (length parties) $
      fmap adaOnly <$> (genOneUTxOFor =<< arbitrary)
  let commitUTxO =
        zip txins $
          uncurry mkCommitUTxO <$> zip vks committedUTxO
  pure $ Map.fromList commitUTxO
 where
  mkCommitUTxO :: (VerificationKey PaymentKey, Party) -> UTxO -> (TxOut CtxUTxO, UTxO)
  mkCommitUTxO (vk, party) utxo =
    ( toUTxOContext $
        TxOut
          (mkScriptAddress @PlutusScriptV2 testNetworkId commitScript)
          commitValue
          (mkTxOutDatumInline commitDatum)
          ReferenceScriptNone
    , utxo
    )
   where
    commitValue =
      mconcat
        [ lovelaceToValue (Coin 2000000)
        , foldMap txOutValue utxo
        , valueFromList
            [ (AssetId testPolicyId (assetNameFromVerificationKey vk), 1)
            ]
        ]

    commitScript = fromPlutusScript Commit.validatorScript

    commitDatum = mkCommitDatum party utxo (toPlutusCurrencySymbol testPolicyId)

prettyEvaluationReport :: EvaluationReport -> String
prettyEvaluationReport (Map.toList -> xs) =
  "Script Evaluation(s):\n" <> intercalate "\n" (prettyKeyValue <$> xs)
 where
  prettyKeyValue (ptr, result) =
    toString ("  - " <> show ptr <> ": " <> prettyResult result)
  prettyResult =
    either (T.replace "\n" " " . show) show

-- NOTE: Uses 'testPolicyId' for the datum.
genAbortableOutputs :: [Party] -> Gen ([(TxIn, TxOut CtxUTxO)], [(TxIn, TxOut CtxUTxO, UTxO)])
genAbortableOutputs parties =
  go
 where
  go = do
    (initParties, commitParties) <- (`splitAt` parties) <$> choose (0, length parties)
    initials <- mapM genInitial initParties
    commits <- fmap (\(a, (b, c)) -> (a, b, c)) . Map.toList <$> generateCommitUTxOs commitParties
    pure (initials, commits)

  genInitial p =
    mkInitial (genVerificationKey `genForParty` p) <$> arbitrary

  mkInitial ::
    VerificationKey PaymentKey ->
    TxIn ->
    (TxIn, TxOut CtxUTxO)
  mkInitial vk txin =
    ( txin
    , initialTxOut vk
    )

  initialTxOut :: VerificationKey PaymentKey -> TxOut CtxUTxO
  initialTxOut vk =
    toUTxOContext $
      TxOut
        (mkScriptAddress @PlutusScriptV2 testNetworkId initialScript)
        (valueFromList [(AssetId testPolicyId (assetNameFromVerificationKey vk), 1)])
        (mkTxOutDatumInline initialDatum)
        ReferenceScriptNone

  initialScript = fromPlutusScript Initial.validatorScript

  initialDatum = Initial.datum (toPlutusCurrencySymbol testPolicyId)

assetNameFromVerificationKey :: VerificationKey PaymentKey -> AssetName
assetNameFromVerificationKey =
  onChainIdToAssetName . verificationKeyToOnChainId

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

third :: (a, b, c) -> c
third (_, _, c) = c

drop2nd :: (a, b, c) -> (a, c)
drop2nd (a, _, c) = (a, c)

drop3rd :: (a, b, c) -> (a, b)
drop3rd (a, b, _) = (a, b)

tripleToPair :: (a, b, c) -> (a, (b, c))
tripleToPair (a, b, c) = (a, (b, c))
