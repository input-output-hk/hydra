import React, { useContext } from 'react'
import { useWallet } from '@meshsdk/react'
import { Transaction, PlutusScript, UTxO, Protocol, resolvePlutusScriptAddress } from '@meshsdk/core'
import { DEFAULT_PROTOCOL_PARAMETERS, Option } from "../../types/option"
import { HydraSocketContext } from '../../lib/hydra-ws/context'
import { readTransaction , resolvePlutusScriptHash, resolveTxHash, resolveDataHash} from '@meshsdk/core'


const Poll: React.FC<{ options: Option[] }> = ({ options }) => {
    const { socket } = useContext(HydraSocketContext)
    const { wallet, connected } = useWallet()

    // Function to send a vote message through WebSocket
    const handleVote = async function(voteOption: number) {
        if (connected) {
            const code = "59064559064201000032323232332222253353332221220023333573466e1cd55ce9baa0034800080208c98c8028cd5ce0038040041999ab9a3370e6aae74dd5001240004010464c6401466ae7001c020020c8cccd5cd19b8735573a0029000119191919191919910919800801801191999ab9a3370e6aae74005200023232323232323232323232323232323232323232323232323232323333332222221233333333333300100701200600500400e00d00c00300a0020083300d2323333573466e1cd55ce800a4000464646466442466002006004604c00460280026ae84d5d128011aba15001135573c004464c6406266ae700b80bc0bcdd5000806198068070051998083ae500f00933301075ca01e0106601aeb8010ccc041d710008011aba135744a0206ae85403cd5d0a8079aba1500f35742a01e6ae85403cd5d0a8079aba1500f35742a01e6ae85403cd5d0a8079aba1500f2322300237580026044446666aae7c00480848cd4080c010d5d080118019aba20020222323333573466e1cd55ce800a4000464646464646464646666444424666600200a008006004646666ae68cdc39aab9d001480008c8c8c8cc8848cc00400c008c090008cc02808c004d5d09aba2500235742a00226aae780088c98c80b4cd5ce0150158159baa00433300d75ca01800664646666ae68cdc3800a4008464244460040086ae84d55cf00191999ab9a3370e0049001119091118008021bae357426aae780108cccd5cd19b87003480008488800c8c98c80c0cd5ce0168170170168161aab9d00137540046600aeb8004d5d09aba2500535742a0086ae854010d5d0a8021119191999ab9a3370e002900011919091180100198030009aba135573c00646666ae68cdc3801240044244002464c6405866ae700a40a80a80a4d55ce8009baa001135744a00226ae8940044d55cf00111931901199ab9c0200210213754002266002eb9d69119118011bab00130202233335573e002403e46466a03e66442466002006004600c6aae754004c014d55cf280098021aba200313574200404026ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aab9e0022326320133357380200220226ea8008c8c8cccd5cd19b87001480188c848888c010014c8c8cccd5cd19b870014803084888888800c8c8c8c8cccd5cd19b87005480288488888880108cccd5cd19b87006480208c8c8cc8848888888cc004024020dd70011bad001357426ae894010d5d0a80191999ab9a3370e00e900311919199109111111198010048041bae002375c0026ae84d5d128031aba1500523333573466e1c021200423232332212222222330060090083013002375c0026ae84d5d128041aba1500723333573466e1c0252002232321222222230070083013001357426aae7802c8cccd5cd19b8700a480008c8c848888888c014020c050004d5d09aab9e00c23263202033573803a03c03c03a03803603403226aae780144d55cf00209aab9e00301535573a0026ea8d5d09aab9e00323333573466e1c0092004232321222230020053009001357426aae780108cccd5cd19b87003480088c8c848888c004014c024004d5d09aab9e00523333573466e1c0112000232122223003005375c6ae84d55cf00311931900b99ab9c01401501501401301235573a0026ea8004d5d09aba2500535742a0084646666ae68cdc39aab9d001480008c8c8c8cc8848cc00400c008c8cccd5cd19b8735573a002900011bae357426aae780088c98c8058cd5ce00980a00a1baa002375a0026ae84d5d128011aba15001135573c004464c6402266ae7003803c03cdd5000919191999ab9a3370e0029001119191919191999110911998008028020019bad003375a0046eb4004d5d09aba2500335742a0046ae8540084d5d1280089aab9e00323333573466e1c00920002323212230020033007001357426aae780108c98c8048cd5ce0078080080079aab9d0013754002464646666ae68cdc3800a400446424460020066eb8d5d09aab9e00323333573466e1c00920002321223002003375c6ae84d55cf00211931900899ab9c00e00f00f00e35573a0026ea80044d55cf00111931900599ab9c0080090093754002200e264c6401266ae7124103505435000071220021221223300100400349010350543100120014988c8c00400488cc00cc0080080041"
            const datumValue = "d87980"
            const script: PlutusScript = {
                version: "V2",
                code
            }
            const scriptAddress = resolvePlutusScriptAddress(script, 0)
            const scriptHash = resolvePlutusScriptHash(scriptAddress)
            const dataHash = resolveDataHash(datumValue)
            const amount = [{
                unit: "lovelace",
                quantity: "98000000"
            }]
            const value: UTxO = {
                input: {
                    outputIndex: 0,
                    txHash: "62452f7f1f382729a0194da76dc3e864479da0e80e8777e260aa6a8be4d26eb5"
                },
                output: {
                    address: scriptAddress,
                    amount,
                    dataHash,
                }
            }
            const parameters: Protocol = {
                ...DEFAULT_PROTOCOL_PARAMETERS,
                minFeeA: 0,
                minFeeB: 0,
                priceMem: 0,
                priceStep: 0,
                collateralPercent: 0,
                coinsPerUTxOSize: '0'
            }
            const tx: any = new Transaction({ initiator: wallet, parameters })
                .redeemValue({
                    value,
                    script,
                    datum: { alternative: 0, fields: [] },
                    redeemer: { alternative: 0, fields: [] }
                })
                .sendLovelace({ address: scriptAddress, datum: { value: { alternative: 0, fields: [] }, inline: true } }, '98000000')
                .setCollateral([value])

            // hack to prevent the builder from trying to balance the tx
            tx["__visits"].push("setTxInputs")
            const unsignedTx = await tx.build()
            
            console.log("-------------------------------------------")
            console.log("dataHash:", dataHash)
            console.log("scriptAddress:", scriptAddress)
            console.log("scriptHash:", scriptHash)
            console.log("-== Resolve unsignedTx ==-")
            console.log("TxHash:", resolveTxHash(unsignedTx))
            console.log("Transaction:", readTransaction(unsignedTx))
            console.log("-------------------------------------------")
            
            const signedTx = await wallet.signTx(unsignedTx, true)
            const messageToSend = JSON.stringify({ "tag": "NewTx", "transaction": signedTx.toString() })
            console.log(JSON.parse(messageToSend))
            socket.send(messageToSend)
        } else {
            console.error("Cant build tx due to missing Lucid instance")
        }
    }

    return (
        <div>
            <div>
                <h1 className="title">
                    <a href="https://hydra.family">Hydra </a>Poll
                </h1>
                <h2>Vote for the next Hydra feature</h2>
            </div>
            <div className='container'>
                <div className="grid">
                    {options.map((option) => (
                        <div className="card" key={option.id}>
                            <h3>{option.text}</h3>
                            <h4>{option.votes}</h4>
                            <button className="button" onClick={() =>
                                handleVote(option.id).catch((error) => {
                                    console.error("Error on handleVote:", error)
                                })
                            }>Vote</button>
                        </div>
                    ))}
                </div>
            </div>
        </div>
    )
}

export default Poll
