"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[3687],{3905:(e,t,a)=>{a.d(t,{Zo:()=>d,kt:()=>m});var n=a(67294);function o(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function r(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function i(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?r(Object(a),!0).forEach((function(t){o(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):r(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function s(e,t){if(null==e)return{};var a,n,o=function(e,t){if(null==e)return{};var a,n,o={},r=Object.keys(e);for(n=0;n<r.length;n++)a=r[n],t.indexOf(a)>=0||(o[a]=e[a]);return o}(e,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(n=0;n<r.length;n++)a=r[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(o[a]=e[a])}return o}var c=n.createContext({}),l=function(e){var t=n.useContext(c),a=t;return e&&(a="function"==typeof e?e(t):i(i({},t),e)),a},d=function(e){var t=l(e.components);return n.createElement(c.Provider,{value:t},e.children)},p="mdxType",h={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},u=n.forwardRef((function(e,t){var a=e.components,o=e.mdxType,r=e.originalType,c=e.parentName,d=s(e,["components","mdxType","originalType","parentName"]),p=l(a),u=o,m=p["".concat(c,".").concat(u)]||p[u]||h[u]||r;return a?n.createElement(m,i(i({ref:t},d),{},{components:a})):n.createElement(m,i({ref:t},d))}));function m(e,t){var a=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var r=a.length,i=new Array(r);i[0]=u;var s={};for(var c in t)hasOwnProperty.call(t,c)&&(s[c]=t[c]);s.originalType=e,s[p]="string"==typeof e?e:o,i[1]=s;for(var l=2;l<r;l++)i[l]=a[l];return n.createElement.apply(null,i)}return n.createElement.apply(null,a)}u.displayName="MDXCreateElement"},58503:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>c,contentTitle:()=>i,default:()=>h,frontMatter:()=>r,metadata:()=>s,toc:()=>l});var n=a(87462),o=(a(67294),a(3905));const r={},i="Protocol overview",s={unversionedId:"protocol-overview",id:"protocol-overview",title:"Protocol overview",description:"Hydra is the layer 2 scalability solution for Cardano, designed to increase transaction speed through low latency and high throughput while minimizing transaction costs. Hydra Head is the first protocol of the Hydra family that lays the foundation for more advanced deployment scenarios using isomorphic, multi-party state channels. For an introduction to the protocol, refer to these two blog posts:",source:"@site/docs/protocol-overview.md",sourceDirName:".",slug:"/protocol-overview",permalink:"/head-protocol/unstable/docs/protocol-overview",draft:!1,editUrl:"https://github.com/cardano-scaling/hydra/tree/master/docs/docs/protocol-overview.md",tags:[],version:"current",frontMatter:{},sidebar:"userDocumentation",previous:{title:"Welcome",permalink:"/head-protocol/unstable/docs/"},next:{title:"Known issues and limitations",permalink:"/head-protocol/unstable/docs/known-issues"}},c={},l=[{value:"Hydra head lifecycle",id:"hydra-head-lifecycle",level:2}],d={toc:l},p="wrapper";function h(e){let{components:t,...r}=e;return(0,o.kt)(p,(0,n.Z)({},d,r,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h1",{id:"protocol-overview"},"Protocol overview"),(0,o.kt)("p",null,"Hydra is the layer 2 scalability solution for Cardano, designed to increase transaction speed through low latency and high throughput while minimizing transaction costs. ",(0,o.kt)("a",{parentName:"p",href:"https://eprint.iacr.org/2020/299.pdf"},"Hydra Head")," is the first protocol of the Hydra family that lays the foundation for more advanced deployment scenarios using isomorphic, multi-party state channels. For an introduction to the protocol, refer to these two blog posts: "),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("a",{parentName:"li",href:"https://iohk.io/en/blog/posts/2021/09/17/hydra-cardano-s-solution-for-ultimate-scalability/"},"Hydra \u2013 Cardano\u2019s solution for ultimate layer 2 scalability")),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("a",{parentName:"li",href:"https://iohk.io/en/blog/posts/2022/02/03/implementing-hydra-heads-the-first-step-towards-the-full-hydra-vision/"},"Implementing Hydra Heads: the first step towards the full Hydra vision"),".")),(0,o.kt)("p",null,"Hydra simplifies off-chain protocol and smart contract development by directly adopting the layer 1 smart contract system, allowing the same code to be used both on- and off-chain. Leveraging the Extended Unspent Transaction Output (EUTXO) model, Hydra enables fast off-chain protocol evolution with minimal round complexity and allows asynchronous and concurrent state channel processing. This design enhances transaction confirmation time and throughput while keeping storage requirements low."),(0,o.kt)("p",null,"In Hydra, a set of parties coordinates to commit a set of UTXOs (owned by the parties) into an off-chain protocol called the Head protocol. This UTXO set constitutes the initial head state, which can be evolved by handling smart contracts and transactions among the parties without blockchain interaction, provided all participants behave optimistically. The isomorphic nature of Hydra heads ensures that transaction validation and script execution proceed according to the same rules as on-chain, simplifying engineering and guaranteeing consistency. In case of disputes or termination, the current state of the head is decommitted back to the blockchain, updating the blockchain state to reflect the off-chain protocol evolution. This decommit process is efficient, independent of the number of parties or the size of the head state, and supports incremental commits and decommits, allowing UTXOs to be added or removed from a running head without closing it."),(0,o.kt)("h2",{id:"hydra-head-lifecycle"},"Hydra head lifecycle"),(0,o.kt)("p",null,"There are different flavors and extensions of the Hydra Head protocol, but first, let's discuss the full lifecycle of a basic Hydra head and how it allows for isomorphic state transfer between layer 1 and layer 2."),(0,o.kt)("p",null,(0,o.kt)("img",{src:a(84990).Z,width:"960",height:"540"})),(0,o.kt)("p",null,"A group of online and responsive participants form a Hydra head. Participants ",(0,o.kt)("strong",{parentName:"p"},"init"),"ialize a head by announcing several parameters on-chain, including the participants list. Then, each participant ",(0,o.kt)("strong",{parentName:"p"},"commit"),"s unspent transaction outputs (UTXOs) from the Cardano main chain to it. These UTXOs are ",(0,o.kt)("strong",{parentName:"p"},"collect"),"ed and made available in the Hydra head as the initial state (U0). Participants can ",(0,o.kt)("strong",{parentName:"p"},"abort")," the process and recover their funds at any time before collecting the UTXOs."),(0,o.kt)("p",null,"While open, participants can utilize the Hydra head via a Hydra node to submit transactions in the head network. These transactions maintain the same format and properties as those on the main chain \u2013 they are isomorphic. When spending UTXO entries and creating new ones in a Hydra head, all participants must acknowledge and agree on the new state in so-called snapshots (U1...Un). Snapshots are not posted back onto layer 1 but are only retained by the participants."),(0,o.kt)("p",null,"Any participant can ",(0,o.kt)("strong",{parentName:"p"},"close")," the head using a snapshot, for example, if they wish to cash out on the mainnet or if another party misbehaves or stalls the head's evolution. There exists a mechanism to ",(0,o.kt)("strong",{parentName:"p"},"contest")," the final state on the main chain. Ultimately, a ",(0,o.kt)("inlineCode",{parentName:"p"},"FanOut")," transaction distributes the final agreed state and makes it available on layer 1, transitioning the state that existed virtually in the head to an on-chain state."))}h.isMDXComponent=!0},84990:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/hydra-head-lifecycle-b8449385e9041a214bf8c6e52830de3c.svg"}}]);