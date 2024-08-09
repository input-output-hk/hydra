"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[3211],{3905:(e,t,a)=>{a.d(t,{Zo:()=>u,kt:()=>m});var n=a(67294);function r(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function o(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function i(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?o(Object(a),!0).forEach((function(t){r(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):o(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function s(e,t){if(null==e)return{};var a,n,r=function(e,t){if(null==e)return{};var a,n,r={},o=Object.keys(e);for(n=0;n<o.length;n++)a=o[n],t.indexOf(a)>=0||(r[a]=e[a]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(n=0;n<o.length;n++)a=o[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(r[a]=e[a])}return r}var l=n.createContext({}),d=function(e){var t=n.useContext(l),a=t;return e&&(a="function"==typeof e?e(t):i(i({},t),e)),a},u=function(e){var t=d(e.components);return n.createElement(l.Provider,{value:t},e.children)},c="mdxType",p={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},h=n.forwardRef((function(e,t){var a=e.components,r=e.mdxType,o=e.originalType,l=e.parentName,u=s(e,["components","mdxType","originalType","parentName"]),c=d(a),h=r,m=c["".concat(l,".").concat(h)]||c[h]||p[h]||o;return a?n.createElement(m,i(i({ref:t},u),{},{components:a})):n.createElement(m,i({ref:t},u))}));function m(e,t){var a=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=a.length,i=new Array(o);i[0]=h;var s={};for(var l in t)hasOwnProperty.call(t,l)&&(s[l]=t[l]);s.originalType=e,s[c]="string"==typeof e?e:r,i[1]=s;for(var d=2;d<o;d++)i[d]=a[d];return n.createElement.apply(null,i)}return n.createElement.apply(null,a)}h.displayName="MDXCreateElement"},91150:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>l,contentTitle:()=>i,default:()=>p,frontMatter:()=>o,metadata:()=>s,toc:()=>d});var n=a(87462),r=(a(67294),a(3905));const o={title:"FAQs"},i="Frequently asked questions",s={unversionedId:"faqs",id:"faqs",title:"FAQs",description:"What is Hydra?",source:"@site/docs/faqs.md",sourceDirName:".",slug:"/faqs",permalink:"/head-protocol/docs/faqs",draft:!1,editUrl:"https://github.com/cardano-scaling/hydra/tree/master/docs/docs/faqs.md",tags:[],version:"current",frontMatter:{title:"FAQs"},sidebar:"userDocumentation",previous:{title:"Extend the node with event source and sinks",permalink:"/head-protocol/docs/how-to/event-sinks-and-sources"},next:{title:"Get involved",permalink:"/head-protocol/docs/get-involved"}},l={},d=[{value:"Examples",id:"examples",level:3}],u={toc:d},c="wrapper";function p(e){let{components:t,...a}=e;return(0,r.kt)(c,(0,n.Z)({},u,a,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"frequently-asked-questions"},"Frequently asked questions"),(0,r.kt)("details",null,(0,r.kt)("summary",null,"What is Hydra?"),"\u200b\u200bHydra is a family of layer 2 protocols designed to address network scalability capabilities. Hydra Head is the first in this protocol suite, providing the foundation on which to build out further scalability."),(0,r.kt)("details",null,(0,r.kt)("summary",null,"When Hydra?"),"The project is available on all Cardano networks (including mainnet), and releases with new features become available every four to six weeks. The roadmap is publicly available on ",(0,r.kt)("a",{href:"https://github.com/orgs/input-output-hk/projects/21/views/25"},"GitHub.")),(0,r.kt)("details",null,(0,r.kt)("summary",null,"What is the difference between layer 1 and layer 2?"),"Layer 1 solutions provide the foundational infrastructure of a blockchain network, while layer 2 solutions introduce supplementary protocols or mechanisms to improve scalability and functionality ",(0,r.kt)("a",{href:"https://www.essentialcardano.io/article/layer-1-and-layer-2-all-you-need-to-know"},"Read more in this blog post.")),(0,r.kt)("details",null,(0,r.kt)("summary",null,"Is Hydra Head secure?"),"Absolutely. Hydra protocols were born out of IOG research, got peer-reviewed, and are implemented using test-driven development. The Hydra Head protocol is a true layer 2 and can fall back directly onto the Cardano layer 1 \u2013 hence inheriting the security model of the Cardano blockchain."),(0,r.kt)("details",null,(0,r.kt)("summary",null,"So what\u2019s this I have heard about \u20181M TPS\u2019?"),"This has been previously referenced as a theoretical maximum, but the reality is more nuanced. For a start, with Cardano\u2019s \u2018transactions within transactions\u2019 EUTXO model, TPS itself isn\u2019t a useful metric. A Hydra Head is like a small community within a larger group. Initially, these communities operate independently. So, adding up their metrics to get a total picture isn't accurate. Since Hydra heads use the EUTXO model, they can process transactions simultaneously without conflicts, especially with good networking, which optimizes resource usage. As the project progresses, we're constantly evaluating its real-world performance in terms of throughput and finality. For more details, read ",(0,r.kt)("a",{href:"https://example.com/more-info"},"this")," blog post and see the latest benchmarking data ",(0,r.kt)("a",{href:"https://example.com/latest-data"},"here"),"."),(0,r.kt)("details",null,(0,r.kt)("summary",null,"Can anyone use the Hydra Head protocol?"),"Yes, it's designed to be accessible for developers and end users alike, requiring minimal changes to existing applications to integrate with Hydra Head. However, it is important to note that Hydra is not a network upgrade and it's not like flipping a switch on Cardano to make it fast - instead, applications need to adopt and build on Hydra heads to benefit from it."),(0,r.kt)("details",null,(0,r.kt)("summary",null,"When is the Hydra Head protocol a good fit?"),(0,r.kt)("p",null,"The Hydra Head protocol is well-suited for situations where a known set of participants know each other well enough to agree on building a network but don\u2019t trust each other enough to manage funds without securing their assets. This security is backed by the possibility of settling disputes on layer 1.")),(0,r.kt)("details",null,(0,r.kt)("summary",null,"Can I run Plutus scripts inside a head?"),(0,r.kt)("p",null,"Yes! Transactions running between head participants are fully developed Alonzo transactions. They carry scripts and spend UTXOs in the same manner as layer 1 transactions. Each Hydra node runs a Cardano ledger and maintains a ledger state.")),(0,r.kt)("details",null,(0,r.kt)("summary",null,"Can a third party run a Hydra node on behalf of wallet owners (eg, running managed Hydra heads)?"),(0,r.kt)("p",null,"Totally! This is similar to ",(0,r.kt)("a",{parentName:"p",href:"https://phoenix.acinq.co/"},"Phoenix")," in Bitcoin Lightning: a non-custodial managed lightning node. As an end-user, you retain full control over your keys and funds, but the underlying infrastructure is managed on your behalf (with associated fees). However, this setup requires some level of trust in the service provider to handle contestations and head closures properly.   ")),(0,r.kt)("details",null,(0,r.kt)("summary",null,"What is the relationship between Hydra heads and Hydra nodes?"),(0,r.kt)("p",null,"It is (at least","*",") a ",(0,r.kt)("strong",{parentName:"p"},"one-to-many")," relationship. Each Hydra head consists of several Hydra nodes. We currently aim for up to 100 nodes per head as a stretch goal. Heads are independent and form isolated networks, allowing for infinitely many heads to run in parallel. "),(0,r.kt)("p",null,(0,r.kt)("em",{parentName:"p"},"(","*",") It is possible for Hydra nodes to support multiple heads, creating a many-to-many relationship."))),(0,r.kt)("details",null,(0,r.kt)("summary",null,"Is the Hydra Head protocol a sidechain?"),(0,r.kt)("p",null,"No, it isn't. There are two crucial reasons why Hydra heads are not considered sidechains:"),(0,r.kt)("ol",null,(0,r.kt)("li",{parentName:"ol"},(0,r.kt)("p",{parentName:"li"},"There's no guaranteed data availability in Hydra. Transactions are (a) only known to head participants and (b) typically forgotten once processed. There are no blocks in a Hydra head, and participants have no incentive to keep the history or make it available to users outside the head.")),(0,r.kt)("li",{parentName:"ol"},(0,r.kt)("p",{parentName:"li"},"A head network is static; new participants cannot join and must be decided upfront. The network is isolated and private, accessible only to a set of well-known participants.")))),(0,r.kt)("details",null,(0,r.kt)("summary",null,"If the Hydra Head ledger is configured with a non-zero transaction fee, where do the paid fees go?"),(0,r.kt)("p",null,"Setting protocol parameters with ",(0,r.kt)("inlineCode",{parentName:"p"},"fee > 0")," ensures that Hydra Head (layer 2) transactions consume more than they produce. On layer 1, the UTXO remains unchanged, and the difference accrues. Currently, when settling an agreed state from layer 2 on layer 1 during fanout, this difference becomes spendable by the head participant who posts the ",(0,r.kt)("inlineCode",{parentName:"p"},"fanoutTx"),".")),(0,r.kt)("details",null,(0,r.kt)("summary",null," Is it possible to use different protocol parameters in the Hydra head?"),(0,r.kt)("p",null,"Yes, the ledger used for layer 2 transactions in a Hydra head is configurable, allowing for the same or different protocol parameters as those used in layer 1. ",(0,r.kt)("strong",{parentName:"p"},"However, there is an important caveat to consider"),":"),(0,r.kt)("p",null,"If UTXOs are snapshotted on layer 2, they must be fanned out on layer 1 ",(0,r.kt)("strong",{parentName:"p"},"exactly")," as they were recorded in the snapshot."),(0,r.kt)("h3",{id:"examples"},"Examples"),(0,r.kt)("ol",null,(0,r.kt)("li",{parentName:"ol"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"Minimum UTXO value (",(0,r.kt)("inlineCode",{parentName:"strong"},"minUTxOValue = 0"),")"),":"),(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"Outputs with no 'ada' on layer 2 would be disallowed on layer 1, preventing their fanout. This restriction makes direct fanout impossible for such outputs. Even using partial fanout, as considered in ",(0,r.kt)("a",{parentName:"li",href:"https://github.com/input-output-hk/hydra/issues/190"},"this feature"),", would not permit the fanout of affected UTXOs."))),(0,r.kt)("li",{parentName:"ol"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"Maximum transaction execution units (",(0,r.kt)("inlineCode",{parentName:"strong"},"maxTxExecutionUnits(L2) > maxTxExecutionUnits(L1)"),")"),":"),(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"Outputs directed to scripts, which are too costly to validate on layer 1, can still be fanned out but will become unspendable due to exceeding the allowable execution limits on layer 1.")))),(0,r.kt)("p",null,(0,r.kt)("strong",{parentName:"p"},"Remember"),", with great power comes great responsibility. It is crucial to carefully manage and align the layer 1 and layer 2 settings to ensure seamless operability and avoid unintended consequences in transaction processing.")),(0,r.kt)("details",null,(0,r.kt)("summary",null,"How do I get involved?"),"Join public monthly meetings to engage with the Hydra team and contribute to its open governance. These meetings provide a platform for community developers to stay updated on the latest developments, ask questions directly to the team, and share their ideas. Start building on Hydra like SundaeLabs, Modulo-P, Obsidian Systems, MLabs, and others!"))}p.isMDXComponent=!0}}]);