"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[5229],{3905:(e,t,n)=>{n.d(t,{Zo:()=>p,kt:()=>m});var a=n(67294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function s(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},o=Object.keys(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var l=a.createContext({}),d=function(e){var t=a.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},p=function(e){var t=d(e.components);return a.createElement(l.Provider,{value:t},e.children)},c="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},h=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,o=e.originalType,l=e.parentName,p=s(e,["components","mdxType","originalType","parentName"]),c=d(n),h=r,m=c["".concat(l,".").concat(h)]||c[h]||u[h]||o;return n?a.createElement(m,i(i({ref:t},p),{},{components:n})):a.createElement(m,i({ref:t},p))}));function m(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=n.length,i=new Array(o);i[0]=h;var s={};for(var l in t)hasOwnProperty.call(t,l)&&(s[l]=t[l]);s.originalType=e,s[c]="string"==typeof e?e:r,i[1]=s;for(var d=2;d<o;d++)i[d]=n[d];return a.createElement.apply(null,i)}return a.createElement.apply(null,n)}h.displayName="MDXCreateElement"},23681:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>l,contentTitle:()=>i,default:()=>u,frontMatter:()=>o,metadata:()=>s,toc:()=>d});var a=n(87462),r=(n(67294),n(3905));const o={},i="Networking",s={unversionedId:"dev/architecture/networking",id:"dev/architecture/networking",title:"Networking",description:"This document provides details about the Hydra networking layer, which encompasses the network of Hydra nodes where heads can be opened.",source:"@site/docs/dev/architecture/networking.md",sourceDirName:"dev/architecture",slug:"/dev/architecture/networking",permalink:"/head-protocol/docs/dev/architecture/networking",draft:!1,editUrl:"https://github.com/cardano-scaling/hydra/tree/master/docs/docs/dev/architecture/networking.md",tags:[],version:"current",frontMatter:{},sidebar:"developerDocumentation",previous:{title:"Architecture",permalink:"/head-protocol/docs/dev/architecture/"},next:{title:"Handling rollbacks",permalink:"/head-protocol/docs/dev/rollbacks/"}},l={},d=[{value:"Questions",id:"questions",level:2},{value:"Investigations",id:"investigations",level:2},{value:"Ouroboros",id:"ouroboros",level:3},{value:"Cardano networking",id:"cardano-networking",level:3},{value:"Implementations",id:"implementations",level:2},{value:"Current state",id:"current-state",level:3},{value:"Gossip diffusion network",id:"gossip-diffusion-network",level:3}],p={toc:d},c="wrapper";function u(e){let{components:t,...o}=e;return(0,r.kt)(c,(0,a.Z)({},p,o,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"networking"},"Networking"),(0,r.kt)("p",null,"This document provides details about the Hydra networking layer, which encompasses the network of Hydra nodes where heads can be opened."),(0,r.kt)("admonition",{type:"warning"},(0,r.kt)("p",{parentName:"admonition"},"\ud83d\udee0 This document is a work in progress. We recognize that the current state of networking is suboptimal, serving as an initial implementation to establish a functional basis. Efforts are underway to enhance the network dynamics through a proposed improvement initiative, detailed in ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/pull/237"},"this proposal"),".")),(0,r.kt)("h2",{id:"questions"},"Questions"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"What's the expected topology of the transport layer?",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"Are connected peers a subset, superset, or identical set of the head parties?"))),(0,r.kt)("li",{parentName:"ul"},"Do we need the delivery ordering and reliability guarantees TCP provides?",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"TCP provides full-duplex, stream-oriented, persistent connections between nodes"),(0,r.kt)("li",{parentName:"ul"},"The Hydra networking layer is based on asynchronous message passing, which seems better suited to UDP"))),(0,r.kt)("li",{parentName:"ul"},"Do we need to consider nodes being reachable through firewalls?",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"This responsibility could be delegated to end users, allowing them to configure their firewalls/NATs to align with Hydra node requirements"),(0,r.kt)("li",{parentName:"ul"},"This may be more manageable for business, corporate, or organizational parties than for individual end-users"))),(0,r.kt)("li",{parentName:"ul"},"Do we want ",(0,r.kt)("em",{parentName:"li"},"privacy")," within a head?",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"Transactions' details should be opaque to outside observers, with only the final outcome of the head's fanout being observable"))),(0,r.kt)("li",{parentName:"ul"},"How do we identify/discover peers/parties?",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"The paper assumes a ",(0,r.kt)("em",{parentName:"li"},"setup")," phase where:",(0,r.kt)("blockquote",{parentName:"li"},(0,r.kt)("p",{parentName:"blockquote"},"To create a head-protocol instance, an initiator invites a set of participants ","{","p1,...,pn","}"," (including themselves) to join by announcing protocol parameters: the participant list, parameters of the (multi-)signature scheme, etc.\nEach party subsequently establishes pairwise authenticated channels with all other parties involved."))))),(0,r.kt)("li",{parentName:"ul"},"What constitutes a ",(0,r.kt)("em",{parentName:"li"},"list of participants"),"? Should each participant be uniquely identifiable? If so, what identification method should be used \u2014 naming scheme, IP: port address, public key, certificate?",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"What do 'pairwise authenticated channels' entail? Are these actual TCP/TLS connections, or do they operate at the Transport (layer 4) or Session (layer 5) level?"))),(0,r.kt)("li",{parentName:"ul"},"How open do we want our network protocol to be?",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"Currently leveraging the Ouroboros stack with CBOR message encoding, integrating other tools into the Hydra network may pose challenges.")))),(0,r.kt)("h2",{id:"investigations"},"Investigations"),(0,r.kt)("h3",{id:"ouroboros"},"Ouroboros"),(0,r.kt)("p",null,"We held a meeting with the networking team on February 14, 2022, to explore the integration of the Ouroboros network stack into Hydra. During the discussion, there was a notable focus on performance, with Neil Davies providing insightful performance metrics."),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"World circumference: 600ms"),(0,r.kt)("li",{parentName:"ul"},"Latency w/in 1 continent: 50-100ms"),(0,r.kt)("li",{parentName:"ul"},"Latency w/in DC: 2-3ms"),(0,r.kt)("li",{parentName:"ul"},"Subsecond roundtrip should be fine wherever the nodes are located"),(0,r.kt)("li",{parentName:"ul"},"Basic reliability of TCP connections decreases w/ distance:",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"w/in DC connection can last forever"),(0,r.kt)("li",{parentName:"ul"},"outside DC: it's hard to keep a single TCP cnx up forever; if a reroute occurs because some intermediate node is down, it takes 90s to resettle a route"),(0,r.kt)("li",{parentName:"ul"},"this implies that as the number of connections goes up, the probability of having at least one connection down at all times increases"))),(0,r.kt)("li",{parentName:"ul"},"Closing of the head must be dissociated from network connections => a TCP cnx disappearing =/=> closing the head"),(0,r.kt)("li",{parentName:"ul"},"Within the Cardano network, propagation of a single empty block takes 400ms (to reach 10K nodes)",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"the Ouroboros network should withstand 1000s of connections (there are some system-level limits)"))),(0,r.kt)("li",{parentName:"ul"},"Modelling the Hydra network",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"a logical framework for modelling the performance of network associate CDF with time for a message to appear at all nodes (this is what is done in the ",(0,r.kt)("a",{parentName:"li",href:"https://github.com/input-output-hk/hydra-sim"},"hydra-sim")),(0,r.kt)("li",{parentName:"ul"},"we could define a layer w/ the semantics we expect; for example, Snocket = PTP connection w/ ordered guaranteed messages delivery \u2013 do we need that in Hydra?"))),(0,r.kt)("li",{parentName:"ul"},"How about ",(0,r.kt)("a",{parentName:"li",href:"https://wireguard.io"},"Wireguard"),"? It's a very interesting approach, with some shortcomings:",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"no global addressing scheme"),(0,r.kt)("li",{parentName:"ul"},"there is one ",(0,r.kt)("inlineCode",{parentName:"li"},"eth")," interface/connection"),(0,r.kt)("li",{parentName:"ul"},"on the plus side, it transparently manages IP address changes"),(0,r.kt)("li",{parentName:"ul"},"does not help w/ Firewalls, eg NAT needs to be configured on each node.")))),(0,r.kt)("h3",{id:"cardano-networking"},"Cardano networking"),(0,r.kt)("p",null,"See ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra.wiki/blob/master/Networking.md#L1"},"this Wiki page")," for detailed notes about how the Cardano network works and uses Ouroboros."),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Cardano is a global network spanning thousands of nodes, with nodes constantly joining and leaving, resulting in a widely varying topology. Its primary function is block propagation: blocks produced by certain nodes according to consensus rules must reach every node in the network within 20 seconds."),(0,r.kt)("li",{parentName:"ul"},"Nodes cannot maintain direct connections to all other nodes; instead, block diffusion occurs through a form of ",(0,r.kt)("em",{parentName:"li"},"gossiping"),". Each node is connected to a limited set of peers with whom it exchanges blocks."),(0,r.kt)("li",{parentName:"ul"},"Nodes must withstand adversarial behavior from peers and other nodes, necessitating control over the amount and rate of data they ingest. Hence, a ",(0,r.kt)("em",{parentName:"li"},"pull-based")," messaging layer is essential."),(0,r.kt)("li",{parentName:"ul"},"Producer nodes, which require access to signing keys, are considered sensitive assets. They are typically operated behind ",(0,r.kt)("em",{parentName:"li"},"relay nodes")," to enhance security and mitigate the risks of DoS attacks or other malicious activities."),(0,r.kt)("li",{parentName:"ul"},"Nodes often operate behind ADSL or cable modems, firewalls, or in other complex networking environments that prevent direct addressing. Therefore, nodes must initiate connections to externally reachable ",(0,r.kt)("em",{parentName:"li"},"relay nodes"),", and rely on a ",(0,r.kt)("em",{parentName:"li"},"pull-based")," messaging approach.")),(0,r.kt)("h2",{id:"implementations"},"Implementations"),(0,r.kt)("h3",{id:"current-state"},"Current state"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Hydra nodes form a network of pairwise connected ",(0,r.kt)("em",{parentName:"li"},"peers")," using point-to-point (eg, TCP) connections that are expected to remain active at all times:",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"Nodes use ",(0,r.kt)("a",{parentName:"li",href:"https://github.com/input-output-hk/ouroboros-network/"},"Ouroboros")," as the underlying network abstraction, which manages connections with peers via a reliable point-to-point stream-based communication framework known as a ",(0,r.kt)("inlineCode",{parentName:"li"},"Snocket")),(0,r.kt)("li",{parentName:"ul"},"All messages are ",(0,r.kt)("em",{parentName:"li"},"broadcast")," to peers using the PTP connections"),(0,r.kt)("li",{parentName:"ul"},"Due to the nature of the Hydra protocol, the lack of a connection to a peer halts any progress of the head."))),(0,r.kt)("li",{parentName:"ul"},"A ",(0,r.kt)("inlineCode",{parentName:"li"},"hydra-node")," can only open a head with ",(0,r.kt)("em",{parentName:"li"},"all")," its peers and exclusively with them. This necessitates that nodes possess prior knowledge of the topology of both peers and heads they intend to establish."),(0,r.kt)("li",{parentName:"ul"},"Connected nodes implement basic ",(0,r.kt)("em",{parentName:"li"},"failure detection")," through heartbeats and monitoring exchanged messages."),(0,r.kt)("li",{parentName:"ul"},"Messages exchanged between peers are signed using the party's Hydra key and validated upon receiving.")),(0,r.kt)("h3",{id:"gossip-diffusion-network"},"Gossip diffusion network"),(0,r.kt)("p",null,"The following diagram illustrates one possible implementation of a pull-based messaging system for Hydra, developed from discussions with IOG\u2019s networking engineers:"),(0,r.kt)("p",null,(0,r.kt)("img",{alt:"Hydra pull-based network",src:n(9968).Z,width:"5827",height:"2745"})))}u.isMDXComponent=!0},9968:(e,t,n)=>{n.d(t,{Z:()=>a});const a=n.p+"assets/images/hydra-pull-based-network-82c3d214f8e8d9b2054a23a4fadd48db.jpg"}}]);