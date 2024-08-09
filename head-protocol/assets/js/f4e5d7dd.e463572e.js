"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[4937],{3905:(e,t,n)=>{n.d(t,{Zo:()=>s,kt:()=>k});var a=n(67294);function i(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function r(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function o(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?r(Object(n),!0).forEach((function(t){i(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):r(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,a,i=function(e,t){if(null==e)return{};var n,a,i={},r=Object.keys(e);for(a=0;a<r.length;a++)n=r[a],t.indexOf(n)>=0||(i[n]=e[n]);return i}(e,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(a=0;a<r.length;a++)n=r[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(i[n]=e[n])}return i}var p=a.createContext({}),d=function(e){var t=a.useContext(p),n=t;return e&&(n="function"==typeof e?e(t):o(o({},t),e)),n},s=function(e){var t=d(e.components);return a.createElement(p.Provider,{value:t},e.children)},m="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},c=a.forwardRef((function(e,t){var n=e.components,i=e.mdxType,r=e.originalType,p=e.parentName,s=l(e,["components","mdxType","originalType","parentName"]),m=d(n),c=i,k=m["".concat(p,".").concat(c)]||m[c]||u[c]||r;return n?a.createElement(k,o(o({ref:t},s),{},{components:n})):a.createElement(k,o({ref:t},s))}));function k(e,t){var n=arguments,i=t&&t.mdxType;if("string"==typeof e||i){var r=n.length,o=new Array(r);o[0]=c;var l={};for(var p in t)hasOwnProperty.call(t,p)&&(l[p]=t[p]);l.originalType=e,l[m]="string"==typeof e?e:i,o[1]=l;for(var d=2;d<r;d++)o[d]=n[d];return a.createElement.apply(null,o)}return a.createElement.apply(null,n)}c.displayName="MDXCreateElement"},76492:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>p,contentTitle:()=>o,default:()=>u,frontMatter:()=>r,metadata:()=>l,toc:()=>d});var a=n(87462),i=(n(67294),n(3905));const r={slug:29,title:"29. EventSource & EventSink abstractions\n",authors:["cardenaso11","quantumplation","ch1bo"],tags:["Accepted"]},o=void 0,l={permalink:"/head-protocol/adr/29",source:"@site/adr/2023-11-07_029-event-source-sink.md",title:"29. EventSource & EventSink abstractions\n",description:"Status",date:"2023-11-07T00:00:00.000Z",formattedDate:"November 7, 2023",tags:[{label:"Accepted",permalink:"/head-protocol/adr/tags/accepted"}],readingTime:3.09,hasTruncateMarker:!1,authors:[{name:"Elaine Cardenas",title:"Software Engineer",url:"https://github.com/cardenaso11",imageURL:"https://github.com/cardenaso11.png",key:"cardenaso11"},{name:"Pi Lanningham",title:"Chief Technology Officer, Sundae Labs",url:"https://github.com/quantumplation",imageURL:"https://github.com/quantumplation.png",key:"quantumplation"},{name:"Sebastian Nagel",title:"Software Engineering Lead",url:"https://github.com/ch1bo",imageURL:"https://github.com/ch1bo.png",key:"ch1bo"}],frontMatter:{slug:"29",title:"29. EventSource & EventSink abstractions\n",authors:["cardenaso11","quantumplation","ch1bo"],tags:["Accepted"]},prevItem:{title:"28. Offline mode\n",permalink:"/head-protocol/adr/28. Offline mode"},nextItem:{title:"30. Use CBOR in external representation of Cardano transactions\n",permalink:"/head-protocol/adr/30"}},p={authorsImageUrls:[void 0,void 0,void 0]},d=[{value:"Status",id:"status",level:2},{value:"Context",id:"context",level:2},{value:"Consequences",id:"consequences",level:2},{value:"Out of scope / future work",id:"out-of-scope--future-work",level:2}],s={toc:d},m="wrapper";function u(e){let{components:t,...n}=e;return(0,i.kt)(m,(0,a.Z)({},s,n,{components:t,mdxType:"MDXLayout"}),(0,i.kt)("h2",{id:"status"},"Status"),(0,i.kt)("p",null,"Accepted"),(0,i.kt)("h2",{id:"context"},"Context"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"The Hydra node represents a significant engineering asset, providing layer 1 monitoring, peer to peer consensus, durable persistence, and an isomorphic Cardano ledger. Because of this, it is being eyed as a key building block not just in Hydra based applications, but other protocols as well.")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Currently the ",(0,i.kt)("inlineCode",{parentName:"p"},"hydra-node")," uses a very basic persistence mechanism for it's internal ",(0,i.kt)("inlineCode",{parentName:"p"},"HeadState"),", that is saving ",(0,i.kt)("inlineCode",{parentName:"p"},"StateChanged")," events to file on disk and reading them back to load and re-aggregate the ",(0,i.kt)("inlineCode",{parentName:"p"},"HeadState")," upon startup."),(0,i.kt)("ul",{parentName:"li"},(0,i.kt)("li",{parentName:"ul"},"Some production setups would benefit from storing these events to a service like Amazon Kinesis data stream instead of local files."))),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"The ",(0,i.kt)("inlineCode",{parentName:"p"},"hydra-node")," websocket-based API is the only available event stream right now and might not fit all purposes."),(0,i.kt)("ul",{parentName:"li"},(0,i.kt)("li",{parentName:"ul"},"See also ADR ",(0,i.kt)("a",{parentName:"li",href:"/adr/3"},"3")," and ",(0,i.kt)("a",{parentName:"li",href:"/adr/25"},"25")),(0,i.kt)("li",{parentName:"ul"},"Internally, this is realized as a single ",(0,i.kt)("inlineCode",{parentName:"li"},"Server")," handle which can ",(0,i.kt)("inlineCode",{parentName:"li"},"sendOutput :: ServerOutput tx -> m ()")),(0,i.kt)("li",{parentName:"ul"},"These ",(0,i.kt)("inlineCode",{parentName:"li"},"ServerOutput"),"s closely relate to ",(0,i.kt)("inlineCode",{parentName:"li"},"StateChanged")," events and ",(0,i.kt)("inlineCode",{parentName:"li"},"ClientEffect"),"s are yielded by the logic layer often together with the ",(0,i.kt)("inlineCode",{parentName:"li"},"StateChanged"),". For example:")),(0,i.kt)("pre",{parentName:"li"},(0,i.kt)("code",{parentName:"pre",className:"language-hs"},"onInitialChainAbortTx newChainState committed headId =\n  StateChanged HeadAborted{chainState = newChainState}\n    <> Effects [ClientEffect $ ServerOutput.HeadIsAborted{headId, utxo = fold committed}]\n"))),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Users of ",(0,i.kt)("inlineCode",{parentName:"p"},"hydra-node")," are interested to add alternative implementations for storing, loading and consuming events of the Hydra protocol."))),(0,i.kt)("h1",{id:"decision"},"Decision"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"We create two new interfaces in the ",(0,i.kt)("inlineCode",{parentName:"p"},"hydra-node")," architecture:"),(0,i.kt)("ul",{parentName:"li"},(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"data EventSource e m = EventSource { getEvents :: m [e] }")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("inlineCode",{parentName:"li"},"data EventSink e m = EventSink { putEvent :: e -> m () }")))),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"We realize our current ",(0,i.kt)("inlineCode",{parentName:"p"},"PersistenceIncremental")," used for persisting ",(0,i.kt)("inlineCode",{parentName:"p"},"StateChanged")," events is both an ",(0,i.kt)("inlineCode",{parentName:"p"},"EventSource")," and an ",(0,i.kt)("inlineCode",{parentName:"p"},"EventSink"))),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"We drop the ",(0,i.kt)("inlineCode",{parentName:"p"},"persistence")," from the main handle ",(0,i.kt)("inlineCode",{parentName:"p"},"HydraNode tx m"),", add ",(0,i.kt)("strong",{parentName:"p"},"one")," ",(0,i.kt)("inlineCode",{parentName:"p"},"EventSource")," and allow ",(0,i.kt)("strong",{parentName:"p"},"many")," ",(0,i.kt)("inlineCode",{parentName:"p"},"EventSinks")))),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-hs"},"data HydraNode tx m = HydraNode\n  { -- ...\n  , eventSource :: EventSource (StateEvent tx) m\n  , eventSinks :: [EventSink (StateEvent tx) m]\n  }\n")),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"The ",(0,i.kt)("inlineCode",{parentName:"p"},"hydra-node")," will load events and ",(0,i.kt)("inlineCode",{parentName:"p"},"hydrate")," its ",(0,i.kt)("inlineCode",{parentName:"p"},"HeadState")," using ",(0,i.kt)("inlineCode",{parentName:"p"},"getEvents")," of the single ",(0,i.kt)("inlineCode",{parentName:"p"},"eventSource"),".")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"The ",(0,i.kt)("inlineCode",{parentName:"p"},"stepHydraNode")," main loop does call ",(0,i.kt)("inlineCode",{parentName:"p"},"putEvent")," on all ",(0,i.kt)("inlineCode",{parentName:"p"},"eventSinks")," in sequence. Any failure will make the ",(0,i.kt)("inlineCode",{parentName:"p"},"hydra-node")," process terminate and require a restart.")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"When loading events from ",(0,i.kt)("inlineCode",{parentName:"p"},"eventSource")," on ",(0,i.kt)("inlineCode",{parentName:"p"},"hydra-node")," startup, it will also re-submit events via ",(0,i.kt)("inlineCode",{parentName:"p"},"putEvent")," to all ",(0,i.kt)("inlineCode",{parentName:"p"},"eventSinks"),".")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"The default ",(0,i.kt)("inlineCode",{parentName:"p"},"hydra-node")," main loop does use the file-based ",(0,i.kt)("inlineCode",{parentName:"p"},"EventSource")," and a single file-based ",(0,i.kt)("inlineCode",{parentName:"p"},"EventSink")," (using the same file).")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"We realize that the ",(0,i.kt)("inlineCode",{parentName:"p"},"EventSource")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"EventSink")," handles, as well as their aggregation in ",(0,i.kt)("inlineCode",{parentName:"p"},"HydraNode")," are used as an API by forks of the ",(0,i.kt)("inlineCode",{parentName:"p"},"hydra-node")," and try to minimize changes to it."))),(0,i.kt)("h2",{id:"consequences"},"Consequences"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"The default operation of the ",(0,i.kt)("inlineCode",{parentName:"p"},"hyda-node")," remains unchanged.")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"There are other things called ",(0,i.kt)("inlineCode",{parentName:"p"},"Event")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"EventQueue(putEvent)")," right now in the ",(0,i.kt)("inlineCode",{parentName:"p"},"hydra-node"),". This is getting confusing and when we implement this, we should also rename several things first (tidying).")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Interface first: Implementations of ",(0,i.kt)("inlineCode",{parentName:"p"},"EventSink")," should specify their format in a non-ambiguous and versioned way, especially when a corresponding ",(0,i.kt)("inlineCode",{parentName:"p"},"EventSource")," exists.")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"The API ",(0,i.kt)("inlineCode",{parentName:"p"},"Server")," can be modelled and refactored as an ",(0,i.kt)("inlineCode",{parentName:"p"},"EventSink"),".")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Projects forking the hydra node have dedicated extension points for producing and consuming events.")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},'Sundae Labs can build a "Save transaction batches to S3" proof of concept ',(0,i.kt)("inlineCode",{parentName:"p"},"EventSink"),".")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},'Sundae Labs can build a "Scrolls source" ',(0,i.kt)("inlineCode",{parentName:"p"},"EventSink"),".")),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},'Sundae Labs can build a "Amazon Kinesis" ',(0,i.kt)("inlineCode",{parentName:"p"},"EventSource")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"EventSink"),"."))),(0,i.kt)("h2",{id:"out-of-scope--future-work"},"Out of scope / future work"),(0,i.kt)("ul",null,(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"Available implementations for ",(0,i.kt)("inlineCode",{parentName:"p"},"EventSource")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"EventSink")," could be"),(0,i.kt)("ul",{parentName:"li"},(0,i.kt)("li",{parentName:"ul"},"configured upon ",(0,i.kt)("inlineCode",{parentName:"li"},"hydra-node")," startup using for example URIs: ",(0,i.kt)("inlineCode",{parentName:"li"},"--event-source file://state")," or ",(0,i.kt)("inlineCode",{parentName:"li"},"--event-sink s3://some-bucket")),(0,i.kt)("li",{parentName:"ul"},"dynamically loaded as plugins without having to fork ",(0,i.kt)("inlineCode",{parentName:"li"},"hydra-node"),"."))),(0,i.kt)("li",{parentName:"ul"},(0,i.kt)("p",{parentName:"li"},"The ",(0,i.kt)("inlineCode",{parentName:"p"},"Network")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"Chain")," parts qualify as ",(0,i.kt)("inlineCode",{parentName:"p"},"EventSink"),"s as well or shall those be triggered by ",(0,i.kt)("inlineCode",{parentName:"p"},"Effect"),"s still?"))))}u.isMDXComponent=!0}}]);