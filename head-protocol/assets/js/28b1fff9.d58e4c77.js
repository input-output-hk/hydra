"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[2627],{3905:(t,a,e)=>{e.d(a,{Zo:()=>d,kt:()=>h});var r=e(67294);function n(t,a,e){return a in t?Object.defineProperty(t,a,{value:e,enumerable:!0,configurable:!0,writable:!0}):t[a]=e,t}function i(t,a){var e=Object.keys(t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(t);a&&(r=r.filter((function(a){return Object.getOwnPropertyDescriptor(t,a).enumerable}))),e.push.apply(e,r)}return e}function l(t){for(var a=1;a<arguments.length;a++){var e=null!=arguments[a]?arguments[a]:{};a%2?i(Object(e),!0).forEach((function(a){n(t,a,e[a])})):Object.getOwnPropertyDescriptors?Object.defineProperties(t,Object.getOwnPropertyDescriptors(e)):i(Object(e)).forEach((function(a){Object.defineProperty(t,a,Object.getOwnPropertyDescriptor(e,a))}))}return t}function g(t,a){if(null==t)return{};var e,r,n=function(t,a){if(null==t)return{};var e,r,n={},i=Object.keys(t);for(r=0;r<i.length;r++)e=i[r],a.indexOf(e)>=0||(n[e]=t[e]);return n}(t,a);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(t);for(r=0;r<i.length;r++)e=i[r],a.indexOf(e)>=0||Object.prototype.propertyIsEnumerable.call(t,e)&&(n[e]=t[e])}return n}var m=r.createContext({}),p=function(t){var a=r.useContext(m),e=a;return t&&(e="function"==typeof t?t(a):l(l({},a),t)),e},d=function(t){var a=p(t.components);return r.createElement(m.Provider,{value:a},t.children)},k="mdxType",N={inlineCode:"code",wrapper:function(t){var a=t.children;return r.createElement(r.Fragment,{},a)}},o=r.forwardRef((function(t,a){var e=t.components,n=t.mdxType,i=t.originalType,m=t.parentName,d=g(t,["components","mdxType","originalType","parentName"]),k=p(e),o=n,h=k["".concat(m,".").concat(o)]||k[o]||N[o]||i;return e?r.createElement(h,l(l({ref:a},d),{},{components:e})):r.createElement(h,l({ref:a},d))}));function h(t,a){var e=arguments,n=a&&a.mdxType;if("string"==typeof t||n){var i=e.length,l=new Array(i);l[0]=o;var g={};for(var m in a)hasOwnProperty.call(a,m)&&(g[m]=a[m]);g.originalType=t,g[k]="string"==typeof t?t:n,l[1]=g;for(var p=2;p<i;p++)l[p]=e[p];return r.createElement.apply(null,l)}return r.createElement.apply(null,e)}o.displayName="MDXCreateElement"},60480:(t,a,e)=>{e.r(a),e.d(a,{assets:()=>m,contentTitle:()=>l,default:()=>N,frontMatter:()=>i,metadata:()=>g,toc:()=>p});var r=e(87462),n=(e(67294),e(3905));const i={sidebar_label:"Transactions Costs",sidebar_position:3},l="Transactions Costs",g={unversionedId:"transaction-cost",id:"transaction-cost",title:"Transactions Costs",description:"Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using arbitrary values and results are not fully deterministic and comparable to previous runs.",source:"@site/benchmarks/transaction-cost.md",sourceDirName:".",slug:"/transaction-cost",permalink:"/head-protocol/benchmarks/transaction-cost",draft:!1,tags:[],version:"current",sidebarPosition:3,frontMatter:{sidebar_label:"Transactions Costs",sidebar_position:3},sidebar:"defaultSidebar",previous:{title:"Plutus Merkle-Tree Benchmarks",permalink:"/head-protocol/benchmarks/mt-benchmarks"},next:{title:"End-to-End Benchmarks",permalink:"/head-protocol/benchmarks/end-to-end-benchmarks"}},m={},p=[{value:"Script summary",id:"script-summary",level:2},{value:"Cost of Init Transaction",id:"cost-of-init-transaction",level:2},{value:"Cost of Commit Transaction",id:"cost-of-commit-transaction",level:2},{value:"Cost of CollectCom Transaction",id:"cost-of-collectcom-transaction",level:2},{value:"Cost of Close Transaction",id:"cost-of-close-transaction",level:2},{value:"Cost of Contest Transaction",id:"cost-of-contest-transaction",level:2},{value:"Cost of Abort Transaction",id:"cost-of-abort-transaction",level:2},{value:"Cost of FanOut Transaction",id:"cost-of-fanout-transaction",level:2}],d={toc:p},k="wrapper";function N(t){let{components:a,...e}=t;return(0,n.kt)(k,(0,r.Z)({},d,e,{components:a,mdxType:"MDXLayout"}),(0,n.kt)("h1",{id:"transactions-costs"},"Transactions Costs"),(0,n.kt)("p",null,"Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using ",(0,n.kt)("inlineCode",{parentName:"p"},"arbitrary")," values and results are not fully deterministic and comparable to previous runs."),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Metadata"),(0,n.kt)("th",{parentName:"tr",align:"left"}))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},(0,n.kt)("em",{parentName:"td"},"Generated at")),(0,n.kt)("td",{parentName:"tr",align:"left"},"2024-05-20 10:56:03.976395996 UTC")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},(0,n.kt)("em",{parentName:"td"},"Max. memory units")),(0,n.kt)("td",{parentName:"tr",align:"left"},"14000000")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},(0,n.kt)("em",{parentName:"td"},"Max. CPU units")),(0,n.kt)("td",{parentName:"tr",align:"left"},"10000000000")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},(0,n.kt)("em",{parentName:"td"},"Max. tx size (kB)")),(0,n.kt)("td",{parentName:"tr",align:"left"},"16384")))),(0,n.kt)("h2",{id:"script-summary"},"Script summary"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Name"),(0,n.kt)("th",{parentName:"tr",align:"left"},"Hash"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Size (Bytes)"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"\u03bdInitial"),(0,n.kt)("td",{parentName:"tr",align:"left"},"bccf2a430c016bc960fbf31b02694011cd399d20da8882aac9d33611"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4110")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"\u03bdCommit"),(0,n.kt)("td",{parentName:"tr",align:"left"},"56b0f0b597150e619c76bed60683f3b1e42d7bc0685ed951b882bfc5"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1975")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"\u03bdHead"),(0,n.kt)("td",{parentName:"tr",align:"left"},"86bff95ba20e9d1d1b34899a56d86bbacc9fed999260b27dcc92d128"),(0,n.kt)("td",{parentName:"tr",align:"right"},"9351")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"\u03bcHead"),(0,n.kt)("td",{parentName:"tr",align:"left"},"88f533cf67cd0fc93d7d9ccf0a8b1d69ffd1208a825efbebbc1d36ba*"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4213")))),(0,n.kt)("ul",null,(0,n.kt)("li",{parentName:"ul"},"The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per Head.")),(0,n.kt)("h2",{id:"cost-of-init-transaction"},"Cost of Init Transaction"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4799"),(0,n.kt)("td",{parentName:"tr",align:"right"},"8.97"),(0,n.kt)("td",{parentName:"tr",align:"right"},"3.44"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.46")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"2"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4995"),(0,n.kt)("td",{parentName:"tr",align:"right"},"10.76"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4.12"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.49")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"3"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5196"),(0,n.kt)("td",{parentName:"tr",align:"right"},"12.70"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4.86"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.52")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5597"),(0,n.kt)("td",{parentName:"tr",align:"right"},"16.53"),(0,n.kt)("td",{parentName:"tr",align:"right"},"6.33"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.58")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"10"),(0,n.kt)("td",{parentName:"tr",align:"right"},"6604"),(0,n.kt)("td",{parentName:"tr",align:"right"},"26.35"),(0,n.kt)("td",{parentName:"tr",align:"right"},"10.11"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.73")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"48"),(0,n.kt)("td",{parentName:"tr",align:"right"},"14245"),(0,n.kt)("td",{parentName:"tr",align:"right"},"99.87"),(0,n.kt)("td",{parentName:"tr",align:"right"},"38.32"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.86")))),(0,n.kt)("h2",{id:"cost-of-commit-transaction"},"Cost of Commit Transaction"),(0,n.kt)("p",null," This is using ada-only outputs for better comparability."),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"UTxO"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"right"},"556"),(0,n.kt)("td",{parentName:"tr",align:"right"},"10.24"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4.04"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.29")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"2"),(0,n.kt)("td",{parentName:"tr",align:"right"},"748"),(0,n.kt)("td",{parentName:"tr",align:"right"},"13.88"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5.64"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.34")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"3"),(0,n.kt)("td",{parentName:"tr",align:"right"},"937"),(0,n.kt)("td",{parentName:"tr",align:"right"},"17.66"),(0,n.kt)("td",{parentName:"tr",align:"right"},"7.29"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.39")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1303"),(0,n.kt)("td",{parentName:"tr",align:"right"},"25.66"),(0,n.kt)("td",{parentName:"tr",align:"right"},"10.74"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.49")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"10"),(0,n.kt)("td",{parentName:"tr",align:"right"},"2239"),(0,n.kt)("td",{parentName:"tr",align:"right"},"48.19"),(0,n.kt)("td",{parentName:"tr",align:"right"},"20.30"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.78")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"19"),(0,n.kt)("td",{parentName:"tr",align:"right"},"3930"),(0,n.kt)("td",{parentName:"tr",align:"right"},"97.83"),(0,n.kt)("td",{parentName:"tr",align:"right"},"40.79"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.41")))),(0,n.kt)("h2",{id:"cost-of-collectcom-transaction"},"Cost of CollectCom Transaction"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,n.kt)("th",{parentName:"tr",align:"left"},"UTxO (bytes)"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"left"},"57"),(0,n.kt)("td",{parentName:"tr",align:"right"},"544"),(0,n.kt)("td",{parentName:"tr",align:"right"},"16.88"),(0,n.kt)("td",{parentName:"tr",align:"right"},"6.67"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.36")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"2"),(0,n.kt)("td",{parentName:"tr",align:"left"},"114"),(0,n.kt)("td",{parentName:"tr",align:"right"},"654"),(0,n.kt)("td",{parentName:"tr",align:"right"},"27.39"),(0,n.kt)("td",{parentName:"tr",align:"right"},"10.90"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.48")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"3"),(0,n.kt)("td",{parentName:"tr",align:"left"},"171"),(0,n.kt)("td",{parentName:"tr",align:"right"},"768"),(0,n.kt)("td",{parentName:"tr",align:"right"},"37.79"),(0,n.kt)("td",{parentName:"tr",align:"right"},"15.22"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.60")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"4"),(0,n.kt)("td",{parentName:"tr",align:"left"},"225"),(0,n.kt)("td",{parentName:"tr",align:"right"},"874"),(0,n.kt)("td",{parentName:"tr",align:"right"},"53.58"),(0,n.kt)("td",{parentName:"tr",align:"right"},"21.63"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.78")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"283"),(0,n.kt)("td",{parentName:"tr",align:"right"},"984"),(0,n.kt)("td",{parentName:"tr",align:"right"},"69.02"),(0,n.kt)("td",{parentName:"tr",align:"right"},"28.01"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.95")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"6"),(0,n.kt)("td",{parentName:"tr",align:"left"},"338"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1095"),(0,n.kt)("td",{parentName:"tr",align:"right"},"85.61"),(0,n.kt)("td",{parentName:"tr",align:"right"},"34.94"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.14")))),(0,n.kt)("h2",{id:"cost-of-close-transaction"},"Cost of Close Transaction"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"right"},"622"),(0,n.kt)("td",{parentName:"tr",align:"right"},"8.34"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4.63"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.28")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"2"),(0,n.kt)("td",{parentName:"tr",align:"right"},"734"),(0,n.kt)("td",{parentName:"tr",align:"right"},"9.00"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5.65"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.30")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"3"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1017"),(0,n.kt)("td",{parentName:"tr",align:"right"},"10.43"),(0,n.kt)("td",{parentName:"tr",align:"right"},"7.41"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.33")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1262"),(0,n.kt)("td",{parentName:"tr",align:"right"},"11.85"),(0,n.kt)("td",{parentName:"tr",align:"right"},"9.51"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.37")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"10"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1386"),(0,n.kt)("td",{parentName:"tr",align:"right"},"14.43"),(0,n.kt)("td",{parentName:"tr",align:"right"},"8.04"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.39")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"50"),(0,n.kt)("td",{parentName:"tr",align:"right"},"7784"),(0,n.kt)("td",{parentName:"tr",align:"right"},"48.82"),(0,n.kt)("td",{parentName:"tr",align:"right"},"61.58"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.33")))),(0,n.kt)("h2",{id:"cost-of-contest-transaction"},"Cost of Contest Transaction"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"right"},"643"),(0,n.kt)("td",{parentName:"tr",align:"right"},"8.65"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4.73"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.28")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"2"),(0,n.kt)("td",{parentName:"tr",align:"right"},"781"),(0,n.kt)("td",{parentName:"tr",align:"right"},"9.38"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5.82"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.30")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"3"),(0,n.kt)("td",{parentName:"tr",align:"right"},"847"),(0,n.kt)("td",{parentName:"tr",align:"right"},"9.66"),(0,n.kt)("td",{parentName:"tr",align:"right"},"6.52"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.31")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1271"),(0,n.kt)("td",{parentName:"tr",align:"right"},"12.51"),(0,n.kt)("td",{parentName:"tr",align:"right"},"9.75"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.38")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"10"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1806"),(0,n.kt)("td",{parentName:"tr",align:"right"},"15.07"),(0,n.kt)("td",{parentName:"tr",align:"right"},"14.31"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.45")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"50"),(0,n.kt)("td",{parentName:"tr",align:"right"},"8039"),(0,n.kt)("td",{parentName:"tr",align:"right"},"51.10"),(0,n.kt)("td",{parentName:"tr",align:"right"},"63.35"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.37")))),(0,n.kt)("h2",{id:"cost-of-abort-transaction"},"Cost of Abort Transaction"),(0,n.kt)("p",null,"Some variation because of random mixture of still initial and already committed outputs."),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4676"),(0,n.kt)("td",{parentName:"tr",align:"right"},"17.09"),(0,n.kt)("td",{parentName:"tr",align:"right"},"7.40"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.55")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"2"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4661"),(0,n.kt)("td",{parentName:"tr",align:"right"},"20.95"),(0,n.kt)("td",{parentName:"tr",align:"right"},"8.73"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.59")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"3"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4899"),(0,n.kt)("td",{parentName:"tr",align:"right"},"40.70"),(0,n.kt)("td",{parentName:"tr",align:"right"},"17.82"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.82")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"4"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5064"),(0,n.kt)("td",{parentName:"tr",align:"right"},"58.04"),(0,n.kt)("td",{parentName:"tr",align:"right"},"25.49"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.03")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5076"),(0,n.kt)("td",{parentName:"tr",align:"right"},"65.84"),(0,n.kt)("td",{parentName:"tr",align:"right"},"28.62"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.11")))),(0,n.kt)("h2",{id:"cost-of-fanout-transaction"},"Cost of FanOut Transaction"),(0,n.kt)("p",null,"Involves spending head output and burning head tokens. Uses ada-only UTxO for better comparability."),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,n.kt)("th",{parentName:"tr",align:"left"},"UTxO"),(0,n.kt)("th",{parentName:"tr",align:"left"},"UTxO (bytes)"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"0"),(0,n.kt)("td",{parentName:"tr",align:"left"},"0"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4627"),(0,n.kt)("td",{parentName:"tr",align:"right"},"7.85"),(0,n.kt)("td",{parentName:"tr",align:"right"},"3.28"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.44")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"left"},"57"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4661"),(0,n.kt)("td",{parentName:"tr",align:"right"},"9.21"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4.08"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.46")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"285"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4797"),(0,n.kt)("td",{parentName:"tr",align:"right"},"13.19"),(0,n.kt)("td",{parentName:"tr",align:"right"},"6.66"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.52")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"10"),(0,n.kt)("td",{parentName:"tr",align:"left"},"570"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4968"),(0,n.kt)("td",{parentName:"tr",align:"right"},"19.16"),(0,n.kt)("td",{parentName:"tr",align:"right"},"10.31"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.60")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"20"),(0,n.kt)("td",{parentName:"tr",align:"left"},"1138"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5306"),(0,n.kt)("td",{parentName:"tr",align:"right"},"30.69"),(0,n.kt)("td",{parentName:"tr",align:"right"},"17.44"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.76")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"30"),(0,n.kt)("td",{parentName:"tr",align:"left"},"1707"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5647"),(0,n.kt)("td",{parentName:"tr",align:"right"},"42.64"),(0,n.kt)("td",{parentName:"tr",align:"right"},"24.76"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.92")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"40"),(0,n.kt)("td",{parentName:"tr",align:"left"},"2277"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5986"),(0,n.kt)("td",{parentName:"tr",align:"right"},"53.96"),(0,n.kt)("td",{parentName:"tr",align:"right"},"31.81"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.08")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"50"),(0,n.kt)("td",{parentName:"tr",align:"left"},"2846"),(0,n.kt)("td",{parentName:"tr",align:"right"},"6326"),(0,n.kt)("td",{parentName:"tr",align:"right"},"65.50"),(0,n.kt)("td",{parentName:"tr",align:"right"},"38.95"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.24")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"79"),(0,n.kt)("td",{parentName:"tr",align:"left"},"4490"),(0,n.kt)("td",{parentName:"tr",align:"right"},"7302"),(0,n.kt)("td",{parentName:"tr",align:"right"},"98.79"),(0,n.kt)("td",{parentName:"tr",align:"right"},"59.59"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.70")))))}N.isMDXComponent=!0}}]);