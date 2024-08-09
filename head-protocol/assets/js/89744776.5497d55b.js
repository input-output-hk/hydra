"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[204],{3905:(e,t,n)=>{n.d(t,{Zo:()=>s,kt:()=>k});var r=n(67294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function l(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function o(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?l(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):l(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function d(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},l=Object.keys(e);for(r=0;r<l.length;r++)n=l[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(r=0;r<l.length;r++)n=l[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var i=r.createContext({}),m=function(e){var t=r.useContext(i),n=t;return e&&(n="function"==typeof e?e(t):o(o({},t),e)),n},s=function(e){var t=m(e.components);return r.createElement(i.Provider,{value:t},e.children)},p="mdxType",c={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},u=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,l=e.originalType,i=e.parentName,s=d(e,["components","mdxType","originalType","parentName"]),p=m(n),u=a,k=p["".concat(i,".").concat(u)]||p[u]||c[u]||l;return n?r.createElement(k,o(o({ref:t},s),{},{components:n})):r.createElement(k,o({ref:t},s))}));function k(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var l=n.length,o=new Array(l);o[0]=u;var d={};for(var i in t)hasOwnProperty.call(t,i)&&(d[i]=t[i]);d.originalType=e,d[p]="string"==typeof e?e:a,o[1]=d;for(var m=2;m<l;m++)o[m]=n[m];return r.createElement.apply(null,o)}return r.createElement.apply(null,n)}u.displayName="MDXCreateElement"},45490:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>i,contentTitle:()=>o,default:()=>c,frontMatter:()=>l,metadata:()=>d,toc:()=>m});var r=n(87462),a=(n(67294),n(3905));const l={sidebar_label:"End-to-End Benchmarks",sidebar_position:4},o="End-To-End Benchmark Results",d={unversionedId:"end-to-end-benchmarks",id:"end-to-end-benchmarks",title:"End-To-End Benchmark Results",description:"This page is intended to collect the latest end-to-end benchmarks  results produced by Hydra's Continuous Integration system from  the latest master code.",source:"@site/benchmarks/end-to-end-benchmarks.md",sourceDirName:".",slug:"/end-to-end-benchmarks",permalink:"/head-protocol/benchmarks/end-to-end-benchmarks",draft:!1,tags:[],version:"current",sidebarPosition:4,frontMatter:{sidebar_label:"End-to-End Benchmarks",sidebar_position:4},sidebar:"defaultSidebar",previous:{title:"Transactions Costs",permalink:"/head-protocol/benchmarks/transaction-cost"},next:{title:"Profiling Hydra scripts",permalink:"/head-protocol/benchmarks/profiling"}},i={},m=[{value:"Baseline Scenario",id:"baseline-scenario",level:2},{value:"Three local nodes",id:"three-local-nodes",level:2}],s={toc:m},p="wrapper";function c(e){let{components:t,...n}=e;return(0,a.kt)(p,(0,r.Z)({},s,n,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("h1",{id:"end-to-end-benchmark-results"},"End-To-End Benchmark Results"),(0,a.kt)("p",null,"This page is intended to collect the latest end-to-end benchmarks  results produced by Hydra's Continuous Integration system from  the latest ",(0,a.kt)("inlineCode",{parentName:"p"},"master")," code."),(0,a.kt)("admonition",{type:"caution"},(0,a.kt)("p",{parentName:"admonition"},"Please take those results with a grain of  salt as they are currently produced from very limited cloud VMs and not controlled  hardware. Instead of focusing on the ",(0,a.kt)("em",{parentName:"p"},"absolute")," results, the emphasis  should be on relative results, eg. how the timings for a scenario  evolve as the code changes.")),(0,a.kt)("p",null,(0,a.kt)("em",{parentName:"p"},"Generated at"),"  2024-05-20 10:59:07.133941059 UTC"),(0,a.kt)("h2",{id:"baseline-scenario"},"Baseline Scenario"),(0,a.kt)("table",null,(0,a.kt)("thead",{parentName:"table"},(0,a.kt)("tr",{parentName:"thead"},(0,a.kt)("th",{parentName:"tr",align:null},"Number of nodes"),(0,a.kt)("th",{parentName:"tr",align:null},"1"))),(0,a.kt)("tbody",{parentName:"table"},(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Number of txs")),(0,a.kt)("td",{parentName:"tr",align:null},"3000")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Avg. Confirmation Time (ms)")),(0,a.kt)("td",{parentName:"tr",align:null},"4.741498110")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"P99")),(0,a.kt)("td",{parentName:"tr",align:null},"9.95393113999985ms")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"P95")),(0,a.kt)("td",{parentName:"tr",align:null},"6.469637849999998ms")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"P50")),(0,a.kt)("td",{parentName:"tr",align:null},"4.420593ms")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Number of Invalid txs")),(0,a.kt)("td",{parentName:"tr",align:null},"0")))),(0,a.kt)("h2",{id:"three-local-nodes"},"Three local nodes"),(0,a.kt)("table",null,(0,a.kt)("thead",{parentName:"table"},(0,a.kt)("tr",{parentName:"thead"},(0,a.kt)("th",{parentName:"tr",align:null},"Number of nodes"),(0,a.kt)("th",{parentName:"tr",align:null},"3"))),(0,a.kt)("tbody",{parentName:"table"},(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Number of txs")),(0,a.kt)("td",{parentName:"tr",align:null},"9000")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Avg. Confirmation Time (ms)")),(0,a.kt)("td",{parentName:"tr",align:null},"21.952702156")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"P99")),(0,a.kt)("td",{parentName:"tr",align:null},"61.235459320000004ms")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"P95")),(0,a.kt)("td",{parentName:"tr",align:null},"31.5184715ms")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"P50")),(0,a.kt)("td",{parentName:"tr",align:null},"19.649436ms")),(0,a.kt)("tr",{parentName:"tbody"},(0,a.kt)("td",{parentName:"tr",align:null},(0,a.kt)("em",{parentName:"td"},"Number of Invalid txs")),(0,a.kt)("td",{parentName:"tr",align:null},"0")))))}c.isMDXComponent=!0}}]);