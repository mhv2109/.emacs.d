"use strict";(()=>{var $,_,we,sn,D,ge,xe,ne,be,P={},ke=[],an=/acit|ex(?:s|g|n|p|$)|rph|grid|ows|mnc|ntw|ine[ch]|zoo|^ord|itera/i,re=Array.isArray;function F(e,n){for(var t in n)e[t]=n[t];return e}function Te(e){var n=e.parentNode;n&&n.removeChild(e)}function s(e,n,t){var o,r,i,l={};for(i in n)i=="key"?o=n[i]:i=="ref"?r=n[i]:l[i]=n[i];if(arguments.length>2&&(l.children=arguments.length>3?$.call(arguments,2):t),typeof e=="function"&&e.defaultProps!=null)for(i in e.defaultProps)l[i]===void 0&&(l[i]=e.defaultProps[i]);return U(e,l,o,r,null)}function U(e,n,t,o,r){var i={type:e,props:n,key:t,ref:o,__k:null,__:null,__b:0,__e:null,__d:void 0,__c:null,constructor:void 0,__v:r??++we,__i:-1,__u:0};return r==null&&_.vnode!=null&&_.vnode(i),i}function b(e){return e.children}function z(e,n){this.props=e,this.context=n}function E(e,n){if(n==null)return e.__?E(e.__,e.__i+1):null;for(var t;n<e.__k.length;n++)if((t=e.__k[n])!=null&&t.__e!=null)return t.__e;return typeof e.type=="function"?E(e):null}function Le(e){var n,t;if((e=e.__)!=null&&e.__c!=null){for(e.__e=e.__c.base=null,n=0;n<e.__k.length;n++)if((t=e.__k[n])!=null&&t.__e!=null){e.__e=e.__c.base=t.__e;break}return Le(e)}}function te(e){(!e.__d&&(e.__d=!0)&&D.push(e)&&!q.__r++||ge!==_.debounceRendering)&&((ge=_.debounceRendering)||xe)(q)}function q(){var e,n,t,o,r,i,l,u,d;for(D.sort(ne);e=D.shift();)e.__d&&(n=D.length,o=void 0,i=(r=(t=e).__v).__e,u=[],d=[],(l=t.__P)&&((o=F({},r)).__v=r.__v+1,_.vnode&&_.vnode(o),ie(l,o,r,t.__n,l.ownerSVGElement!==void 0,32&r.__u?[i]:null,u,i??E(r),!!(32&r.__u),d),o.__.__k[o.__i]=o,Fe(u,o,d),o.__e!=i&&Le(o)),D.length>n&&D.sort(ne));q.__r=0}function Se(e,n,t,o,r,i,l,u,d,c,p){var a,m,f,w,g,y=o&&o.__k||ke,v=n.length;for(t.__d=d,un(t,n,y),d=t.__d,a=0;a<v;a++)(f=t.__k[a])!=null&&typeof f!="boolean"&&typeof f!="function"&&(m=f.__i===-1?P:y[f.__i]||P,f.__i=a,ie(e,f,m,r,i,l,u,d,c,p),w=f.__e,f.ref&&m.ref!=f.ref&&(m.ref&&se(m.ref,null,f),p.push(f.ref,f.__c||w,f)),g==null&&w!=null&&(g=w),65536&f.__u||m.__k===f.__k?d=Ce(f,d,e):typeof f.type=="function"&&f.__d!==void 0?d=f.__d:w&&(d=w.nextSibling),f.__d=void 0,f.__u&=-196609);t.__d=d,t.__e=g}function un(e,n,t){var o,r,i,l,u,d=n.length,c=t.length,p=c,a=0;for(e.__k=[],o=0;o<d;o++)(r=e.__k[o]=(r=n[o])==null||typeof r=="boolean"||typeof r=="function"?null:typeof r=="string"||typeof r=="number"||typeof r=="bigint"||r.constructor==String?U(null,r,null,null,r):re(r)?U(b,{children:r},null,null,null):r.constructor===void 0&&r.__b>0?U(r.type,r.props,r.key,r.ref?r.ref:null,r.__v):r)!=null?(r.__=e,r.__b=e.__b+1,u=ln(r,t,l=o+a,p),r.__i=u,i=null,u!==-1&&(p--,(i=t[u])&&(i.__u|=131072)),i==null||i.__v===null?(u==-1&&a--,typeof r.type!="function"&&(r.__u|=65536)):u!==l&&(u===l+1?a++:u>l?p>d-l?a+=u-l:a--:a=u<l&&u==l-1?u-l:0,u!==o+a&&(r.__u|=65536))):(i=t[o])&&i.key==null&&i.__e&&(i.__e==e.__d&&(e.__d=E(i)),oe(i,i,!1),t[o]=null,p--);if(p)for(o=0;o<c;o++)(i=t[o])!=null&&!(131072&i.__u)&&(i.__e==e.__d&&(e.__d=E(i)),oe(i,i))}function Ce(e,n,t){var o,r;if(typeof e.type=="function"){for(o=e.__k,r=0;o&&r<o.length;r++)o[r]&&(o[r].__=e,n=Ce(o[r],n,t));return n}return e.__e!=n&&(t.insertBefore(e.__e,n||null),n=e.__e),n&&n.nextSibling}function ln(e,n,t,o){var r=e.key,i=e.type,l=t-1,u=t+1,d=n[t];if(d===null||d&&r==d.key&&i===d.type)return t;if(o>(d!=null&&!(131072&d.__u)?1:0))for(;l>=0||u<n.length;){if(l>=0){if((d=n[l])&&!(131072&d.__u)&&r==d.key&&i===d.type)return l;l--}if(u<n.length){if((d=n[u])&&!(131072&d.__u)&&r==d.key&&i===d.type)return u;u++}}return-1}function ve(e,n,t){n[0]==="-"?e.setProperty(n,t??""):e[n]=t==null?"":typeof t!="number"||an.test(n)?t:t+"px"}function O(e,n,t,o,r){var i;e:if(n==="style")if(typeof t=="string")e.style.cssText=t;else{if(typeof o=="string"&&(e.style.cssText=o=""),o)for(n in o)t&&n in t||ve(e.style,n,"");if(t)for(n in t)o&&t[n]===o[n]||ve(e.style,n,t[n])}else if(n[0]==="o"&&n[1]==="n")i=n!==(n=n.replace(/(PointerCapture)$|Capture$/,"$1")),n=n.toLowerCase()in e?n.toLowerCase().slice(2):n.slice(2),e.l||(e.l={}),e.l[n+i]=t,t?o?t.u=o.u:(t.u=Date.now(),e.addEventListener(n,i?he:ye,i)):e.removeEventListener(n,i?he:ye,i);else{if(r)n=n.replace(/xlink(H|:h)/,"h").replace(/sName$/,"s");else if(n!=="width"&&n!=="height"&&n!=="href"&&n!=="list"&&n!=="form"&&n!=="tabIndex"&&n!=="download"&&n!=="rowSpan"&&n!=="colSpan"&&n!=="role"&&n in e)try{e[n]=t??"";break e}catch{}typeof t=="function"||(t==null||t===!1&&n[4]!=="-"?e.removeAttribute(n):e.setAttribute(n,t))}}function ye(e){var n=this.l[e.type+!1];if(e.t){if(e.t<=n.u)return}else e.t=Date.now();return n(_.event?_.event(e):e)}function he(e){return this.l[e.type+!0](_.event?_.event(e):e)}function ie(e,n,t,o,r,i,l,u,d,c){var p,a,m,f,w,g,y,v,h,k,N,C,me,B,ee,L=n.type;if(n.constructor!==void 0)return null;128&t.__u&&(d=!!(32&t.__u),i=[u=n.__e=t.__e]),(p=_.__b)&&p(n);e:if(typeof L=="function")try{if(v=n.props,h=(p=L.contextType)&&o[p.__c],k=p?h?h.props.value:p.__:o,t.__c?y=(a=n.__c=t.__c).__=a.__E:("prototype"in L&&L.prototype.render?n.__c=a=new L(v,k):(n.__c=a=new z(v,k),a.constructor=L,a.render=dn),h&&h.sub(a),a.props=v,a.state||(a.state={}),a.context=k,a.__n=o,m=a.__d=!0,a.__h=[],a._sb=[]),a.__s==null&&(a.__s=a.state),L.getDerivedStateFromProps!=null&&(a.__s==a.state&&(a.__s=F({},a.__s)),F(a.__s,L.getDerivedStateFromProps(v,a.__s))),f=a.props,w=a.state,a.__v=n,m)L.getDerivedStateFromProps==null&&a.componentWillMount!=null&&a.componentWillMount(),a.componentDidMount!=null&&a.__h.push(a.componentDidMount);else{if(L.getDerivedStateFromProps==null&&v!==f&&a.componentWillReceiveProps!=null&&a.componentWillReceiveProps(v,k),!a.__e&&(a.shouldComponentUpdate!=null&&a.shouldComponentUpdate(v,a.__s,k)===!1||n.__v===t.__v)){for(n.__v!==t.__v&&(a.props=v,a.state=a.__s,a.__d=!1),n.__e=t.__e,n.__k=t.__k,n.__k.forEach(function(W){W&&(W.__=n)}),N=0;N<a._sb.length;N++)a.__h.push(a._sb[N]);a._sb=[],a.__h.length&&l.push(a);break e}a.componentWillUpdate!=null&&a.componentWillUpdate(v,a.__s,k),a.componentDidUpdate!=null&&a.__h.push(function(){a.componentDidUpdate(f,w,g)})}if(a.context=k,a.props=v,a.__P=e,a.__e=!1,C=_.__r,me=0,"prototype"in L&&L.prototype.render){for(a.state=a.__s,a.__d=!1,C&&C(n),p=a.render(a.props,a.state,a.context),B=0;B<a._sb.length;B++)a.__h.push(a._sb[B]);a._sb=[]}else do a.__d=!1,C&&C(n),p=a.render(a.props,a.state,a.context),a.state=a.__s;while(a.__d&&++me<25);a.state=a.__s,a.getChildContext!=null&&(o=F(F({},o),a.getChildContext())),m||a.getSnapshotBeforeUpdate==null||(g=a.getSnapshotBeforeUpdate(f,w)),Se(e,re(ee=p!=null&&p.type===b&&p.key==null?p.props.children:p)?ee:[ee],n,t,o,r,i,l,u,d,c),a.base=n.__e,n.__u&=-161,a.__h.length&&l.push(a),y&&(a.__E=a.__=null)}catch(W){n.__v=null,d||i!=null?(n.__e=u,n.__u|=d?160:32,i[i.indexOf(u)]=null):(n.__e=t.__e,n.__k=t.__k),_.__e(W,n,t)}else i==null&&n.__v===t.__v?(n.__k=t.__k,n.__e=t.__e):n.__e=cn(t.__e,n,t,o,r,i,l,d,c);(p=_.diffed)&&p(n)}function Fe(e,n,t){n.__d=void 0;for(var o=0;o<t.length;o++)se(t[o],t[++o],t[++o]);_.__c&&_.__c(n,e),e.some(function(r){try{e=r.__h,r.__h=[],e.some(function(i){i.call(r)})}catch(i){_.__e(i,r.__v)}})}function cn(e,n,t,o,r,i,l,u,d){var c,p,a,m,f,w,g,y=t.props,v=n.props,h=n.type;if(h==="svg"&&(r=!0),i!=null){for(c=0;c<i.length;c++)if((f=i[c])&&"setAttribute"in f==!!h&&(h?f.localName===h:f.nodeType===3)){e=f,i[c]=null;break}}if(e==null){if(h===null)return document.createTextNode(v);e=r?document.createElementNS("http://www.w3.org/2000/svg",h):document.createElement(h,v.is&&v),i=null,u=!1}if(h===null)y===v||u&&e.data===v||(e.data=v);else{if(i=i&&$.call(e.childNodes),y=t.props||P,!u&&i!=null)for(y={},c=0;c<e.attributes.length;c++)y[(f=e.attributes[c]).name]=f.value;for(c in y)f=y[c],c=="children"||(c=="dangerouslySetInnerHTML"?a=f:c==="key"||c in v||O(e,c,null,f,r));for(c in v)f=v[c],c=="children"?m=f:c=="dangerouslySetInnerHTML"?p=f:c=="value"?w=f:c=="checked"?g=f:c==="key"||u&&typeof f!="function"||y[c]===f||O(e,c,f,y[c],r);if(p)u||a&&(p.__html===a.__html||p.__html===e.innerHTML)||(e.innerHTML=p.__html),n.__k=[];else if(a&&(e.innerHTML=""),Se(e,re(m)?m:[m],n,t,o,r&&h!=="foreignObject",i,l,i?i[0]:t.__k&&E(t,0),u,d),i!=null)for(c=i.length;c--;)i[c]!=null&&Te(i[c]);u||(c="value",w!==void 0&&(w!==e[c]||h==="progress"&&!w||h==="option"&&w!==y[c])&&O(e,c,w,y[c],!1),c="checked",g!==void 0&&g!==e[c]&&O(e,c,g,y[c],!1))}return e}function se(e,n,t){try{typeof e=="function"?e(n):e.current=n}catch(o){_.__e(o,t)}}function oe(e,n,t){var o,r;if(_.unmount&&_.unmount(e),(o=e.ref)&&(o.current&&o.current!==e.__e||se(o,null,n)),(o=e.__c)!=null){if(o.componentWillUnmount)try{o.componentWillUnmount()}catch(i){_.__e(i,n)}o.base=o.__P=null,e.__c=void 0}if(o=e.__k)for(r=0;r<o.length;r++)o[r]&&oe(o[r],n,t||typeof e.type!="function");t||e.__e==null||Te(e.__e),e.__=e.__e=e.__d=void 0}function dn(e,n,t){return this.constructor(e,t)}function ae(e,n,t){var o,r,i,l;_.__&&_.__(e,n),r=(o=typeof t=="function")?null:t&&t.__k||n.__k,i=[],l=[],ie(n,e=(!o&&t||n).__k=s(b,null,[e]),r||P,P,n.ownerSVGElement!==void 0,!o&&t?[t]:r?null:n.firstChild?$.call(n.childNodes):null,i,!o&&t?t:r?r.__e:n.firstChild,o,l),Fe(i,e,l)}function Ie(e,n){var t={__c:n="__cC"+be++,__:e,Consumer:function(o,r){return o.children(r)},Provider:function(o){var r,i;return this.getChildContext||(r=[],(i={})[n]=this,this.getChildContext=function(){return i},this.shouldComponentUpdate=function(l){this.props.value!==l.value&&r.some(function(u){u.__e=!0,te(u)})},this.sub=function(l){r.push(l);var u=l.componentWillUnmount;l.componentWillUnmount=function(){r.splice(r.indexOf(l),1),u&&u.call(l)}}),o.children}};return t.Provider.__=t.Consumer.contextType=t}$=ke.slice,_={__e:function(e,n,t,o){for(var r,i,l;n=n.__;)if((r=n.__c)&&!r.__)try{if((i=r.constructor)&&i.getDerivedStateFromError!=null&&(r.setState(i.getDerivedStateFromError(e)),l=r.__d),r.componentDidCatch!=null&&(r.componentDidCatch(e,o||{}),l=r.__d),l)return r.__E=r}catch(u){e=u}throw e}},we=0,sn=function(e){return e!=null&&e.constructor==null},z.prototype.setState=function(e,n){var t;t=this.__s!=null&&this.__s!==this.state?this.__s:this.__s=F({},this.state),typeof e=="function"&&(e=e(F({},t),this.props)),e&&F(t,e),e!=null&&this.__v&&(n&&this._sb.push(n),te(this))},z.prototype.forceUpdate=function(e){this.__v&&(this.__e=!0,e&&this.__h.push(e),te(this))},z.prototype.render=b,D=[],xe=typeof Promise=="function"?Promise.prototype.then.bind(Promise.resolve()):setTimeout,ne=function(e,n){return e.__v.__b-n.__v.__b},q.__r=0,be=0;function S(){}S.prototype={diff:function(n,t){var o=arguments.length>2&&arguments[2]!==void 0?arguments[2]:{},r=o.callback;typeof o=="function"&&(r=o,o={}),this.options=o;var i=this;function l(g){return r?(setTimeout(function(){r(void 0,g)},0),!0):g}n=this.castInput(n),t=this.castInput(t),n=this.removeEmpty(this.tokenize(n)),t=this.removeEmpty(this.tokenize(t));var u=t.length,d=n.length,c=1,p=u+d;o.maxEditLength&&(p=Math.min(p,o.maxEditLength));var a=[{newPos:-1,components:[]}],m=this.extractCommon(a[0],t,n,0);if(a[0].newPos+1>=u&&m+1>=d)return l([{value:this.join(t),count:t.length}]);function f(){for(var g=-1*c;g<=c;g+=2){var y=void 0,v=a[g-1],h=a[g+1],k=(h?h.newPos:0)-g;v&&(a[g-1]=void 0);var N=v&&v.newPos+1<u,C=h&&0<=k&&k<d;if(!N&&!C){a[g]=void 0;continue}if(!N||C&&v.newPos<h.newPos?(y=pn(h),i.pushComponent(y.components,void 0,!0)):(y=v,y.newPos++,i.pushComponent(y.components,!0,void 0)),k=i.extractCommon(y,t,n,g),y.newPos+1>=u&&k+1>=d)return l(fn(i,y.components,t,n,i.useLongestToken));a[g]=y}c++}if(r)(function g(){setTimeout(function(){if(c>p)return r();f()||g()},0)})();else for(;c<=p;){var w=f();if(w)return w}},pushComponent:function(n,t,o){var r=n[n.length-1];r&&r.added===t&&r.removed===o?n[n.length-1]={count:r.count+1,added:t,removed:o}:n.push({count:1,added:t,removed:o})},extractCommon:function(n,t,o,r){for(var i=t.length,l=o.length,u=n.newPos,d=u-r,c=0;u+1<i&&d+1<l&&this.equals(t[u+1],o[d+1]);)u++,d++,c++;return c&&n.components.push({count:c}),n.newPos=u,d},equals:function(n,t){return this.options.comparator?this.options.comparator(n,t):n===t||this.options.ignoreCase&&n.toLowerCase()===t.toLowerCase()},removeEmpty:function(n){for(var t=[],o=0;o<n.length;o++)n[o]&&t.push(n[o]);return t},castInput:function(n){return n},tokenize:function(n){return n.split("")},join:function(n){return n.join("")}};function fn(e,n,t,o,r){for(var i=0,l=n.length,u=0,d=0;i<l;i++){var c=n[i];if(c.removed){if(c.value=e.join(o.slice(d,d+c.count)),d+=c.count,i&&n[i-1].added){var a=n[i-1];n[i-1]=n[i],n[i]=a}}else{if(!c.added&&r){var p=t.slice(u,u+c.count);p=p.map(function(f,w){var g=o[d+w];return g.length>f.length?g:f}),c.value=e.join(p)}else c.value=e.join(t.slice(u,u+c.count));u+=c.count,c.added||(d+=c.count)}}var m=n[l-1];return l>1&&typeof m.value=="string"&&(m.added||m.removed)&&e.equals("",m.value)&&(n[l-2].value+=m.value,n.pop()),n}function pn(e){return{newPos:e.newPos,components:e.components.slice(0)}}var zn=new S;var Ne=/^[A-Za-z\xC0-\u02C6\u02C8-\u02D7\u02DE-\u02FF\u1E00-\u1EFF]+$/,De=/\S/,Re=new S;Re.equals=function(e,n){return this.options.ignoreCase&&(e=e.toLowerCase(),n=n.toLowerCase()),e===n||this.options.ignoreWhitespace&&!De.test(e)&&!De.test(n)};Re.tokenize=function(e){for(var n=e.split(/([^\S\r\n]+|[()[\]{}'"\r\n]|\b)/),t=0;t<n.length-1;t++)!n[t+1]&&n[t+2]&&Ne.test(n[t])&&Ne.test(n[t+2])&&(n[t]+=n[t+2],n.splice(t+1,2),t--);return n};var He=new S;He.tokenize=function(e){var n=[],t=e.split(/(\n|\r\n)/);t[t.length-1]||t.pop();for(var o=0;o<t.length;o++){var r=t[o];o%2&&!this.options.newlineIsToken?n[n.length-1]+=r:(this.options.ignoreWhitespace&&(r=r.trim()),n.push(r))}return n};var _n=new S;_n.tokenize=function(e){return e.split(/(\S.+?[.!?])(?=\s+|$)/)};var mn=new S;mn.tokenize=function(e){return e.split(/([{}:;,]|\s+)/)};function J(e){"@babel/helpers - typeof";return typeof Symbol=="function"&&typeof Symbol.iterator=="symbol"?J=function(n){return typeof n}:J=function(n){return n&&typeof Symbol=="function"&&n.constructor===Symbol&&n!==Symbol.prototype?"symbol":typeof n},J(e)}var gn=Object.prototype.toString,M=new S;M.useLongestToken=!0;M.tokenize=He.tokenize;M.castInput=function(e){var n=this.options,t=n.undefinedReplacement,o=n.stringifyReplacer,r=o===void 0?function(i,l){return typeof l>"u"?t:l}:o;return typeof e=="string"?e:JSON.stringify(ue(e,null,null,r),r,"  ")};M.equals=function(e,n){return S.prototype.equals.call(M,e.replace(/,([\r\n])/g,"$1"),n.replace(/,([\r\n])/g,"$1"))};function ue(e,n,t,o,r){n=n||[],t=t||[],o&&(e=o(r,e));var i;for(i=0;i<n.length;i+=1)if(n[i]===e)return t[i];var l;if(gn.call(e)==="[object Array]"){for(n.push(e),l=new Array(e.length),t.push(l),i=0;i<e.length;i+=1)l[i]=ue(e[i],n,t,o,r);return n.pop(),t.pop(),l}if(e&&e.toJSON&&(e=e.toJSON()),J(e)==="object"&&e!==null){n.push(e),l={},t.push(l);var u=[],d;for(d in e)e.hasOwnProperty(d)&&u.push(d);for(u.sort(),i=0;i<u.length;i+=1)d=u[i],l[d]=ue(e[d],n,t,o,d);n.pop(),t.pop()}else l=e;return l}var j=new S;j.tokenize=function(e){return e.slice()};j.join=j.removeEmpty=function(e){return e};function Ee(e,n,t){return j.diff(e,n,t)}var V,x,le,Ae,Y=0,Ue=[],K=[],Pe=_.__b,Me=_.__r,Ve=_.diffed,Be=_.__c,We=_.unmount;function de(e,n){_.__h&&_.__h(x,e,Y||n),Y=0;var t=x.__H||(x.__H={__:[],__h:[]});return e>=t.__.length&&t.__.push({__V:K}),t.__[e]}function Z(e){return Y=1,vn(qe,e)}function vn(e,n,t){var o=de(V++,2);if(o.t=e,!o.__c&&(o.__=[t?t(n):qe(void 0,n),function(u){var d=o.__N?o.__N[0]:o.__[0],c=o.t(d,u);d!==c&&(o.__N=[c,o.__[1]],o.__c.setState({}))}],o.__c=x,!x.u)){var r=function(u,d,c){if(!o.__c.__H)return!0;var p=o.__c.__H.__.filter(function(m){return m.__c});if(p.every(function(m){return!m.__N}))return!i||i.call(this,u,d,c);var a=!1;return p.forEach(function(m){if(m.__N){var f=m.__[0];m.__=m.__N,m.__N=void 0,f!==m.__[0]&&(a=!0)}}),!(!a&&o.__c.props===u)&&(!i||i.call(this,u,d,c))};x.u=!0;var i=x.shouldComponentUpdate,l=x.componentWillUpdate;x.componentWillUpdate=function(u,d,c){if(this.__e){var p=i;i=void 0,r(u,d,c),i=p}l&&l.call(this,u,d,c)},x.shouldComponentUpdate=r}return o.__N||o.__}function A(e,n){var t=de(V++,7);return wn(t.__H,n)?(t.__V=e(),t.i=n,t.__h=e,t.__V):t.__}function R(e,n){return Y=8,A(function(){return e},n)}function ze(e){var n=x.context[e.__c],t=de(V++,9);return t.c=e,n?(t.__==null&&(t.__=!0,n.sub(x)),n.props.value):e.__}function yn(){for(var e;e=Ue.shift();)if(e.__P&&e.__H)try{e.__H.__h.forEach(G),e.__H.__h.forEach(ce),e.__H.__h=[]}catch(n){e.__H.__h=[],_.__e(n,e.__v)}}_.__b=function(e){x=null,Pe&&Pe(e)},_.__r=function(e){Me&&Me(e),V=0;var n=(x=e.__c).__H;n&&(le===x?(n.__h=[],x.__h=[],n.__.forEach(function(t){t.__N&&(t.__=t.__N),t.__V=K,t.__N=t.i=void 0})):(n.__h.forEach(G),n.__h.forEach(ce),n.__h=[],V=0)),le=x},_.diffed=function(e){Ve&&Ve(e);var n=e.__c;n&&n.__H&&(n.__H.__h.length&&(Ue.push(n)!==1&&Ae===_.requestAnimationFrame||((Ae=_.requestAnimationFrame)||hn)(yn)),n.__H.__.forEach(function(t){t.i&&(t.__H=t.i),t.__V!==K&&(t.__=t.__V),t.i=void 0,t.__V=K})),le=x=null},_.__c=function(e,n){n.some(function(t){try{t.__h.forEach(G),t.__h=t.__h.filter(function(o){return!o.__||ce(o)})}catch(o){n.some(function(r){r.__h&&(r.__h=[])}),n=[],_.__e(o,t.__v)}}),Be&&Be(e,n)},_.unmount=function(e){We&&We(e);var n,t=e.__c;t&&t.__H&&(t.__H.__.forEach(function(o){try{G(o)}catch(r){n=r}}),t.__H=void 0,n&&_.__e(n,t.__v))};var Oe=typeof requestAnimationFrame=="function";function hn(e){var n,t=function(){clearTimeout(o),Oe&&cancelAnimationFrame(n),setTimeout(e)},o=setTimeout(t,100);Oe&&(n=requestAnimationFrame(t))}function G(e){var n=x,t=e.__c;typeof t=="function"&&(e.__c=void 0,t()),x=n}function ce(e){var n=x;e.__c=e.__(),x=n}function wn(e,n){return!e||e.length!==n.length||n.some(function(t,o){return t!==e[o]})}function qe(e,n){return typeof n=="function"?n(e):n}var $e=e=>!!e;var jn=Symbol("unset");function Je(e){let n=[];for(let t of e)n=n.concat(t);return n}var Kn=2**31-1;var je=e=>function({value:t,onChange:o}){return s("div",{className:"decision-buttons"},e.map(r=>s("button",{key:r,onClick:()=>o(r),className:t===r?"active":""},r)))};var Ze="<node_internals>",Xe="node:",Qe=e=>e.config.type==="pwa-node"||e.config.type==="pwa-extensionHost"||e.config.type==="node-terminal",X=e=>e.config.type==="pwa-chrome"||e.config.type==="pwa-msedge",fe=e=>e.absolutePath.startsWith(Ze)||e.url.startsWith(Xe)?2:e.absolutePath.includes("node_modules")?1:0,Q=(e,n)=>e.url.startsWith(Xe)?e.url:e.absolutePath.startsWith(Ze)?e.absolutePath:xn(e.absolutePath)&&n.config.__workspaceFolder?kn(n.config.__workspaceFolder,e.absolutePath):e.absolutePath||e.url,H=e=>{let n=(e.prettyName||e.url).split(/\\|\//g);return n[n.length-1]},xn=e=>en(e)||bn(e),en=e=>e.startsWith("/"),bn=e=>/^[a-z]:/i.test(e),Ke=(e,n)=>{let t=e.split("/"),o=n.split("/");for(;t.length&&o[0]===t[0];)t.shift(),o.shift();return(t.length?new Array(t.length).fill(".."):["."]).concat(o).join("/")},kn=(e,n)=>en(e)?Ke(e,n):Ke(Ge(Ye(e)),Ge(Ye(n))),Ge=e=>e.replace(/\\\//g,"/").replace(/\\/g,"/"),Ye=e=>e.slice(0,1).toUpperCase()+e.slice(1);var pe=Ie(void 0),T=()=>ze(pe);var _e=acquireVsCodeApi(),Tn=(e,n)=>{let t=_e.getState()?.componentState||{};return t.hasOwnProperty(e)?t[e]:n},Ln=(e,n)=>{let t=_e.getState();_e.setState({...t,componentState:{...t?.componentState,[e]:n}})},I=(e,n)=>{let[t,o]=Z(()=>Tn(e,n)),r=R(i=>{Ln(e,i),o(i)},[e,o]);return[t,r]};var nn=()=>{let e=T();return s(b,null,e.breakpoints.map((n,t)=>s(In,{bp:n,key:t})))},Sn=(e,n)=>e.cdp.some(t=>{if("location"in t.args)return!0;if(t.args.url){let o=t.args.url;return n.sources.some(r=>r.url===o)}if(t.args.urlRegex){let o=new RegExp(t.args.urlRegex);return n.sources.some(r=>o.test(r.url))}return!1}),Cn=(e,n)=>{let t=0,o=[s("li",{key:t++},s("p",null,"\u2705 This breakpoint was initially set in:"),s("p",null,s("code",null,e.source.path)," line ",e.params.line," column ",e.params.column||1))];if(!Sn(e,n))return o.push(s(Nn,{bp:e,key:t++})),o;o.push(s("li",{key:t++},s("p",null,"\u2705 In the runtime, the breakpoint was set in:"),s("p",null,s("ul",null,e.cdp.map((l,u)=>s(Hn,{cdp:l,index:u,key:u}))))));let r=e.cdp.filter(l=>l.state===1),i=Je(r.map(l=>l.state===1?l.uiLocations:[]));return i.length?(o.push(s("li",{key:t++},s("p",null,"\u2705 The runtime acknowledged and adjusted the breakpoint, and it mapped back to the following locations:"),s("ul",null,i.map((l,u)=>s(Rn,{loc:l,key:u})))),s("li",{key:t++},s("p",null,"If this is not right, your compiled code might be out of date with your sources. If you don't think this is the case and something else is wrong, please"," ",s("a",{href:"https://github.com/microsoft/vscode-js-debug/issues/new/choose"},"open an issue"),"!"))),o):(o.push(s("li",{key:t++},s(Fn,null))),o)},Fn=()=>{let e=T();return s("p",null,"\u2753 We sent the breakpoint, but it didn't bind to any locations. If this is unexpected:",s("ul",null,s("li",null,"Make sure that your program is loading or running this script. You can add a"," ",s("code",null,"debugger;")," statement to check this: your program will pause when it hits it."),s("li",null,"If your breakpoint is set in certain places, such as on the last empty line of a file, the runtime might not be able to find anywhere to place it."),Qe(e)&&s("li",null,"Unless you"," ",s("a",{href:"https://code.visualstudio.com/docs/nodejs/nodejs-debugging#_breakpoint-validation"},"run with --nolazy"),", Node.js might not resolve breakpoints for code it hasn't parsed yet."),s("li",null,"If necessary, make sure your compiled files are up-to-date with your source files.")))},In=({bp:e})=>{if(!e.source.path)return null;let n=T();return s("div",{className:"content source-container"},s("h2",null,Q({absolutePath:e.source.path,url:e.source.path},n),":",e.params.line,":",e.params.column||1),s("ul",{className:"bp-tracing"},Cn(e,n)))},Nn=({bp:e})=>{let n=T(),t=H({url:e.source.path}),o=n.sources.filter(r=>H(r).toLowerCase()===t.toLowerCase());return o.length?s("li",null,s("p",null,"\u2753 We couldn't find a corresponding source location, but found some other files with the same name:"),s("ul",null,o.map(r=>s("li",{key:r},s(Dn,{original:e.source.path,updated:r.absolutePath||r.url})))),X(n)?s("p",null,"You may need to adjust the ",s("code",null,"webRoot")," in your ",s("code",null,"launch.json")," if you're building from a subfolder, or tweak your ",s("code",null,"sourceMapPathOverrides"),"."):s("p",null,"If this is the same file, you may need to adjust your build tool"," ",X(n)&&s(b,null,"or ",s("code",null,"webRoot")," in the launch.json")," ","to correct the paths.")):s("li",null,s("p",null,s(Pn,{basename:t})))},Dn=({original:e,updated:n})=>s("span",{className:"text-diff"},Ee(e.split(/[/\\]/g),n.split(/[/\\]/g),{ignoreCase:!0}).map((t,o)=>s("span",{className:t.added?"add":t.removed?"rm":"",key:o},o>0?"/":"",t.value.join("/")))),Rn=({loc:e})=>{let t=T().sources.find(o=>o.sourceReference===e.sourceReference);return s(b,null,s("code",null,t?.absolutePath??t?.url??"unknown")," line ",e.lineNumber," column"," ",e.columnNumber)},Hn=({cdp:e,index:n})=>{let t=T(),[o,r]=I(`showCdpBp${n}`,!1),{url:i,line:l,col:u,regex:d}="location"in e.args?{url:t.sources.find(c=>!c.compiledSourceRefToUrl&&c.scriptIds.includes(e.args.location.scriptId))?.url,regex:void 0,line:e.args.location.lineNumber+1,col:(e.args.location.columnNumber||0)+1}:{url:e.args.urlRegex?En(e.args.urlRegex):e.args.url,regex:e.args.urlRegex,line:e.args.lineNumber+1,col:(e.args.columnNumber||0)+1};return s("li",null,s("p",null,s("code",null,i)," line ",l," column ",u," ",d&&s("a",{onClick:()=>r(!o)},"via this regex")),o&&s("p",null,s("code",null,d)))},En=e=>e.replace(/\[([[a-z])[A-Z]\]/g,(n,t)=>t).replace(/\\\\/,"\\").replace(/\\\//g,"/").replace(/\|.+$/g,"").replace(/\\\./g,".");var An=je(["Loaded in directly","Be parsed from a sourcemap"]),Pn=({basename:e})=>{let n=T(),[t,o]=Z(e.endsWith(".js")?void 0:"Be parsed from a sourcemap");return s(b,null,s("p",null,"\u2753 We couldn't find a corresponding source location, and didn't find any source with the name ",s("code",null,e),"."),s("p",null,"How did you expect this file to be loaded? (If you have a compilation step, you should pick 'sourcemap')",s(An,{onChange:o,value:t}),t==="Loaded in directly"&&(X(n)?s("p",null,"It looks like your webpage didn't load this script; breakpoints won't be bound until the file they're set in is loaded. Make sure your script is imported from the right location using a ",s("code",null,"<script>")," tag."):s("p",null,"It looks like your program didn't load this script; breakpoints won't be bound until the file they're set in is loaded. Make sure your script is imported with a"," ",s("code",null,"require()")," or ",s("code",null,"import")," statement, such as"," ",s("code",null,"require('./",e,"')"),".")),t==="Be parsed from a sourcemap"&&s("p",null,"Here's some hints that might help you:",s("ul",null,/\.tsx?$/.test(e)?s("li",null,"Make sure you have ",s("code",null,'"sourceMap": true')," in your tsconfig to generate sourcemaps."):s("li",null,"Make sure your build tool is set up to create sourcemaps."),!n.config.outFiles.includes("!**/node_modules/**")&&s("li",null,"It looks like you narrowed the ",s("code",null,"outFiles")," in your launch.json. Try removing this: it now defaults to the whole workspace, and overspecifying it can unnecessarily narrow places where we'll resolve sourcemaps.")))))};var tn=({onPick:e})=>s("div",{className:"intro"},s("div",null,s("header",null,"Debug Doctor"),s("div",{className:"intro-content"},s("p",null,"What are you trying to find out?"),s("ul",null,s("li",null,s("a",{role:"button",onClick:()=>e(1)},"Why my breakpoints don't bind")),s("li",null,s("a",{role:"button",onClick:()=>e(2)},"What scripts and sourcemaps are loaded")),s("li",null,s("a",{href:"https://github.com/microsoft/vscode-js-debug/issues/new/choose"},"Something else..."))))));var on=()=>{let e=T(),n=A(()=>{let u=new Map;for(let d of e.sources)u.set(d.uniqueId,d);return u},[e.sources]),t=A(()=>e.sources.map(u=>[[u.url,u.absolutePath,u.prettyName].join(" ").toLowerCase(),u]).sort((u,d)=>fe(u[1])-fe(d[1])),[e.sources]),[o,r]=I("filter",""),i=A(()=>o?t.filter(([u])=>u.includes(o.toLowerCase())).map(([,u])=>u):t.map(u=>u[1]),[t,o]),l=R(u=>r(u.target.value),[]);return s(b,null,s("input",{placeholder:"Filter sources...",className:"source-filter",value:o,onChange:l,onKeyUp:l}),s("small",{style:{marginBottom:"1rem"}},"Showing ",i.length," of ",e.sources.length," sources..."),i.map(u=>s(Mn,{source:u,allSources:n,key:u.sourceReference})))},Mn=({source:e,allSources:n})=>{let[t,o]=I(`sourceBreadCrumbs${e.uniqueId}`,[e.uniqueId]),r=A(()=>t.map(c=>n.get(c)).filter($e),[n,t]),[i,l]=I(`sourceExpanded${e.uniqueId}`,!1),u=T(),d=R(()=>l(!i),[i]);return s("div",{className:`source-container ${i?" expanded":""}`},s("h2",{onClick:d},Q(e,u)),i&&s(b,null,r.length>1&&s(Vn,{sources:r,update:o}),s(Bn,{source:r[r.length-1],open:c=>{let p=u.sources.find(a=>a.sourceReference===c);p&&o(t.concat(p.uniqueId))}})))},Vn=({sources:e,update:n})=>s("ol",{className:"source-breadcrumbs"},e.map((t,o)=>{let r=`${H(t)} (#${t.sourceReference})`;return o===e.length-1?s("li",null,r):s("li",{key:o},s("a",{key:o,onClick:()=>n(e.slice(0,o+1).map(i=>i.uniqueId))},r)," ","\xBB"," ")})),Bn=({source:e,open:n})=>s("dl",{className:"source-data-grid"},s("dt",null,"url"),s("dd",null,s("code",null,e.url)),s("dt",null,"sourceReference"),s("dd",null,s("code",null,e.sourceReference)),s("dt",null,"absolutePath"),s("dd",null,s("code",null,e.absolutePath)),s("dt",null,"absolutePath verified?"),s("dd",null,e.compiledSourceRefToUrl?"\u2705 From sourcemap, assumed correct":e.actualAbsolutePath?"\u2705 Verified on disk":"\u274C Disk verification failed (does not exist or different content)"),s("dt",null,"sourcemap children:"),s("dd",null,e.sourceMap?s("ul",null,Object.entries(e.sourceMap.sources).map(([t,o])=>s("li",{key:t},s(Wn,{url:t,sourceRef:o,pick:n})))):"None (does not have a sourcemap)"),s("dt",null,"referenced from sourcemap:"),s("dd",null,e.compiledSourceRefToUrl?s("ul",null,e.compiledSourceRefToUrl.map(([t,o])=>s("li",{key:o},s(On,{url:o,sourceRef:t,pick:n})))):"None (not from a sourcemap)")),Wn=({url:e,sourceRef:n,pick:t})=>{let r=T().sources.find(l=>l.sourceReference===n),i=R(()=>t(n),[n]);return s(b,null,e," \u2192 ",s("a",{onClick:i},r?`${H(r)} (#${n})`:"unknown"))},On=({url:e,sourceRef:n,pick:t})=>{let r=T().sources.find(l=>l.sourceReference===n),i=R(()=>t(n),[n]);return s(b,null,s("a",{onClick:i},r?`${H(r)} (#${n})`:"unknown")," as ",e," ","\u2192 this")};var rn=({dump:e})=>{let[n,t]=I("experience",0);return s(pe.Provider,{value:e},n===0?s(tn,{onPick:t}):s(b,null,s("a",{role:"button",onClick:()=>t(0),className:"back"},"\u2190 Back"),n===1?s(nn,null):s(on,null)))};typeof DUMP<"u"?ae(s(rn,{dump:DUMP}),document.body):fetch(document.location.search.slice(1)).then(e=>e.json()).then(e=>ae(s(rn,{dump:e}),document.body));})();
