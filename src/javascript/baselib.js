"use strict";

const eqInt = {
  "==": function(x) { return function(y) { 
      return x == y; 
  }},
  "/=": function(x) { return function(y) { 
    return x != y; 
  }}
}

const eqDouble = {
  "==": function(x) { return function(y) { 
      return x == y; 
  }},
  "/=": function(x) { return function(y) { 
    return x != y; 
  }}
}

const eqChar = {
  "==": function(x) { return function(y) { 
      return x == y; 
  }},
  "/=": function(x) { return function(y) { 
    return x != y; 
  }}
}

const eqBool = {
  "==": function(x) { return function(y) { 
      return x == y; 
  }},
  "/=": function(x) { return function(y) { 
    return x != y; 
  }}
}

const ordInt = {
  ">": function(x) { return function(y) { return x > y }},
  "<": function(x) { return function(y) { return x < y }},
  ">=": function(x) { return function(y) { return x >= y }},
  "<=": function(x) { return function(y) { return x <= y }},
  "==": eqInt["=="],
  "/=": eqInt["/="]
}

const ordDouble = {
  ">": function(x) { return function(y) { return x > y }},
  "<": function(x) { return function(y) { return x < y }},
  ">=": function(x) { return function(y) { return x >= y }},
  "<=": function(x) { return function(y) { return x <= y }},
  "==": eqDouble["=="],
  "/=": eqDouble["/="]
}

const ordChar = {
  ">": function(x) { return function(y) { return x > y }},
  "<": function(x) { return function(y) { return x < y }},
  ">=": function(x) { return function(y) { return x >= y }},
  "<=": function(x) { return function(y) { return x <= y }},
  "==": eqChar["=="],
  "/=": eqChar["/="]
}

const numInt = {
  "+": function(x) { return function(y) { return x + y; }},
  "*": function(x) { return function(y) { return x * y; }},
  "-": function(x) { return function(y) { return x - y; }},
  "fromInteger": function(x) { return x; }
}

const fractionalInt = {
  "+": numInt["+"],
  "*": numInt["*"],
  "-": numInt["-"],
  "/": function(x) { return function(y) { return ~~(x/y); }},
  "fromInteger": numInt["fromInteger"],
  "fromRational": function(x) { return Math.floor(x); }
}

const integralInt = {
  "+": numInt["+"],
  "*": numInt["*"],
  "-": numInt["-"],
  "fromInteger": numInt["fromInteger"],
  "mod": function(x) { return function(y) { return x % y; }},
}

const numDouble = {
  "+": function(x) { return function(y) { return x + y; }},
  "*": function(x) { return function(y) { return x * y; }},
  "-": function(x) { return function(y) { return x - y; }},
  "fromInteger": function(x) { return x; }
}

const fractionalDouble = {
  "+": numDouble["+"],
  "*": numDouble["*"],
  "-": numDouble["-"],
  "/": function(x) { return function(y) { return x / y; }},
  "fromInteger": numDouble["fromInteger"],
  "fromRational": function(x) { return x; }
}

const floatingDouble = {
  "cos": Math.cos,
  "sin": Math.sin,
  "sqrt": Math.sqrt
}

const numTuple2 = function(instA) {
  return function(instB) {
    const addA = instA["+"];
    const addB = instB["+"];
    const subA = instA["-"];
    const subB = instB["-"];
    const mulA = instA["*"];
    return {
      "fromInteger": function(x) { return [x, x]; },
      "+": function([x1, y1]){ return function([x2, y2]) { return [addA(x1)(x2), addB(y1)(y2)]  } },
      "-": function([x1, y1]){ return function([x2, y2]) { return [subA(x1)(x2), subB(y1)(y2)]  } },
      "*": function([x1, y1]){ return function([x2, y2]) { 
        return [subA(mulA(x1)(x2))(mulA(y1)(y2)), addA(mulA(x1)(y2))(mulA(y1)(x2))];
      } 
    }
    }
  }
}

const __mod = function(inst) { return inst["mod"]; }

const fromInteger = function(inst) { return inst["fromInteger"]; }

const fromRational = function(inst) { return inst["fromRational"]; }

const __add = function(inst) { return inst["+"]; }

const __mul = function(inst) { return inst["*"]; }

const __sub = function(inst) { return inst["-"]; }

const __div = function(inst) { return inst["/"]; }

const __eqeq = function(inst) { return inst["=="]; }

const __noteq = function(inst) { return inst["/="]; }

const __lt = function(inst)   { return inst["<"]; }

const __gt = function(inst)   { return inst[">"]; }

const __gteq = function(inst) { return inst[">="]; }

const __lteq = function(inst) { return inst["<="]; }

const __or = function(x) {
    return function(y) {
      return x || y;
    }
}

const __and = function(x) {
    return function(y) {
      return x && y;
    }
}

const __colon = function(x) {
  return function(xs) {
    const ys = xs.slice();
    ys.unshift(x);
    return ys;
  }
}

const isEmptyList = function(xs) {
  return xs.length == 0;
}

const head = function(xs) {
  return xs[0];
}

const tail = function(xs) {
  return xs.slice(1);
}

const __dot = function(f) {
  return function(g) {
    return function(x) {
      return f(g(x));
    }
  }
}

const toDouble = function(x) {
  return x + 0.0;
}

const truncate = instA => instB => Math.floor;

const sin = function(inst) { return inst["sin"]; }

const cos = function(inst) { return inst["cos"]; }

const sqrt = function(inst) { return inst["sqrt"]; }

const fst = function([x,y]){
  return x;
}

const snd = function([x,y]){
  return y;
}

const display = function(imageData) {
  clearPanels();
  const canvas = document.getElementById('canvas');
  const ctx = canvas.getContext('2d');
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  canvas.width  = imageData.length;
  canvas.height = imageData.length; 
  canvas.style.display = "block";
  const buffer = ctx.createImageData(imageData.length - 1, imageData.length - 1);
  var index = 0;
  for(let y = 0; y < imageData.length; y++){
      for(let x = 0; x < imageData[y].length; x++){
          buffer.data[index++] = imageData[y][x][0];
          buffer.data[index++] = imageData[y][x][1];
          buffer.data[index++] = imageData[y][x][2];
          buffer.data[index++] = 255;
      }
  }
  try { 
    ctx.putImageData(buffer, 0, 0);
  } catch(e) {
    console.log(e);
  }
  return imageData;
}

const mkParser = function(f) {
  return f;
}

const runParser = function(m) {
  return m;
}

const toCharList = function(s) {
  return Array.from(s);
}

const applicativeParser = {
  "pure": function(x) {
    return function(inp) {
      return [[x, inp]];
    }
  }
}

const monadParser = {
  "pure": applicativeParser["pure"],
  "bind": function(m) {
    return function(f){
      return function(inp){
        return Array.prototype.concat.apply([], m(inp).map(([x,rest]) => f(x)(rest)));
      }
    }
  }
}

const functorList = {
  "fmap": f => xs => xs.map(f)
}

const applicativeList = {
  "pure": function(x) { return [x]; }
}

const monadList = {
  "pure": applicativeList["pure"],
  "bind": function(xs) {
    return function(f){
      return Array.prototype.concat.apply([], xs.map(x => f(x)));
    }
  }
}

const bind = function(inst) { return inst["bind"]; }

const pure = function(inst) { return inst["pure"]; }

const fmap = function(inst) { return inst["fmap"]; }

const ord = function(ch) {
  return ch.charCodeAt(0);
}

const range = (f) => (start) => (end) => Array(end - start + 1).fill(start).map((x, y) => f(x + y))

const split = (size) => (array) => {
  let result = []
  if (size > 0 && array.length > 0) { 
    for (let i = 0; i < array.length; i += size) {
        let chunk = array.slice(i, i + size)
        result.push(chunk)
    }
  }
  return result
}

const fromListToMap = xs => {
  var dict = {};
  xs.forEach(([k,v]) => dict[k] = v);
  return dict;
}

//************** Type Level Fixed Point **************/

class InClass {
  constructor(value0) {
    this.value0 = value0;
  }
}

const In = function(x) {
  return new InClass(x);
}

const out = function(x) {
  return x.value0;
}

class __Cons {
  constructor(value0, value1) {
    this.value0 = value0
    this.value1 = value1
  }
}

class __Empty {
  constructor() {
  }
}

const Cons = function(x) {
  return function(y) {
    return new __Cons(x, y);
  }
}

const Empty = new __Empty();

const isEmpty = function(v) {
  return v instanceof __Empty; 
}

const isCons = function(v) {
  return v instanceof __Cons; 
}

const extractCons = function(v) {
  return [v.value0, v.value1];
}

const functorListF = {
  "fmap": function (f) {
      return function (m) {
          if (m instanceof __Empty) {
              return Empty;
          };
          if (m instanceof __Cons) {
              return new __Cons(m.value0, f(m.value1));
          };
          throw new Error("Failed pattern match");
      };
  }
};

const functorAsync = {
  "fmap": function(f) {
      return function(m) {
          return new Promise((resolve, reject) => {
             m.then(x => resolve(f(x))).catch(r => reject(x))
          })
      }; 
   }
}

const trace = function(x){
  console.log(x)
  return x;
}

const mkAsync = function(x){
  return Promise.resolve(x);
}

const applicativeAsync = {
  "pure": mkAsync
}

const monadAsync = {
  "pure": applicativeAsync["pure"],
  "bind": function(m) {
    return function(f){
      return m.then(x => f(x))
    }
  }
}

class __Just {
  constructor(value) {
    this.value = value
  }
}

class __Nothing {
  constructor() {
  }
}

const Just = function (x) {
    return new __Just(x);
}

const Nothing = new __Nothing();

const functorMaybe = {
  "fmap": function(f) {
      return function(m) {
          if (m instanceof __Nothing) {
              return Nothing;
          };
          if (m instanceof __Just) {
              return new __Just(f(m.value));
          };
          throw new Error("Failed pattern match");
      }; 
   }
}

const applicativeMaybe = {
  "pure": function(x) { return new __Just(x); },
  "<*>":  function(mf) {
    return function(mx){
        if (mf instanceof __Nothing || mx instanceof __Nothing) {
          return Nothing;
        };
        if (mf instanceof __Just || mx instanceof __Just) {
           return new __Just((mf.value(mx.value)));
        };
        throw new Error("Failed pattern match");
      }
    }
}

const monadMaybe = {
  "pure": applicativeMaybe["pure"],
  "bind": function(m) {
    return function(f){
        if (m instanceof __Nothing) {
            return Nothing;
        };
        if (m instanceof __Just) {
            return f(m.value);
        };
        throw new Error("Failed pattern match");
    }
  }
}

