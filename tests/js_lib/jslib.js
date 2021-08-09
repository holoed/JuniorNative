"use strict";

const eqInt = {
  "==": function(x) { return function(y) { 
      return x == y; 
  }}
}

const eqDouble = {
  "==": function(x) { return function(y) { 
      return x == y; 
  }}
}

const eqChar = {
  "==": function(x) { return function(y) { 
      return x == y; 
  }}
}

const ordInt = {
  ">": function(x) { return function(y) { return x > y }},
  "<": function(x) { return function(y) { return x < y }},
  ">=": function(x) { return function(y) { return x >= y }},
  "<=": function(x) { return function(y) { return x <= y }},
  "==": eqInt["=="]
}

const ordDouble = {
  ">": function(x) { return function(y) { return x > y }},
  "<": function(x) { return function(y) { return x < y }},
  ">=": function(x) { return function(y) { return x >= y }},
  "<=": function(x) { return function(y) { return x <= y }},
  "==": eqDouble["=="]
}

const ordChar = {
  ">": function(x) { return function(y) { return x > y }},
  "<": function(x) { return function(y) { return x < y }},
  ">=": function(x) { return function(y) { return x >= y }},
  "<=": function(x) { return function(y) { return x <= y }},
  "==": eqChar["=="]
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
  "cos": function(x) { return Math.cos(x); },
  "sin": function(x) { return Math.sin(x); }
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

const fromInteger = function(inst) {
  return function(x) {
     return inst["fromInteger"](x);
  }
}

const fromRational = function(inst) {
  return function(x) {
     return inst["fromRational"](x);
  }
}

const __add = function(inst) {
  return function(x) {
    return function(y) {
      return inst["+"](x)(y)
    }
  }
}

const __mul = function(inst) {
  return function(x) {
    return function(y) {
      return inst["*"](x)(y)
    }
  }
}

const __sub = function(inst) {
  return function(x) {
    return function(y) {
      return inst["-"](x)(y)
    }
  }
}

const __div = function(inst) {
  return function(x) {
    return function(y) {
      return inst["/"](x)(y)
    }
  }
}

const __eqeq = function(inst) {
  return function(x) {
    return function(y) {
      return inst["=="](x)(y)
    }
  }
}

const __gt = function(inst) {
  return function(x) {
    return function(y) {
      return inst[">"](x)(y)
    }
  }
}

const __gteq = function(inst) {
  return function(x) {
    return function(y) {
      return inst[">="](x)(y)
    }
  }
}

const __lteq = function(inst) {
  return function(x) {
    return function(y) {
      return inst["<="](x)(y)
    }
  }
}

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

const isEmpty = function(xs) {
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

const truncate = function(x) {
  return Math.floor(x);
}

const cos = function(inst) {
  return function(x) {
    return inst["cos"](x);
  }
}

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
  createImageBitmap(buffer).then(img =>  
  ctx.drawImage(img, 0, 0));
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

const bind = function(inst) {
  return function(m) {
    return function(f) {
      return inst["bind"](m)(f);
    }
  }
}

const pure = function(inst) {
  return function(x) {
    return inst["pure"](x);
  }
}

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