function __eqeqInt2([env, y]){
    return env["x"] == y;
}

const eqInt = {
    "==": mkClosure(function([_,x]) { return setEnv("x", x, mkClosure(__eqeqInt2))})
  }

const eqDouble = {
    "==": mkClosure(function([_,x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] == y; }))})
  }

function __eqeqChar2([env, y]){
    return env["x"] == y;
}

const eqChar = {
    "==": mkClosure(function([_,x]) { return setEnv("x", x, mkClosure(__eqeqChar2))})
  }

const ordChar = {
    ">": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] > y }))}),
    "<": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] < y }))}),
    ">=": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] >= y }))}),
    "<=": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] <= y }))}),
    "==": eqChar["=="]
  }

function __plusInt2([env, y]){
    return env["x"] + y; 
}

function __mulInt2([env, y]){
    return env["x"] * y; 
}

function __subInt2([env, y]){
    return env["x"] - y; 
}

const ordDouble = {
  ">": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] > y }))}),
  "<": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] < y }))}),
  ">=": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] >= y }))}),
  "<=": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] <= y }))}),
  "==": eqDouble["=="]
}


const numInt = {
    "+": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(__plusInt2))}),
    "*": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(__mulInt2))}),
    "-": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(__subInt2))}),
    "fromInteger": mkClosure(function([_, x]) { return x; })
  }

  const fractionalInt = {
    "+": numInt["+"],
    "*": numInt["*"],
    "-": numInt["-"],
    "/": mkClosure(function([_,x]) { return setEnv("x", x, mkClosure(function([env, y]) { return ~~(env["x"]/y); }))}),
    "fromInteger": numInt["fromInteger"],
    "fromRational": mkClosure(function([_, x]) { return Math.floor(x); })
  }

  const integralInt = {
    "+": numInt["+"],
    "*": numInt["*"],
    "-": numInt["-"],
    "fromInteger": numInt["fromInteger"],
    "mod": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] % y; }))}),
  }

  const numDouble = {
    "+": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] + y; }))}),
    "*": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] * y; }))}),
    "-": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] - y; }))}),
    "fromInteger": mkClosure(function([_, x]) { return x; })
  }

  const floatingDouble = {
    "cos": mkClosure(function([_, x]){ return Math.cos(x) }),
    "sin": mkClosure(function([_, x]){ return Math.sin(x) }),
    "sqrt": mkClosure(function([_, x]){ return Math.sqrt(x) }),
  }

  const numTuple2 = mkClosure(function([_, instA]) {
    return mkClosure(function([_, instB]) {
      const addA = instA["+"];
      const addB = instB["+"];
      const subA = instA["-"];
      const subB = instB["-"];
      const mulA = instA["*"];
      return {
        "fromInteger": mkClosure(function([_, x]) { return [x, x]; }),
        "+": mkClosure(function([_, [x1, y1]]){ 
          return setEnv("x1y1", [x1, y1], mkClosure(function([env, [x2, y2]]) { 
          return [applyClosure(applyClosure(addA, env["x1y1"][0]), x2), 
                  applyClosure(applyClosure(addB, env["x1y1"][1]), y2)]  })) }),
        "-": mkClosure(function([_, [x1, y1]]){ 
          return setEnv("x1y1", [x1, y1], mkClosure(function([env, [x2, y2]]) { 
          return [applyClosure(applyClosure(subA, env["x1y1"][0]), x2), 
                  applyClosure(applyClosure(subB, env["x1y1"][1]), y2)]  })) }),
        "*": mkClosure(function([_, [x1, y1]]){ 
          return setEnv("x1y1", [x1, y1], mkClosure(function([env, [x2, y2]]) { 
          
          let ret1 = applyClosure(mulA, env["x1y1"][0])
          let ret2 = applyClosure(ret1, x2)
          let ret3 = applyClosure(subA, ret2)
          let ret4 = applyClosure(mulA, env["x1y1"][1])
          let ret5 = applyClosure(ret4, y2)
          let ret6 = applyClosure(ret3, ret5)

          let ret7 = applyClosure(mulA, env["x1y1"][0])
          let ret8 = applyClosure(ret7, y2)
          let ret9 = applyClosure(addA, ret8)
          let ret10 = applyClosure(mulA, env["x1y1"][1])
          let ret11 = applyClosure(ret10, x2)
          let ret12 = applyClosure(ret9, ret11)

          return [ret6, ret12];
        } ))
      })
      }
    })
  })
  
  const fractionalDouble = {
    "+": numDouble["+"],
    "*": numDouble["*"],
    "-": numDouble["-"],
    "/": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] / y; }))}),
    "fromInteger": numDouble["fromInteger"],
    "fromRational": mkClosure(function([_, x]) { return x; })
  }

const __eqeq = mkClosure(function([_, inst]) { return inst["=="]; })

const __lteq = mkClosure(function([_, inst]) { return inst["<="]; })

const __gt = mkClosure(function([_, inst])   { return inst[">"]; })

const __gteq = mkClosure(function([_, inst]) { return inst[">="]; })

const fromInteger = mkClosure(function([_, inst]) { return inst["fromInteger"]; })

const fromRational = mkClosure(function([_, inst]) { return inst["fromRational"]; })

const __mod = mkClosure(function([_, inst]) { return inst["mod"]; });

const __add = mkClosure(function([_, inst]) { return inst["+"]; });

const __sub = mkClosure(function([_, inst]) { return inst["-"]; });

const __mul = mkClosure(function([_, inst]) { return inst["*"]; });

const __div = mkClosure(function([_, inst]) { return inst["/"]; });

const toDouble = mkClosure(function([_, x]) { return x + 0.0; })

const truncate = mkClosure(function([_, instA]) { 
  return mkClosure(function([_, instB]){ 
  return mkClosure(function([_, x]) { return Math.floor(x); }); }); });

const sin = mkClosure(function([_, inst]) { return inst["sin"]; })

const cos = mkClosure(function([_, inst]) { return inst["cos"]; })

const sqrt = mkClosure(function([_, inst]) { return inst["sqrt"]; })

function range_3([env, end]) {
    return Array(end - env["start"] + 1).fill(env["start"]).map((x, y) => applyClosure(env["f"], (x + y)))
}

function range_2([env, start]) {
    return setEnv("f", env["f"], setEnv("start", start, mkClosure(range_3)))
}

function range_1([_, f]) {
    return setEnv("f", f, mkClosure(range_2))
}

const range = mkClosure(range_1)

const isEmptyList = mkClosure(function([_, xs]) {
    return xs.length == 0;
  })

const head = mkClosure(function([_, xs]) {
    return xs[0];
  })

const tail = mkClosure(function([_, xs]) {
    return xs.slice(1);
  })

const ord = mkClosure(function([_, ch]) {
    return ch.charCodeAt(0);
  })

const toCharList = mkClosure(function([_, s]) {
    return Array.from(s);
  })

const mkParser = mkClosure(function([_, f]) {
    return f;
  })

const runParser = mkClosure(function([_, m]) {
    return m;
  })

const applicativeParser = {
    "pure": mkClosure(function([_, x]) {
      return setEnv("x", x, mkClosure(function([env, inp]) {
        return [[env["x"], inp]];
      }))
    })
}

const monadParser = {
    "pure": applicativeParser["pure"],
    "bind": mkClosure(function([_, m]) {
      return setEnv("m", m, mkClosure(function([env1, f]){
        return setEnv("f", f, setEnv("m", env1["m"], mkClosure(function([env2, inp]){
          return Array.prototype.concat.apply([], applyClosure(env2["m"], inp).map(([x,rest]) => applyClosure(applyClosure(env2["f"], x), rest)));
        })))
      }))
    })
  }

const applicativeList = {
    "pure": mkClosure(function([_, x]) { return [x]; })
  }

const monadList = {
    "pure": applicativeList["pure"],
    "bind": mkClosure(function([_, xs]) {
      return setEnv("xs", xs, mkClosure(function([env, f]){
        return Array.prototype.concat.apply([], env["xs"].map(x => applyClosure(f, x)));
      }))
    })
  }

const bind = mkClosure(function([_, inst]) { return inst["bind"]; })

const pure = mkClosure(function([_, inst]) { return inst["pure"]; })

const fmap = mkClosure(function([_, inst]) { return inst["fmap"]; })

const __dot = mkClosure(function([_, f]) {
    return setEnv("f", f, mkClosure(function([env, g]) {
      return setEnv("g", g, setEnv("f", env["f"], mkClosure(function([env, x]) {
        return applyClosure(env["f"], applyClosure(env["g"], x));
      })))
    }))
  })

const __colon = mkClosure(function([_, x]) {
    return setEnv("x", x, mkClosure(function([env, xs]) {
      const ys = xs.slice();
      ys.unshift(env["x"]);
      return ys;
    }))
  })

const split = mkClosure(function ([_, size]) {
  return setEnv("size", size, mkClosure(function ([env, array]) {
    let result = []
    if (env["size"] > 0 && array.length > 0) { 
      for (let i = 0; i < array.length; i += env["size"]) {
          let chunk = array.slice(i, i + env["size"])
          result.push(chunk)
      }
    }
    return result
  }))
})

const __and = mkClosure(function([_, x]) {
    return setEnv("x", x, mkClosure(function([env, y]) {
      return env["x"] && y;
    }))
})

const __or = mkClosure(function([_, x]) {
  return setEnv("x", x, mkClosure(function([env, y]) {
    return env["x"] || y;
  }))
})

const display = mkClosure(function([_, imageData]) {
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
});

function getFunction(e) {
    return e.fun;
}

function mkClosure(f) {
    return {
        fun: f,
        env: {}
    };
}

function applyClosure(c, arg) {
    return c.fun([c.env, arg]);
}

function setEnv(name, value, c) {
    c.env[name] = value;
    return c;
}

function getEnv(name, env) {
    return (env[name])();
}

const nativeMulInt = mkClosure(function([_, x]) {
  return setEnv("x", x, mkClosure(function([env, y]) {
    return env["x"] * y;
  }))
})

function div(x, y) {
  return ~~(x / y)
}

//************** Type Level Fixed Point **************/

class InClass {
  constructor(value0) {
    this.value0 = value0;
  }
}

const In = mkClosure(function([_, x]) {
  return new InClass(x);
})

const fixOut = mkClosure(function([_, x]) {
  return x.value0;
})

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

const Cons = mkClosure(function([_, x]) {
  return setEnv("x", x, mkClosure(function([env, y]) {
    return new __Cons(env["x"], y);
  }))
})

const isCons = mkClosure(function([_, v]) {
  return v instanceof __Cons;
})

const extractCons =  mkClosure(function([_, v]) {
  return [v.value0, v.value1];
})

const Empty = new __Empty();

const isEmpty = mkClosure(function([_, v]) {
  return v instanceof __Empty;
})

const functorListFInt = {
  "fmap": mkClosure(function ([_, f]) {
      return setEnv("f", f, mkClosure(function ([env, m]) {
          if (m instanceof __Empty) {
              return Empty;
          };
          if (m instanceof __Cons) {
              return new __Cons(m.value0, applyClosure(env["f"], m.value1));
          };
          throw new Error("Failed pattern match");
      })); 
   })
}