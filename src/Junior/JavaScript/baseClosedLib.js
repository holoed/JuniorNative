function __eqeqInt2([env, y]){
    return env["x"] == y;
}

function __noteqInt2([env, y]){
  return env["x"] != y;
}

const eqInt = {
    "==": mkClosure(function([_,x]) { return setEnv("x", x, mkClosure(__eqeqInt2))}),
    "/=": mkClosure(function([_,x]) { return setEnv("x", x, mkClosure(__noteqInt2))})
  }

const eqDouble = {
    "==": mkClosure(function([_,x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] == y; }))}),
    "/=": mkClosure(function([_,x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] != y; }))})
  }

const eqString = {
  "==": mkClosure(function([_,x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] == y; }))}),
  "/=": mkClosure(function([_,x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] != y; }))})
}

function __eqeqChar2([env, y]){
    return env["x"] == y;
}

function __noteqChar2([env, y]){
  return env["x"] != y;
}

const eqChar = {
    "==": mkClosure(function([_,x]) { return setEnv("x", x, mkClosure(__eqeqChar2))}),
    "/=": mkClosure(function([_,x]) { return setEnv("x", x, mkClosure(__noteqChar2))})
  }

const ordChar = {
    ">": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] > y }))}),
    "<": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] < y }))}),
    ">=": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] >= y }))}),
    "<=": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] <= y }))}),
    "==": eqChar["=="],
    "/=": eqChar["/="]
  }

const ordString = {
    ">": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] > y }))}),
    "<": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] < y }))}),
    ">=": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] >= y }))}),
    "<=": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] <= y }))}),
    "==": eqChar["=="],
    "/=": eqChar["/="]
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
  "==": eqDouble["=="],
  "/=": eqDouble["/="]
}

const ordInt = {
  ">": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] > y }))}),
  "<": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] < y }))}),
  ">=": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] >= y }))}),
  "<=": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] <= y }))}),
  "==": eqInt["=="],
  "/=": eqInt["/="]
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
    "acos": mkClosure(function([_, x]){ return Math.acos(x) }),
    "asin": mkClosure(function([_, x]){ return Math.asin(x) }),
    "atan2": mkClosure(function([_, [x, y]]){ return Math.atan2(x, y) }),
    "sqrt": mkClosure(function([_, x]){ return Math.sqrt(x) }),
    "log": mkClosure(function([_, x]){ return Math.log(x) }),
    "exp": mkClosure(function([_, x]){ return Math.exp(x) })
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

const __liftA2 = mkClosure(function([_, inst]) { return inst["<*>"]; }) 

const __eqeq = mkClosure(function([_, inst]) { return inst["=="]; })

const __noteq = mkClosure(function([_, inst]) { return inst["/="]; })

const __lt = mkClosure(function([_, inst])   { return inst["<"]; })

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

const __pow = mkClosure(function([_, instA]) {
  return mkClosure(function([_, instB]){
    return mkClosure(function([_, x]){
      return setEnv("x", x, mkClosure(function([env, y]){
        return Math.pow(env["x"], y);
      }))
    })
  })
})

const pow = mkClosure(function([_, inst]) {
    return mkClosure(function([_, x]){
      return setEnv("x", x, mkClosure(function([env, y]){
        return Math.pow(env["x"], y);
      }))
    })
})

const toDouble = mkClosure(function([_, x]) { return x + 0.0; })

const truncate = mkClosure(function([_, instA]) { 
  return mkClosure(function([_, instB]){ 
  return mkClosure(function([_, x]) { return Math.floor(x); }); }); });

const sin = mkClosure(function([_, inst]) { return inst["sin"]; })

const cos = mkClosure(function([_, inst]) { return inst["cos"]; })

const asin = mkClosure(function([_, inst]) { return inst["asin"]; })

const acos = mkClosure(function([_, inst]) { return inst["acos"]; })

const atan2 = mkClosure(function([_, inst]) { return inst["atan2"]; })

const log = mkClosure(function([_, inst]) { return inst["log"]; })

const sqrt = mkClosure(function([_, inst]) { return inst["sqrt"]; })

const exp = mkClosure(function([_, inst]) { return inst["exp"]; })

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

const fromCharList = mkClosure(function([_, xs]) {
    return xs.join("");
  })

const unlines = mkClosure(function([_, xs]) {
    return xs.join("\n");
  })

const lines = mkClosure(function([_, str]){
    return str.split(/\r?\n/);
  })

const mkParser = mkClosure(function([_, f]) {
    return f;
  })

const runParser = mkClosure(function([_, m]) {
    return m;
  })

const functorParser = {
  "fmap": mkClosure(function ([_, f]) {
    return setEnv("f", f, mkClosure(function([env1, m]){
      return setEnv("m", m, setEnv("f", env1["f"], mkClosure(function([env2, inp]){
        return applyClosure(env2["m"], inp).map(([x,rest]) => [applyClosure(env2["f"], x), rest]);
      })))
    }))
  })
}

const applicativeParser = {
    "fmap" : functorParser["fmap"],
    "pure": mkClosure(function([_, x]) {
      return setEnv("x", x, mkClosure(function([env, inp]) {
        return [[env["x"], inp]];
      }))
    }),
    // data Parser a = String -> [(a, String)]
    "<*>":  mkClosure(function([_, mf]) {
      return setEnv("mf", mf, mkClosure(function([env, mx]){
        return setEnv("mf", env["mf"], setEnv("mx", mx, mkClosure(function([env2, inp]){
          return Array.prototype.concat.apply([],
                 applyClosure(env2["mf"], inp).map(([f, rest]) => 
                 applyClosure(applyClosure(applyClosure(functorParser["fmap"], f), env2["mx"]), rest)   ))
        })))
        }))
      })
}

const monadParser = {
    "pure": applicativeParser["pure"],
    ">>=": mkClosure(function([_, m]) {
      return setEnv("m", m, mkClosure(function([env1, f]){
        return setEnv("f", f, setEnv("m", env1["m"], mkClosure(function([env2, inp]){
          return Array.prototype.concat.apply([], applyClosure(env2["m"], inp).map(([x,rest]) => applyClosure(applyClosure(env2["f"], x), rest)));
        })))
      }))
    })
  }

const functorList = {
   "fmap": mkClosure(function ([_, f]) {
     return setEnv("f", f, mkClosure(function ([env, m]) {
       return m.map(x => applyClosure(env["f"], x))
     }))
   })
}

const applicativeList = {
    "fmap": functorList["fmap"],
    "pure": mkClosure(function([_, x]) { return [x]; }),
    "<*>":  mkClosure(function([_, mf]) {
      return setEnv("mf", mf, mkClosure(function([env, mx]){
          return Array.prototype.concat.apply([], env["mf"].map(f => mx.map(x => applyClosure(f, x))))
        }))
      })
  }

const monadList = {
    "pure": applicativeList["pure"],
    ">>=": mkClosure(function([_, xs]) {
      return setEnv("xs", xs, mkClosure(function([env, f]){
        return Array.prototype.concat.apply([], env["xs"].map(x => applyClosure(f, x)));
      }))
    })
  }

const mkReader = mkClosure(function([_, f]) {
  return f;
})

const runReader = mkClosure(function([_, m]) {
    return m;
})

const functorReader = {
  "fmap": mkClosure(function ([_, f]) {
    return setEnv("f", f, mkClosure(function([env1, m]){
      return setEnv("m", m, setEnv("f", env1["f"], mkClosure(function([env2, ctx]){
        return applyClosure(env1["f"], applyClosure(env2["m"], ctx));
      })))
    }))
  })
}

const applicativeReader = {
    "fmap" : functorReader["fmap"],
    "pure": mkClosure(function([_, x]) {
      return setEnv("x", x, mkClosure(function([env, ctx]) {
        return env["x"];
      }))
    }),
    "<*>":  mkClosure(function([_, mf]) {
      return setEnv("mf", mf, mkClosure(function([env, mx]){
        return setEnv("mf", env["mf"], setEnv("mx", mx, mkClosure(function([env2, ctx]){
          return applyClosure(applyClosure(env2["mf"], ctx), applyClosure(env2["mx"], ctx))
        })))}))
      })
}

const monadReader = {
    "pure": applicativeReader["pure"],
    ">>=": mkClosure(function([_, m]) {
      return setEnv("m", m, mkClosure(function([env1, f]){
        return setEnv("f", f, setEnv("m", env1["m"], mkClosure(function([env2, ctx]){
          return applyClosure(applyClosure(env2["f"], applyClosure(env2["m"], ctx)), ctx)
        })))
      }))
    })
  }

const __bind = mkClosure(function([_, inst]) { return inst[">>="]; })

const pure = mkClosure(function([_, inst]) { return inst["pure"]; })

const fmap = mkClosure(function([_, inst]) { return inst["fmap"]; })

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
  return new Promise((resolve, reject) => {
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
    resolve({});
  })
});

const renderPlot3D = mkClosure(function([_, xyz]) {
  return new Promise((resolve, reject) => {
    clearPanels();
    plotChart = document.getElementById("plotlyChart");
    plotChart.style.display = "block"
    const data = [{
      type: 'scatter3d',
      mode: 'markers',
      marker: {
        size: 5,
        line: {
        color: 'rgba(217, 217, 217, 0.14)',
        width: 0.5},
        opacity: 0.8},
      x: xyz[0],
      y: xyz[1],
      z: xyz[2]
    }];
    Plotly.newPlot('plotlyChart', data);
    resolve({});
  });
});

const renderPlot = mkClosure(function([_, z1]) {
  return new Promise((resolve, reject) => {
    clearPanels();
    plotChart = document.getElementById("plotlyChart");
    plotChart.style.display = "block"
    Plotly.newPlot("plotlyChart", [{z: z1, type: 'surface'}]);
    resolve({});
  });
});

const renderTimeSeries = mkClosure(function([_, vs]) {
  return new Promise((resolve, reject) => {
    clearPanels();
    plotChart = document.getElementById("plotlyChart");
    plotChart.style.display = "block"
    Plotly.newPlot("plotlyChart", vs.map(([s, xs, ys]) => {
      const ysUndefined = ys.map(x => x === 0 ? null : x);
      return {
        x: xs,
        y: ysUndefined,
        name: s,
        mode: 'lines',
        connectgaps: true
      };
    }), {showlegend: true});
    resolve({});
  });
});

const renderBarChart = mkClosure(function([_, vs]) {
  return new Promise((resolve, reject) => {
    clearPanels();
    plotChart = document.getElementById("plotlyChart");
    plotChart.style.display = "block"
    Plotly.newPlot("plotlyChart", vs.map(([s, xs, ys]) => {
      const ys1 = ys.map(x => x == 0 ? null : x)
      return {x: xs, y: ys1, name: s, type: 'bar'}
    }), {showlegend: true});
    resolve({});
  });
});

const renderPieChart = mkClosure(function([_, vs]) {
  return new Promise((resolve, reject) => {
    clearPanels();
    plotChart = document.getElementById("plotlyChart");
    plotChart.style.display = "block"
    Plotly.newPlot("plotlyChart", vs.map(([s, xs, ys]) => {
      const ys1 = ys.map(x => x == 0 ? null : x)
      return {labels: xs, values: ys1, name: s, type: 'pie'}
    }), {showlegend: true});
    resolve({});
  });
});

const renderDataGrid = mkClosure(function([_, gridOptions]) {
  return new Promise((resolve, reject) => {
    clearPanels();
    document.getElementById("myGrid").style.display = "block"
    const gridDiv = document.querySelector('#myGrid');
    new agGrid.Grid(gridDiv, gridOptions);
    resolve({});
  });
});

const timeStampToDate = mkClosure(function([_, x]){
  var date = new Date(x*1000);
  return date.toISOString();
})

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

const nativeMulDouble = mkClosure(function([_, x]) {
  return setEnv("x", x, mkClosure(function([env, y]) {
    return env["x"] * y;
  }))
})

const nativeAddInt = mkClosure(function([_, x]) {
  return setEnv("x", x, mkClosure(function([env, y]) {
    return env["x"] + y;
  }))
})

const nativeAddDouble = mkClosure(function([_, x]) {
  return setEnv("x", x, mkClosure(function([env, y]) {
    return env["x"] + y;
  }))
})

const nativeSubInt = mkClosure(function([_, x]) {
  return setEnv("x", x, mkClosure(function([env, y]) {
    return env["x"] - y;
  }))
})

const nativeSubDouble = mkClosure(function([_, x]) {
  return setEnv("x", x, mkClosure(function([env, y]) {
    return env["x"] - y;
  }))
})

const nativeDivDouble = mkClosure(function([_, x]) {
  return setEnv("x", x, mkClosure(function([env, y]) {
    return env["x"] / y;
  }))
})

function div(x, y) {
  return ~~(x / y)
}

const httpGet = mkClosure(function([_, url]) {
  return fetch("/fetch", {
    method: "POST",
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify({
      'url': url
    })
   }).then(response => response.text())
})

function _base64ToArrayBuffer(base64) {
  var binary_string = window.atob(base64);
  var len = binary_string.length;
  var bytes = new Uint8Array(len);
  for (var i = 0; i < len; i++) {
      bytes[i] = binary_string.charCodeAt(i);
  }
  return bytes.buffer;
}

const decompress = mkClosure(function([_, strData]) {
  const buffer = _base64ToArrayBuffer(strData);
  console.log("Decompressing...")
  const data = pako.ungzip(buffer);
  console.log("Decompressed")
  return data;
})

const bufferToIntList = mkClosure(function([_, buffer]){
  return Array.from(buffer);
})

const functorAsync = {
  "fmap": mkClosure(function ([_, f]) {
      return setEnv("f", f, mkClosure(function ([env, m]) {
          return new Promise((resolve, reject) => {
             m.then(x => resolve(applyClosure(env["f"], x))).catch(r => reject(r))
          })
      })); 
   })
}

const traversableAsync = {
  "fmap": functorAsync["fmap"],
  "traverse": mkClosure(function([_, inst]){
    return setEnv("inst", inst, mkClosure(function([env, f]) {
    return setEnv("inst", env["inst"], setEnv("f", f, mkClosure(function([env2, xs]){
        return xs.then(x => applyClosure(applyClosure(env2["inst"]["fmap"], mkAsync), applyClosure(env2["f"], x)));
    })))
  }))
  })
}

const trace = mkClosure(function([_, x]){
  console.log(x)
  return x;
})

const mkAsync = mkClosure(function([_, x]){
  return Promise.resolve(x);
})

const applicativeAsync = {
  "fmap": functorAsync["fmap"],
  "pure": mkAsync,
  "<*>":  mkClosure(function([_, mf]) {
    return setEnv("mf", mf, mkClosure(function([env, mx]){
        return env["mf"].then(f => mx.then(x => applyClosure(f, x)))
      }))
    })
}

const monadAsync = {
  "pure": applicativeAsync["pure"],
  ">>=": mkClosure(function([_, m]) {
    return setEnv("m", m, mkClosure(function([env, f]){
      return env["m"].then(x => applyClosure(f, x))
    }))
  })
}

const JsonValue = mkClosure(function([_, x]){
   return x;
})

const JsonNode = mkClosure(function([_, xs]){
  return xs.map(([k, v]) => JSON.parse(`{ "${k}":${JSON.stringify(v)} }`))
           .reduce((x, y) => Object.assign(x, y))
})

class __Just {
  constructor(value) {
    this.value = value
  }
}

class __Nothing {
  constructor() {
  }
}

const Just = mkClosure(function([_, x]) {
    return new __Just(x);
})

const Nothing = new __Nothing();

const isJust = mkClosure(function([_, m]) {
  return m instanceof __Just
})

const isNothing = mkClosure(function([_, m]) {
  return m instanceof __Nothing
})


const extractJust = mkClosure(function([_, m]){
  if (m instanceof __Just) {
    return m.value;
  } else {
    throw "Maybe is empty"
  }
})

const fromJust = extractJust;

const functorMaybe = {
  "fmap": mkClosure(function ([_, f]) {
      return setEnv("f", f, mkClosure(function ([env, m]) {
          if (m instanceof __Nothing) {
              return Nothing;
          };
          if (m instanceof __Just) {
              return new __Just(applyClosure(env["f"], m.value));
          };
          throw new Error("Failed pattern match");
      })); 
   })
}

const applicativeMaybe = {
  "fmap": functorMaybe["fmap"],
  "pure": mkClosure(function([_, x]) { return new __Just(x); }),
  "<*>":  mkClosure(function([_, mf]) {
    return setEnv("mf", mf, mkClosure(function([env, mx]){
        if (env["mf"] instanceof __Nothing || mx instanceof __Nothing) {
          return Nothing;
        };
        if (env["mf"] instanceof __Just || mx instanceof __Just) {
           return new __Just(applyClosure(env["mf"].value, mx.value));
        };
        throw new Error("Failed pattern match");
      }))
    })
}

const monadMaybe = {
  "pure": applicativeMaybe["pure"],
  ">>=": mkClosure(function([_, m]) {
    return setEnv("m", m, mkClosure(function([env, f]){
        if (env["m"] instanceof __Nothing) {
            return Nothing;
        };
        if (env["m"] instanceof __Just) {
            return applyClosure(f, m.value);
        };
        throw new Error("Failed pattern match");
    }))
  })
}

const parseJson = mkClosure(function([_, s]){
  try {
    return applyClosure(Just, JSON.parse(s));
  } catch (e) {
    console.log(`Error while parsing JSON ${e}`)
    return Nothing;
  }
})

const getJsonValue = mkClosure(function([_, x]) {
  return setEnv("x", x, mkClosure(function([env, y]){
    const z = y[env["x"]];
    if (z != undefined) { return applyClosure(Just, z) }
    else { return Nothing; }
  }))
})

const getJsonList = mkClosure(function([_, x]) {
  return setEnv("x", x, mkClosure(function([env, y]){
    const z = y[env["x"]];
    if (z && Array.isArray(z)) { return applyClosure(Just, z) }
    else { return Nothing; }
  }))
})

const jsonToString = mkClosure(function([_, x]){
  if (typeof x === 'string' || x instanceof String) {
    return applyClosure(Just, x);
  } else { return Nothing; }
})

const jsonToInt = mkClosure(function([_, x]){
  if (Number.isInteger(x)) {
    return applyClosure(Just, x);
  } else { return Nothing; }
})

const jsonToDouble = mkClosure(function([_, x]){
  if (x == null) {
    return applyClosure(Just, 0);
  }
  try {
    const v = Number.parseFloat(x);
    return applyClosure(Just, v);
  } catch(e) { return Nothing; }
})

const stringToJson = mkClosure(function([_, x]){
  return x;
})

const intToJson = mkClosure(function([_, x]){
  return x;
})

const traversableMaybe = {
  "fmap": functorMaybe["fmap"],
  "traverse": mkClosure(function([_, inst]){
    return setEnv("inst", inst, mkClosure(function([env, f]) {
    return setEnv("inst", env["inst"], setEnv("f", f, mkClosure(function([env2, xs]){
        if (xs instanceof __Nothing) {
            return env2["inst"]["pure"](Nothing);
        };
        if (xs instanceof __Just) {
            return applyClosure(applyClosure(env2["inst"]["fmap"], Just), applyClosure(env2["f"], xs.value));
        };
        throw new Error("Failed pattern match");
    })))
  }))
  })
}

function cons_f(inst, g, x, ys){
  const fmap = inst["fmap"]
  const ap = inst["<*>"]
  const liftA2 = mkClosure(function([_, f]){
    return setEnv("f", f, mkClosure(function([env, x]){ 
      return setEnv("f", env["f"], setEnv("x", x, mkClosure(function([_, y]){
      return applyClosure(applyClosure(ap, applyClosure(applyClosure(fmap, env["f"]), x)), y)
    })))
  }))
  })
  return applyClosure(applyClosure(applyClosure(liftA2, __colon), applyClosure(g, x)), ys)
}

const traversableList = {
  "fmap": functorList["fmap"],
  "traverse": mkClosure(function([_, inst]){
    return setEnv("inst", inst, mkClosure(function([env, f]) {
    return setEnv("inst", env["inst"], setEnv("f", f, mkClosure(function([env2, xs]){
      return xs.reduceRight(function(acc, cur){
        return cons_f(env2["inst"], f, cur, acc);
      }, applyClosure(env2["inst"]["pure"], []));
    })))
  }))
  })
}

const traverse = mkClosure(function([_, inst]) { 
  return setEnv("inst", inst, mkClosure(function([env, inst2]) {
    return applyClosure(inst2["traverse"], env["inst"])
  }))
})

const foldableList = {
  "foldr": mkClosure(function([_, f]){
    return setEnv("f", f, mkClosure(function([env, v]){
      return setEnv("f", env["f"],
             setEnv("v", v, mkClosure(function([env2, xs]){
               return xs.reduceRight(function(acc, cur){
                 return applyClosure(applyClosure(env2["f"], cur), acc);
               }, env2["v"]);
             })))
    }))
  }),
  "foldl": mkClosure(function([_, f]){
    return setEnv("f", f, mkClosure(function([env, v]){
      return setEnv("f", env["f"],
             setEnv("v", v, mkClosure(function([env2, xs]){
               return xs.reduce(function(acc, cur){
                 return applyClosure(applyClosure(env2["f"], acc), cur);
               }, env2["v"]);
             })))
    }))
  })
}

const foldableMaybe = {
  "foldr": mkClosure(function([_, f]){
    return setEnv("f", f, mkClosure(function([env, v]){
      return setEnv("f", env["f"],
             setEnv("v", v, mkClosure(function([env2, xs]){
              if (xs instanceof __Nothing) {
                return env2["v"];
              };
              if (xs instanceof __Just) {
                return applyClosure(applyClosure(env2["f"], xs.value), env2["v"]);
              };
             })))
    }))
  })
}

const foldr = mkClosure(function([_, inst]) { return inst["foldr"]; })
const foldl = mkClosure(function([_, inst]) { return inst["foldl"]; })

const semigroupString = {
  "<>" : mkClosure(function([_, xs]){
    return setEnv("xs", xs, mkClosure(function([env, ys]){
      return env["xs"] + ys;
    }))
  })
}

const semigroupList = {
  "<>" : mkClosure(function([_, xs]){
    return setEnv("xs", xs, mkClosure(function([env, ys]){
      return env["xs"].concat(ys);
    }))
  })
}

const mappend = mkClosure(function([_, inst]) { return inst["<>"]; })

const fromMaybe = mkClosure(function([_, x]){
  return setEnv("x", x, mkClosure(function([env, mx]){
    if (mx instanceof __Nothing) {
      return env["x"];
    };
    if (mx instanceof __Just) {
      return mx.value;
    };
  }))
})

const fromMaybeLazy = mkClosure(function([_, x]){
  return setEnv("x", x, mkClosure(function([env, mx]){
    if (mx instanceof __Nothing) {
      return applyClosure(env["x"], null);
    };
    if (mx instanceof __Just) {
      return mx.value;
    };
  }))
})

const error = mkClosure(function([_, x]){
  return mkClosure(function([_, _2]){ throw x; });
})

const showInt = {
  "show" : mkClosure(function([_, x]) {
    return `${x}`
  })
}

const showDouble = {
  "show" : mkClosure(function([_, x]) {
    return `${x}`
  })
}

const showList = mkClosure(function([_, inst]) {
  return {
    "show" : mkClosure(function([_, xs]){
      return xs.map(x => applyClosure(inst["show"], x))
    })
  }
})

const show = mkClosure(function([_, inst]) { return inst["show"]})

const fromListToMap = mkClosure(function([_, xs]){
  return Object.fromEntries(xs);
})

const mapToJson = mkClosure(function([_, xs]){
  return xs;
})

const listToJson = mkClosure(function([_, xs]){
  return xs;
})

const jsonToList = mkClosure(function([_, xs]){
  return xs;
})

const headStr = mkClosure(function([_, xs]){
  return xs[0];
})

const tailStr = mkClosure(function([_, xs]){
  return xs.slice(1);
})

const charToStr = mkClosure(function([_, x]){
  return `${x}`
})

const __exclexcl = mkClosure(function([_, xs]){
  return setEnv("xs", xs, mkClosure(function([env, n]){
    const vs = env["xs"]
    if (n < 0) throw "Exception: Prelude.!!: negative index"
    if (n >= vs.length) throw "Exception: Prelude.!!: index too large"
    return env["xs"][n];
  }))
})

const length = mkClosure(function([_, xs]){
  return xs.length;
})

const take = mkClosure(function([_, n]){
  return setEnv("n", n, mkClosure(function([env, xs]){
    return xs.slice(0, env["n"]);
  }))
})

const drop = mkClosure(function([_, n]){
  return setEnv("n", n, mkClosure(function([env, xs]){
    return xs.slice(env["n"]);
  }))
})

async function runCode(fn, x, retries) {
    try {
    const response = await fetch("/eval", {
      method: "POST",
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        'code': codeBase64,
        'fn': fn,
        'arg': JSON.stringify(x)
      })
    });
    return await response.json();
  } catch (e) {
    console.log(e)
    if (retries > 0) {
      return await runCode(fn, x, retries - 1)
    }
  }
}

const remote = mkClosure(function([_, f]){
      return setEnv("f", f, mkClosure(async function([env, x]){
        if (typeof codeBase64 != 'undefined') {
          try {
            return await runCode(env["f"], x, 3)
          } catch (e) {
            console.log(e)
          }
        }
      }))
    })

const quote = mkClosure(function([_, s]){
  return s
})

const serializableInt = {}

class IO {
  constructor(fn) {
    this.fn = fn
  }

  run(world) {
    return applyClosure(this.fn, world);
  }
}

const randomIO = new IO(mkClosure(function([_, world]) { return Promise.resolve([1 - Math.random(), world + 1]) }))

const currentDate = new IO(mkClosure(function([_, world]) { return Promise.resolve([new Date().toISOString().slice(0, 10), world + 1]) }))

const asyncToIO = mkClosure(function ([_, f]) { 
  return new IO(setEnv("f", f, mkClosure(function([env, world]) { 
    return new Promise((resolve, reject) => {
      env["f"].then(x => resolve([x, world + 1]).catch(r => reject(r)))
   })
   })))
 })

const functorIO = {
  "fmap": mkClosure(function ([_, f]) {
    return setEnv("f", f, mkClosure(function([env1, m]){
      return new IO (setEnv("m", m, setEnv("f", env1["f"], mkClosure(function([env2, world]){
        return env2["m"].run(world).then(([x, _]) => [applyClosure(env2["f"], x), world]);
      }))))
    }))
  })
}

const applicativeIO = {
    "fmap" : functorIO["fmap"],
    "pure": mkClosure(function([_, x]) {
      return new IO(setEnv("x", x, mkClosure(function([env, world]) { return Promise.resolve([env["x"], world]) })))
    }),
    "<*>":  mkClosure(function([_, mf]) {
      return setEnv("mf", mf, mkClosure(function([env, mx]){
        return new IO(setEnv("mf", env["mf"], setEnv("mx", mx, mkClosure(function([env2, world]){
          return env2["mf"].run(world).then(([f, world2]) =>
                 env2["mx"].run(world2).then(([x, world3]) =>
                 [applyClosure(f, x), world3]))
        }))))
        }))
      })
}

const monadIO = {
    "pure": applicativeIO["pure"],
    ">>=": mkClosure(function([_, m]) {
      return setEnv("m", m, mkClosure(function([env1, f]){
        return new IO(setEnv("f", f, setEnv("m", env1["m"], mkClosure(function([env2, world]){
          return env2["m"].run(world).then(([x, world2]) => applyClosure(env2["f"], x).run(world2))
        }))))
      }))
    })
  }

const round = mkClosure(function([_, inst]) {
  return mkClosure(function([_, x]) {
    return Math.round(x)
  })
})

function eqListEqEq([env2, ys]){
  return env2["xs"].length == ys.length &&
         env2["xs"].every((v, i) => applyClosure(applyClosure(env2["inst"]["=="], v), ys[i]))   
}

function eqListNotEqEq([env2, ys]){
  return env2["xs"].length != ys.length ||
         env2["xs"].some((v, i) => applyClosure(applyClosure(env2["inst"]["/="], v), ys[i])) 
}

const eqList = mkClosure(function([_, inst]){
  return {
    "==": setEnv("inst", inst, mkClosure(function([env,xs]) { return setEnv("inst", env["inst"], setEnv("xs", xs, mkClosure(eqListEqEq)))})),
    "/=": setEnv("inst", inst, mkClosure(function([env,xs]) { return setEnv("inst", env["inst"], setEnv("xs", xs, mkClosure(eqListNotEqEq)))}))
  }
})

// Neural Networks with TensorFlow

const feedForwardModel = mkClosure(function([_, xs]) {
  // Define a model Feed Forward Network
  model = tf.sequential();
  model.add(tf.layers.dense({ inputShape: [xs[0]], units: xs[1], activation: 'relu' }));
  xs.slice(2).map(x => model.add(tf.layers.dense({ units: x, activation: 'sigmoid' })));
  // Prepare the model for training: Specify the loss and the optimizer.
  model.compile({ optimizer: 'adam', loss: 'meanSquaredError', metrics: ['binaryAccuracy'] });
  return model;
})

const mkTensor2D = mkClosure(function([_, input]) {
  return mkClosure(function([_, x]) {
    return tf.tensor(input, [input.length, x]);
  })
})

const surface = { name: 'show.history', tab: 'Training' };

const trainModel = mkClosure(function([_, model]) {
  return mkClosure(function([_, inputTensor]) {
    return mkClosure(function([_, outputTensor]) {
      return mkClosure(function([_, epochs]) {
        return mkClosure(async function([_, shuffle]) {
          const trainingHistory = await model.fit(inputTensor, outputTensor, {epochs: epochs, shuffle:shuffle})
          const lossHistory =  trainingHistory.history.loss.map(x => { return { loss: x }});
          tfvis.show.history(surface, lossHistory, ['loss']);
          return model
        })
      })
    })
  })
})

const predict = mkClosure(function([_, model]) {
  return mkClosure(function([_, testTensor]) {
    return model.predict(testTensor);
  })
})

const tensor2DToArray = mkClosure(async function ([_, tensor]){
  return tensor.array();
})

const memoize = 
  mkClosure(function ([_, o]){
    const cache = {};
    return mkClosure(function ([_, fn]){
      return mkClosure(function ([_, x]) {
        const key = JSON.stringify(x);
        if (cache[key]) {
          return cache[key];
        }
        const result = applyClosure(fn, x)
        cache[key] = result;
        return result;
      })
    })
  })

