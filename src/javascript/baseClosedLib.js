function __eqeqInt2([env, y]){
    return env["x"] == y;
}

const eqInt = {
    "==": mkClosure(function([_,x]) { return setEnv("x", x, mkClosure(__eqeqInt2))})
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

const numInt = {
    "+": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(__plusInt2))}),
    "*": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(__mulInt2))}),
    "-": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(__subInt2))}),
    "fromInteger": mkClosure(function([_, x]) { return x; })
  }

  const numDouble = {
    "+": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] + y; }))}),
    "*": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] * y; }))}),
    "-": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] - y; }))}),
    "fromInteger": mkClosure(function([_, x]) { return x; })
  }
  
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

const fromInteger = mkClosure(function([_, inst]) { return inst["fromInteger"]; })

const fromRational = mkClosure(function([_, inst]) { return inst["fromRational"]; })

const __add = mkClosure(function([_, inst]) { return inst["+"]; });

const __sub = mkClosure(function([_, inst]) { return inst["-"]; });

const __mul = mkClosure(function([_, inst]) { return inst["*"]; });

const __div = mkClosure(function([_, inst]) { return inst["/"]; });

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

const isEmpty = mkClosure(function([_, xs]) {
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

const __and = mkClosure(function([_, x]) {
    return setEnv("x", x, mkClosure(function([env, y]) {
      return env["x"] && y;
    }))
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
    return env[name];
}