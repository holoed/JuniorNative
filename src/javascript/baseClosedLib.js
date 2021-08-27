
const numInt = {
    "+": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] + y; }))}),
    "*": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] * y; }))}),
    "-": mkClosure(function([_, x]) { return setEnv("x", x, mkClosure(function([env, y]) { return env["x"] - y; }))}),
    "fromInteger": { fun : function([_, x]) { return x; } }
  }

const fromInteger = mkClosure(function([_, inst]) { return inst["fromInteger"]; })

const __add = mkClosure(function([_, inst]) { return inst["+"]; });

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