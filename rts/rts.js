/* A global variable that represents the current
   closure we're evaluating */
var NODE = undefined

/* Another global variable with the current argument stack */
var ARG_STACK  = []

/* Another stack with all the current evaluated primitives */
var EVAL_STACK = []

/* Stack for continuations */
var CONT_STACK = []

var mkClosure = function(entry_code, closed, update){
    return {entry : entry_code, closed_vars : closed, shouldUpdate : update}
}

var primReturn = function(num){
    return function(){
        EVAL_STACK.push(num);
        cont = CONT_STACK.pop();
        cont();
    }
}

var doUpdate = function (closure) {
    return function(){
        var result = EVAL_STACK[0]; // Notice that we don't pop this
        closure.entry = primReturn(result);
        jumpNext(); // Continue on with the computation
    };
}

var enter = function(closure){
    NODE = closure;
    if(closure.shouldUpdate) {
        CONT_STACK.push(doUpdate(closure));
    }
    closure.entry();
}

var enterMain = function(){
    var clos = mkClosure(main, [], 1);
    CONT_STACK.push(terminal);
    enter(clos);
}

var jumpNext = function(){
    var next = CONT_STACK.pop();
    next();
}

var nextArg = function(){
    return ARG_STACK.pop();
}

var evalFirst = function(){
    var closure = ARG_STACK.pop();
    NODE = closure;
    enter(closure);
}

var sif = function(n, l, r){
    ARG_STACK.push(l);
    ARG_STACK.push(r);
    CONT_STACK.push(function(){
        var nRes = EVAL_STACK.pop();
        var r = ARG_STACK.pop()
        var l = ARG_STACK.pop()
        if(nRes === 0){
            enter(l);
        } else {
            enter(r);
        }
    });
    enter(n);
}

var doPlus = function(){
    right = EVAL_STACK.pop();
    left  = EVAL_STACK.pop();
    EVAL_STACK.push(left + right);
    jumpNext();
}
var doMinus = function(){
    right = EVAL_STACK.pop();
    left  = EVAL_STACK.pop();
    EVAL_STACK.push(left - right);
    jumpNext();
}
var doMult = function(){
    right = EVAL_STACK.pop();
    left  = EVAL_STACK.pop();
    EVAL_STACK.push(left * right);
    jumpNext();
}
var doDiv = function(){
    right = EVAL_STACK.pop();
    left  = EVAL_STACK.pop();
    EVAL_STACK.push(left / right);
    jumpNext();
}

var terminal = function(){
    console.log(EVAL_STACK.pop());
}
