/* A global variable that represents the current
   closure we're evaluating */
var NODE = undefined

/* INT_REG is the place where all fully evaluated integers
   get stuck. */
var INT_REG = undefined

/* Another global variable with the current argument stack */
var ARG_STACK  = []

/* Another stack with all the current evaluated primitives */
var EVAL_STACK = []

/* Stack for continuations */
var CONT_STACK = []



var mkClosure = function(entry_code, closed){
    return {entry : entry_code, closed_vars : closed}
}

var primReturn = function(num){
    return function(){
        EVAL_STACK.push(num);
        cont = CONT_STACK.pop();
        cont();
    }
}

var enter = function(closure){
    NODE = closure;
    closure.entry();
}

var enterMain = function(){
    var clos = mkClosure(main, []);
    CONT_STACK.push(terminal);
    enter(clos);
}

var jumpNext = function(f){
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
