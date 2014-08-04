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
    closure.entry();
}

var evalFirst = function(){
    var closure = ARG_STACK.pop();
    NODE = closure;
    enter(closure);
}

var doPlus = function(){
    left  = EVAL_STACK.pop();
    right = EVAL_STACK.pop();
    EVAL_STACK.push(left + right);
}
var doMinus = function(){
    left  = EVAL_STACK.pop();
    right = EVAL_STACK.pop();
    EVAL_STACK.push(left - right);
}
var doMult = function(){
    left  = EVAL_STACK.pop();
    right = EVAL_STACK.pop();
    EVAL_STACK.push(left * right);
}
var doDiv = function(){
    left  = EVAL_STACK.pop();
    right = EVAL_STACK.pop();
    EVAL_STACK.push(left / right);
}
