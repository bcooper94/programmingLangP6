library(methods)
library(testthat)
library(hash)

exprC <- setClass("ExprC", 
slots = character(0),
prototype=list(),
)

trueC <- setClass("TrueC",
slots = character(0),
prototype=list(),
contains = "ExprC")

falseC <- setClass("FalseC",
slots = character(0),
prototype=list(),
contains = "ExprC")

numC <- setClass("NumC",
slots = c(num = "numeric"),
prototype = list(),
contains = "ExprC")

binop <- setClass("BinopC",
slots = c(symbol = "character",
		left = "ExprC",
		right = "ExprC"),
prototype = list(),
contains = "ExprC")

ifC <- setClass("IfC",
slots = c(test = "ExprC",
		thenBlock = "ExprC",
		elseBlock = "ExprC"),
prototype = list(),
contains = "ExprC")

appC <- setClass("AppC",
slots = c(func = "ExprC",
		args = "list"),
prototype = list(),
contains = "ExprC")

idC <- setClass("IdC",
slots = c(symbol = "character"),
prototype = list(),
contains = "ExprC")

lamC <- setClass("LamC",
slots = c(params = "list",
		body = "ExprC"),
prototype = list(),
contains = "ExprC")

Value <- setClass(
"Value",
slots = character(0),
prototype=list())

numV <- setClass(
"numV",
slots = c(num = "numeric"),
prototype=list(),
contains = "Value")

boolV <- setClass(
"boolV",
slots = c(val = "logical"),
prototype=list(),
contains = "Value")

closV <- setClass(
"closV",
slots = c(args = "list", body = "ExprC", env = "environment"),
prototype=list(),
contains = "Value")

Env <- setClass(
"Env",
slots = c(values = "list"),
prototype = list())

interp <- function(expr, env) {
   if (is(expr, 'NumC')) {
      return(numV(num=expr@num))
   }
   else if (is(expr, 'TrueC')) {
      return(boolV(val=TRUE))
   }
   else if (is(expr, 'FalseC')) {
      return(boolV(val=FALSE))
   }
   else if (is(expr, 'LamC')) {
      return(closV(args=expr@params, body=expr@body, env=env))
   }
   else if (is(expr, 'AppC')) {
      interpApp(expr, env)
   }
   else if (is(expr, 'IfC')) {
      interpIf(expr)
   }
   else if (is(expr, 'BinopC')) {

   }
   else {
      print(class(expr))
      cat('looking up: ', expr@symbol, '\n')
      return(get(expr@symbol, env))
   }
}

interpBinop <- function(expr) {
   #if (is())
}

interpIf <- function(expr) {
   testVal <- interp(expr@test)

   if (is(testVal, "boolV")) {
      if (testVal@val) {
         return(interp(expr@thenBlock))
      }
      else {
         return(interp(expr@elseBlock))
      }
   }
   else {
      stop('InterpIf: test value is not a boolV')
   }
}

interpApp <- function(expr, env) {
   func <- interp(expr@func, env)
   #print(cat('env: ', names(func@env)))

   if (is(func, 'closV')) {
      params <- func@args
      allArgs <- expr@args
      funcBody <- func@body
      numParams <- length(params)
      print(cat('params: ', numParams, '\n'))

      for (index in 1:numParams) {
         print(cat('index', index, '\n'))
         print(allArgs[index])
         assign(params[index], interp(allArgs[index], func@env))
      }

      return(interp(funcBody, func@env))
   }
}

createIdC <- function(id, env) {
   #newEnv <- extendEnv(env, expr, )
}

extendEnv <- function(env, id, expr) {
   assign(id, expr, env)
}

lookup <- function(env, id) {
   cat('looking up', id)
   if (id %in% names(env)) {
      return(get(id, env))
   }
   else {
      stop('Could not find id in Env')
   }
}

#testEnv <- Env(values=list())
#e <- environment()
#assign('test', numV(num=-12), e)
#assign('meh', boolV(val=TRUE), e)
#interp(idC(symbol='test'), e)

#e2 <- environment()
#lam1 <- lamC(params=list('x', 'y'), body=trueC())
#args1 <- list(numC(num=12), numC(num=-3))
#interp(appC(func=lam1, args=args1), e2)