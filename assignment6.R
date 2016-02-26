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
		elseBlock = "ExprC")
prototype = list(),
contains = "ExprC")

appC <- setClass("AppC",
slots = c(function = "ExprC",
		args = "list")
prototype = list(),
contains = "ExprC")

idC <- setClass("IdC",
slots = c(symbol = "character"),
prototype = list(),
contains = "ExprC")

lamC <- setClass("LamC",
slots = c(params = "list",
		body = "ExprC")
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
slots = c(args = "list", body = "ExprC"),
prototype=list(),
contains = "Value")

interp <- function(expr) {
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
      print('LamC')
   }
   else if (is(expr, 'AppC')) {

   }
   else if (is(expr, 'IfC')) {
      interpIf(expr)
   }
   else if (is(expr, 'BinopC')) {

   }
   else {
      # create IdC
      print('IdC')
   }
}

interpBinop <- function(expr) {
   #if (is())
}

interpIf <- function(expr) {
   testVal <- interp(expr@test)

   if (is(testVal, "boolV")) {
      if (testVal@val) {
         return(interp(thenBlock))
      }
      else {
         return(interp(elseBlock))
      }
   }
   else {
      stop('InterpIf: test value is not a boolV')
   }
}

