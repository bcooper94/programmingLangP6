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

fdC <- setClass("FdC",
slots = c(params = "list",
		body = "ExprC")
prototype = list(),
contains = "ExprC")

interp = function(expr) {
   if (is(expr, 'NumC')) {
      #return(expr@num)
      return(expr@num)
   }
   else if (is(expr, 'TrueC')) {
      print('TrueC')
   }
   else if (is(expr, 'FalseC')) {
      print('FalseC')
   }
   else if (is(expr, 'LambdaC')) {
      print('LambdaC')
   }
   else if (is(expr, 'AppC')) {

   }
   else if (is(expr, 'IfC')) {

   }
   else if (is(expr, 'BinopC')) {

   }
   else {
      # create IdC
      print('IdC')
   }
}

interpBinop = function(expr) {
   #if (is())
}
