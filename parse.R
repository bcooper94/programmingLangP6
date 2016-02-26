exprC <- setClass(
"ExprC",
slots = character(0),
prototype=list())

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
slots = c(fun = "ExprC",
		args = "list"),
prototype = list(),
contains = "ExprC")



idC <- setClass("IdC",
slots = c(symbol = "character"),
prototype = list(),
contains = "ExprC")

fdC <- setClass("FdC",
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
slots = c(args = "list", body = "ExprC"),
prototype=list(),
contains = "Value")

