library(testthat)

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

binopC <- setClass("BinopC",
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

lamC <- setClass("lamC",
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

isBinop <- function(input){
	return(input == '+' || input == '-' || input == '*' || input == '/'
		|| input == 'eq?' || input == '<=')
}

parseArgs <- function(argsList, start){
	tempArgs = list()
	for(i in start:length(argsList)){
		tempArgs <- c(tempArgs, parse(unlist(argsList[i], recursive = FALSE)))
	}
	return(tempArgs)
}

parse <- function(input){
   if(is(input, 'numeric')){
      return(numC(num=input))
   }
   else if(is(input, 'logical')){
   		if(input){
   			return(trueC())
   		}
   		else{
   			return(falseC())
   		}
   }
   else if(is(input, 'character')){
   		return(idC(symbol=input))
   }
   else if(is(input, 'list')){
   		if(!is(unlist(input[1], recursive = FALSE), "list") && isBinop(unlist(input[1], recursive = FALSE))){
   			return(binopC(symbol=unlist(input[1], recursive = FALSE), left=parse(unlist(input[2], recursive = FALSE )),
   			 right=parse(unlist(input[3], recursive = FALSE))))
   		}
   		else if(input[1] == 'if'){
   			return(ifC(test=parse(unlist(input[2], recursive = FALSE)), thenBlock=parse(unlist(input[3], recursive = FALSE)),
   			 elseBlock=parse(unlist(input[4], recursive = FALSE))))
   		}
   		else if(input[1] == 'func'){
   			tempArgs = parseArgs(unlist(input[2], recursive=FALSE), 1)
   			return(lamC(params=tempArgs, body=parse(unlist(input[length(input)], recursive=FALSE))))
   		}
   		else{
   			tempArgs = parseArgs(input, 2)
   			return(appC(fun=parse(unlist(input[1], recursive = FALSE)), args=tempArgs))
   		}
   }
}


#print(parse(list('if', TRUE, 3, 4)))
#print(isBinop('+'))
#print(parse(list('+', 3, 4)))
#print(parse(list (list("+", 'x', 'y'), 4, 5)))
#print(parse(list('func', list('x', 'y'), list('+', 2, 3))))
#print('TEST 6')
#print(parse(list('func', list('x', 'y'), list('+', list('-', 6, 1), 3))))

expect_that(parse(10), is_a("NumC"))
expect_that(parse(TRUE), is_a("NumC"))
