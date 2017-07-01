LEFT_ASSIGN_TOKEN = 'LEFT_ASSIGN'
EXPR_TOKEN = 'expr'
SYMBOL_TOKEN = 'SYMBOL'
NUM_CONST_TOKEN = 'NUM_CONST'
MATH_TOKENS = c("'+'")
LOGICAL_TOKENS = c('EQ')
IF_TOKEN = 'IF'
SYMBOL_FUNCTION_CALL_TOKEN = 'SYMBOL_FUNCTION_CALL'

doesTokensHaveALeftAssign <- function(tokens) {
  if(nrow(tokens[which(tokens$token==LEFT_ASSIGN_TOKEN), ]) == 0) {
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}

getExprWithIdInTokens <- function(id, tokens) {
  return(tokens[which(tokens$token==EXPR_TOKEN & tokens$id == id), ])
}

getSymbolsInTokens <- function(tokens) {
  return(tokens[which(tokens$token==SYMBOL_TOKEN), ])
}

getTokensWithParent <- function(parent, tokens) {
  return(tokens[which(tokens$parent==parent), ])
}

filterOutLeftAssignTokens <- function(tokens) {
  return(tokens[which(tokens$token!=LEFT_ASSIGN_TOKEN), ])
}

filterOutSymbolsWithText <- function(text, tokens) {
  return(tokens[which(!(tokens$text==text & tokens$token==SYMBOL_TOKEN)), ])
}

filterOutTokenWithId <- function(id, tokens) {
  return(tokens[which(tokens$id!=id), ])
}

getAllSymbolsWithText <- function(text, tokens) {
  return(tokens[which(tokens$text==text & tokens$token==SYMBOL_TOKEN), ])
}

getExprTokens <- function(tokens) {
  return(tokens[which(tokens$token==EXPR_TOKEN), ])
}

filterOutExprTokens <- function(tokens) {
  return(tokens[which(tokens$token != EXPR_TOKEN), ])
}
