LEFT_ASSIGN_TOKEN <- 'LEFT_ASSIGN'
EXPR_TOKEN <- 'expr'
SYMBOL_TOKEN <- 'SYMBOL'
NUM_CONST_TOKEN <- 'NUM_CONST'
STR_CONST_TOKEN <- 'STR_CONST'
MATH_TOKENS <- c("'+'", "'-'", "'*'", "'/'")

EQUAL_TO_TOKEN <- 'EQ'
LESS_THAN_OR_EQUAL_TO_TOKEN <- 'LE'
GREATER_THAN_OR_EQUAL_TO_TOKEN <- 'GE'
GREATER_THAN_TOKEN <- 'GT'
LESS_THAN_TOKEN <- 'LT'
AND_TOKEN <- 'AND'
OR_TOKEN <- 'OR'
NOT_EQUAL_TO_TOKEN <- 'NE'
NOT_TOKEN <- "'!'"
LOGICAL_TOKENS <- c(EQUAL_TO_TOKEN, LESS_THAN_OR_EQUAL_TO_TOKEN, GREATER_THAN_OR_EQUAL_TO_TOKEN, GREATER_THAN_TOKEN, LESS_THAN_TOKEN, AND_TOKEN, OR_TOKEN, NOT_EQUAL_TO_TOKEN, NOT_TOKEN)

IF_TOKEN <- 'IF'
SYMBOL_FUNCTION_CALL_TOKEN <- 'SYMBOL_FUNCTION_CALL'

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

getSymbolFunctionCallsWithText <- function(text, tokens) {
  return(tokens[which(tokens$token == SYMBOL_FUNCTION_CALL_TOKEN & tokens$text == text), ])
}

doesTokensHaveSourceFunctionCall <- function(tokens) {
  return(nrow(getSymbolFunctionCallsWithText('source', tokens)) == 1)
}
