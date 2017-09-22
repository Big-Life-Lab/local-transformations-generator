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

FUNCTION_TOKEN <- 'FUNCTION'
SYMBOL_FORMALS_TOKEN <- 'SYMBOL_FORMALS'

getExprWithIdInTokens <- function(id, tokens) {
  return(tokens[which(tokens$token==EXPR_TOKEN & tokens$id == id), ])
}

getSymbolsInTokens <- function(tokens) {
  return(tokens[which(tokens$token==SYMBOL_TOKEN), ])
}

getSymbolFormalsTokens <- function(tokens) {
  return(tokens[which(tokens$token==SYMBOL_FORMALS_TOKEN), ])
}

getTokensWithParent <- function(parent, tokens) {
  return(tokens[which(tokens$parent==parent), ])
}

getTokenWithId <- function(id, tokens) {
  return(tokens[which(tokens$id == id), ])
}

getFunctionTokens <- function(tokens) {
  return(tokens[which(tokens$token == FUNCTION_TOKEN), ])
}

getChildTokensForParent <- function(parent, tokens) {
  return(tokens[which(tokens$parent == parent$id), ])
}

#Returns the token which is the parent of the child token argument
getParentToken <- function(childNode, nodes) {
  return(nodes[which(nodes$id == childNode$parent), ][1, ])
}

#Checks if the node arg is a descendant of the node with id provided in the id arg from the nodes arg
isDescendantOfTokenWithId <- function(id, node, nodes) {
  #If this is the root node return false because we have reached the beginning of the tree
  if(node$parent == 0) {
    return(FALSE)
  }
  #if this is a direct child of the parent return true
  else if(node$parent == id) {
    return(TRUE)
  }
  #Otherwise
  else {
    #Get the parent of the node
    parentNode <- getParentToken(node, nodes)
    #If the parent does not exist
    if(is.na(parentNode$id) == TRUE) {
      return(FALSE)
    }

    #Check if th parent is a descendant
    return(isDescendantOfTokenWithId(id, parentNode, nodes))
  }
}

#Returns all the descendants of the node arg from the nodes arg
getDescendantsOfToken <- function(node, nodes) {
  #The id fields of all the nodes which are descendants
  descendantIds <- c()

  #Go thorugh all the nodes and each one which is a descendant add it's id to the descendantIds vector
  for(i in 1:nrow(nodes)) {
    if(isDescendantOfTokenWithId(node$id, nodes[i, ], nodes)) {
      descendantIds <- c(descendantIds, nodes[i, 'id'])
    }
  }

  #Returns all nodes whose id field is part of the descendantIds vector
  return(nodes[which(nodes$id %in% descendantIds), ])
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

doesTokensHaveRowWithToken <- function(tokens, token) {
  if(nrow(tokens[which(tokens$token==token), ]) == 0) {
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}

doesTokensHaveALeftAssign <- function(tokens) {
  return(doesTokensHaveRowWithToken(tokens, LEFT_ASSIGN_TOKEN))
}

doesTokensHaveSourceFunctionCall <- function(tokens) {
  return(nrow(getSymbolFunctionCallsWithText('source', tokens)) == 1)
}

doesTokensHaveFunctionDefinition <- function(tokens) {
  return(doesTokensHaveRowWithToken(tokens, FUNCTION_TOKEN))
}

isLeftAssignExprToken <- function(exprToken, tokens) {
  childTokensForExprToken = getChildTokensForParent(exprToken, tokens)

  return(doesTokensHaveALeftAssign(childTokensForExprToken))
}

#Is the symbolToken argument part of a left assign expression and is it the symbol to which a value is being assigned
#eg. test <- 1. Here if the test symbol token was passed as the symbolToken argument the function would return true
isLeftAssignmentSymbolToken <- function(symbolToken, tokens) {
  parentTokenForSymbolToken = getParentToken(symbolToken, tokens)

  if(isLeftAssignExprToken(parentTokenForSymbolToken, tokens)) {
    childTokensForLeftAssignExprToken <- getChildTokensForParent(parentTokenForSymbolToken, tokens)
    return(childTokensForLeftAssignExprToken[1, 'id'] == symbolToken$id)
  } else {
    return(FALSE)
  }
}
