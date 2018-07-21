source('R/token_to_pmml.R')

getDerivedFieldNameOrFunctionNameForTokens <- function(tokens) {
  leftAssignToken <- tokens[which(tokens$token == LEFT_ASSIGN_TOKEN), ]
  if(nrow(leftAssignToken) > 1) {
    leftAssignToken <- leftAssignToken[1, ]
  }
  
  firstSymbol <- getFirstSymbolInExpr(getTokensWithParent(leftAssignToken$parent, tokens)[1, ], tokens)
  
  if(nrow(firstSymbol) == 0) {
    stop('derivedFieldName or functionName is unkown')
  } else {
    return(firstSymbol$text)
  }
}

getDerivedFieldPmmlStringForTokens <- function(tokens, derivedFieldName, addDerivedField=TRUE) {
  leftAssignToken <- tokens[which(tokens$token == LEFT_ASSIGN_TOKEN), ][1, ]
  
  tokenWithAssignmentCode <- getTokenAfterTokenWithId(tokens, leftAssignToken$id)
  
  transformationsPmmlString <- ''
  if(tokenWithAssignmentCode$token == EXPR_TOKEN) {
    transformationsPmmlString <- getPmmlStringForExpr(getTokenAfterTokenWithId(tokens, leftAssignToken$id), tokens)    
  } else if(tokenWithAssignmentCode$token == NUM_CONST_TOKEN | tokenWithAssignmentCode$token == STR_CONST_TOKEN | tokenWithAssignmentCode$token == NULL_CONST_TOKEN) {
    transformationsPmmlString <- getPmmlStringForConstant(tokenWithAssignmentCode)
  } else if(tokenWithAssignmentCode$token == SYMBOL_TOKEN) {
    transformationsPmmlString <- getPmmlStringForSymbol(tokenWithAssignmentCode)
  } else {
    stop(glue::glue('Unhandled token type {tokenWithAssignmentCode$token} for field {derivedFieldName}'))
  }
  
  if(addDerivedField) {
    return(glue::glue('<DerivedField name="{derivedFieldName}" optype="continuous">{transformationsPmmlString}</DerivedField>'))
  } else {
    return(transformationsPmmlString)
  }
}