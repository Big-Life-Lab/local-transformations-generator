source('R/tokens.R')
source('R/util.R')

isIfExpr <- function(tokens) {
  return(tokens[2, 'token'] == 'IF')
}

getPmmlStringForIfExpr <- function(expr, tokens, returnFinalPmmlString=TRUE) {
  derivedFieldsSet <- data.frame();
  # The index of the dataframe is the derived field name
  # conditionExprIds: String which has the ids of the expr that holds the condition code which when true the corresponsing true code sets this derived field. Each expr id is seperated by a comma
  # exprBlockIds: String which has the ids of the expr tokens which is run when the corresponsing condition expr is set to true. Each expr id is seperated by a comma and matches with one from the conditionExprIds
  
  childrenForRootExp <- getChildTokensForParent(expr, tokens)
  
  # The expr tokens which are run when the condition for this if statement evaluates to true
  whenConditionTrueExprs <- getExprTokens(getChildTokensForParent(childrenForRootExp[5, ], tokens))
  if(childrenForRootExp[1, 'text'] == '{') {
    whenConditionTrueExprs <- getExprTokens(childrenForRootExp)  
  }
  
  for(i in 1:nrow(whenConditionTrueExprs)) {
    derivedFieldName <- getDerivedFieldNameOrFunctionNameForTokens(getDescendantsOfToken(whenConditionTrueExprs[i, ], tokens))
  
    derivedFieldsSet[derivedFieldName, 'conditionExprId'] <- childrenForRootExp[3, 'id']
    derivedFieldsSet[derivedFieldName, 'exprBlockId'] <- whenConditionTrueExprs[i, 'id']
  }
  
  elsePmmlStrings <- NA
  if(!is.na(childrenForRootExp[6, ]) & childrenForRootExp[6, 'token'] == 'ELSE') {
    elsePmmlStrings <- getPmmlStringForIfExpr(childrenForRootExp[7, ], tokens, FALSE)
  }
  
  derivedFieldNameWithPmmlString <- data.frame()
  for(derivedFieldName in row.names(derivedFieldsSet)) {
    conditionPmmlString <- getPmmlStringForExpr(getExprWithIdInTokens(derivedFieldsSet[derivedFieldName, 'conditionExprId'], tokens), tokens)
    whenTruePmmlString <- getDerivedFieldPmmlStringForTokens(getDescendantsOfToken(getExprWithIdInTokens(derivedFieldsSet[derivedFieldName, 'exprBlockId'], tokens), tokens), derivedFieldName, FALSE)
    whenFalsePmmlString <- glue::glue('<FieldRef field="{derivedFieldName}"/>')
    if(!(is.na(elsePmmlStrings) | (derivedFieldName %in% row.names(elsePmmlStrings)) == FALSE)) {
      whenFalsePmmlString <- elsePmmlStrings[derivedFieldName, 'pmmlString']
    }
    
    if(conditionPmmlString != '') {
      derivedFieldNameWithPmmlString[derivedFieldName, 'pmmlString'] <- glue::glue('<Apply function="if">{conditionPmmlString}{whenTruePmmlString}{whenFalsePmmlString}</Apply>')
    } else {
      derivedFieldNameWithPmmlString[derivedFieldName, 'pmmlString'] <- whenTruePmmlString
    }
  }
  
  if(!is.na(elsePmmlStrings)) {
    for(derivedFieldName in row.names(elsePmmlStrings)) {
      if(!derivedFieldName %in% row.names(derivedFieldNameWithPmmlString)) {
        derivedFieldNameWithPmmlString[derivedFieldName, 'pmmlString'] <- elsePmmlStrings[derivedFieldName, 'pmmlString']
      }
    }
  }

  
  if(!returnFinalPmmlString) {
    return(derivedFieldNameWithPmmlString)
  } else {
    pmmlString <- ''
    
    for(derivedFieldName in row.names(derivedFieldNameWithPmmlString)) {
      pmmlString <- glue::glue('{pmmlString}<DerivedField name="{derivedFieldName}">{derivedFieldNameWithPmmlString[derivedFieldName, "pmmlString"]}</DerivedField>')
    }
    
    return(pmmlString)
  }
}