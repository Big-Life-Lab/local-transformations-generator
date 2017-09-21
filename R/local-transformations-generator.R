source(file.path(getwd(), 'R', './tokens.R'))
source(file.path(getwd(), 'R', './token_to_pmml.R'))

getDerivedFieldNameForTokens <- function(tokens) {
  symbols <- getSymbolsInTokens(tokens)

  derivedFieldName <- 'unknown'

  for(i in 1:nrow(symbols)) {
    parentExprForCurrentSymbol <- getExprWithIdInTokens(symbols[i, 'parent'], tokens)

    tokensWithSameParentAsParentExprForCurrentSymbol <- getTokensWithParent(parentExprForCurrentSymbol$parent, tokens)

    if(doesTokensHaveALeftAssign(tokensWithSameParentAsParentExprForCurrentSymbol)) {
      derivedFieldName <- symbols[i, 'text']
      break;
    }
  }

  if(derivedFieldName == 'unknown') {
    stop('derivedFieldName is unkown')
  }

  return(derivedFieldName)
}

getPmmlStringForIfToken <- function(conditionExpr, trueResultExpr, falseResultExpr, tokens) {
  pmmlStringForTrueExpr <- getPmmlStringForExpr(trueResultExpr, tokens)
  pmmlStringForFalseExpr <- getPmmlStringForExpr(falseResultExpr, tokens)
  pmmlStringForCondition <- getPmmlStringForExpr(conditionExpr, tokens)

  return(glue::glue('<Apply function="if">{pmmlStringForCondition}{pmmlStringForTrueExpr}{pmmlStringForFalseExpr}</Apply>'))
}

isSymbolFunctionCallExpr <- function(exprToken, tokens) {
  exprTokensWhoseParentIsTheCurrentToken <- getExprTokens(getTokensWithParent(exprToken$id, tokens))

  for(i in 1:nrow(exprTokensWhoseParentIsTheCurrentToken)) {
    if(getTokensWithParent(exprTokensWhoseParentIsTheCurrentToken[1, 'id'], tokens)[1, 'token'] == SYMBOL_FUNCTION_CALL_TOKEN) {
      return(TRUE)
    }
  }

  return(FALSE);
}

getPmmlStringForExpr <- function(expr, tokens) {
  tokensWhoseParentIsTheCurrentExpr = getTokensWithParent(expr$id, tokens)

  tokensWhoseParentIsTheCurrentExprHasOneRow <- (nrow(tokensWhoseParentIsTheCurrentExpr) != 0)

  if(tokensWhoseParentIsTheCurrentExprHasOneRow & tokensWhoseParentIsTheCurrentExpr[1, ]$token == IF_TOKEN) {
    conditionExpr <- tokensWhoseParentIsTheCurrentExpr[3, ]
    trueResultExpr <- tokensWhoseParentIsTheCurrentExpr[5, ]
    falseResultExpr <- tokensWhoseParentIsTheCurrentExpr[7, ]

    return(getPmmlStringForIfToken(conditionExpr, trueResultExpr, falseResultExpr, tokens))
  } else {
    exprTokensWhoseParentIsTheCurrentExpr = getExprTokens(tokensWhoseParentIsTheCurrentExpr)
    nonExprTokensWhoseParentIsTheCurrentExpr = filterOutExprTokens(tokensWhoseParentIsTheCurrentExpr)
    pmmlStringForExprTokens <- ''

    if(nrow(exprTokensWhoseParentIsTheCurrentExpr) != 0) {
      if(isSymbolFunctionCallExpr(expr, tokens)) {
        functionArgsSymbolTokensPmmlString <- ''
        functionSymbolToken <- getTokensWithParent(exprTokensWhoseParentIsTheCurrentExpr[1, 'id'], tokens)[1, ]
        exprTokensWhoseParentIsTheCurrentExprAndAreFunctionArgs <- exprTokensWhoseParentIsTheCurrentExpr[-1, ]
        for(i in 1:nrow(exprTokensWhoseParentIsTheCurrentExprAndAreFunctionArgs)) {
          functionArgsSymbolTokensPmmlString <- paste(functionArgsSymbolTokensPmmlString, getPmmlStringForExpr(exprTokensWhoseParentIsTheCurrentExprAndAreFunctionArgs[i, ], tokens), sep='')
        }

        return(getPmmlStringForSymbolFunctionCall(functionSymbolToken, functionArgsSymbolTokensPmmlString))
      } else {
        for(i in 1:nrow(exprTokensWhoseParentIsTheCurrentExpr)) {
          pmmlStringForExprTokens <- paste(pmmlStringForExprTokens, getPmmlStringForExpr(exprTokensWhoseParentIsTheCurrentExpr[i, ], tokens), sep='')
        }
      }
    }

    if(nrow(nonExprTokensWhoseParentIsTheCurrentExpr) == 0) {
      return(pmmlStringForExprTokens)
    }

    nonExprToken = nonExprTokensWhoseParentIsTheCurrentExpr[1, ]
    nonExprTokenToken = nonExprToken$token

    if(nonExprTokenToken == SYMBOL_TOKEN) {
      return(getPmmlStringForSymbol(nonExprToken))
    } else if(nonExprTokenToken == NUM_CONST_TOKEN || nonExprTokenToken == STR_CONST_TOKEN) {
      return(getPmmlStringForConstant(nonExprToken))
    } else if(nonExprTokenToken %in% MATH_TOKENS) {
      return(getPmmlStringForMathToken(nonExprToken, pmmlStringForExprTokens))
    } else if(nonExprTokenToken %in% LOGICAL_TOKENS) {
      return(getPmmlStringForLogicalOperator(nonExprToken, pmmlStringForExprTokens))
    } else {
      return(pmmlStringForExprTokens)
    }
  }
}

getDerivedFieldPmmlStringForTokens <- function(tokens) {
  derivedFieldName <- getDerivedFieldNameForTokens(tokens)
  symbolsWithDerivedFieldNameForText <- getAllSymbolsWithText(derivedFieldName, tokens)

  tokensToConvertToDerivedFieldPmml <- filterOutLeftAssignTokens(tokens)
  tokensToConvertToDerivedFieldPmml <- filterOutSymbolsWithText(derivedFieldName, tokensToConvertToDerivedFieldPmml)

  for(i in 1:nrow(symbolsWithDerivedFieldNameForText)) {
    tokensToConvertToDerivedFieldPmml <- filterOutTokenWithId(symbolsWithDerivedFieldNameForText[i, 'parent'], tokensToConvertToDerivedFieldPmml)
  }

  transformationPmmlString <- getPmmlStringForExpr(tokensToConvertToDerivedFieldPmml[1, ], tokensToConvertToDerivedFieldPmml)
  return(glue::glue('<DerivedField name="{derivedFieldName}">{transformationPmmlString}</DerivedField>'))
}

# Get the index of the not the first but the second row in the parseData array which has the parent field set to 0
getIndexOfNextZeroParent <- function(parseData) {
  numZeroParents = 0

  for(i in 1:nrow(parseData)) {
    if(parseData[i,'parent'] == 0) {
      if(numZeroParents == 1) {
        return(i)
      }
      else {
        numZeroParents = numZeroParents + 1
      }
    }
  }

  return(nrow(parseData))
}

getPmmlStringFromSouceFunctionCallTokens <- function(sourceFunctionCallTokens) {
  sourceFunctionCallArgExprToken <- getTokensWithParent(sourceFunctionCallTokens[1, ]$id, sourceFunctionCallTokens)[3, ]
  sourceFunctionCallArgCodeString <- getParseText(sourceFunctionCallTokens, sourceFunctionCallArgExprToken$id)
  sourceFilePath <- eval(parse(text=sourceFunctionCallArgCodeString))

  return(getPmmlStringFromRFile(sourceFilePath))
}

getPmmlStringFromRFile <- function(filePath, srcFile=FALSE) {
  tokens = getParseData(parse(file = filePath))
  nextZeroParentIndex = getIndexOfNextZeroParent(tokens)

  localTransformationString <- ''

  while(nextZeroParentIndex != 0) {
    tokensForCurrentParentIndex = tokens[1:nextZeroParentIndex, ]

    if(doesTokensHaveSourceFunctionCall(tokensForCurrentParentIndex) == FALSE) {
      localTransformationString <- paste(localTransformationString, getDerivedFieldPmmlStringForTokens(tokensForCurrentParentIndex), sep='')
    } else {
      localTransformationString <- paste(localTransformationString, getPmmlStringFromSouceFunctionCallTokens(tokensForCurrentParentIndex), sep='')
    }

    if(nextZeroParentIndex == nrow(tokens)) {
      break
    }

    tokens = tokens[nextZeroParentIndex:nrow(tokens), ]

    nextZeroParentIndex = getIndexOfNextZeroParent(tokens)
  }

  if(srcFile == TRUE) {
    return(paste('<LocalTransformations>', localTransformationString, '</LocalTransformations>'))
  } else {
    return(localTransformationString)
  }
}
