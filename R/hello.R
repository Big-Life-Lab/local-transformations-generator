# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

library('pryr')
source(file.path(getwd(), 'R', './tokens.R'))

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

getPmmlStringForSymbol <- function(symbol) {
  fieldRefName <- symbol$text

  return(glue::glue('<FieldRef field="{fieldRefName}"/>'))
}

getPmmlStringForConstant <- function(constant) {
  dataType <- 'double'
  value <- constant$text

  if(typeof(constant) == 'character') {
    dataType <- 'string'
  }

  return(glue::glue('<Constant dataType="{dataType}">{value}</Constant>'))
}

getPmmlStringForLogicalOperator <- function(logicalToken, nestedPmmlString) {
  functionType <- 'equal'

  return(glue::glue('<Apply function="{functionType}">{nestedPmmlString}</Apply>'))
}

getPmmlStringForMathToken <- function(mathToken, nestedPmmlString) {
  functionType <- gsub("'", "", mathToken$token)

  return(glue::glue('<Apply function="{functionType}">{nestedPmmlString}</Apply>'))
}

getPmmlStringForSymbolFunctionCall <- function(symbolFunctionCallToken, nestedPmmlString) {
  functionType <- symbolFunctionCallToken$text

  return(glue::glue('<Apply function="{functionType}">{nestedPmmlString}</Apply>'))
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
          functionArgsSymbolTokensPmmlString <- paste(functionArgsSymbolTokensPmmlString, getPmmlStringForExpr(exprTokensWhoseParentIsTheCurrentExprAndAreFunctionArgs[1, ], tokens))
        }

        return(getPmmlStringForSymbolFunctionCall(functionSymbolToken, functionArgsSymbolTokensPmmlString))
      } else {
        for(i in 1:nrow(exprTokensWhoseParentIsTheCurrentExpr)) {
          pmmlStringForExprTokens <- paste(pmmlStringForExprTokens, getPmmlStringForExpr(exprTokensWhoseParentIsTheCurrentExpr[i, ], tokens))
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
    } else if(nonExprTokenToken == NUM_CONST_TOKEN) {
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

getPmmlStringFromRFile <- function(filePath) {
  tokens = getParseData(parse(file = filePath))
  print(tokens)
  nextZeroParentIndex = getIndexOfNextZeroParent(tokens)

  localTransformationString <- ''

  while(nextZeroParentIndex != 0) {
    localTransformationString <- paste(localTransformationString, getDerivedFieldPmmlStringForTokens(tokens[0:nextZeroParentIndex+1, ]))

    if(nextZeroParentIndex == nrow(tokens)) {
      break
    }

    tokens = tokens[nextZeroParentIndex:nrow(tokens), ]

    nextZeroParentIndex = getIndexOfNextZeroParent(tokens)
  }

  return(localTransformationString)
}

cat(getPmmlStringFromRFile(file.path(getwd(), 'R', 'test_math.R')), file="out.xml")
