source(file.path(getwd(), 'R', './tokens.R'))
source(file.path(getwd(), 'R', './token_to_pmml.R'))

getDerivedFieldNameOrFunctionNameForTokens <- function(tokens) {
  symbols <- getSymbolsInTokens(tokens)

  derivedFieldNameOrFunctionName <- 'unknown'

  for(i in 1:nrow(symbols)) {
    parentExprForCurrentSymbol <- getExprWithIdInTokens(symbols[i, 'parent'], tokens)

    tokensWithSameParentAsParentExprForCurrentSymbol <- getTokensWithParent(parentExprForCurrentSymbol$parent, tokens)

    if(doesTokensHaveALeftAssign(tokensWithSameParentAsParentExprForCurrentSymbol)) {
      derivedFieldNameOrFunctionName <- symbols[i, 'text']
      break;
    }
  }

  if(derivedFieldNameOrFunctionName == 'unknown') {
    stop('derivedFieldName or functionName is unkown')
  }

  return(derivedFieldNameOrFunctionName)
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

  childSpecialTokensForCurrentExpr <- getSpecialTokens(getChildTokensForParent(expr, tokens))
  if(nrow(childSpecialTokensForCurrentExpr) != 0) {
    if(childSpecialTokensForCurrentExpr[1, 'text'] == '%in%') {
      childExprTokens <- getExprTokens(getChildTokensForParent(expr, tokens))
      leftExprTokenPmmlString <- getPmmlStringForExpr(childExprTokens[1, ], tokens)
      rightExprTokenPmmlString <- getPmmlStringForExpr(childExprTokens[2, ], tokens)

      return(glue::glue('<Apply function="isIn">{leftExprTokenPmmlString}{rightExprTokenPmmlString}</Apply>'))
    }
    else {
      stop(glue::glue('Unhandled special symbol {childSpecialTokensForCurrentExpr[1, "text"]}'))
    }
  }
  else if(tokensWhoseParentIsTheCurrentExprHasOneRow & tokensWhoseParentIsTheCurrentExpr[1, ]$token == IF_TOKEN) {
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
        functionSymbolToken <- getTokensWithParent(exprTokensWhoseParentIsTheCurrentExpr[1, 'id'], tokens)[1, ]
        exprTokensWhoseParentIsTheCurrentExprAndAreFunctionArgs <- exprTokensWhoseParentIsTheCurrentExpr[-1, ]
        functionArgsSymbolTokensPmmlString <- ''
        for(i in 1:nrow(exprTokensWhoseParentIsTheCurrentExprAndAreFunctionArgs)) {
          functionArgsSymbolTokensPmmlString <- paste(functionArgsSymbolTokensPmmlString, getPmmlStringForExpr(exprTokensWhoseParentIsTheCurrentExprAndAreFunctionArgs[i, ], tokens), sep='')
        }

        # Handle c functions by taking the arguments to the functions and concating the pmml string for each argument
        if(functionSymbolToken$text == 'c') {
          return(functionArgsSymbolTokensPmmlString)
        } else {
          return(getPmmlStringForSymbolFunctionCall(functionSymbolToken, functionArgsSymbolTokensPmmlString))
        }
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
  derivedFieldName <- getDerivedFieldNameOrFunctionNameForTokens(tokens)
  symbolsWithDerivedFieldNameForText <- getAllSymbolsWithText(derivedFieldName, tokens)

  tokensToConvertToDerivedFieldPmml <- filterOutLeftAssignTokens(tokens)
  tokensToConvertToDerivedFieldPmml <- filterOutSymbolsWithText(derivedFieldName, tokensToConvertToDerivedFieldPmml)

  for(i in 1:nrow(symbolsWithDerivedFieldNameForText)) {
    tokensToConvertToDerivedFieldPmml <- filterOutTokenWithId(symbolsWithDerivedFieldNameForText[i, 'parent'], tokensToConvertToDerivedFieldPmml)
  }

  transformationPmmlString <- getPmmlStringForExpr(tokensToConvertToDerivedFieldPmml[1, ], tokensToConvertToDerivedFieldPmml)
  return(glue::glue('<DerivedField name="{derivedFieldName}">{transformationPmmlString}</DerivedField>'))
}

getRArgumentsIntoFunctionString <- function(originalFunctionArgTokens) {
  # When no arguments return an empty string
  if(nrow(originalFunctionArgTokens) == 0) {
    return('')
  }

  #The r string for the arguments into the function
  rArgumentsIntoFunctionString <- ''
  for(i in 1:nrow(originalFunctionArgTokens)) {
    rArgumentsIntoFunctionString <- paste(rArgumentsIntoFunctionString, originalFunctionArgTokens[i, 'text'], sep=',')
  }

  return(rArgumentsIntoFunctionString)
}

getDefineFunctionPmmlStringForTokens <- function(tokens) {
  functionName <- getDerivedFieldNameOrFunctionNameForTokens(tokens)

  functionTokens = getFunctionTokens(tokens)
  if(nrow(functionTokens) > 1) {
    stop('Too many function tokens found within function definition')
  }

  functionTokenParentExprId = functionTokens[1, ]$parent

  functionExprToken = getTokenWithId(functionTokenParentExprId, tokens)

  functionDefinitionTokens = getDescendantsOfToken(functionExprToken, tokens)

  #The names of the functions arguments
  functionArgNameTokens <- getSymbolFormalsTokens(tokens)

  #Expression token for the body of the function
  functionBodyExprToken <- getExprTokens(getChildTokensForParent(functionExprToken, tokens))[1, ]

  #Get all the tokens which together make up the function body
  functionBodyExprTokenDescendants = getDescendantsOfToken(functionBodyExprToken, tokens)

  #Get the top level expr tokens for the function body. These are the tokens which hold all the logic in the function body as well as the return call
  topLevelFunctionBodyExprTokens <- getExprTokens(getChildTokensForParent(functionBodyExprToken, tokens))

  pmmlFunctionString <- ''

  for(i in 1:nrow(topLevelFunctionBodyExprTokens)) {
    if(i != nrow(topLevelFunctionBodyExprTokens)) {
      pmmlFunctionString <- paste(pmmlFunctionString, getPmmlStringForExprTokenWithinFunction(topLevelFunctionBodyExprTokens[i, ], functionArgNameTokens, functionName, tokens), sep='')
    }
    #It's the last expression so it has to be a function return call
    else {
      #Get the expression for the argument to the return function call
      returnArgExprToken <- getExprTokens(getChildTokensForParent(topLevelFunctionBodyExprTokens[i, ], tokens))[2, ]

      #Convert the expression to it's PMML string
      pmmlStringForReturnArgExprToken <- getPmmlStringForExpr(returnArgExprToken, tokens)

      #Find all the symbols used within the expression which are not part of the function arguments
      symbolsWithinReturnArgExprWhichAreNotFunctionArguments <- getSymbolsInTokens(getDescendantsOfToken(returnArgExprToken, tokens))
      symbolsWithinReturnArgExprWhichAreNotFunctionArguments <- subset(
        symbolsWithinReturnArgExprWhichAreNotFunctionArguments,
        !(symbolsWithinReturnArgExprWhichAreNotFunctionArguments$text %in% functionArgNameTokens$text)
      )

      #1. Convert each symbol into R code which calls a function whose name is a combination of the symbol and original function name and whose arguments are the
      # original function arguments and generate PMML code for it
      #2. Replace each FieldRef within the above PMML string for each symbol with the generated PMML string for that function call R code
      for(j in 1:nrow(symbolsWithinReturnArgExprWhichAreNotFunctionArguments)) {
        # 1.
        rFunctionNameForCurrentSymbol <- glue::glue('{functionName}_{symbolsWithinReturnArgExprWhichAreNotFunctionArguments[j, "text"]}')
        rFunctionArgs <- getRArgumentsIntoFunctionString(functionArgNameTokens)
        rCode <- glue::glue('{rFunctionNameForCurrentSymbol}({rFunctionArgs})')
        tokensForRCode <- getParseData(parse(text = rCode))
        pmmlStringForRCode <- getPmmlStringForExpr(tokensForRCode[1, ], tokensForRCode)
        pmmlStringForRCode <- gsub(
          rFunctionNameForCurrentSymbol,
          getFunctionNameForInnerFunctionExprToken(functionName, symbolsWithinReturnArgExprWhichAreNotFunctionArguments[j, 'text']),
          pmmlStringForRCode
        )

        # 2.
        pmmlStringForReturnArgExprToken <- gsub(
          glue::glue('<FieldRef field="{symbolsWithinReturnArgExprWhichAreNotFunctionArguments[j, ]$text}"/>'),
          pmmlStringForRCode,
          pmmlStringForReturnArgExprToken
        )
      }

      #Make the DefineFunction PMML string
      pmmlDefineFunctionString <- getPmmlStringForDefineFunction(functionName, functionArgNameTokens, pmmlStringForReturnArgExprToken)

      #Add it to the pmmlFunctionString
      pmmlFunctionString <- paste(pmmlFunctionString, pmmlDefineFunctionString, sep='')
    }
  }

  return(pmmlFunctionString)
}

getFunctionNameForInnerFunctionExprToken <- function(originalFunctionName, variableName) {
  #Make the name of the function using the variableName and the orignal function name which is
  functionName <- glue::glue('{originalFunctionName}({variableName})')

  return(functionName)
}

getPmmlStringForExprTokenWithinFunction <- function(innerFunctionExprToken, originalFunctionArgTokens, originalFunctionName, tokens) {
  #Get the name of the variable this expression is being used to initialize. This will be used for the name of the function
  variableName <- getDerivedFieldNameOrFunctionNameForTokens(getDescendantsOfToken(innerFunctionExprToken, tokens))
  functionName <- getFunctionNameForInnerFunctionExprToken(originalFunctionName, variableName)

  #Get the expression token which has the initialization code
  initializationExprToken <- getChildTokensForParent(innerFunctionExprToken, tokens)[3, ]

  #Convert the expression which has the initialization code into it's PMML string
  pmmlStringForInitializationExprToken <- getPmmlStringForExpr(initializationExprToken, tokens)

  originalFunctionArgNames <- originalFunctionArgTokens$text
  #Get all the symbols within this expression which are not part of the original function arguments. These need to be converted to Function calls in the PMML string for this expression
  symbolsNotPartOfOriginalFunctionArgs <- getSymbolsInTokens(getDescendantsOfToken(initializationExprToken, tokens))
  symbolsNotPartOfOriginalFunctionArgs <- subset(symbolsNotPartOfOriginalFunctionArgs, !(symbolsNotPartOfOriginalFunctionArgs$text %in% originalFunctionArgNames))

  if(nrow(symbolsNotPartOfOriginalFunctionArgs) != 0) {
    #For every symbol not part of the original function argument we
    #1. Make a string that has R code for the a function call with the orignal function arguments and the function name for this symbol we defined
    #2. Generate a PMML string for the function call code
    #3. Replace every FieldRef for this symbol with the function call PMML string
    for(i in 1:nrow(symbolsNotPartOfOriginalFunctionArgs)) {
      #The r string for the arguments into the function
      rArgumentsIntoFunctionString <- getRArgumentsIntoFunctionString(originalFunctionArgTokens)

      symbolName <- symbolsNotPartOfOriginalFunctionArgs[i, 'text']

      rFunctionName <- glue::glue('{originalFunctionName}_{symbolsNotPartOfOriginalFunctionArgs[i, "text"]}')
      rFunctionCallStringForCurrentSymbol <- glue::glue('{rFunctionName}({rArgumentsIntoFunctionString})')
      tokensForRFunctionCallString <- getParseData(parse(text = rFunctionCallStringForCurrentSymbol))

      pmmlStringForFunctionCallForCurrentSymbol <- getPmmlStringForExpr(tokensForRFunctionCallString[1, ], tokensForRFunctionCallString)

      #Replace the function name with the actual one. We used the above one since R would have a problem with the one we use
      pmmlStringForFunctionCallForCurrentSymbol <- gsub(rFunctionName, getFunctionNameForInnerFunctionExprToken(originalFunctionName, symbolName), pmmlStringForFunctionCallForCurrentSymbol)

      pmmlStringForInitializationExprToken <- gsub(glue::glue('<FieldRef field="{symbolName}"/>'), pmmlStringForFunctionCallForCurrentSymbol, pmmlStringForInitializationExprToken)
    }
  }

  defineFunctionPmmlString <- getPmmlStringForDefineFunction(functionName, originalFunctionArgTokens, pmmlStringForInitializationExprToken)

  #Return the final DefineFunction PMML string
  return(defineFunctionPmmlString)
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
  tokensWithComments = getParseData(parse(file = filePath))
  tokens <- filterOutCommentTokens(tokensWithComments)

  nextZeroParentIndex = getIndexOfNextZeroParent(tokens)

  localTransformationString <- ''

  while(nextZeroParentIndex != 0) {
    tokensForCurrentParentIndex = tokens[1:nextZeroParentIndex, ]

    if(doesTokensHaveSourceFunctionCall(tokensForCurrentParentIndex) == TRUE) {
      localTransformationString <- paste(localTransformationString, getPmmlStringFromSouceFunctionCallTokens(tokensForCurrentParentIndex), sep='')
    } else if(doesTokensHaveFunctionDefinition(tokensForCurrentParentIndex) == TRUE) {
      localTransformationString <- paste(localTransformationString, getDefineFunctionPmmlStringForTokens(tokens), sep='')
    } else {
      localTransformationString <- paste(localTransformationString, getDerivedFieldPmmlStringForTokens(tokensForCurrentParentIndex), sep='')
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
