source(file.path(getwd(), 'R', './tokens.R'))
source(file.path(getwd(), 'R', './token_to_pmml.R'))

isDataFrameShortAccessExpr <- function(exprToCheck, tokens) {
  childTokens <- getChildTokensForParent(exprToCheck, tokens)
  
  if(nrow(childTokens) > 2 & childTokens[2, 'text'] == '[' & childTokens[nrow(childTokens), 'text'] == ']') {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

getFirstSymbolInExpr <- function(expr, tokens) {
  firstSymbol <- NA
  childTokenForExpr <- getChildTokensForParent(expr, tokens)
  
  for(i in 1:nrow(childTokenForExpr)) {
    if(isSymbolToken(childTokenForExpr[i, ])) {
      firstSymbol <- childTokenForExpr[i, ]
    } else if(childTokenForExpr[i, 'token'] == 'expr') {
      firstSymbol <- getFirstSymbolInExpr(childTokenForExpr[i, ], tokens)
    }
    
    if(is.na(firstSymbol) == FALSE) {
      break
    }
  }
  
  return(firstSymbol)
}

getDerivedFieldNameOrFunctionNameForTokens <- function(tokens) {
  firstSymbol <- getFirstSymbolInExpr(getChildTokensForParent(tokens[1, ], tokens)[1, ], tokens)
  
  if(is.na(firstSymbol)) {
    stop('derivedFieldName or functionName is unkown')
  }
  derivedFieldNameOrFunctionName <- 'unknown'
  
  return(firstSymbol$text)
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
  # This means this expression has table access expression which looks like Table[Table$Name == 'Age_cont', ]$Mean_female
  else if(!is.na(tokensWhoseParentIsTheCurrentExpr[2, ]$token) & tokensWhoseParentIsTheCurrentExpr[2, ]$token == "'$'") {
    # Get the name of the column in the table which is the output column for this table access
    outputColumnName <- tokensWhoseParentIsTheCurrentExpr[3, 'text']
    
    # Get the expression tokens which has the code for getting the row for table from which we will access the outputColumn
    exprTokenWithTableAccessConditions <- tokensWhoseParentIsTheCurrentExpr[1, ]
    # Get the child tokens of the above expr token
    childsTokensForExprTokenWithTableAccessConditions <- getChildTokensForParent(exprTokenWithTableAccessConditions, tokens)
    
    # The first token in the above child tokens is an expressions which has the name of the table we want to search
    exprTokenWithTableName <- childsTokensForExprTokenWithTableAccessConditions[1, ]
    # Get the name of the table
    tableName <- getChildTokensForParent(exprTokenWithTableName, tokens)[1, ]$text
    
    # Get the expr token which has the table search conditions like tableName$col == 'a' along with the AND between the conditions
    exprTokenWithTableEntireSearchConditions <- childsTokensForExprTokenWithTableAccessConditions[3, ]
    
    # Get the descendants of the expr token with table entires. It will have the information we need for the FieldColumnPairs
    tokensToUseForFieldColumnPairStrings <- getDescendantsOfToken(exprTokenWithTableEntireSearchConditions, tokens)
    
    # The string which at the end of the following loop will have all the FieldColumnPairs
    fieldColumnPairs <- ''
    
    # Go though the descendants
    for(i in 1:nrow(tokensToUseForFieldColumnPairStrings)) {
      # If the token is op type $
      if(tokensToUseForFieldColumnPairStrings[i, 'token'] == "'$'") {
        # The token  after this is the column referenced in the table
        column <- tokensToUseForFieldColumnPairStrings[i+1, ]
        # The token 2 after this is the field or constant we need to compare the column to
        fieldOrConstant <- tokensToUseForFieldColumnPairStrings[i+3, ]
        
        # Make the column pmml string
        columnString <- glue::glue('column="{column$text}"')
        # Make the field or constant pmml string
        fieldOrConstantString <- ifelse(isSymbolToken(fieldOrConstant), glue::glue('field="{fieldOrConstant$text}"'), glue::glue('constant="{formatConstantTokenText(fieldOrConstant)}"'))
        # Make the FieldColumnPair string and append it to the master list 
        fieldColumnPairs <- paste(fieldColumnPairs, glue::glue('<FieldColumnPair {columnString} {fieldOrConstantString}/>'))
      } 
    }
    
    # Return the MapValues pmml string
    return(glue::glue('<MapValues outputColumn="{outputColumnName}">{fieldColumnPairs}<TableLocator location="taxonomy" name="{tableName}"/></MapValues>'))
  }
  # This is a data frame access that looks like Table['Age_cont', 'Mean_male']
  else if(isDataFrameShortAccessExpr(expr, tokens)) {
    outputColumnName <- formatSymbolName(getChildTokensForParent(tokensWhoseParentIsTheCurrentExpr[5, ], tokens)[1, ])
    indexColumnValue <- formatSymbolName(getChildTokensForParent(tokensWhoseParentIsTheCurrentExpr[3, ], tokens)[1, ])
    tableName <- formatSymbolName(getChildTokensForParent(tokensWhoseParentIsTheCurrentExpr[1, ], tokens)[1, ])
    
    fieldColumnPairString <- glue::glue('<FieldColumnPair column="index" constant="{indexColumnValue}"/>')
    return(glue::glue('<MapValues outputColumn="{outputColumnName}">{fieldColumnPairString}<TableLocator location="taxonomy" name="{tableName}"/></MapValues>'))
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
      # If this expression has a function call in it
      if(isSymbolFunctionCallExpr(expr, tokens)) {
        functionSymbolToken <- getTokensWithParent(exprTokensWhoseParentIsTheCurrentExpr[1, 'id'], tokens)[1, ]
        exprTokensWhoseParentIsTheCurrentExprAndAreFunctionArgs <- exprTokensWhoseParentIsTheCurrentExpr[-1, ]
        functionArgsSymbolTokensPmmlString <- ''
        for(i in 1:nrow(exprTokensWhoseParentIsTheCurrentExprAndAreFunctionArgs)) {
          functionArgsSymbolTokensPmmlString <- paste(
            functionArgsSymbolTokensPmmlString,
            getPmmlStringForExpr(exprTokensWhoseParentIsTheCurrentExprAndAreFunctionArgs[i, ], tokens),
            sep=''
          )
        }

        # Handle c functions by taking the arguments to the functions and concating the pmml string for each argument
        if(functionSymbolToken$text == 'c') {
          return(functionArgsSymbolTokensPmmlString)
        } # If read.csv function call. Do nothing since we handle converting csv files to PMML tables at the beginning
        else if(functionSymbolToken$text == 'read.csv') {
          
        } else {
          return(getPmmlStringForSymbolFunctionCall(functionSymbolToken, functionArgsSymbolTokensPmmlString))
        }
      } else {
        for(i in 1:nrow(exprTokensWhoseParentIsTheCurrentExpr)) {
          pmmlStringForExprTokens <- paste(
            pmmlStringForExprTokens,
            getPmmlStringForExpr(exprTokensWhoseParentIsTheCurrentExpr[i, ], tokens),
            sep=''
          )
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

getDerivedFieldPmmlStringForTokens <- function(tokens, derivedFieldName) {
  symbolsWithDerivedFieldNameForText <- getAllSymbolsWithText(derivedFieldName, tokens)

  tokensToConvertToDerivedFieldPmml <- filterOutLeftAssignTokens(tokens)
  tokensToConvertToDerivedFieldPmml <- filterOutSymbolsWithText(derivedFieldName, tokensToConvertToDerivedFieldPmml)

  for(i in 1:nrow(symbolsWithDerivedFieldNameForText)) {
    tokensToConvertToDerivedFieldPmml <- filterOutTokenWithId(symbolsWithDerivedFieldNameForText[i, 'parent'], tokensToConvertToDerivedFieldPmml)
  }

  transformationPmmlString <- getPmmlStringForExpr(tokensToConvertToDerivedFieldPmml[1, ], tokensToConvertToDerivedFieldPmml)
  return(glue::glue('<DerivedField name="{derivedFieldName}" optype="continuous">{transformationPmmlString}</DerivedField>'))
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

getDefineFunctionPmmlStringForTokens <- function(tokens, functionName) {
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
      # There are two way to return a value in PMML. One way is just return what the last expression does or use an explicit return statement
      # We initially assume that it's the first way
      returnArgExprToken <- topLevelFunctionBodyExprTokens[i, ];
      
      # For the first way if there is a left assign then we need to set the return expr to the expr token which is the right hand side of the assignment
      childTokensForReturnArgExprToken <- getChildTokensForParent(returnArgExprToken, tokens)
      # Check if there's a left assign. If there is then the right hand assignment expr token is the third child
      if(doesTokensHaveALeftAssign(childTokensForReturnArgExprToken)) {
        returnArgExprToken <- childTokensForReturnArgExprToken[3, ]
      }
      # Check if it's the second way and if it is
      else if(nrow(getSymbolFunctionCallsWithText('return', getDescendantsOfToken(returnArgExprToken, tokens))) == 1) {
        #Get the expression for the argument to the return function call
        returnArgExprToken <- getExprTokens(getChildTokensForParent(topLevelFunctionBodyExprTokens[i, ], tokens))[2, ] 
      }
      
      #Convert the expression to it's PMML string
      pmmlStringForReturnArgExprToken <- getPmmlStringForExpr(returnArgExprToken, getDescendantsOfToken(returnArgExprToken, tokens))
      
      #Find all the symbols used within the expression which are not part of the function arguments
      symbolsWithinReturnArgExprWhichAreNotFunctionArguments <- getSymbolsInTokens(getDescendantsOfToken(returnArgExprToken, tokens))
      symbolsWithinReturnArgExprWhichAreNotFunctionArguments <- subset(
        symbolsWithinReturnArgExprWhichAreNotFunctionArguments,
        !(symbolsWithinReturnArgExprWhichAreNotFunctionArguments$text %in% functionArgNameTokens$text)
      )
      
      if(nrow(symbolsWithinReturnArgExprWhichAreNotFunctionArguments) != 0) {
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

getPmmlStringFromSouceFunctionCallTokens <- function(sourceFunctionCallTokens, mutatedVariables) {
  sourceFunctionCallArgExprToken <- getTokensWithParent(sourceFunctionCallTokens[1, ]$id, sourceFunctionCallTokens)[3, ]
  sourceFunctionCallArgCodeString <- getParseText(sourceFunctionCallTokens, sourceFunctionCallArgExprToken$id)
  sourceFilePath <- eval(parse(text=sourceFunctionCallArgCodeString))

  return(getPmmlStringFromRFile(sourceFilePath, FALSE, mutatedVariables))
}

# Generates the PMML table string for the data frame in the dataFrame argument whose name is the tableName argument
getTablePmmlStringsForDataFrame <- function(dataFrame, tableName) {
  # This is where we will store the entire InlineTable xml element
  pmmlTableString <- ''
  
  rownames <- rownames(dataFrame)
  
  # Go through all the rows of the table
  for(i in 1:nrow(dataFrame)) {
    # For each row add a <row> opening tag
    pmmlTableString <- glue::glue('{pmmlTableString}<row><index>{rownames[[i]]}</index>')
    
    # Go through the columns of the row
    for(j in 1:ncol(dataFrame)) {
      # For each column add <colname>Value of the column in this row</colname>
      pmmlTableString <- glue::glue('{pmmlTableString}<{colnames(dataFrame)[j]}>{dataFrame[i,j]}</{colnames(dataFrame)[j]}>')
    }
    
    # End of this row so add a closing row xml tag
    pmmlTableString <- glue::glue("{pmmlTableString}</row>")
  }
  
  # The final table string
  pmmlTableString <- glue::glue('<Taxonomy name="{tableName}"><InlineTable>{pmmlTableString}</InlineTable></Taxonomy>')

  # Return the string along with the variable to which the table data was assigned
  return(list(pmmlTableString, table))
}

getMutatedVariableName <- function(variableName, mutationNumber) {
  if(mutationNumber <= 0) {
    return(variableName)
  } else {
    return(glue::glue('{variableName}_Mutated_{mutationNumber}'))
  }
}

# Goes through the mutation logic for the list tokens in the tokens arg for the variable with name variableName
mutateRelevantVariables <- function(variableName, tokens, mutatedVariables) {
  # Check if there is an entry in the mutatedVariables data frame for the current variable. if there isn't, then create one and set the number of times it's been mutated to -1
  if(variableName %in% row.names(mutatedVariables) == FALSE) {
    mutatedVariables[variableName, 'mutationIteration'] <- -1
  }
  
  # Get the expr token which encapsulates the left hand side of an assignment statement
  exprTokenForRightAssign <- getChildTokensForParent(tokens[1, ], tokens)[3, ]
  # Get the expr token which encapsulates the right hand side of an assignment statement
  exprTokenForLeftAssign <- getChildTokensForParent(tokens[1, ], tokens)[1, ]
  # For each token in the list of them check if it's a child of the LHS or RHS expr token
  for(i in 1:nrow(tokens)) {
    # If child of RHS token then set all symbols to their approriate new mutated value. For example if testOne is variable has been mutated twice then we set it to testOne_Mutated_2
    if(isDescendantOfTokenWithId(exprTokenForRightAssign$id, tokens[i, ], tokens)) {
      if(isSymbolToken(tokens[i, ]) & tokens[i, 'text'] %in% row.names(mutatedVariables)) {
        tokens[i, 'text'] <- getMutatedVariableName(tokens[i, 'text'], mutatedVariables[tokens[i, 'text'], 'mutationIteration'])
      }  
    } 
    # Otherwise if it's part of the LHS it has to be the symbol for the current variable so set it's new name to number of times it's been mutated till now plus one
    else if(tokens[i, ]$parent == exprTokenForLeftAssign$id) {
      tokens[i, 'text'] <- getMutatedVariableName(variableName, mutatedVariables[variableName, 'mutationIteration'] + 1)
    }
  } 
  
  # Get the list of all the variables we are currently tracking for mutation
  currentVariables <- row.names(mutatedVariables)
  # Find the one that matches with the variableName variable and increase the mutation count by one
  for(i in currentVariables) {
    if(i == variableName) {
      mutatedVariables[i, 'mutationIteration'] <- mutatedVariables[i, 'mutationIteration'] + 1
    }
  }
  
  # Return the mutated tokens
  return(list(tokens=tokens, mutatedVariables=mutatedVariables))
}

# mutatedVariables - Keeps track of all the variables and the number of times they have been mutated. Each row is the name of the variable and every row has one column called mutation iteration which is the number of times this variable has been mutated. When function is called for the first time should not be passed in
getPmmlStringFromRFile <- function(filePath, srcFile=FALSE, mutatedVariables = data.frame()) {
  if(srcFile) {
    # Create directory where we store temperoray files during the addin operation
    dir.create(file.path(getwd(), 'temp'), showWarnings = FALSE)
    # Save the current workspace in the temp directory. Since we are going to be evaluating each line of code we don't want to overwrite a person's workspace objects as we execute the code
    save.image(file=file.path(getwd(), 'temp/temp.RData'))
  }

  tokensWithComments = getParseData(parse(file = filePath))
  tokens <- filterOutCommentTokens(tokensWithComments)

  nextZeroParentIndex = getIndexOfNextZeroParent(tokens)

  localTransformationString <- ''
  taxonomy <- ''
  
  # Each line of code is consists of several tokens but they all start with  an expr token whose parent is 0. This is how we know that we have reached a new line of code
  while(nextZeroParentIndex != 0) {
    tokensForCurrentParentIndex = tokens[1:nextZeroParentIndex, ]
    #print(tokensForCurrentParentIndex);
    
    if(doesTokensHaveSourceFunctionCall(tokensForCurrentParentIndex) == TRUE) {
      localTransformationString <- paste(localTransformationString, getPmmlStringFromSouceFunctionCallTokens(tokensForCurrentParentIndex, mutatedVariables), sep='')
    } else {
      variableName <- getDerivedFieldNameOrFunctionNameForTokens(tokensForCurrentParentIndex)
      mutateRelevantVariablesResult <- mutateRelevantVariables(variableName, tokensForCurrentParentIndex, mutatedVariables)
      tokensForCurrentParentIndex <- mutateRelevantVariablesResult$tokens
      mutatedVariables <- mutateRelevantVariablesResult$mutatedVariables
      
      # The new name for the possible mutated variable we are assigning to
      mutatedVariableName <- getMutatedVariableName(variableName, mutatedVariables[variableName, 'mutationIteration'])
      
      # We are going to evaluate the code represented by the tokens in the variable tokensForCurrentParentIndex and depending on the value returned called the right pmml parsing function
      evaluatedValue <- NA
      tryCatch({
        # Evaluate the line of code
        evaluatedValue <- eval(parse(text=getParseText(tokensForCurrentParentIndex, tokensForCurrentParentIndex[1, 'id'])))
      }, error = function(e) {
        # If there's an error set it to NA
        evaluatedValue <<- NA
      })
      
      if(mutatedVariables[variableName, 'mutationIteration'] != 0) {
        for(obj in ls()) {
          if(obj == variableName) {
            evaluatedValue = get(obj)
          }
        }
      }
      
      # If the evaluated value is a string then it's most probably a string assignment statement so call the function to create a DerivedField Pmml node
      if(class(evaluatedValue) == 'character') {
        localTransformationString <- paste(localTransformationString, getDerivedFieldPmmlStringForTokens(tokensForCurrentParentIndex, mutatedVariableName), sep='')
      }
      # if the evaluated value is a data frame
      else if(class(evaluatedValue) == 'data.frame') {
        # The return value is a list with the pmml string and the name of the variable to which the table was assigned
        returnValues <- getTablePmmlStringsForDataFrame(evaluatedValue, mutatedVariableName)
        
        # Add the pmml table string to the taxonomy string
        taxonomy <- paste(taxonomy,returnValues[1])
        #print(returnValues[2])
      }
      else if(doesTokensHaveFunctionDefinition(tokensForCurrentParentIndex) == TRUE) {
        localTransformationString <- paste(localTransformationString, getDefineFunctionPmmlStringForTokens(tokensForCurrentParentIndex, mutatedVariableName), sep='')
      } else {
        localTransformationString <- paste(localTransformationString, getDerivedFieldPmmlStringForTokens(tokensForCurrentParentIndex, mutatedVariableName), sep='')
      } 
    }

    if(nextZeroParentIndex == nrow(tokens)) {
      break
    }

    tokens = tokens[nextZeroParentIndex:nrow(tokens), ]

    nextZeroParentIndex = getIndexOfNextZeroParent(tokens)
  }

  if(srcFile == TRUE) {
    # Reset the workspace to before the addin was run
    load(file.path(getwd(), 'temp/temp.RData'))
    # Remove the file which had the workspace objects
    file.remove(file.path(getwd(), 'temp/temp.RData'))
    
    return(paste(taxonomy, '<LocalTransformations>', localTransformationString, '</LocalTransformations>'))
  } else {
    return(localTransformationString)
  }
}
