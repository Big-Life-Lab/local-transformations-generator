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

  print(derivedFieldNameOrFunctionName)
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

getDerivedFieldPmmlStringForTokens <- function(tokens) {
  derivedFieldName <- getDerivedFieldNameOrFunctionNameForTokens(tokens)
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

getPmmlStringFromSouceFunctionCallTokens <- function(sourceFunctionCallTokens) {
  sourceFunctionCallArgExprToken <- getTokensWithParent(sourceFunctionCallTokens[1, ]$id, sourceFunctionCallTokens)[3, ]
  sourceFunctionCallArgCodeString <- getParseText(sourceFunctionCallTokens, sourceFunctionCallArgExprToken$id)
  sourceFilePath <- eval(parse(text=sourceFunctionCallArgCodeString))

  return(getPmmlStringFromRFile(sourceFilePath))
}

getTablePmmlStringsForReadCsvFunctionCall <- function(tokens) {
  # Get the name of the variable to which the table contents have been assigned
  variableAssignedToTable <- getDerivedFieldNameOrFunctionNameForTokens(tokens)
  
  # Get id of the expression which is the parent of the entire read csv function call
  idOfExprWhichHasFunctionCall <- tokens[5, ]
  # Get the expression which is the parent of the argument that goes into the read csv funtion call
  exprWithArgumentToReadCsvFunctionCall <- getChildTokensForParent(idOfExprWhichHasFunctionCall, tokens)[3, ]
  # Get the R code as a string for the argument to the read.csv function call
  readCsvFilePathCodeString <- getParseText(tokens, exprWithArgumentToReadCsvFunctionCall$id)
  # Evaluate the code string to get the path to the table
  csvFilePath <- eval(parse(text=readCsvFilePathCodeString))
  
  # get the csv file as a dataframe
  table <- read.csv(csvFilePath)
  
  # This is where we will store the entire InlineTable xml element
  pmmlTableString <- ''
  
  rownames <- rownames(table)
  
  # Go through all the rows of the table
  for(i in 1:nrow(table)) {
    # For each row add a <row> opening tag
    pmmlTableString <- glue::glue('{pmmlTableString}<row><index>{rownames[[i]]}</index>')
    
    # Go through the columns of the row
    for(j in 1:ncol(table)) {
      # For each column add <colname>Value of the column in this row</colname>
      pmmlTableString <- glue::glue('{pmmlTableString}<{colnames(table)[j]}>{table[i,j]}</{colnames(table)[j]}>')
    }
    
    # End of this row so add a closing row xml tag
    pmmlTableString <- glue::glue("{pmmlTableString}</row>")
  }
  
  # The final table string
  pmmlTableString <- glue::glue('<Taxonomy name="{variableAssignedToTable}"><InlineTable>{pmmlTableString}</InlineTable></Taxonomy>')

  # Return the string along with the variable to which the table data was assigned
  return(list(pmmlTableString, variableAssignedToTable))
}

getPmmlStringFromRFile <- function(filePath, srcFile=FALSE) {
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
      localTransformationString <- paste(localTransformationString, getPmmlStringFromSouceFunctionCallTokens(tokensForCurrentParentIndex), sep='')
    } 
    # If this a read csv function call then we have to convert the imported csv file into a PMML table string
    else if(doesTokensHaveReadCsvFunctionCall(tokens) == TRUE) {
       # The return value is a list with the pmml string and the name of the variable to which the table was assigned
       returnValues <- getTablePmmlStringsForReadCsvFunctionCall(tokens)
       
       # Add the pmml table string to the taxonomy string
       taxonomy <- paste(taxonomy,returnValues[1])
       print(returnValues[2])
    }
    else if(doesTokensHaveFunctionDefinition(tokensForCurrentParentIndex) == TRUE) {
      localTransformationString <- paste(localTransformationString, getDefineFunctionPmmlStringForTokens(tokensForCurrentParentIndex), sep='')
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
    return(paste(taxonomy, '<LocalTransformations>', localTransformationString, '</LocalTransformations>'))
  } else {
    return(localTransformationString)
  }
}
