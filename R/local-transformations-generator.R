source(file.path(getwd(), 'R', './tokens.R'))
source(file.path(getwd(), 'R', './token_to_pmml.R'))
source(file.path(getwd(), 'R', './pmml-custom-func.R'))
source('R/if-expr.R')
source('R/util.R')
source('R/data-frame.R')
source('R/dollar-operator.R')
source('R/define-function.R')
source('R/function-call.R')
source('R/globals/gl-row-functions.R')
source('R/globals/gl-row-vars.R')

isDataFrameShortAccessExpr <- function(exprToCheck, tokens) {
  childTokens <- getChildTokensForParent(exprToCheck, tokens)

  if(nrow(childTokens) == 0) {
    return(FALSE)
  }
  else if(nrow(childTokens) > 2 & childTokens[2, 'text'] == '[' & childTokens[nrow(childTokens), 'text'] == ']') {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

getFirstSymbolInExpr <- function(expr, tokens) {
  firstSymbol <- NA
  childTokensForExpr <- getChildTokensForParent(expr, tokens)

  for(i in 1:nrow(childTokensForExpr)) {
    if(isSymbolToken(childTokensForExpr[i, ])) {
      firstSymbol <- childTokensForExpr[i, ]
    } else if(childTokensForExpr[i, 'token'] == 'expr') {
      firstSymbol <- getFirstSymbolInExpr(childTokensForExpr[i, ], tokens)
    }

    if(is.na(firstSymbol) == FALSE) {
      break
    } else {
      next
    }
  }

  return(firstSymbol)
}

getPmmlStringForIfToken <- function(conditionExpr, trueResultExpr, falseResultExpr, tokens) {
  pmmlStringForTrueExpr <- getPmmlStringForExpr(trueResultExpr, tokens)
  pmmlStringForFalseExpr <- getPmmlStringForExpr(falseResultExpr, tokens)
  pmmlStringForCondition <- getPmmlStringForExpr(conditionExpr, tokens)

  return(glue::glue('<Apply function="if">{pmmlStringForCondition}{pmmlStringForTrueExpr}{pmmlStringForFalseExpr}</Apply>'))
}

isSymbolFunctionCallExpr <- function(exprToken, tokens) {
  exprTokensWhoseParentIsTheCurrentToken <- getExprTokens(getTokensWithParent(exprToken$id, tokens))
  
  if(nrow(exprTokensWhoseParentIsTheCurrentToken) !=  0) {
    for(i in 1:nrow(exprTokensWhoseParentIsTheCurrentToken)) {
      if(getTokensWithParent(exprTokensWhoseParentIsTheCurrentToken[1, 'id'], tokens)[1, 'token'] == SYMBOL_FUNCTION_CALL_TOKEN) {
        return(TRUE)
      }
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
  # If this is an expression to access the column from a row and store it in a variable for eg. var1 <- row$col1
  else if(dollar_op.is_get_col_from_row_expr(expr, tokens)) {
    row_var_name <- dollar_op.get_var(expr, tokens)
    
    return(dollar_op.get_pmml_node(expr, tokens, globals.get_pmml_str_for_row_var_name(row_var_name)))
  }
  else if(dollar_op.is_expr(expr, tokens)) {
    if(data_frame.is_expr(tokensWhoseParentIsTheCurrentExpr[1, ], tokens)) {
      return(dollar_op.get_pmml_node(
        expr, tokens, data_frame.get_pmml_node(tokensWhoseParentIsTheCurrentExpr[1, ], tokens)))
    }
  } else if(data_frame.is_expr(expr, tokens)) {
    return(data_frame.get_pmml_node(expr, tokens))
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

        # Handle c functions by taking the arguments to the functions and concating the pmml string for each argument
        if(functionSymbolToken$text == 'c') {
          return(function_call.get_pmml_str_for_args(expr, tokens))
        } else if(functionSymbolToken$text == 'exists') {
          function_arg_expr_tokens <- function_call.get_function_arg_expr_tokens(expr, tokens)
          exitsArg <- formatConstantTokenText(getTokensWithParent(function_arg_expr_tokens[1, 'id'], tokens)[1, ])
          return(getPmmlStringForSymbolFunctionCall(functionSymbolToken, glue::glue('<FieldRef field="{exitsArg}"/>')))
        }# If read.csv function call. Do nothing since we handle converting csv files to PMML tables at the beginning
        else if(functionSymbolToken$text == 'read.csv') {} 
        else {
            return(getPmmlStringForSymbolFunctionCall(functionSymbolToken, 
                                                      function_call.get_pmml_str_for_args(expr, tokens)))
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
    } else if(nonExprTokenToken == NUM_CONST_TOKEN | nonExprTokenToken == STR_CONST_TOKEN | nonExprTokenToken == NULL_CONST_TOKEN) {
      return(getPmmlStringForConstant(nonExprToken))
    } else if(nonExprTokenToken %in% MATH_TOKENS) {
      return(getPmmlStringForMathToken(nonExprToken, pmmlStringForExprTokens))
    } else if(nonExprTokenToken %in% LOGICAL_TOKENS) {
      return(getPmmlStringForLogicalOperator(nonExprToken, pmmlStringForExprTokens))
    } else if(nonExprToken$token == "':'") {
      return(getPmmlStringForColonToken(pmmlStringForExprTokens))
    } 
    else {
      return(pmmlStringForExprTokens)
    }
  }
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

getDefineFunctionForDefaultArgExpr <- function(argSymbolFormal, allArgSymbolFormals, tokens) {
  possibleEqFormalsToken <- getTokenWithId(argSymbolFormal$id+1, tokens)
  doesArgHaveDefaultValue <- possibleEqFormalsToken$token == EQ_FORMALS

  if(!doesArgHaveDefaultValue) {
    return('')
  } else {
    eqFormalsToken <- possibleEqFormalsToken

    tokenAfterEqFormalsToken <- getTokenAfterTokenWithId(tokens, eqFormalsToken$id)

    argName <- argSymbolFormal$text

    defaultValue <- NA
    if(tokenAfterEqFormalsToken$token == EXPR_TOKEN) {
      defaultValue <- getPmmlStringForExpr(tokenAfterEqFormalsToken, tokens)
    } else {
      defaultValue <- getPmmlStringForConstant(tokenAfterEqFormalsToken)
    }

    return(glue::glue(getPmmlStringForDefineFunction(glue::glue('default({argName})'), allArgSymbolFormals, glue::glue('<Apply function="if"><Apply function="equal"><FieldRef field="{argName}"/><Constant dataType="NA">NA</Constant></Apply>{defaultValue}<FieldRef field="{argName}"/></Apply>'))))
  }
}


# Returns the pmmlString arg where every reference to an argument that has been defaulted is replaced with a function call that returns the formatted value
getPmmlStringWithDefaultedArgsCorrectlySet <- function(defaultedArgTokens, allArgTokens, pmmlString) {
  formattedPmmlString <- pmmlString

  if(nrow(defaultedArgTokens) != 0) {
    for(i in 1:nrow(defaultedArgTokens)) {
      defaultFunctionArgsPmmlString <- ''
      for(j in 1:nrow(allArgTokens)) {
        # Added placeholder to the beginning to prevent subsequent replacement calls from replacing the earlier replacement. THis will be put back to the right string later
        defaultFunctionArgsPmmlString <- glue::glue(defaultFunctionArgsPmmlString, '<FieldRef field="placeholder_{allArgTokens[j, "text"]}"/>')
      }

      formattedPmmlString <- gsub(glue::glue('<FieldRef field="{defaultedArgTokens[i, "text"]}"/>'), glue::glue('<Apply function="default({defaultedArgTokens[i, "text"]})">{defaultFunctionArgsPmmlString}</Apply>'), formattedPmmlString)
    }

    for(i in 1:nrow(allArgTokens)) {
      formattedPmmlString <- gsub(glue::glue('<FieldRef field="placeholder_{allArgTokens[i, "text"]}"/>'), glue::glue('<FieldRef field="{allArgTokens[i, "text"]}"/>'), formattedPmmlString)
    }
  }

  return(formattedPmmlString)
}

# Get the index of the not the first but the second row in the parseData array which has the parent field set to 0
getIndexOfNextZeroParent <- function(parseData) {
  numZeroParents <- 0

  for(i in 1:nrow(parseData)) {
    if(parseData[i,'parent'] == 0) {
      if(numZeroParents == 1) {
        return(i)
      }
      else {
        numZeroParents <- numZeroParents + 1
      }
    }
  }

  return(nrow(parseData))
}

getPmmlStringFromSouceFunctionCallTokens <- function(sourceFunctionCallTokens, mutatedVariables, evaluated_variables) {
  sourceFunctionCallArgExprToken <- getTokensWithParent(sourceFunctionCallTokens[1, ]$id, sourceFunctionCallTokens)[3, ]
  sourceFunctionCallArgCodeString <- getParseText(sourceFunctionCallTokens, sourceFunctionCallArgExprToken$id)
  sourceFilePath <- eval(parse(text=sourceFunctionCallArgCodeString))
  
  return(getPmmlStringFromRFile(sourceFilePath, FALSE, mutatedVariables, evaluated_variables))
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

mutateVariable <- function(mutatedVariables, variableName) {
  # Get the list of all the variables we are currently tracking for mutation
  currentVariables <- row.names(mutatedVariables)
  
  # Find the one that matches with the variableName variable and increase the mutation count by one
  for(i in currentVariables) {
    if(i == variableName) {
      mutatedVariables[i, 'mutationIteration'] <- mutatedVariables[i, 'mutationIteration'] + 1
    }
  }
  
  return(mutatedVariables)
}

# Goes through the mutation logic for the list tokens in the tokens arg for the variable with name variableName
mutateRelevantVariables <- function(variableName, tokens, mutatedVariables) {
  # Get the expr token which encapsulates the left hand side of an assignment statement
  exprTokenForRightAssign <- getChildTokensForParent(tokens[1, ], tokens)[3, ]
  # Get the expr token which encapsulates the right hand side of an assignment statement
  exprTokenForLeftAssign <- getChildTokensForParent(tokens[1, ], tokens)[1, ]
  
  # First go through all the tokens which are in the RHS and update all the relevant
  # variables to their mutated variable names
  for(i in 1:nrow(tokens)) {
    # For example if testOne is variable has been mutated twice then we set it to testOne_Mutated_2
    if(isDescendantOfTokenWithId(exprTokenForRightAssign$id, tokens[i, ], tokens)) {
      if(isSymbolToken(tokens[i, ]) & tokens[i, 'text'] %in% row.names(mutatedVariables)) {
        tokens[i, 'text'] <- getMutatedVariableName(tokens[i, 'text'], mutatedVariables[tokens[i, 'text'], 'mutationIteration'])
      }
    }
  }
  
  
  # Check if there is an entry in the mutatedVariables data frame for the current variable. if there isn't, then create one and set the number of times it's been mutated to 0
  if(variableName %in% row.names(mutatedVariables) == FALSE) {
    mutatedVariables[variableName, 'mutationIteration'] <- 0
  } else { # If there is an entry then update it's it's mutation count
    mutatedVariables <- mutateVariable(mutatedVariables, variableName)
  }

  # Next. If the variable on the left is being set using itself then it implies 
  # it's being mutated, so update the mutationIteration for the variable
  # if it happens. For eg, a <- a would be a statement that mutates itself
  # Do this by checking if any of symbols on the RHS has the same name has the
  # variableName arg. Remember that the tokens have been updated though so technically
  # this should only work for the very first self mutation
  for(tokenIndex in 1:nrow(tokens)) {
    currentToken <- tokens[tokenIndex, ]
    
    if(isDescendantOfTokenWithId(exprTokenForRightAssign$id, currentToken, tokens)) {
      if(isSymbolToken(currentToken) & currentToken$text == variableName) {
        mutatedVariables <- mutateVariable(mutatedVariables, variableName)
        
        break
      }
    }  
  }
  
  
  # For each token in the list of them check if it's a child of the LHS or RHS expr token
  for(i in 1:nrow(tokens)) {
    # Otherwise if it's part of the LHS it has to be the symbol for the current variable so set it's new name to number of times it's been mutated till now plus one
    if(tokens[i, ]$parent == exprTokenForLeftAssign$id) {
      tokens[i, 'text'] <- getMutatedVariableName(variableName, mutatedVariables[variableName, 'mutationIteration'])
    }
  }

  # Return the mutated tokens
  return(list(tokens=tokens, mutatedVariables=mutatedVariables))
}

# mutatedVariables - Keeps track of all the variables and the number of times they have been mutated. Each row is the name of the variable and every row has one column called mutation iteration which is the number of times this variable has been mutated. When function is called for the first time should not be passed in
# evaluated_variables - A HashMap that maps the variable name from each line of code to it's evaluated value
getPmmlStringFromRFile <- function(filePath, srcFile=FALSE, mutatedVariables = data.frame(), evaluated_variables = new.env(hash = TRUE)) {
  if(srcFile) {
    # Create directory where we store temperoray files during the addin operation
    dir.create(file.path(getwd(), 'temp'), showWarnings = FALSE)
    # Save the current workspace in the temp directory. Since we are going to be evaluating each line of code we don't want to overwrite a person's workspace objects as we execute the code
    save.image(file=file.path(getwd(), 'temp/temp.RData'))
    
    assign("row_vars", data.frame(), envir = .GlobalEnv)
    assign("gl_row_functions", list(), envir = .GlobalEnv)
  }

  tokensWithComments <- getParseData(parse(file = filePath, keep.source = TRUE))
  tokens <- filterOutCommentTokens(tokensWithComments)
  ##### DEBUG
  #print(tokens)

  nextZeroParentIndex <- getIndexOfNextZeroParent(tokens)

  localTransformationString <- ''
  taxonomy <- ''

  # Each line of code is consists of several tokens but they all start with  an expr token whose parent is 0. This is how we know that we have reached a new line of code
  while(nextZeroParentIndex != 0) {
    tokensForCurrentParentIndex = tokens[1:nextZeroParentIndex, ]

    # Get all the comments for this expression
    comments_for_current_expr <- getCommentTokensWithParent(
      tokensForCurrentParentIndex[1, ]$id,
      tokensWithComments
    )

    if(doesTokensHaveSourceFunctionCall(tokensForCurrentParentIndex) == TRUE) {
      sourceReturnValues <- getPmmlStringFromSouceFunctionCallTokens(tokensForCurrentParentIndex, mutatedVariables, evaluated_variables)

      taxonomy <- paste(taxonomy, sourceReturnValues$taxonomy, sep='')
      localTransformationString <- paste(localTransformationString, sourceReturnValues$localTransformationString, sep='')
      mutatedVariables <- sourceReturnValues$mutatedVariables
    } else {
      if(isIfExpr(tokensForCurrentParentIndex)) {
        localTransformationString <- paste(
          localTransformationString, getPmmlStringForIfExpr(
            tokensForCurrentParentIndex[1, ],
            tokensForCurrentParentIndex,
            comments_for_current_expr,
            evaluated_variables
          ),
          sep=''
        )
      } else {
        variableName <- getDerivedFieldNameOrFunctionNameForTokens(tokensForCurrentParentIndex)

        mutateRelevantVariablesResult <- mutateRelevantVariables(variableName, tokensForCurrentParentIndex, mutatedVariables)
        tokensForCurrentParentIndex <- mutateRelevantVariablesResult$tokens
        mutatedVariables <- mutateRelevantVariablesResult$mutatedVariables

        # The new name for the possible mutated variable we are assigning to
        mutatedVariableName <- getMutatedVariableName(variableName, mutatedVariables[variableName, 'mutationIteration'])
        print(mutatedVariableName)

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

        # Set the evaluated value to it's mutated variable value in the evaluated variables environment
        evaluated_variables[[mutatedVariableName]] <- evaluatedValue

        # If the evaluated value is a string then it's most probably a string assignment statement so call the function to create a DerivedField Pmml node
        if(class(evaluatedValue) == 'character') {
          localTransformationString <- paste(
            localTransformationString,
            getDerivedFieldPmmlStringForTokens(
              tokensForCurrentParentIndex, mutatedVariableName, comments_for_current_expr, evaluated_variables),
            sep=''
          )
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
          localTransformationString <- paste(localTransformationString, define_function.get_pmml_string(tokensForCurrentParentIndex, mutatedVariableName), sep='')
        } 
        else {
          assign_expr_token <- getTokenWithAssignmentCode(tokensForCurrentParentIndex)
          child_tokens <- getChildTokensForParent(assign_expr_token, tokensForCurrentParentIndex)
          possible_row_var <- getChildTokensForParent(child_tokens[1, ], tokensForCurrentParentIndex)[1, ]
          # If this is an expression to get the row from a data frame and store it in a variable for eg. table[col1 == 'val' & col2 == 'val2', ]
          # We will add it to the table of row accesses and use it when we encounter an expression that accesses the column from this row
          if(dollar_op.is_expr(assign_expr_token, tokensForCurrentParentIndex) == FALSE & 
             data_frame.is_expr(assign_expr_token, tokensForCurrentParentIndex) & data_frame.is_wildcard_expr(assign_expr_token, tokensForCurrentParentIndex)) {
            row_vars <<- rbind(row_vars,
              data.frame(
                row_name = c(mutatedVariableName),
                pmml = c(data_frame.get_pmml_node(assign_expr_token, tokensForCurrentParentIndex))
            ))
          } 
          else if(function_call.is_row_function_call_expr(assign_expr_token, tokensForCurrentParentIndex)) {
            gl_row_function <- globals.get_row_function(function_call.get_function_name_token(assign_expr_token, tokens)$text)
            func_arg_expr_tokens <- function_call.get_function_arg_expr_tokens(assign_expr_token, tokens)
            non_row_func_arg_expr_tokens <- func_arg_expr_tokens
            for(i in 1:nrow(func_arg_expr_tokens)) {
              if(gl_row_function$args[i] %in% gl_row_function$row_args) {
                non_row_func_arg_expr_tokens <- non_row_func_arg_expr_tokens[non_row_func_arg_expr_tokens$id != func_arg_expr_tokens[i, ]$id, ]
              }
            }
            row_func_arg_expr_tokens <- func_arg_expr_tokens[func_arg_expr_tokens$id != non_row_func_arg_expr_tokens$id, ]
            
            define_function_pmml_str <- ''
            define_function_pmml_str <- gsub(
              gl_row_function$func_name,
              glue::glue("{gl_row_function$func_name}_{mutatedVariableName}"),
              gl_row_function$pmml_str
            )
            for(i in 1:length(gl_row_function$row_args)) {
              row_param_name <- gl_row_function$row_args[i]
              row_arg_name <- getChildTokensForParent(row_func_arg_expr_tokens[i, ], tokens)[1, ]$text
              define_function_pmml_str <- gsub(
                paste("\\{", row_param_name, "\\}", sep = ""),
                globals.get_pmml_str_for_row_var_name(row_arg_name),
                define_function_pmml_str
              )
            }
            
            derived_field_pmml_str <- getDerivedFieldPmmlStringForTokens(
              tokensForCurrentParentIndex, mutatedVariableName, comments_for_current_expr, evaluated_variables)
            derived_field_pmml_str <- gsub(
              gl_row_function$func_name,
              glue::glue("{gl_row_function$func_name}_{mutatedVariableName}"),
              derived_field_pmml_str
            )
            
            localTransformationString <- paste(
              localTransformationString, 
              define_function_pmml_str, 
              derived_field_pmml_str)
          } 
          else {
            localTransformationString <- paste(localTransformationString, getDerivedFieldPmmlStringForTokens(
              tokensForCurrentParentIndex, mutatedVariableName, comments_for_current_expr, evaluated_variables), sep='')
          }
        }
      }
    }

    if(nextZeroParentIndex == nrow(tokens)) {
      break
    }

    tokens <- tokens[nextZeroParentIndex:nrow(tokens), ]

    nextZeroParentIndex <- getIndexOfNextZeroParent(tokens)
  }

  if(srcFile == TRUE) {
    # Reset the workspace to before the addin was run
    load(file.path(getwd(), 'temp/temp.RData'))
    # Remove the file which had the workspace objects
    file.remove(file.path(getwd(), 'temp/temp.RData'))
    
    remove("gl_row_functions", envir = .GlobalEnv)
    
    return(paste('<PMML>', taxonomy, '<LocalTransformations>', localTransformationString, '</LocalTransformations></PMML>'))
  } else {
    return(list('taxonomy' = taxonomy, 'localTransformationString' = localTransformationString, mutatedVariables = mutatedVariables))
  }
}
