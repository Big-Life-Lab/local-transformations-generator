source("R/strings.R")

define_function.get_pmml_string <- function(tokens, functionName) {
  functionTokens <- getFunctionTokens(tokens)
  if(nrow(functionTokens) > 1) {
    stop('Too many function tokens found within function definition')
  }
  
  functionTokenParentExprId <- functionTokens[1, ]$parent
  
  functionExprToken <- getTokenWithId(functionTokenParentExprId, tokens)
  
  functionDefinitionTokens <- getDescendantsOfToken(functionExprToken, tokens)
  
  #The names of the functions arguments
  function_param_name_tokens <- getSymbolFormalsTokens(tokens)

  #Expression tokens which are children of the function expr. The expr tokens within the function could be one of the following:
  #1. Expr for default values assigned to an argument
  #2. Expr for the function body which is what we want
  # The function body expr will be at the end so get the last row in the data frame consisting of all the expr tokens
  functionBodyExprToken <- tail(getExprTokens(getChildTokensForParent(functionExprToken, tokens)), n=1)
  
  #Get all the tokens which together make up the function body
  functionBodyExprTokenDescendants = getDescendantsOfToken(functionBodyExprToken, tokens)
  
  #Get the top level expr tokens for the function body. These are the tokens which hold all the logic in the function body as well as the return call
  topLevelFunctionBodyExprTokens <- getExprTokens(getChildTokensForParent(functionBodyExprToken, tokens))
  
  row_args <- define_function.get_row_args(topLevelFunctionBodyExprTokens, function_param_name_tokens$text, tokens)
  non_row_function_arg_tokens <- function_param_name_tokens[function_param_name_tokens$text %in% row_args == FALSE, ]
  
  pmmlFunctionString <- ''
  
  defaultedArgs <- data.frame()
  if(nrow(function_param_name_tokens) != 0) {
    for(i in 1:nrow(function_param_name_tokens)) {
      defaultFunctionPmmlStringForCurrentArg <- getDefineFunctionForDefaultArgExpr(function_param_name_tokens[i, ], function_param_name_tokens, tokens)
      
      if(defaultFunctionPmmlStringForCurrentArg != '') {
        if(function_param_name_tokens[i, "text"] %in% row_args) {
          stop("Argument which is used as a row has a default value")
        }
        defaultedArgs <- rbind(defaultedArgs, function_param_name_tokens[i, ])
        pmmlFunctionString <- glue::glue(pmmlFunctionString, defaultFunctionPmmlStringForCurrentArg)
      }
    }
  }
  
  for(i in 1:nrow(topLevelFunctionBodyExprTokens)) {
    test_unsupported_exprs(topLevelFunctionBodyExprTokens[i, ], tokens)
    
    if(i != nrow(topLevelFunctionBodyExprTokens)) {
      inner_func_expr <- topLevelFunctionBodyExprTokens[i, ]
      inner_func_name <- define_function.get_inner_func_name(inner_func_expr, tokens, functionName)
      pmmlFunctionString <- paste(pmmlFunctionString, getPmmlStringForExprTokenWithinFunction(inner_func_name, topLevelFunctionBodyExprTokens[i, ], non_row_function_arg_tokens, functionName, defaultedArgs, tokens), sep='')
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
      pmmlStringForReturnArgExprToken <- define_function.get_pmml_str_for_expr(returnArgExprToken, getDescendantsOfToken(returnArgExprToken, tokens))
      
      #Find all the symbols used within the expression which are not part of the function arguments
      symbolsWithinReturnArgExprWhichAreNotFunctionArguments <- getSymbolsInTokens(getDescendantsOfToken(returnArgExprToken, tokens))
      symbolsWithinReturnArgExprWhichAreNotFunctionArguments <- subset(
        symbolsWithinReturnArgExprWhichAreNotFunctionArguments,
        !(symbolsWithinReturnArgExprWhichAreNotFunctionArguments$text %in% non_row_function_arg_tokens$text)
      )
      
      if(nrow(symbolsWithinReturnArgExprWhichAreNotFunctionArguments) != 0) {
        #1. Convert each symbol into R code which calls a function whose name is a combination of the symbol and original function name and whose arguments are the
        # original function arguments and generate PMML code for it
        #2. Replace each FieldRef within the above PMML string for each symbol with the generated PMML string for that function call R code
        for(j in 1:nrow(symbolsWithinReturnArgExprWhichAreNotFunctionArguments)) {
          # 1.
          rFunctionNameForCurrentSymbol <- glue::glue('{functionName}_{symbolsWithinReturnArgExprWhichAreNotFunctionArguments[j, "text"]}')
          rFunctionArgs <- getRArgumentsIntoFunctionString(non_row_function_arg_tokens)
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
      
      pmmlStringForReturnArgExprToken <- getPmmlStringWithDefaultedArgsCorrectlySet(defaultedArgs, non_row_function_arg_tokens, pmmlStringForReturnArgExprToken)
      
      #Make the DefineFunction PMML string
      pmmlDefineFunctionString <- getPmmlStringForDefineFunction(functionName, non_row_function_arg_tokens, pmmlStringForReturnArgExprToken)
      
      #Add it to the pmmlFunctionString
      pmmlFunctionString <- paste(pmmlFunctionString, pmmlDefineFunctionString, sep='')
    }
  }
  
  if(length(row_args) == 0) {
    return(pmmlFunctionString)
  } else {
    row_function <- list(
      func_name = functionName,
      args = function_param_name_tokens$text,
      row_args = row_args,
      pmml_str = pmmlFunctionString
    )
    gl_row_functions[[length(gl_row_functions) + 1]] <<- row_function
    
    return('')
  }
}

# Currently unsupported expressions within functions
# 1. Accessing column values from data frame 
test_unsupported_exprs <- function(top_level_expr, tokens) {
  assign_expr_token <- getTokenWithAssignmentCode(getDescendantsOfToken(top_level_expr, tokens))
  
  if(is.na(assign_expr_token) == FALSE) {
    if(data_frame.is_col_access(assign_expr_token, tokens) | data_frame.is_expr(assign_expr_token, tokens)) {
      stop(strings.unsupported_df_col_access_expr_error)  
    }
  }
}

define_function.get_pmml_str_for_expr <- function(expr, tokens) {
  get_pmml_str_for_row_access <- function(expr, tokens) {
    row_var_name <- dollar_op.get_var(expr, tokens)
    inner_text <- paste("{", row_var_name, "}", sep = "")
    return(dollar_op.get_pmml_node(expr, tokens, inner_text))
  }
  
  return(getPmmlStringForExpr(expr, tokens, get_pmml_str_for_row_access))
}

getPmmlStringForExprTokenWithinFunction <- function(inner_func_name, innerFunctionExprToken, originalFunctionArgTokens, originalFunctionName, defaultedArgTokens, tokens) {
  #Get the expression token which has the initialization code
  initializationExprToken <- getTokenWithAssignmentCode(getChildTokensForParent(innerFunctionExprToken, tokens))
  
  #Convert the expression which has the initialization code into it's PMML string
  pmmlStringForInitializationExprToken <- define_function.get_pmml_str_for_expr(initializationExprToken, tokens)
  
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
  
  pmmlStringForInitializationExprToken <- getPmmlStringWithDefaultedArgsCorrectlySet(defaultedArgTokens, originalFunctionArgTokens, pmmlStringForInitializationExprToken)
  
  defineFunctionPmmlString <- getPmmlStringForDefineFunction(inner_func_name, originalFunctionArgTokens, pmmlStringForInitializationExprToken)
  
  #Return the final DefineFunction PMML string
  return(defineFunctionPmmlString)
}

getFunctionNameForInnerFunctionExprToken <- function(originalFunctionName, variableName) {
  #Make the name of the function using the variableName and the orignal function name which is
  functionName <- glue::glue('{originalFunctionName}({variableName})')
  
  return(functionName)
}

define_function.get_inner_func_name <- function(inner_func_expr, tokens, orig_func_name) {
  var_name <- util.get_var_and_func_names(getDescendantsOfToken(inner_func_expr, tokens))[1]
  
  return(getFunctionNameForInnerFunctionExprToken(orig_func_name, var_name))
} 

define_function.get_row_args <- function(func_body_exprs, function_args, tokens) {
  row_args <- c()
  for(i in 1:nrow(func_body_exprs)) {
    token_with_assignment_code <- NA
    func_body_expr <- func_body_exprs[i, ]
    if(define_function.is_return_expr(func_body_expr, tokens)) {
      token_with_assignment_code <- define_function.get_return_arg_expr(func_body_expr, tokens)
    } else {
      token_with_assignment_code <- getTokenWithAssignmentCode(
        getChildTokensForParent(func_body_expr, tokens)
      )
    }
    
    # For this expression
    # 1. Get all the symbol tokens which are descendants of this expr and is
    # a function parameter
    # 2. Check whether this function parameter is used as a row from a # dataframe. If it is then add it to the list row_args
    # Step 1
    descendant_symbol_tokens <- getSymbolsInTokens(getDescendantsOfToken(token_with_assignment_code, tokens))
    descendant_arg_symbol_tokens <- descendant_symbol_tokens[descendant_symbol_tokens$text %in% function_args, ]
    # Step 2
    if(nrow(descendant_arg_symbol_tokens) > 0) {
      for(j in 1:nrow(descendant_arg_symbol_tokens)) {
        parent_token_to_check_for_row_access <- getParentToken(getParentToken(descendant_arg_symbol_tokens[j, ], tokens), tokens)
        if(data_frame.is_row_access(parent_token_to_check_for_row_access, tokens)) {
          row_var_name <- dollar_op.get_var(parent_token_to_check_for_row_access, tokens)
          if(row_var_name %in% function_args & row_var_name %in% row_args == FALSE) {
            row_args <- c(row_args, row_var_name)
          }
        }
      }
    }
  }
  
  return(row_args)
}

define_function.is_return_expr <- function(expr, tokens) {
  return(nrow(getSymbolFunctionCallsWithText('return', getDescendantsOfToken(expr, tokens))) == 1)
}

define_function.get_return_arg_expr <- function(return_expr, tokens) {
  return(getExprTokens(getChildTokensForParent(return_expr, tokens))[2, ])
}

define_function.is <- function(expr, tokens) {
  return(getChildTokensForParent(expr, tokens)[1, "token"] == FUNCTION_TOKEN)
}