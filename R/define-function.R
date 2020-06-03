source("R/strings.R")
source(file.path(getwd(), 'R', './tokens/expr-token.R'))
source("R/tokens/symbol-function-call-token.R")
source("R/tokens/token.R")
source("R/pmml.R")
source("R/expr.R")

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
      returnArgExprToken <- topLevelFunctionBodyExprTokens[i, ]
      pmmlStringForReturnArgExprToken <- ''
      if(if_expr.is(returnArgExprToken, tokens)) {
        pmmlStringForReturnArgExprToken <- define_function.get_pmml_str_for_expr(
          returnArgExprToken, tokens, functionName, function_param_name_tokens, TRUE
        )
      } else {
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
        pmmlStringForReturnArgExprToken <- define_function.get_pmml_str_for_expr(returnArgExprToken, getDescendantsOfToken(returnArgExprToken, tokens), functionName, function_param_name_tokens, TRUE)
      }
      
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
          pmmlStringForRCode <- define_function.get_pmml_str_for_expr(tokensForRCode[1, ], tokensForRCode, functionName, function_param_name_tokens, TRUE)
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
  
  if(token.is_na(assign_expr_token) == FALSE) {
    if(data_frame.is_col_access(assign_expr_token, tokens) | data_frame.is_expr(assign_expr_token, tokens)) {
      stop(strings.unsupported_df_col_access_expr_error)  
    }
  }
}

get_pmml_str_for_row_access <- function(expr, tokens) {
  row_var_name <- dollar_op.get_var(expr, tokens)
  inner_text <- paste("{", row_var_name, "}", sep = "")
  return(dollar_op.get_pmml_node(expr, tokens, inner_text))
}


get_pmml_str_for_if_expr <- function(cond_expr_to_block_exprs_mappings, tokens, orig_func_name, orig_func_param_tokens, is_last_expr) {
  if(is_last_expr == FALSE) {
    var_name_to_if_expr_mappings <- list()
    for(i in 1:length(cond_expr_to_block_exprs_mappings)) {
      current_map <- cond_expr_to_block_exprs_mappings[[i]]
      
      for(j in 1:length(current_map$block_expr_ids)) {
        current_block_expr_id <- current_map$block_expr_ids[[j]]
        
        var_name <- util.get_var_and_func_names(
          getDescendantsOfToken(getTokenWithId(current_block_expr_id, tokens), tokens)
        )[[1]]
        new_var_name_to_if_expr_map <- list(
          cond_expr_id = current_map$cond_expr_id,
          expr_id = current_block_expr_id
        )
        
        current_var_name_mapping <- var_name_to_if_expr_mappings[[var_name]]
        if(is.null(current_var_name_mapping)) {
          var_name_to_if_expr_mappings[[var_name]] <- list()
          var_name_to_if_expr_mappings[[var_name]][[1]] <- new_var_name_to_if_expr_map
        } else {
          current_var_name_mapping[[length(current_var_name_mapping) + 1]] <- new_var_name_to_if_expr_map
          var_name_to_if_expr_mappings[[var_name]] <- current_var_name_mapping
        }
      }
    }
    
    var_names <- names(var_name_to_if_expr_mappings)
    pmml_str <- ''
    for(i in 1:length(var_names)) {
      pmml_str_for_var <- ''
      
      cur_var_name <- var_names[[i]]
      
      reverse_cond_expr_mappings <- rev(var_name_to_if_expr_mappings[[cur_var_name]])
      if(is.na(reverse_cond_expr_mappings[[1]]$cond_expr_id) == FALSE) {
        pmml_str_for_var <- '<Constant dataType="NULL">NULL</Constant>'
      }
      for(j in 1:length(reverse_cond_expr_mappings)) {
        cur_cond_expr_mapping <- reverse_cond_expr_mappings[[j]]
        
        pmml_str_for_cond <- ''
        if(is.na(cur_cond_expr_mapping$cond_expr_id) == FALSE) {
          pmml_str_for_cond <- define_function.get_pmml_str_for_expr(
            getTokenWithId(cur_cond_expr_mapping$cond_expr_id, tokens),
            tokens,
            orig_func_name,
            orig_func_param_tokens,
            is_last_expr
          )
        }
        pmml_str_for_expr <- define_function.get_pmml_str_for_token(
          getTokenWithAssignmentCode(
            getDescendantsOfToken(getTokenWithId(cur_cond_expr_mapping$expr_id, tokens), tokens)
          ),
          tokens,
          orig_func_name,
          orig_func_param_tokens,
          is_last_expr
        )
        
        if(is.na(cur_cond_expr_mapping$cond_expr_id) == FALSE) {
          pmml_str_for_var <- glue::glue('<Apply function="if">{pmml_str_for_cond}{pmml_str_for_expr}{pmml_str_for_var}</Apply>')
        } else {
          pmml_str_for_var <- pmml_str_for_expr
        }
      }
      
      inner_func_name <- glue::glue("{orig_func_name}({cur_var_name})")
      pmml_str <- paste(pmml_str, getPmmlStringForDefineFunction(inner_func_name, orig_func_param_tokens, pmml_str_for_var), sep = '')
    }
    
    return(pmml_str)
  } else {
    new_cond_expr_to_block_expr_mappings <- list()
    for(i in 1:length(cond_expr_to_block_exprs_mappings)) {
      current_mapping <- cond_expr_to_block_exprs_mappings[[i]]
      new_cond_expr_to_block_expr_map <- list(
        cond_expr_id = current_mapping$cond_expr_id,
        expr_id = current_mapping$block_expr_ids[[length(current_mapping$block_expr_ids)]]
      )
      
      new_cond_expr_to_block_expr_mappings[[length(new_cond_expr_to_block_expr_mappings) + 1]] <- new_cond_expr_to_block_expr_map
    }
    
    pmml_str <- ''
    rev_cond_expr_to_block_expr_maps <- rev(new_cond_expr_to_block_expr_mappings)
    for(i in 1:length(rev_cond_expr_to_block_expr_maps)) {
      current_mapping <- rev_cond_expr_to_block_expr_maps[[i]]
      
      if(i == 1 & is.na(current_mapping$cond_expr_id) == FALSE) {
        pmml_str <- '<Constant dataType="NULL">NULL</Constant>'
      }
      
      cond_pmml_str <- NA
      if(is.na(current_mapping$cond_expr_id) == FALSE) {
        cond_pmml_str <- define_function.get_pmml_str_for_expr(
          getTokenWithId(current_mapping$cond_expr_id, tokens),
          tokens,
          orig_func_name,
          orig_func_param_tokens,
          is_last_expr
        )
      }
      
      expr_pmml_str <- ''
      expr_token_to_run <- getTokenWithId(current_mapping$expr_id, tokens)

      if(expr_token.is_assignment_expr(expr_token_to_run, tokens)) {
        assignment_token <- getTokenWithAssignmentCode(
          getDescendantsOfToken(expr_token_to_run, tokens)
        )
        
        expr_pmml_str <- define_function.get_pmml_str_for_token(
          assignment_token, tokens, orig_func_name, orig_func_param_tokens, is_last_expr
        )
      } else if(symbol_function_call_token.is_expr_symbol_function_call_with_name(expr_token_to_run, token_constants.return_symbol_function_call_text, tokens)) {
        expr_pmml_str <- define_function.get_pmml_str_for_expr(
          function_call.get_function_arg_expr_tokens(expr_token_to_run, tokens)[1, ],
          tokens,
          orig_func_name,
          orig_func_param_tokens,
          is_last_expr
        )
      } else {
        expr_pmml_str <- define_function.get_pmml_str_for_token(expr_token_to_run, tokens, orig_func_name, orig_func_param_tokens, is_last_expr)
      }
      
      if(is.na(current_mapping$cond_expr_id) == FALSE) {
        pmml_str <- glue::glue('<Apply function="if">{cond_pmml_str}{expr_pmml_str}{pmml_str}</Apply>')
      } else {
        pmml_str <- expr_pmml_str
      }
    }
    
    return(pmml_str)
  }
}

define_function.get_pmml_str_for_expr <- function(expr, tokens, orig_func_name, orig_func_param_tokens, is_last_expr) {
  return(
    expr.generic_get_pmml_str_for_expr(
      get_pmml_str_for_row_access,
      function() {return("")},
      function(cond_expr_id_to_block_expr_ids_mappings) {
        return(get_pmml_str_for_if_expr(cond_expr_id_to_block_expr_ids_mappings, tokens, orig_func_name, orig_func_param_tokens, is_last_expr))
      }
    )(expr, tokens)
  )
}

define_function.get_pmml_str_for_token <- function(token, tokens, orig_func_name, orig_func_param_tokens, is_last_expr) {
  get_pmml_str_for_token <- pmml.generic_get_pmml_str_for_token(define_function.get_pmml_str_for_expr)
  
  return(get_pmml_str_for_token(token, tokens, tokens.create_empty_tokens_df(), list(),  orig_func_name, orig_func_param_tokens, is_last_expr))
}

getPmmlStringForExprTokenWithinFunction <- function(inner_func_name, innerFunctionExprToken, originalFunctionArgTokens, originalFunctionName, defaultedArgTokens, tokens) {
  pmmlStringForInitializationExprToken <- ''
  if(if_expr.is(innerFunctionExprToken, tokens)) {
    pmmlStringForInitializationExprToken <- define_function.get_pmml_str_for_expr(innerFunctionExprToken, tokens, originalFunctionName, originalFunctionArgTokens, FALSE)
  } else {
    #Get the expression token which has the initialization code
    initializationExprToken <- getTokenWithAssignmentCode(getChildTokensForParent(innerFunctionExprToken, tokens))
    
    pmmlStringForInitializationExprToken <- define_function.get_pmml_str_for_expr(initializationExprToken, tokens, originalFunctionName, FALSE)
  }

  originalFunctionArgNames <- originalFunctionArgTokens$text
  
  #Get all the symbols within this expression which are not part of the original function arguments. These need to be converted to Function calls in the PMML string for this expression
  symbolsNotPartOfOriginalFunctionArgs <- getSymbolsInTokens(getDescendantsOfToken(innerFunctionExprToken, tokens))
  symbolsNotPartOfOriginalFunctionArgs <- symbolsNotPartOfOriginalFunctionArgs[isLeftAssignmentSymbolToken(symbolsNotPartOfOriginalFunctionArgs, tokens) == FALSE, ]
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
      
      pmmlStringForFunctionCallForCurrentSymbol <- define_function.get_pmml_str_for_expr(tokensForRFunctionCallString[1, ], tokensForRFunctionCallString, originalFunctionName, FALSE)
      
      #Replace the function name with the actual one. We used the above one since R would have a problem with the one we use
      pmmlStringForFunctionCallForCurrentSymbol <- gsub(rFunctionName, getFunctionNameForInnerFunctionExprToken(originalFunctionName, symbolName), pmmlStringForFunctionCallForCurrentSymbol)
      
      pmmlStringForInitializationExprToken <- gsub(glue::glue('<FieldRef field="{symbolName}"/>'), pmmlStringForFunctionCallForCurrentSymbol, pmmlStringForInitializationExprToken)
    }
  }
  
  pmmlStringForInitializationExprToken <- getPmmlStringWithDefaultedArgsCorrectlySet(defaultedArgTokens, originalFunctionArgTokens, pmmlStringForInitializationExprToken)
  
  defineFunctionPmmlString <- pmmlStringForInitializationExprToken
  if(if_expr.is(innerFunctionExprToken, tokens) == FALSE) {
    defineFunctionPmmlString <- getPmmlStringForDefineFunction(inner_func_name, originalFunctionArgTokens, pmmlStringForInitializationExprToken)
  }
  
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
    
    # For this expression
    # 1. Get all the symbol tokens which are descendants of this expr and is
    # a function parameter
    # 2. Check whether this function parameter is used as a row from a # dataframe. If it is then add it to the list row_args
    # Step 1
    descendant_symbol_tokens <- getSymbolsInTokens(getDescendantsOfToken(func_body_expr, tokens))
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