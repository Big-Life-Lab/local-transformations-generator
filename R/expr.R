expr.generic_get_pmml_str_for_expr <- function(
  get_pmml_str_for_row_access,
  get_pmml_str_for_func_call_row_access,
  get_pmml_str_for_if_expr) {
  get_pmml_str_for_expr <- function(expr, tokens) {
    tokensWhoseParentIsTheCurrentExpr = getTokensWithParent(expr$id, tokens)
    tokensWhoseParentIsTheCurrentExprHasOneRow <- (nrow(tokensWhoseParentIsTheCurrentExpr) != 0)
    
    childSpecialTokensForCurrentExpr <- getSpecialTokens(getChildTokensForParent(expr, tokens))
    
    if(nrow(childSpecialTokensForCurrentExpr) != 0) {
      if(childSpecialTokensForCurrentExpr[1, 'text'] == '%in%') {
        childExprTokens <- getExprTokens(getChildTokensForParent(expr, tokens))
        leftExprTokenPmmlString <- get_pmml_str_for_expr(childExprTokens[1, ], tokens)
        rightExprTokenPmmlString <- get_pmml_str_for_expr(childExprTokens[2, ], tokens)
        
        return(glue::glue('<Apply function="isIn">{leftExprTokenPmmlString}{rightExprTokenPmmlString}</Apply>'))
      }
      else {
        stop(glue::glue('Unhandled special symbol {childSpecialTokensForCurrentExpr[1, "text"]}'))
      }
    }
    else if(if_expr.is(expr, tokens)) {
      cond_expr_to_block_exprs_mappings <- if_expr.get_cond_expr_to_block_exprs_map(expr, tokens)
      get_pmml_str_for_if_expr(cond_expr_to_block_exprs_mappings)
    }
    else if(data_frame.is_row_access(expr, tokens)) {
      return(get_pmml_str_for_row_access(expr, tokens))
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
    else {
      exprTokensWhoseParentIsTheCurrentExpr = getExprTokens(tokensWhoseParentIsTheCurrentExpr)
      nonExprTokensWhoseParentIsTheCurrentExpr = filterOutExprTokens(tokensWhoseParentIsTheCurrentExpr)
      pmmlStringForExprTokens <- ''
      
      if(nrow(exprTokensWhoseParentIsTheCurrentExpr) != 0) {
        # If this expression has a function call in it
        if(isSymbolFunctionCallExpr(expr, tokens)) {
          if(function_call.is_row_function_call_expr(expr, tokens)) {
            return(get_pmml_str_for_func_call_row_access(expr, tokens))
          }
          
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
            # Get the PMML string for the arguments passed into the function represented
            # by the function call expr in the func_call_expr arg
            function_arg_expr_tokens <- function_call.get_function_arg_expr_tokens(expr, tokens)
            functionArgsSymbolTokensPmmlString <- ''
            for(i in 1:nrow(function_arg_expr_tokens)) {
              functionArgsSymbolTokensPmmlString <- paste(
                functionArgsSymbolTokensPmmlString,
                get_pmml_str_for_expr(function_arg_expr_tokens[i, ], tokens),
                sep=''
              )
            }
            
            return(getPmmlStringForSymbolFunctionCall(functionSymbolToken, functionArgsSymbolTokensPmmlString))
          }
        } else {
          for(i in 1:nrow(exprTokensWhoseParentIsTheCurrentExpr)) {
            pmmlStringForExprTokens <- paste(
              pmmlStringForExprTokens,
              get_pmml_str_for_expr(exprTokensWhoseParentIsTheCurrentExpr[i, ], tokens),
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
  
  return(get_pmml_str_for_expr)
}