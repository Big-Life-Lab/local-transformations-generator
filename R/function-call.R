# Get the PMML string for the arguments passed into the function represented
# by the function call expr in the func_call_expr arg
function_call.get_pmml_str_for_args <- function(func_call_expr, tokens) {
  function_arg_expr_tokens <- function_call.get_function_arg_expr_tokens(func_call_expr, tokens)
  
  return(function_call.get_pmml_str_for_arg_exprs(
    function_arg_expr_tokens,
    tokens
  ))
}

# Get all the expr tokens which hold the function argument code
function_call.get_function_arg_expr_tokens <- function(func_call_expr, tokens) {
  child_expr_tokens <- getExprTokens(getChildTokensForParent(func_call_expr, tokens))
  
  return(child_expr_tokens[-1, ])
}

function_call.get_pmml_str_for_arg_exprs <- function(arg_expr_tokens, tokens) {
  functionArgsSymbolTokensPmmlString <- ''
  for(i in 1:nrow(arg_expr_tokens)) {
    functionArgsSymbolTokensPmmlString <- paste(
      functionArgsSymbolTokensPmmlString,
      getPmmlStringForExpr(arg_expr_tokens[i, ], tokens),
      sep=''
    )
  }
  
  return(functionArgsSymbolTokensPmmlString)
}

function_call.get_function_name_token <- function(func_call_expr, tokens) {
  child_expr_tokens <- getExprTokens(getChildTokensForParent(func_call_expr, tokens))

  return(
    getChildTokensForParent(child_expr_tokens[1, ], tokens)[1, ]
  )
}

# Check whether the function which is called is one which is accessing
# a row 
function_call.is_row_function_call_expr <- function(expr, tokens) {
  return(isSymbolFunctionCallExpr(expr, tokens) &
    globals.is_row_function(function_call.get_function_name_token(expr, tokens)$text))
}