# Get all the expr tokens which hold the function argument code
function_call_get_function_arg_expr_tokens <- function(func_call_expr, tokens) {
  child_expr_tokens <- get_expr_tokens(get_child_tokens_for_parent(func_call_expr, tokens))

  return(child_expr_tokens[-1, ])
}

function_call_get_function_name_token <- function(func_call_expr, tokens) {
  child_expr_tokens <- get_expr_tokens(get_child_tokens_for_parent(func_call_expr, tokens))

  return(
    get_child_tokens_for_parent(child_expr_tokens[1, ], tokens)[1, ]
  )
}

# Check whether the function which is called is one which is accessing
# a row
function_call_is_row_function_call_expr <- function(expr, tokens) {
  return(is_symbol_function_call_expr(expr, tokens) &
    globals_is_row_function(function_call_get_function_name_token(expr, tokens)$text))
}
