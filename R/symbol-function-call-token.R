symbol_function_call_token_is_expr_symbol_function_call_with_name <- function(expr, func_name, tokens) {
  child_expr_tokens <- get_expr_tokens(get_child_tokens_for_parent(expr, tokens))

  if(nrow(child_expr_tokens) == 0) {
    return(FALSE)
  }

  possible_symbol_function_call_token <- get_child_tokens_for_parent(child_expr_tokens[1, ], tokens)[1, ]

  if(possible_symbol_function_call_token$token != SYMBOL_FUNCTION_CALL_TOKEN) {
    return(FALSE)
  }

  return(possible_symbol_function_call_token$text == func_name)
}
