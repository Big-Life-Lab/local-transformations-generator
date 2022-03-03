# Check whether this is a $ access expr for eg.
# row$col1
# table[col1 == "val1", ]$ col2
dollar_op_is_expr <- function(expr, tokens) {
  child_tokens <- get_tokens_with_parent(expr$id, tokens)

  return(!is.na(child_tokens[2, ]$token) & child_tokens[2, ]$token == "'$'")
}

dollar_op_get_pmml_node <- function(expr, tokens, innerText) {
  output_col <- dollar_op_get_output_col(expr, tokens)

  return(glue::glue('<MapValues outputColumn="{output_col}">{innerText}</MapValues>'))
}

dollar_op_get_output_col <- function(expr, tokens) {
  return(get_child_tokens_for_parent(expr, tokens)[3, 'text'])
}

# Get the variable which this $ operator is being used on
# For eg, for row$col, this function would return row
dollar_op_get_var <- function(expr, tokens) {
  symbol_token <- get_child_tokens_for_parent(
    get_child_tokens_for_parent(expr, tokens)[1, ], tokens)[1, ]

  if(is_symbol_token(symbol_token) == FALSE) {
    stop("Trying to get variable for $ operator but this is not a column access")
  }

  return(symbol_token$text)
}

dollar_op_is_get_col_from_row_expr <- function(expr, tokens) {
  child_expr_tokens <- get_expr_tokens(get_child_tokens_for_parent(expr, tokens))
  possible_row_var_symbol <- get_child_tokens_for_parent(child_expr_tokens[1, ], tokens)[1, ]

  return(dollar_op_is_expr(expr, tokens) & is_symbol_token(possible_row_var_symbol))
}
