# Check whether this is a $ access expr for eg.
# row$col1
# table[col1 == "val1", ]$ col2
dollar_op.is_expr <- function(expr, tokens) {
  child_tokens <- getTokensWithParent(expr$id, tokens)
  
  return(!is.na(child_tokens[2, ]$token) & child_tokens[2, ]$token == "'$'")
}

dollar_op.get_pmml_node <- function(expr, tokens, innerText) {
  output_col <- getChildTokensForParent(expr, tokens)[3, 'text']
  
  return(glue::glue('<MapValues outputColumn="{output_col}">{innerText}</MapValues>'))
} 

# Get the variable which this $ operator is being used on
# For eg, for row$col, this function would return row
dollar_op.get_var <- function(expr, tokens) {
  symbol_token <- getChildTokensForParent(
    getChildTokensForParent(expr, tokens)[1, ], tokens)[1, ]
  
  if(isSymbolToken(symbol_token) == FALSE) {
    print(expr)
    print(tokens)
    stop("Trying to get variable for $ operator but this is not a column access")
  }
  
  return(symbol_token$text)
}

dollar_op.is_get_col_from_row_expr <- function(expr, tokens) {
  child_expr_tokens <- getExprTokens(getChildTokensForParent(expr, tokens))
  possible_row_var_symbol <- getChildTokensForParent(child_expr_tokens[1, ], tokens)[1, ]
  
  return(dollar_op.is_expr(expr, tokens) & isSymbolToken(possible_row_var_symbol))
} 