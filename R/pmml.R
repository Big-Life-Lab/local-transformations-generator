pmml.generic_get_pmml_str_for_token <- function(get_pmml_str_for_expr) {
  get_pmml_str_for_token <- function(token, tokens, comment_tokens, evaluated_variables, ...) {
    # If there's a custom pmml function comment for this expression set it to this variable
    custom_pmml_func_comment_token <- NA
    if (nrow(comment_tokens) != 0) {
      custom_pmml_func_comment_token <-
        get_custom_pmml_func_comment_token(comment_tokens)
    }
    # If this line needs to be converted to a custom pmml expression
    if (!is.na(custom_pmml_func_comment_token)) {
      return(
        get_pmml_node_for_pmml_func_comment_token(custom_pmml_func_comment_token,
                                                  evaluated_variables)
      )
    } else if (token$token == EXPR_TOKEN) {
      return(get_pmml_str_for_expr(
        token,
        tokens,
        ...
      ))
    }
    else if (token$token == NUM_CONST_TOKEN |
             token$token == STR_CONST_TOKEN | token$token == NULL_CONST_TOKEN) {
      return(getPmmlStringForConstant(token))
    } else if (token$token == SYMBOL_TOKEN) {
      return(getPmmlStringForSymbol(token))
    } else {
      stop(glue::glue('Unhandled token type {token$token}'))
    }
  }
  
  return(get_pmml_str_for_token)
}