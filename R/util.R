util_get_var_and_func_names <- function(tokens) {
  leftAssignTokens <- tokens[which(tokens$token == LEFT_ASSIGN_TOKEN), ]

  if(nrow(leftAssignTokens) == 0) {
    return(leftAssignTokens)
  }

  var_and_func_names <- c()
  func_expr_tokens <- tokens_create_empty_tokens_df()
  for(i in 1:nrow(leftAssignTokens)) {
    left_assign_token <- leftAssignTokens[i, ]

    is_within_function <- FALSE
    if(nrow(func_expr_tokens) != 0) {
      for(j in 1:nrow(func_expr_tokens)) {
        if(is_descendant_of_token_with_id(
          func_expr_tokens[j, "id"], left_assign_token, tokens)) {
          is_within_function <- TRUE
          break;
        }
      }
    }

    if(is_within_function == FALSE) {
      child_tokens <- tokens_get_child_tokens_for_parent_id(left_assign_token$parent, tokens)
      var_or_func_name_expr_token <- child_tokens[1, ]
      var_or_func_name_symbol_token <- get_child_tokens_for_parent(
        var_or_func_name_expr_token,
        tokens
      )[1, ]
      var_and_func_names <- c(
        var_and_func_names,
        var_or_func_name_symbol_token$text
      )

      if(define_function_is(child_tokens[3, ], tokens)) {
        func_expr_tokens <- rbind(func_expr_tokens, child_tokens[3, ])
      }
    }
  }

  return(var_and_func_names)
}

get_token_with_assignment_code <- function(tokens) {
  left_assign_token <- tokens[which(tokens$token == LEFT_ASSIGN_TOKEN), ][1, ]

  if(is.na(left_assign_token$id)) {
    return(tokens_create_empty_tokens_df())
  }

  return(get_token_after_token_with_id(tokens, left_assign_token$id))
}
