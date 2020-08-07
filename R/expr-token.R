expr_token_is_assignment_expr <- function(expr, tokens) {
  child_tokens <- get_child_tokens_for_parent(expr, tokens)

  if(nrow(child_tokens) != 3) {
    return(FALSE)
  }

  return(child_tokens[2, ]$token == LEFT_ASSIGN_TOKEN)
}
