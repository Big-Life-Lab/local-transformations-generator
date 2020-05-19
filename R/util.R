source('R/token_to_pmml.R')
source(file.path(getwd(), 'R/pmml-custom-func.R'))

util.get_var_and_func_names <- function(tokens) {
  leftAssignTokens <- tokens[which(tokens$token == LEFT_ASSIGN_TOKEN), ]
  
  if(nrow(leftAssignTokens) == 0) {
    return(leftAssignTokens)
  }
  
  var_and_func_names <- c()
  func_expr_tokens <- tokens.create_empty_tokens_df()
  for(i in 1:nrow(leftAssignTokens)) {
    leftAssignToken <- leftAssignTokens[i, ]
    
    is_within_function <- FALSE
    if(nrow(func_expr_tokens) != 0) {
      for(j in 1:nrow(func_expr_tokens)) {
        if(isDescendantOfTokenWithId(
          func_expr_tokens[j, "id"], leftAssignToken, tokens)) {
          is_within_function <- TRUE
          break;
        } 
      }  
    }
    
    if(is_within_function == FALSE) {
      parent_token <- getParentToken(leftAssignToken, tokens)
      child_tokens <- getChildTokensForParent(parent_token, tokens)
      var_or_func_name_expr_token <- child_tokens[1, ]
      var_or_func_name_symbol_token <- getChildTokensForParent(
        var_or_func_name_expr_token,
        tokens
      )[1, ]
      var_and_func_names <- c(
        var_and_func_names,
        var_or_func_name_symbol_token$text
      )
      
      if(define_function.is(child_tokens[3, ], tokens)) {
        func_expr_tokens <- rbind(func_expr_tokens, child_tokens[3, ])
      }
    }
  }
  
  return(var_and_func_names)
}

getTokenWithAssignmentCode <- function(tokens) {
  leftAssignToken <- tokens[which(tokens$token == LEFT_ASSIGN_TOKEN), ][1, ]
  
  if(is.na(leftAssignToken$id)) {
    return(NA)
  }
  
  return(getTokenAfterTokenWithId(tokens, leftAssignToken$id))
}