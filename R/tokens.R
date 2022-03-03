LEFT_ASSIGN_TOKEN <- 'LEFT_ASSIGN'
EXPR_TOKEN <- 'expr'
SYMBOL_TOKEN <- 'SYMBOL'
NUM_CONST_TOKEN <- 'NUM_CONST'
STR_CONST_TOKEN <- 'STR_CONST'
NULL_CONST_TOKEN <- 'NULL_CONST'
MATH_TOKENS <- c("'+'", "'-'", "'*'", "'/'", "'^'")
COMMENT_TOKEN <- 'COMMENT'
EQUAL_TO_TOKEN <- 'EQ'
LESS_THAN_OR_EQUAL_TO_TOKEN <- 'LE'
GREATER_THAN_OR_EQUAL_TO_TOKEN <- 'GE'
GREATER_THAN_TOKEN <- 'GT'
LESS_THAN_TOKEN <- 'LT'
AND_TOKEN <- 'AND'
AND2_TOKEN <- 'AND2'
OR_TOKENS <- c('OR', 'OR2')
NOT_EQUAL_TO_TOKEN <- 'NE'
NOT_TOKEN <- "'!'"
LOGICAL_TOKENS <- c(EQUAL_TO_TOKEN, LESS_THAN_OR_EQUAL_TO_TOKEN, GREATER_THAN_OR_EQUAL_TO_TOKEN, GREATER_THAN_TOKEN, LESS_THAN_TOKEN, AND_TOKEN, OR_TOKENS, NOT_EQUAL_TO_TOKEN, NOT_TOKEN, AND2_TOKEN)

IF_TOKEN <- 'IF'
ELSE_TOKEN <- 'ELSE'
SYMBOL_FUNCTION_CALL_TOKEN <- 'SYMBOL_FUNCTION_CALL'
SYMBOL_PACKAGE <- 'SYMBOL_PACKAGE'

FUNCTION_TOKEN <- 'FUNCTION'
SYMBOL_FORMALS_TOKEN <- 'SYMBOL_FORMALS'
EQ_FORMALS <- 'EQ_FORMALS'

SPECIAL_TOKEN <- 'SPECIAL'

COMMENT_TOKEN <- 'COMMENT'

is_symbol_token <- function(token) {
  return(token$token == SYMBOL_TOKEN)
}

get_expr_with_id_in_tokens <- function(id, tokens) {
  return(tokens[which(tokens$token==EXPR_TOKEN & tokens$id == id), ])
}

get_symbols_in_tokens <- function(tokens) {
  return(tokens[which(tokens$token==SYMBOL_TOKEN), ])
}

get_symbol_formals_tokens <- function(tokens) {
  return(tokens[which(tokens$token==SYMBOL_FORMALS_TOKEN), ])
}

get_tokens_with_parent <- function(parent, tokens) {
  return(tokens[which(tokens$parent==parent), ])
}

get_comment_tokens_with_parent <- function(parent_id, tokens) {
  neg_parent_id <- parent_id*-1;

  return(tokens[which(tokens$parent == neg_parent_id & tokens$token == COMMENT_TOKEN), ])
}

get_token_with_id <- function(id, tokens) {
  return(tokens[which(tokens$id == id), ])
}

get_function_tokens <- function(tokens) {
  return(tokens[which(tokens$token == FUNCTION_TOKEN), ])
}

get_child_tokens_for_parent <- function(parent, tokens) {
  return(tokens[which(tokens$parent == parent$id), ])
}

get_special_tokens <- function(tokens) {
  return(tokens[which(tokens$token == SPECIAL_TOKEN), ])
}

#Returns the token which is the parent of the child token argument
get_parent_token <- function(childNode, nodes) {
  return(nodes[nodes$id %in% childNode$parent, ][1, ])
}

#Checks if the node arg is a descendant of the node with id provided in the id arg from the nodes arg
is_descendant_of_token_with_id <- function(id, node, nodes) {
  #If this is the root node return false because we have reached the beginning of the tree
  if(node$parent == 0) {
    return(FALSE)
  }
  #if this is a direct child of the parent return true
  else if(node$parent == id) {
    return(TRUE)
  }
  #Otherwise
  else {
    #Get the parent of the node
    parent_node <- get_parent_token(node, nodes)
    #If the parent does not exist
    if(is.na(parent_node$id) == TRUE) {
      return(FALSE)
    }

    #Check if th parent is a descendant
    return(is_descendant_of_token_with_id(id, parent_node, nodes))
  }
}

#Returns all the descendants of the node arg from the nodes arg
get_descendants_of_token <- function(node, nodes) {
  #The id fields of all the nodes which are descendants
  descendant_ids <- c()

  #Go thorugh all the nodes and each one which is a descendant add it's id to the descendant_ids vector
  if(nrow(nodes) > 0) {
    for(i in 1:nrow(nodes)) {
      if(is_descendant_of_token_with_id(node$id, nodes[i, ], nodes)) {
        descendant_ids <- c(descendant_ids, nodes[i, 'id'])
      }
    }
  }

  #Returns all nodes whose id field is part of the descendant_ids vector
  return(nodes[which(nodes$id %in% descendant_ids), ])
}

get_expr_tokens <- function(tokens) {
  return(tokens[which(tokens$token==EXPR_TOKEN), ])
}

filter_out_expr_tokens <- function(tokens) {
  return(tokens[which(tokens$token != EXPR_TOKEN), ])
}

filter_out_comment_tokens <- function(tokens) {
  return(tokens[which(tokens$token != COMMENT_TOKEN), ])
}

get_symbol_function_calls_with_text <- function(text, tokens) {
  return(tokens[which(tokens$token == SYMBOL_FUNCTION_CALL_TOKEN & tokens$text == text), ])
}

does_tokens_have_row_with_token <- function(tokens, token) {
  if(nrow(tokens[which(tokens$token==token), ]) == 0) {
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}

does_tokens_have_a_left_assign <- function(tokens) {
  return(does_tokens_have_row_with_token(tokens, LEFT_ASSIGN_TOKEN))
}

does_tokens_have_source_function_call <- function(tokens) {
  return(nrow(get_symbol_function_calls_with_text('source', tokens)) == 1)
}

does_tokens_have_function_definition <- function(tokens) {
  return(does_tokens_have_row_with_token(tokens, FUNCTION_TOKEN))
}

isleft_assign_expr_token <- function(expr_token, tokens) {
  child_tokens_for_expr_token <- get_child_tokens_for_parent(expr_token, tokens)

  return(does_tokens_have_a_left_assign(child_tokens_for_expr_token))
}

#Is the symbol_token argument part of a left assign expression and is it the symbol to which a value is being assigned
#eg. test <- 1. Here if the test symbol token was passed as the symbol_token argument the function would return true
is_left_assignment_symbol_token <- function(symbol_token, tokens) {
  parent_token_for_symbol_token <- get_parent_token(symbol_token, tokens)

  if(isleft_assign_expr_token(get_parent_token(parent_token_for_symbol_token, tokens), tokens)) {
    child_tokens_for_left_assign_expr_token <- get_child_tokens_for_parent(parent_token_for_symbol_token, tokens)
    return(child_tokens_for_left_assign_expr_token[1, 'id'] == symbol_token$id)
  } else {
    return(FALSE)
  }
}

# Returns the token row that comes after the token with id the same as the id arg
get_token_after_token_with_id <- function(tokens, id) {
  token_index <- NA

  for(i in 1:nrow(tokens)) {
    if(tokens[i, 'id'] == id) {
      token_index <- i
      break
    }
  }

  if(is.na(token_index)) {
    stop(glue::glue('No token with id {id} found'))
  } else {
    return(tokens[i+1, ])
  }
}

tokens_create_empty_tokens_df <- function() {
  df <- data.frame(matrix(ncol = 9, nrow = 0))
  colnames(df) <- c("line1", "col1", "line2", "col2", "id", "parent", "token", "terminal", "text")

  return(df)
}

tokens_get_child_tokens_for_parent_id <- function(parent_id, tokens) {
  return(tokens[tokens$parent == parent_id, ])
}
