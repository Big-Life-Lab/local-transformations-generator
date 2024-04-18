define_function_get_pmml_string <- function(tokens, function_name) {
  function_tokens <- get_function_tokens(tokens)
  if(nrow(function_tokens) > 1) {
    stop('Too many function tokens found within function definition')
  }

  function_token_parent_expr_id <- function_tokens[1, ]$parent

  function_expr_token <- get_token_with_id(function_token_parent_expr_id, tokens)

  function_definition_tokens <- get_descendants_of_token(function_expr_token, tokens)

  #The names of the functions arguments
  function_param_name_tokens <- get_symbol_formals_tokens(tokens)

  #Expression tokens which are children of the function expr. The expr tokens within the function could be one of the following:
  #1. Expr for default values assigned to an argument
  #2. Expr for the function body which is what we want
  # The function body expr will be at the end so get the last row in the data frame consisting of all the expr tokens
  function_body_expr_token <- tail(get_expr_tokens(get_child_tokens_for_parent(function_expr_token, tokens)), n=1)

  #Get all the tokens which together make up the function body
  function_body_expr_token_descendants = get_descendants_of_token(function_body_expr_token, tokens)

  #Get the top level expr tokens for the function body. These are the tokens which hold all the logic in the function body as well as the return call
  top_level_function_body_expr_tokens <- get_expr_tokens(get_child_tokens_for_parent(function_body_expr_token, tokens))

  row_args <- define_function_get_row_args(top_level_function_body_expr_tokens, function_param_name_tokens$text, tokens)

  pmml_function_string <- ''

  defaulted_args <- data.frame()
  if(nrow(function_param_name_tokens) != 0) {
    for(i in 1:nrow(function_param_name_tokens)) {
      default_function_pmml_string_for_current_arg <- get_define_function_for_default_arg_expr(function_param_name_tokens[i, ], function_param_name_tokens, tokens)

      if(default_function_pmml_string_for_current_arg != '') {
        if(function_param_name_tokens[i, "text"] %in% row_args) {
          stop("Argument which is used as a row has a default value")
        }
        defaulted_args <- rbind(defaulted_args, function_param_name_tokens[i, ])
        pmml_function_string <- glue::glue(pmml_function_string, default_function_pmml_string_for_current_arg)
      }
    }
  }

  # names of variables that are in the local scope of this function
  function_scope_variables <- c(function_param_name_tokens$text)
  for(i in 1:nrow(top_level_function_body_expr_tokens)) {
    if(i != nrow(top_level_function_body_expr_tokens)) {
      inner_func_expr <- top_level_function_body_expr_tokens[i, ]
      inner_func_name <- define_function_get_inner_func_name(inner_func_expr, tokens, function_name)
      pmml_function_string <- paste(
        pmml_function_string,
        get_pmml_string_for_expr_token_within_function(
          inner_func_name,
          top_level_function_body_expr_tokens[i, ],
          function_param_name_tokens,
          function_name,
          function_scope_variables,
          defaulted_args,
          tokens
        ),
        sep=''
      )
    }
    #It's the last expression so it has to be a function return call
    else {
      return_arg_expr_token <- top_level_function_body_expr_tokens[i, ]
      pmml_string_for_return_arg_expr_token <- ''
      if(if_expr_is(return_arg_expr_token, tokens)) {
        pmml_string_for_return_arg_expr_token <- define_function_get_pmml_str_for_expr(
          return_arg_expr_token, tokens, function_name, function_param_name_tokens, TRUE, function_scope_variables
        )
      } else {
        # For the first way if there is a left assign then we need to set the return expr to the expr token which is the right hand side of the assignment
        child_tokens_for_return_arg_expr_token <- get_child_tokens_for_parent(return_arg_expr_token, tokens)
        # Check if there's a left assign. If there is then the right hand assignment expr token is the third child
        if(does_tokens_have_a_left_assign(child_tokens_for_return_arg_expr_token)) {
          return_arg_expr_token <- child_tokens_for_return_arg_expr_token[3, ]
        }
        # Check if it's the second way and if it is
        else if(nrow(get_symbol_function_calls_with_text('return', get_descendants_of_token(return_arg_expr_token, tokens))) == 1) {
          #Get the expression for the argument to the return function call
          return_arg_expr_token <- get_expr_tokens(get_child_tokens_for_parent(top_level_function_body_expr_tokens[i, ], tokens))[2, ]
        }

        #Convert the expression to it's PMML string
        pmml_string_for_return_arg_expr_token <- define_function_get_pmml_str_for_expr(
          return_arg_expr_token,
          get_descendants_of_token(return_arg_expr_token, tokens),
          function_name,
          function_param_name_tokens,
          TRUE,
          function_scope_variables
        )
      }

      symbols_within_return_arg_expr_which_are_not_function_arguments <- define_function_get_symbols_to_convert_to_func_calls(
        function_param_name_tokens$text,
        return_arg_expr_token,
        tokens
      )

      if(nrow(symbols_within_return_arg_expr_which_are_not_function_arguments) != 0) {
        #1. Convert each symbol into R code which calls a function whose name is a combination of the symbol and original function name and whose arguments are the
        # original function arguments and generate PMML code for it
        #2. Replace each FieldRef within the above PMML string for each symbol with the generated PMML string for that function call R code
        for(j in 1:nrow(symbols_within_return_arg_expr_which_are_not_function_arguments)) {
          # 1.
          r_function_name_for_current_symbol <- glue::glue('{function_name}_{symbols_within_return_arg_expr_which_are_not_function_arguments[j, "text"]}')
          r_function_args <- get_r_arguments_into_function_string(function_param_name_tokens)
          r_code <- glue::glue('{r_function_name_for_current_symbol}({r_function_args})')
          tokens_for_r_Code <- getParseData(parse(text = r_code, keep.source = TRUE))
          pmml_string_for_r_code <- define_function_get_pmml_str_for_expr(tokens_for_r_Code[1, ], tokens_for_r_Code, function_name, function_param_name_tokens, TRUE, function_scope_variables)
          pmml_string_for_r_code <- gsub(
            r_function_name_for_current_symbol,
            get_function_name_for_inner_function_expr_token(function_name, symbols_within_return_arg_expr_which_are_not_function_arguments[j, 'text']),
            pmml_string_for_r_code
          )

          # 2.
          pmml_string_for_return_arg_expr_token <- gsub(
            glue::glue('<FieldRef field="{symbols_within_return_arg_expr_which_are_not_function_arguments[j, ]$text}"/>'),
            pmml_string_for_r_code,
            pmml_string_for_return_arg_expr_token
          )
        }
      }

      pmml_string_for_return_arg_expr_token <- get_pmml_string_with_defaulted_args_correctly_set(
        defaulted_args,
        function_param_name_tokens,
        pmml_string_for_return_arg_expr_token
      )

      #Make the DefineFunction PMML string
      pmml_define_function_string <- get_pmml_string_for_define_function(
        function_name,
        function_param_name_tokens,
        pmml_string_for_return_arg_expr_token
      )

      #Add it to the pmml_function_string
      pmml_function_string <- paste(pmml_function_string, pmml_define_function_string, sep='')
    }
  }

  if(nrow(defaulted_args) > 0) {
    globals_add_default_param_function(function_name, nrow(function_param_name_tokens), nrow(defaulted_args))
  }

  return(pmml_function_string)
}

get_pmml_str_for_row_access <- function(expr, tokens, scope_variables) {
  inner_text <- NULL
  table_expr <- get_table_expression(expr, tokens)
  # Get the expression for the data frame that holds the output column
  if(is_symbol_function_call_expr(table_expr, tokens)) {
    inner_text <- glue::glue("<TableLocator>{define_function_get_pmml_str_for_expr(table_expr, tokens, '', data.frame(), FALSE, scope_variables)}</TableLocator>")
  }
  else {
    row_var_name <- dollar_op_get_var(expr, tokens)
    if(row_var_name %in% scope_variables) {
      inner_text <- glue::glue('<TableLocator location="local" name="{row_var_name}" />')
    } else {
      inner_text <- glue::glue('<TableLocator location="taxonomy" name="{row_var_name}" />')
    }
  }

  return(dollar_op_get_pmml_node(expr, tokens, inner_text))
}


get_pmml_str_for_if_expr <- 
    function(cond_expr_to_block_exprs_mappings, 
             tokens, 
             orig_func_name, 
             orig_func_param_tokens, 
             is_last_expr,
             function_scope_variables) {
  if(is_last_expr == FALSE) {
    var_name_to_if_expr_mappings <- list()
    for(i in 1:length(cond_expr_to_block_exprs_mappings)) {
      current_map <- cond_expr_to_block_exprs_mappings[[i]]

      for(j in 1:length(current_map$block_expr_ids)) {
        current_block_expr_id <- current_map$block_expr_ids[[j]]

        var_name <- util_get_var_and_func_names(
          get_descendants_of_token(get_token_with_id(current_block_expr_id, tokens), tokens)
        )[[1]]
        new_var_name_to_if_expr_map <- list(
          cond_expr_id = current_map$cond_expr_id,
          expr_id = current_block_expr_id
        )

        current_var_name_mapping <- var_name_to_if_expr_mappings[[var_name]]
        if(is.null(current_var_name_mapping)) {
          var_name_to_if_expr_mappings[[var_name]] <- list()
          var_name_to_if_expr_mappings[[var_name]][[1]] <- new_var_name_to_if_expr_map
        } else {
          current_var_name_mapping[[length(current_var_name_mapping) + 1]] <- new_var_name_to_if_expr_map
          var_name_to_if_expr_mappings[[var_name]] <- current_var_name_mapping
        }
      }
    }

    var_names <- names(var_name_to_if_expr_mappings)
    pmml_str <- ''
    for(i in 1:length(var_names)) {
      pmml_str_for_var <- ''

      cur_var_name <- var_names[[i]]

      reverse_cond_expr_mappings <- rev(var_name_to_if_expr_mappings[[cur_var_name]])
      if(is.na(reverse_cond_expr_mappings[[1]]$cond_expr_id) == FALSE) {
        pmml_str_for_var <- '<Constant dataType="NULL">NULL</Constant>'
      }
      for(j in 1:length(reverse_cond_expr_mappings)) {
        cur_cond_expr_mapping <- reverse_cond_expr_mappings[[j]]

        pmml_str_for_cond <- ''
        if(is.na(cur_cond_expr_mapping$cond_expr_id) == FALSE) {
          pmml_str_for_cond <- define_function_get_pmml_str_for_expr(
            get_token_with_id(cur_cond_expr_mapping$cond_expr_id, tokens),
            tokens,
            orig_func_name,
            orig_func_param_tokens,
            is_last_expr,
            function_scope_variables
          )
        }
        pmml_str_for_expr <- define_function_get_pmml_str_for_token(
          get_token_with_assignment_code(
            get_descendants_of_token(get_token_with_id(cur_cond_expr_mapping$expr_id, tokens), tokens)
          ),
          tokens,
          orig_func_name,
          orig_func_param_tokens,
          is_last_expr
        )

        if(is.na(cur_cond_expr_mapping$cond_expr_id) == FALSE) {
          pmml_str_for_var <- glue::glue('<Apply function="if">{pmml_str_for_cond}{pmml_str_for_expr}{pmml_str_for_var}</Apply>')
        } else {
          pmml_str_for_var <- pmml_str_for_expr
        }
      }

      inner_func_name <- glue::glue("{orig_func_name}({cur_var_name})")
      pmml_str <- paste(pmml_str, get_pmml_string_for_define_function(inner_func_name, orig_func_param_tokens, pmml_str_for_var), sep = '')
    }

    return(pmml_str)
  } else {
    new_cond_expr_to_block_expr_mappings <- list()
    for(i in 1:length(cond_expr_to_block_exprs_mappings)) {
      current_mapping <- cond_expr_to_block_exprs_mappings[[i]]
      new_cond_expr_to_block_expr_map <- list(
        cond_expr_id = current_mapping$cond_expr_id,
        expr_id = current_mapping$block_expr_ids[[length(current_mapping$block_expr_ids)]]
      )

      new_cond_expr_to_block_expr_mappings[[length(new_cond_expr_to_block_expr_mappings) + 1]] <- new_cond_expr_to_block_expr_map
    }

    pmml_str <- ''
    rev_cond_expr_to_block_expr_maps <- rev(new_cond_expr_to_block_expr_mappings)
    for(i in 1:length(rev_cond_expr_to_block_expr_maps)) {
      current_mapping <- rev_cond_expr_to_block_expr_maps[[i]]

      if(i == 1 & is.na(current_mapping$cond_expr_id) == FALSE) {
        pmml_str <- '<Constant dataType="NULL">NULL</Constant>'
      }

      cond_pmml_str <- NA
      if(is.na(current_mapping$cond_expr_id) == FALSE) {
        cond_pmml_str <- define_function_get_pmml_str_for_expr(
          get_token_with_id(current_mapping$cond_expr_id, tokens),
          tokens,
          orig_func_name,
          orig_func_param_tokens,
          is_last_expr,
          function_scope_variables
        )
      }

      expr_pmml_str <- ''
      expr_token_to_run <- get_token_with_id(current_mapping$expr_id, tokens)

      if(expr_token_is_assignment_expr(expr_token_to_run, tokens)) {
        assignment_token <- get_token_with_assignment_code(
          get_descendants_of_token(expr_token_to_run, tokens)
        )

        expr_pmml_str <- define_function_get_pmml_str_for_token(
          assignment_token, tokens, orig_func_name, orig_func_param_tokens, is_last_expr
        )
      } else if(symbol_function_call_token_is_expr_symbol_function_call_with_name(expr_token_to_run, token_constants_return_symbol_function_call_text, tokens)) {
        expr_pmml_str <- define_function_get_pmml_str_for_expr(
          function_call_get_function_arg_expr_tokens(expr_token_to_run, tokens)[1, ],
          tokens,
          orig_func_name,
          orig_func_param_tokens,
          is_last_expr,
          function_scope_variables
        )
      } else {
        expr_pmml_str <- define_function_get_pmml_str_for_token(expr_token_to_run, tokens, orig_func_name, orig_func_param_tokens, is_last_expr)
      }

      if(is.na(current_mapping$cond_expr_id) == FALSE) {
        pmml_str <- glue::glue('<Apply function="if">{cond_pmml_str}{expr_pmml_str}{pmml_str}</Apply>')
      } else {
        pmml_str <- expr_pmml_str
      }
    }

    return(pmml_str)
  }
}

define_function_get_pmml_str_for_expr <- function(
    expr,
    tokens,
    orig_func_name,
    orig_func_param_tokens,
    is_last_expr,
    function_scope_variables
  ) {
  return(
    expr_generic_get_pmml_str_for_expr(
      get_pmml_str_for_row_access,
      function() {return("")},
      function(cond_expr_id_to_block_expr_ids_mappings) {
        return(get_pmml_str_for_if_expr(cond_expr_id_to_block_expr_ids_mappings, tokens, orig_func_name, orig_func_param_tokens, is_last_expr, function_scope_variables))
      }
    )(expr, tokens, function_scope_variables)
  )
}

define_function_get_pmml_str_for_token <- function(token, tokens, orig_func_name, orig_func_param_tokens, is_last_expr) {
  get_pmml_str_for_token <- pmml_generic_get_pmml_str_for_token(define_function_get_pmml_str_for_expr)

  return(get_pmml_str_for_token(token, tokens, tokens_create_empty_tokens_df(), list(),  orig_func_name, orig_func_param_tokens, is_last_expr))
}

get_pmml_string_for_expr_token_within_function <- 
    function(inner_func_name, 
             inner_function_expr_token, 
             original_function_arg_tokens, 
             original_function_name, 
             function_scope_variables,
             defaulted_arg_tokens, 
             tokens
             ) {
  pmml_string_for_initialization_expr_token <- ''
  if(if_expr_is(inner_function_expr_token, tokens)) {
    pmml_string_for_initialization_expr_token <- define_function_get_pmml_str_for_expr(inner_function_expr_token, tokens, original_function_name, original_function_arg_tokens, FALSE, function_scope_variables)
  } else {
    #Get the expression token which has the initialization code
    initialization_expr_token <- get_token_with_assignment_code(get_child_tokens_for_parent(inner_function_expr_token, tokens))

    pmml_string_for_initialization_expr_token <- define_function_get_pmml_str_for_expr(initialization_expr_token, tokens, original_function_name, original_function_arg_tokens, FALSE, function_scope_variables)
  }

  original_function_arg_names <- original_function_arg_tokens$text

  symbols_not_part_of_original_function_args <- define_function_get_symbols_to_convert_to_func_calls(original_function_arg_tokens$text, inner_function_expr_token, tokens)

  if(nrow(symbols_not_part_of_original_function_args) != 0) {
    #For every symbol not part of the original function argument we
    #1. Make a string that has R code for the a function call with the orignal function arguments and the function name for this symbol we defined
    #2. Generate a PMML string for the function call code
    #3. Replace every FieldRef for this symbol with the function call PMML string
    for(i in 1:nrow(symbols_not_part_of_original_function_args)) {
      #The r string for the arguments into the function
      r_arguments_into_function_string <- get_r_arguments_into_function_string(original_function_arg_tokens)

      symbol_name <- symbols_not_part_of_original_function_args[i, 'text']

      r_function_name <- glue::glue('{original_function_name}_{symbols_not_part_of_original_function_args[i, "text"]}')
      r_function_call_string_for_current_symbol <- glue::glue('{r_function_name}({r_arguments_into_function_string})')
      tokens_for_r_function_call_string <- getParseData(parse(text = r_function_call_string_for_current_symbol))

      pmmlStringFor_function_call_for_current_symbol <- define_function_get_pmml_str_for_expr(tokens_for_r_function_call_string[1, ], tokens_for_r_function_call_string, original_function_name, original_function_arg_tokens, FALSE, function_scope_variables)

      #Replace the function name with the actual one. We used the above one since R would have a problem with the one we use
      pmmlStringFor_function_call_for_current_symbol <- gsub(r_function_name, get_function_name_for_inner_function_expr_token(original_function_name, symbol_name), pmmlStringFor_function_call_for_current_symbol)

      pmml_string_for_initialization_expr_token <- gsub(glue::glue('<FieldRef field="{symbol_name}"/>'), pmmlStringFor_function_call_for_current_symbol, pmml_string_for_initialization_expr_token)
    }
  }

  pmml_string_for_initialization_expr_token <- get_pmml_string_with_defaulted_args_correctly_set(defaulted_arg_tokens, original_function_arg_tokens, pmml_string_for_initialization_expr_token)

  define_function_pmml_string <- pmml_string_for_initialization_expr_token
  if(if_expr_is(inner_function_expr_token, tokens) == FALSE) {
    define_function_pmml_string <- get_pmml_string_for_define_function(inner_func_name, original_function_arg_tokens, pmml_string_for_initialization_expr_token)
  }

  #Return the final DefineFunction PMML string
  return(define_function_pmml_string)
}

get_function_name_for_inner_function_expr_token <- function(original_function_name, variableName) {
  #Make the name of the function using the variableName and the orignal function name which is
  function_name <- glue::glue('{original_function_name}({variableName})')

  return(function_name)
}

define_function_get_inner_func_name <- function(inner_func_expr, tokens, orig_func_name) {
  var_name <- util_get_var_and_func_names(get_descendants_of_token(inner_func_expr, tokens))[1]

  return(get_function_name_for_inner_function_expr_token(orig_func_name, var_name))
}

define_function_get_row_args <- function(func_body_exprs, function_args, tokens) {
  row_args <- c()
  for(i in 1:nrow(func_body_exprs)) {
    token_with_assignment_code <- NA
    func_body_expr <- func_body_exprs[i, ]

    # For this expression
    # 1. Get all the symbol tokens which are descendants of this expr and is
    # a function parameter
    # 2. Check whether this function parameter is used as a row from a # dataframe. If it is then add it to the list row_args
    # Step 1
    descendant_symbol_tokens <- get_symbols_in_tokens(get_descendants_of_token(func_body_expr, tokens))
    descendant_arg_symbol_tokens <- descendant_symbol_tokens[descendant_symbol_tokens$text %in% function_args, ]
    # Step 2
    if(nrow(descendant_arg_symbol_tokens) > 0) {
      for(j in 1:nrow(descendant_arg_symbol_tokens)) {
        parent_token_to_check_for_row_access <- get_parent_token(get_parent_token(descendant_arg_symbol_tokens[j, ], tokens), tokens)
        if(data_frame_is_row_access(parent_token_to_check_for_row_access, tokens)) {
          row_var_name <- dollar_op_get_var(parent_token_to_check_for_row_access, tokens)
          if(row_var_name %in% function_args & row_var_name %in% row_args == FALSE) {
            row_args <- c(row_args, row_var_name)
          }
        }
      }
    }
  }

  return(row_args)
}

define_function_is <- function(expr, tokens) {
  return(get_child_tokens_for_parent(expr, tokens)[1, "token"] == FUNCTION_TOKEN)
}

# For all the symbols that are descendants of the expr token, return only those
# which need to be converted to their respective function calls.
define_function_get_symbols_to_convert_to_func_calls <- function(func_arg_names, expr, tokens) {
  # Find all the symbols used within the expression which are not part of the function arguments
  symbol_tokens <- get_symbols_in_tokens(get_descendants_of_token(expr, tokens))

  left_assign_symbol_ids <- c()
  for(i in seq_len(nrow(symbol_tokens))) {
     if(is_left_assignment_symbol_token(symbol_tokens[i, ], tokens)) {
       left_assign_symbol_ids <- c(left_assign_symbol_ids, symbol_tokens[i, ]$id)
     }
  }
  symbol_tokens <- symbol_tokens[!(symbol_tokens$id %in% left_assign_symbol_ids), ]

  symbol_tokens <- subset(
    symbol_tokens,
    !(symbol_tokens$text %in% func_arg_names)
  )

  if(data_frame_is_col_access(expr, tokens)) {
    data_frame_expr <- get_expr_tokens(get_child_tokens_for_parent(expr, tokens))[1, ]

    table_name <- data_frame_get_table_name(data_frame_expr, tokens)
    symbol_tokens <- subset(
      symbol_tokens,
      symbol_tokens$text != table_name
    )

    data_frame_iterate_column_conditions(data_frame_expr, tokens, function(column, field_or_constant) {
      symbol_tokens <<- subset(
        symbol_tokens,
        symbol_tokens$text != column$text
      )
    })
  }

  if(dollar_op_is_expr(expr, tokens)) {
    symbol_tokens <- subset(
      symbol_tokens,
      symbol_tokens$text != dollar_op_get_output_col(expr, tokens)
    )
  }

  return(symbol_tokens[complete.cases(symbol_tokens), ])
}
