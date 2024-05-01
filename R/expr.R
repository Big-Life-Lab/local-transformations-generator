expr_generic_get_pmml_str_for_expr <- function(
  get_pmml_str_for_row_access,
  get_pmml_str_for_func_call_row_access,
  get_pmml_str_for_if_expr) {
  get_pmml_str_for_expr <- function(expr, tokens, scope_variables) {
    tokens_whose_parent_is_the_current_expr <- get_tokens_with_parent(expr$id, tokens)
    tokens_whose_parent_is_the_current_expr_has_one_row <- (nrow(tokens_whose_parent_is_the_current_expr) != 0)

    child_special_tokens_for_current_expr <- get_special_tokens(get_child_tokens_for_parent(expr, tokens))

    if(nrow(child_special_tokens_for_current_expr) != 0) {
      if(child_special_tokens_for_current_expr[1, 'text'] == '%in%') {
        child_expr_tokens <- get_expr_tokens(get_child_tokens_for_parent(expr, tokens))
        left_expr_token_pmml_string <- get_pmml_str_for_expr(child_expr_tokens[1, ], tokens)
        right_expr_token_pmml_string <- get_pmml_str_for_expr(child_expr_tokens[2, ], tokens)

        return(glue::glue('<Apply function="isIn">{left_expr_token_pmml_string}{right_expr_token_pmml_string}</Apply>'))
      }
      else {
        stop(glue::glue('Unhandled special symbol {child_special_tokens_for_current_expr[1, "text"]}'))
      }
    }
    else if(if_expr_is(expr, tokens)) {
      cond_expr_to_block_exprs_mappings <- if_expr_get_cond_expr_to_block_exprs_map(expr, tokens)
      get_pmml_str_for_if_expr(cond_expr_to_block_exprs_mappings)
    }
    else if(data_frame_is_row_access(expr, tokens)) {
      return(get_pmml_str_for_row_access(expr, tokens, scope_variables))
    }
    # If this is an expression to access the column from a row and store it in a variable for eg. var1 <- row$col1
    else if(dollar_op_is_get_col_from_row_expr(expr, tokens)) {
      row_var_name <- dollar_op_get_var(expr, tokens)

      return(dollar_op_get_pmml_node(expr, tokens, globals_get_pmml_str_for_row_var(row_var_name)))
    }
    else if(dollar_op_is_expr(expr, tokens)) {
      if(data_frame_is_expr(tokens_whose_parent_is_the_current_expr[1, ], tokens)) {
        return(dollar_op_get_pmml_node(
          expr, tokens, data_frame_get_pmml_node(tokens_whose_parent_is_the_current_expr[1, ], tokens, scope_variables)))
      }
    }
    else if(data_frame_is_expr(expr, tokens)) {
      data_frame_node <- data_frame_get_pmml_node(expr, tokens, scope_variables)
      if(data_frame_is_wildcard_expr(expr, tokens)) {
        return(glue::glue('<MapValues>{data_frame_node}</MapValues>'))
      }
      return(data_frame_node)
    }
    else {
      expr_tokens_whose_parent_is_the_current_expr <- get_expr_tokens(tokens_whose_parent_is_the_current_expr)
      non_expr_tokens_whose_parent_is_the_current_expr <- filter_out_expr_tokens(tokens_whose_parent_is_the_current_expr)
      pmml_string_for_expr_tokens <- ''

      if(nrow(expr_tokens_whose_parent_is_the_current_expr) != 0) {
        # If this expression has a function call in it
        if(is_symbol_function_call_expr(expr, tokens)) {
          if(function_call_is_row_function_call_expr(expr, tokens)) {
            return(get_pmml_str_for_func_call_row_access(expr, tokens))
          }

          # A function can be called from a package or not; Here we properly
          # get the name of the function taking this into account.
          function_name_tokens <- get_tokens_with_parent(expr_tokens_whose_parent_is_the_current_expr[1, 'id'], tokens)
          function_name <- ""
          # The function is from a package
          if(function_name_tokens[1, "token"] == SYMBOL_PACKAGE) {
            function_name <- paste(
              function_name_tokens[1, "text"],
              ".",
              function_name_tokens[3, "text"],
              sep = ""
            )
          }
          # Normal function call
          else {
            function_name <- function_name_tokens[1, "text"]
          }

          # Handle c functions by taking the arguments to the functions and concating the pmml string for each argument
          if(function_name == 'c') {
            return(get_pmml_str_for_arg_exprs(function_call_get_function_arg_expr_tokens(expr, tokens), tokens))
          } else if(function_name == 'exists') {
            function_arg_expr_tokens <- function_call_get_function_arg_expr_tokens(expr, tokens)
            exits_arg <- format_constant_token_text(get_tokens_with_parent(function_arg_expr_tokens[1, 'id'], tokens)[1, ])
            return(get_pmml_string_for_symbol_function_call(function_name, glue::glue('<FieldRef field="{exits_arg}"/>')))
          }# If read.csv function call. Do nothing since we handle converting csv files to PMML tables at the beginning
          else if(function_name == 'read.csv') {}
          else if(function_name == 'switch') {
            return(convert_switch_statement_to_pmml(expr, tokens, scope_variables, get_pmml_str_for_expr))
          }
          else {
            # Get the PMML string for the arguments passed into the function represented
            # by the function call expr in the func_call_expr arg
            function_arg_expr_tokens <- function_call_get_function_arg_expr_tokens(expr, tokens)
            function_args_symbol_tokens_pmml_string <- get_pmml_str_for_arg_exprs(function_arg_expr_tokens, tokens)

            # If the function that is called has parameters can be defaulted, then check whether the user
            # passed them. If they were not passed then add NA's to the end
            # in the spots where the defaulted args need to go
            if(globals_is_default_param_function(function_name)) {
              default_param_function_info <- globals_get_default_param_function(function_name)
              num_nas_to_add <- default_param_function_info$num_function_params - nrow(function_arg_expr_tokens)
              if(num_nas_to_add > 0) {
                for(i in 1:num_nas_to_add) {
                  function_args_symbol_tokens_pmml_string <- paste(
                    function_args_symbol_tokens_pmml_string, '<Constant dataType="NA">NA</Constant>', sep = "")
                }
              }
            }

            return(get_pmml_string_for_symbol_function_call(function_name, function_args_symbol_tokens_pmml_string))
          }
        } else {
          for(i in 1:nrow(expr_tokens_whose_parent_is_the_current_expr)) {
            pmml_string_for_expr_tokens <- paste(
              pmml_string_for_expr_tokens,
              get_pmml_str_for_expr(expr_tokens_whose_parent_is_the_current_expr[i, ], tokens, scope_variables),
              sep=''
            )
          }
        }
      }

      if(nrow(non_expr_tokens_whose_parent_is_the_current_expr) == 0) {
        return(pmml_string_for_expr_tokens)
      }

      non_expr_token = non_expr_tokens_whose_parent_is_the_current_expr[1, ]
      non_expr_token_token = non_expr_token$token

      if(non_expr_token_token == SYMBOL_TOKEN) {
        return(get_pmml_string_for_symbol(non_expr_token))
      } else if(non_expr_token_token == NUM_CONST_TOKEN | non_expr_token_token == STR_CONST_TOKEN | non_expr_token_token == NULL_CONST_TOKEN) {
        return(get_pmml_string_for_constant(non_expr_token))
      } else if(non_expr_token_token %in% MATH_TOKENS) {
        return(get_pmml_string_for_math_token(non_expr_token, pmml_string_for_expr_tokens))
      } else if(non_expr_token_token %in% LOGICAL_TOKENS) {
        return(get_pmml_string_for_logical_operator(non_expr_token, pmml_string_for_expr_tokens))
      } else if(non_expr_token$token == "':'") {
        return(get_pmml_string_for_colon_token(pmml_string_for_expr_tokens))
      }
      else {
        return(pmml_string_for_expr_tokens)
      }
    }
  }

  get_pmml_str_for_arg_exprs <- function(arg_expr_tokens, tokens) {
    function_args_symbol_tokens_pmml_string <- ''
    for(i in seq_len(nrow(arg_expr_tokens))) {
      function_args_symbol_tokens_pmml_string <- paste(
        function_args_symbol_tokens_pmml_string,
        get_pmml_str_for_expr(arg_expr_tokens[i, ], tokens, c()),
        sep=''
      )
    }

    return(function_args_symbol_tokens_pmml_string)
  }

  return(get_pmml_str_for_expr)
}
