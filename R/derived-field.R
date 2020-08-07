# Use this for generating DerivedField PMML strings
derived_field_get_pmml_str_for_var <- function(var_name, expr, tokens, comment_tokens, evaluated_variables) {
  # Keeps track of the function calls which access df rows and the number of
  # times they have been called in this expression
  # Has fields:
  # func_name
  # call_count
  row_access_func_call_count <- list()
  # All the DefineFunction PMML strings for this expression. This happens for:
  # 1. Function calls which access rows from df
  define_function_pmml_strs <- ""
  derived_field_pmmls <- list()

  get_pmml_str_for_func_call_row_access <-
    function(expr, tokens) {
      # Get the index of the call count entry for the function which is
      # called in this expression
      row_access_func_call_index <- NA
      func_name <-
        function_call_get_function_name_token(expr, tokens)$text
      if (length(row_access_func_call_count) > 0) {
        for (i in 1:length(row_access_func_call_count)) {
          if (row_access_func_call_count[[i]]$func_name == func_name) {
            row_access_func_call_index <- i

            break

          }
        }
      }
      # If an entry for this function exists then increment it's call
      # count by one, otherwise add an entry for it
      if (is.na(row_access_func_call_index) == FALSE) {
        row_access_func_call_count[[row_access_func_call_index]]$call_count <<-
          row_access_func_call_count[[row_access_func_call_index]]$call_count + 1
      } else {
        row_access_func_call_count[[length(row_access_func_call_count) + 1]] <<-
          list(func_name = func_name,
               call_count = 1)
        row_access_func_call_index <-
          length(row_access_func_call_count)
      }

      gl_row_function <- globals_get_row_function(func_name)
      func_arg_expr_tokens <-
        function_call_get_function_arg_expr_tokens(expr, tokens)

      # Get all the expr tokens which are not df row accesses
      non_row_func_arg_expr_tokens <- func_arg_expr_tokens
      for (i in 1:nrow(func_arg_expr_tokens)) {
        if (gl_row_function$args[i] %in% gl_row_function$row_args) {
          non_row_func_arg_expr_tokens <-
            non_row_func_arg_expr_tokens[non_row_func_arg_expr_tokens$id != func_arg_expr_tokens[i, ]$id, ]
        }
      }
      row_func_arg_expr_tokens <-
        func_arg_expr_tokens[func_arg_expr_tokens$id != non_row_func_arg_expr_tokens$id, ]

      # Make the new name of the function which we will replace all
      # function call in this expression with
      current_row_access_func_call_count <-
        row_access_func_call_count[[row_access_func_call_index]]$call_count
      new_func_name <- glue::glue(
        "{gl_row_function$func_name}_{var_name}_{current_row_access_func_call_count}"
      )

      define_function_pmml_str <- ''
      define_function_pmml_str <- gsub(
        gl_row_function$func_name,
        new_func_name,
        gl_row_function$pmml_str
      )
      for (i in 1:length(gl_row_function$row_args)) {
        row_param_name <- gl_row_function$row_args[i]
        row_arg_name <-
          get_child_tokens_for_parent(row_func_arg_expr_tokens[i, ], tokens)[1, ]$text
        define_function_pmml_str <- gsub(
          paste("\\{", row_param_name, "\\}", sep = ""),
          globals_get_pmml_str_for_row_var_name(row_arg_name),
          define_function_pmml_str
        )
      }
      define_function_pmml_strs <<- paste(define_function_pmml_strs, define_function_pmml_str, sep = '')

      func_args_pmml_str <- ''
      for(i in 1:nrow(non_row_func_arg_expr_tokens)) {
        func_args_pmml_str <- get_pmml_str_for_expr(non_row_func_arg_expr_tokens[i, ], tokens)
      }

      return(glue::glue('<Apply function="{new_func_name}">{func_args_pmml_str}</Apply>'))
    }

  get_pmml_str_for_row_access <- function(expr, tokens) {
    row_var_name <- dollar_op_get_var(expr, tokens)

    return(dollar_op_get_pmml_node(expr, tokens, globals_get_pmml_str_for_row_var_name(row_var_name)))
  }

  get_pmml_str_for_if_expr <- function(cond_expr_to_block_exprs_mappings) {
    new_mappings <- list()

    for(i in 1:length(cond_expr_to_block_exprs_mappings)) {
      cur_mapping <- cond_expr_to_block_exprs_mappings[[i]]

      expr_id_with_var <- NA
      for(j in 1:length(cur_mapping$block_expr_ids)) {
        cur_expr_token <- get_token_with_id(cur_mapping$block_expr_ids[[j]], tokens)
        var_names_in_cur_expr <- util_get_var_and_func_names(get_descendants_of_token(cur_expr_token, tokens))
        if(var_name %in% var_names_in_cur_expr) {
          expr_id_with_var <- cur_mapping$block_expr_ids[[j]]
        }
      }

      if(is.na(expr_id_with_var) == FALSE) {
        new_mappings[[length(new_mappings) + 1]] <- list(
          cond_expr_id = cur_mapping$cond_expr_id,
          expr_id = expr_id_with_var
        )
      }
    }

    pmml_str <- ''
    reverse_mappings <- rev(new_mappings)
    if(is.na(reverse_mappings[[1]]$cond_expr_id) == FALSE) {
      pmml_str <- '<Constant dataType="NULL">NULL</Constant>'
    }
    for(i in 1:length(reverse_mappings)) {
      cur_mapping <- reverse_mappings[[i]]

      pmml_str_for_cond <- ''
      if(is.na(cur_mapping$cond_expr_id) == FALSE) {
        pmml_str_for_cond <- get_pmml_str_for_expr(
          get_token_with_id(cur_mapping$cond_expr_id, tokens),
          tokens
        )
      }
      pmml_str_for_expr <- get_pmml_str_for_token(
        get_token_with_assignment_code(
          get_descendants_of_token(get_token_with_id(cur_mapping$expr_id, tokens), tokens)
        ),
        tokens,
        tokens_create_empty_tokens_df(),
        evaluated_variables
      )

      pmml_str <- glue::glue('<Apply function="if">{pmml_str_for_cond}{pmml_str_for_expr}{pmml_str}</Apply>')
    }

    return(pmml_str)
  }

  get_pmml_str_for_expr <- expr_generic_get_pmml_str_for_expr(get_pmml_str_for_row_access, get_pmml_str_for_func_call_row_access, get_pmml_str_for_if_expr)
  get_pmml_str_for_token <- pmml_generic_get_pmml_str_for_token(get_pmml_str_for_expr)

  if(if_expr_is(expr, tokens)) {
    transformations_pmml_str <- get_pmml_str_for_token(expr, tokens, comment_tokens, evaluated_variables)
    return(paste(
      define_function_pmml_strs,
      glue::glue('<DerivedField name="{var_name}" optype="continuous">{transformations_pmml_str}</DerivedField>'),
      sep = ''
    ))
  } else {
    child_tokens <- get_child_tokens_for_parent(expr, tokens)

    var_name_token <- get_child_tokens_for_parent(child_tokens[1, ], tokens)[1, ]
    if(is_symbol_token(var_name_token) == FALSE & var_name_token$text != var_name) {
      stop(glue::glue("Current expression is not for assigning variable {var_name}
                      but for assigning variable {var_name_token$text}"))
    }

    leftAssignToken <- tokens[which(tokens$token == LEFT_ASSIGN_TOKEN), ][1, ]

    tokenWithAssignmentCode <- child_tokens[3, ]

    transformations_pmml_str <- get_pmml_str_for_token(tokenWithAssignmentCode, tokens, comment_tokens, evaluated_variables)
    return(paste(
      define_function_pmml_strs,
      glue::glue('<DerivedField name="{var_name}" optype="continuous">{transformations_pmml_str}</DerivedField>'),
      sep = ''
    ))
  }
}
