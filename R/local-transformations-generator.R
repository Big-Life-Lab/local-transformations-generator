is_symbol_function_call_expr <- function(expr_token, tokens) {
  expr_tokens_whose_parent_is_the_current_token <- get_expr_tokens(get_tokens_with_parent(expr_token$id, tokens))
  if(nrow(expr_tokens_whose_parent_is_the_current_token) !=  0) {
    for(i in 1:nrow(expr_tokens_whose_parent_is_the_current_token)) {
      # If any of the child tokens has a symbol function call token, then this
      # expression is for a function call. This takes into account functions
      # that are from packages.
      if(SYMBOL_FUNCTION_CALL_TOKEN %in% get_tokens_with_parent(expr_tokens_whose_parent_is_the_current_token[1, 'id'], tokens)$token) {
        return(TRUE)
      }
    }
  }

  return(FALSE);
}

get_r_arguments_into_function_string <- function(original_function_arg_tokens) {
  # When no arguments return an empty string
  if(nrow(original_function_arg_tokens) == 0) {
    return('')
  }

  #The r string for the arguments into the function
  r_arguments_into_function_string <- NA
  for(i in 1:nrow(original_function_arg_tokens)) {
    if(is.na(r_arguments_into_function_string)) {
      r_arguments_into_function_string <- original_function_arg_tokens[i, 'text']
    } else {
      r_arguments_into_function_string <- paste(r_arguments_into_function_string, original_function_arg_tokens[i, 'text'], sep=',')
    }
  }

  return(r_arguments_into_function_string)
}

get_define_function_for_default_arg_expr <- function(arg_symbol_formal, all_arg_symbol_formals, tokens, func_name, function_param_tokens) {
  possible_eq_formals_token <- get_token_with_id(arg_symbol_formal$id+1, tokens)
  does_arg_have_default_value <- possible_eq_formals_token$token == EQ_FORMALS

  if(!does_arg_have_default_value) {
    return('')
  } else {
    eq_formals_token <- possible_eq_formals_token

    token_after_eq_formals_token <- get_token_after_token_with_id(tokens, eq_formals_token$id)

    arg_name <- arg_symbol_formal$text

    default_value <- NA
    if(token_after_eq_formals_token$token == EXPR_TOKEN) {
      default_value <- define_function_get_pmml_str_for_expr(token_after_eq_formals_token, tokens, func_name, function_param_tokens, FALSE, c())
    } else {
      default_value <- get_pmml_string_for_constant(token_after_eq_formals_token)
    }

    return(glue::glue(get_pmml_string_for_define_function(glue::glue('default({arg_name})'), all_arg_symbol_formals, glue::glue('<Apply function="if"><Apply function="equal"><FieldRef field="{arg_name}"/><Constant dataType="NA">NA</Constant></Apply>{default_value}<FieldRef field="{arg_name}"/></Apply>'))))
  }
}


# Returns the pmml_string arg where every reference to an argument that has been defaulted is replaced with a function call that returns the formatted value
get_pmml_string_with_defaulted_args_correctly_set <- function(defaulted_arg_tokens, all_arg_tokens, pmml_string) {
  formatted_pmml_string <- pmml_string

  if(nrow(defaulted_arg_tokens) != 0) {
    for(i in 1:nrow(defaulted_arg_tokens)) {
      default_function_args_pmml_string <- ''
      for(j in 1:nrow(all_arg_tokens)) {
        # Added placeholder to the beginning to prevent subsequent replacement calls from replacing the earlier replacement. THis will be put back to the right string later
        default_function_args_pmml_string <- glue::glue(default_function_args_pmml_string, '<FieldRef field="placeholder_{all_arg_tokens[j, "text"]}"/>')
      }

      formatted_pmml_string <- gsub(glue::glue('<FieldRef field="{defaulted_arg_tokens[i, "text"]}"/>'), glue::glue('<Apply function="default({defaulted_arg_tokens[i, "text"]})">{default_function_args_pmml_string}</Apply>'), formatted_pmml_string)
    }

    for(i in 1:nrow(all_arg_tokens)) {
      formatted_pmml_string <- gsub(glue::glue('<FieldRef field="placeholder_{all_arg_tokens[i, "text"]}"/>'), glue::glue('<FieldRef field="{all_arg_tokens[i, "text"]}"/>'), formatted_pmml_string)
    }
  }

  return(formatted_pmml_string)
}

# Get the index of the not the first but the second row in the parse_data array which has the parent field set to 0
get_index_of_next_zero_parent <- function(parse_data) {
  num_zero_parents <- 0

  for(i in 1:nrow(parse_data)) {
    if(parse_data[i,'parent'] == 0) {
      if(num_zero_parents == 1) {
        return(i)
      }
      else {
        num_zero_parents <- num_zero_parents + 1
      }
    }
  }

  return(nrow(parse_data))
}

get_pmml_string_from_source_function_call_tokens <- function(source_function_call_tokens, mutated_variables, evaluated_variables, log = TRUE) {
  source_function_call_arg_expr_token <- get_tokens_with_parent(source_function_call_tokens[1, ]$id, source_function_call_tokens)[3, ]
  source_function_call_arg_code_string <- getParseText(source_function_call_tokens, source_function_call_arg_expr_token$id)
  source_file_Path <- eval(parse(text=source_function_call_arg_code_string))
  return(get_pmml_string_from_r_file(source_file_Path, FALSE, mutated_variables, evaluated_variables, log = log))
}

# Generates the PMML table string for the data frame in the data_frame argument whose name is the table_name argument
get_table_pmml_strings_for_data_frame <- function(data_frame, table_name) {
  # This is where we will store the entire InlineTable xml element
  pmml_table_string <- ''

  row_names <- rownames(data_frame)

  # Go through all the rows of the table
  for(i in 1:nrow(data_frame)) {
    # For each row add a <row> opening tag
    pmml_table_string <- glue::glue('{pmml_table_string}<row><index>{row_names[[i]]}</index>')

    # Go through the columns of the row
    for(j in 1:ncol(data_frame)) {
      # For each column add <colname>Value of the column in this row</colname>
      pmml_table_string <- glue::glue('{pmml_table_string}<{colnames(data_frame)[j]}>{data_frame[i,j]}</{colnames(data_frame)[j]}>')
    }

    # End of this row so add a closing row xml tag
    pmml_table_string <- glue::glue("{pmml_table_string}</row>")
  }

  # The final table string
  pmml_table_string <- glue::glue('<Taxonomy name="{table_name}"><InlineTable>{pmml_table_string}</InlineTable></Taxonomy>')

  # Return the string along with the variable to which the table data was assigned
  return(list(pmml_table_string, table))
}

get_mutated_variable_name <- function(variable_name, mutation_number) {
  if(mutation_number <= 0) {
    return(variable_name)
  } else {
    return(glue::glue('{variable_name}_Mutated_{mutation_number}'))
  }
}

mutate_variable <- function(mutated_variables, variable_name) {
  # Get the list of all the variables we are currently tracking for mutation
  current_variables <- row.names(mutated_variables)

  # Find the one that matches with the variable_name variable and increase the mutation count by one
  for(i in current_variables) {
    if(i == variable_name) {
      mutated_variables[i, 'mutationIteration'] <- mutated_variables[i, 'mutationIteration'] + 1
    }
  }

  return(mutated_variables)
}

# Goes through the mutation logic for the list tokens in the tokens arg for the variable with name variable_name
mutate_relevant_variables <- function(variable_name, tokens, mutated_variables) {
  # Get the expr token which encapsulates the left hand side of an assignment statement
  expr_token_for_right_assign <- get_child_tokens_for_parent(tokens[1, ], tokens)[3, ]
  # Get the expr token which encapsulates the right hand side of an assignment statement
  expr_token_for_left_assign <- get_child_tokens_for_parent(tokens[1, ], tokens)[1, ]

  # First go through all the tokens which are in the RHS and update all the relevant
  # variables to their mutated variable names
  for(i in 1:nrow(tokens)) {
    # For example if testOne is variable has been mutated twice then we set it to testOne_Mutated_2
    if(is_descendant_of_token_with_id(expr_token_for_right_assign$id, tokens[i, ], tokens)) {
      if(is_symbol_token(tokens[i, ]) & tokens[i, 'text'] %in% row.names(mutated_variables)) {
        tokens[i, 'text'] <- get_mutated_variable_name(tokens[i, 'text'], mutated_variables[tokens[i, 'text'], 'mutationIteration'])
      }
    }
  }


  # Check if there is an entry in the mutated_variables data frame for the current variable. if there isn't, then create one and set the number of times it's been mutated to 0
  if(variable_name %in% row.names(mutated_variables) == FALSE) {
    mutated_variables[variable_name, 'mutationIteration'] <- 0
  } else { # If there is an entry then update it's it's mutation count
    mutated_variables <- mutate_variable(mutated_variables, variable_name)
  }

  # Next. If the variable on the left is being set using itself then it implies
  # it's being mutated, so update the mutationIteration for the variable
  # if it happens. For eg, a <- a would be a statement that mutates itself
  # Do this by checking if any of symbols on the RHS has the same name has the
  # variable_name arg. Remember that the tokens have been updated though so technically
  # this should only work for the very first self mutation
  for(token_index in 1:nrow(tokens)) {
    current_token <- tokens[token_index, ]

    if(is_descendant_of_token_with_id(expr_token_for_right_assign$id, current_token, tokens)) {
      if(is_symbol_token(current_token) & current_token$text == variable_name) {
        mutated_variables <- mutate_variable(mutated_variables, variable_name)

        break
      }
    }
  }


  # For each token in the list of them check if it's a child of the LHS or RHS expr token
  for(i in 1:nrow(tokens)) {
    # Otherwise if it's part of the LHS it has to be the symbol for the current variable so set it's new name to number of times it's been mutated till now plus one
    if(tokens[i, ]$parent == expr_token_for_left_assign$id) {
      tokens[i, 'text'] <- get_mutated_variable_name(variable_name, mutated_variables[variable_name, 'mutationIteration'])
    }
  }

  # Return the mutated tokens
  return(list(tokens=tokens, mutated_variables=mutated_variables))
}

# mutated_variables - Keeps track of all the variables and the number of times they have been mutated. Each row is the name of the variable and every row has one column called mutation iteration which is the number of times this variable has been mutated. When function is called for the first time should not be passed in
# evaluated_variables - A HashMap that maps the variable name from each line of code to it's evaluated value
get_pmml_string_from_r_file <- function(file_path, src_file=FALSE, mutated_variables = data.frame(), evaluated_variables = new.env(hash = TRUE), out_to_file = TRUE, log = TRUE) {
  if(src_file) {
    # Create directory where we store temperoray files during the addin operation
    dir.create(file.path(getwd(), 'temp'), showWarnings = FALSE)
    # Save the current workspace in the temp directory. Since we are going to be evaluating each line of code we don't want to overwrite a person's workspace objects as we execute the code
    save.image(file=file.path(getwd(), 'temp/temp.RData'))

    assign("row_vars", list(), envir = .GlobalEnv)
    assign("gl_row_functions", list(), envir = .GlobalEnv)
  }

  tokens_with_comments <- getParseData(parse(file = file_path, keep.source = TRUE))
  tokens <- filter_out_comment_tokens(tokens_with_comments)
  ##### DEBUG
  #print(tokens)

  next_zero_parent_index <- get_index_of_next_zero_parent(tokens)

  local_transformation_string <- ''
  taxonomy <- ''

  # Each line of code is consists of several tokens but they all start with  an expr token whose parent is 0. This is how we know that we have reached a new line of code
  while(next_zero_parent_index != 0) {
    tokens_for_current_parent_index = tokens[1:next_zero_parent_index, ]

    # Get all the comments for this expression
    comments_for_current_expr <- get_comment_tokens_with_parent(
      tokens_for_current_parent_index[1, ]$id,
      tokens_with_comments
    )

    # If this is a call to initialize a library then skip it
    if(nrow(get_symbol_function_calls_with_text('library', tokens_for_current_parent_index)) == 1) {}
    else if(does_tokens_have_source_function_call(tokens_for_current_parent_index) == TRUE) {
      source_return_values <- get_pmml_string_from_source_function_call_tokens(tokens_for_current_parent_index, mutated_variables, evaluated_variables, log = log)

      taxonomy <- paste(taxonomy, source_return_values$taxonomy, sep='')
      local_transformation_string <- paste(local_transformation_string, source_return_values$local_transformation_string, sep='')
      mutated_variables <- source_return_values$mutated_variables
    } else {
      derived_field_names <- unique(util_get_var_and_func_names(tokens_for_current_parent_index))

      for(i in 1:length(derived_field_names)) {
        mutate_relevant_variables_result <- mutate_relevant_variables(derived_field_names[i], tokens_for_current_parent_index, mutated_variables)
        tokens_for_current_parent_index <- mutate_relevant_variables_result$tokens
        mutated_variables <- mutate_relevant_variables_result$mutated_variables
      }

      # We are going to evaluate the code represented by the tokens in the variable tokens_for_current_parent_index and depending on the value returned called the right pmml parsing function
      evaluated_value <- NA
      tryCatch({
        # Evaluate the line of code
        evaluated_value <- eval(parse(text=getParseText(tokens_for_current_parent_index, tokens_for_current_parent_index[1, 'id'])))
      }, error = function(e) {
        # If there's an error set it to NA
        evaluated_value <<- NA
      })

      for(i in 1:length(derived_field_names)) {
        variable_name <- derived_field_names[i]
        mutated_variable_name <- get_mutated_variable_name(variable_name, mutated_variables[variable_name, 'mutationIteration'])
        if(log) {
          print(glue::glue("Parsing variable {mutated_variable_name}"))
        }

        if(mutated_variables[variable_name, 'mutationIteration'] != 0) {
          for(obj in ls()) {
            if(obj == variable_name) {
              evaluated_value = get(obj)
            }
          }
        }

        # Set the evaluated value to it's mutated variable value in the evaluated variables environment
        evaluated_variables[[mutated_variable_name]] <- evaluated_value

        # if the evaluated value is a data frame
        if(class(evaluated_value) == 'data.frame') {
          # The return value is a list with the pmml string and the name of the variable to which the table was assigned
          return_values <- get_table_pmml_strings_for_data_frame(evaluated_value, mutated_variable_name)

          # Add the pmml table string to the taxonomy string
          taxonomy <- paste(taxonomy, return_values[1], sep = '')
        }
        else if(does_tokens_have_function_definition(tokens_for_current_parent_index) == TRUE) {
          local_transformation_string <- paste(local_transformation_string, define_function_get_pmml_string(tokens_for_current_parent_index, mutated_variable_name), sep='')
        }
        else {
          assign_expr_token <- get_token_with_assignment_code(tokens_for_current_parent_index)
          child_tokens <- get_child_tokens_for_parent(assign_expr_token, tokens_for_current_parent_index)
          possible_row_var <- get_child_tokens_for_parent(child_tokens[1, ], tokens_for_current_parent_index)[1, ]
          pmml_str_for_var <- derived_field_get_pmml_str_for_var(
            mutated_variable_name,
            tokens_for_current_parent_index[1, ],
            tokens_for_current_parent_index,
            comments_for_current_expr,
            evaluated_variables
          )
          local_transformation_string <- paste(local_transformation_string, pmml_str_for_var, sep='')
        }

        if(log) {
          print(glue::glue("Done parsing variable {mutated_variable_name}"))
        }
      }
    }

    if(next_zero_parent_index == nrow(tokens)) {
      break
    }

    tokens <- tokens[next_zero_parent_index:nrow(tokens), ]

    next_zero_parent_index <- get_index_of_next_zero_parent(tokens)
  }

  if(src_file == TRUE) {
    # Reset the workspace to before the addin was run
    load(file.path(getwd(), 'temp/temp.RData'))
    # Remove the file which had the workspace objects
    file.remove(file.path(getwd(), 'temp/temp.RData'))

    remove("gl_row_functions", envir = .GlobalEnv)
    if(exists("gl_default_param_functions")) {
      remove("gl_default_param_functions", envir = .GlobalEnv)
    }
    remove("row_vars", envir = .GlobalEnv)

    return(paste('<PMML>', taxonomy, '<LocalTransformations>', local_transformation_string, '</LocalTransformations></PMML>', sep = ''))
  } else {
    return(list('taxonomy' = taxonomy, 'local_transformation_string' = local_transformation_string, mutated_variables = mutated_variables))
  }
}
