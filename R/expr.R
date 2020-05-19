# Use this for generating DerivedField PMML strings
expr.get_pmml_for_var <- function(var_name, expr, tokens, comment_tokens, evaluated_variables) {
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
  
  if(isIfExpr(getDescendantsOfToken(expr, tokens))) {
    derived_field_pmmls <- getPmmlStringForIfExpr(expr, tokens, comment_tokens, evaluated_variables)
  } else {
    child_tokens <- getChildTokensForParent(expr, tokens)
    
    var_name_token <- getChildTokensForParent(child_tokens[1, ], tokens)[1, ]
    if(isSymbolToken(var_name_token) == FALSE & var_name_token$text != var_name) {
      stop(glue::glue("Current expression is not for assigning variable {var_name}
                      but for assigning variable {var_name_token$text}"))
    }
    
    leftAssignToken <- tokens[which(tokens$token == LEFT_ASSIGN_TOKEN), ][1, ]
    
    tokenWithAssignmentCode <- child_tokens[3, ]
    
    transformationsPmmlString <- ''
    
    # If there's a custom pmml function comment for this expression set it to this variable
    custom_pmml_func_comment_token <- NA
    if(nrow(comment_tokens) != 0) {
      custom_pmml_func_comment_token <- get_custom_pmml_func_comment_token(comment_tokens)
    }
    # If this line needs to be converted to a custom pmml expression
    if(!is.na(custom_pmml_func_comment_token)) {
      transformationsPmmlString <- get_pmml_node_for_pmml_func_comment_token(
        custom_pmml_func_comment_token,
        evaluated_variables
      )
    }
    else if(tokenWithAssignmentCode$token == EXPR_TOKEN) {
      get_pmml_str_for_func_call_row_access <-
        function(expr, tokens) {
          # Get the index of the call count entry for the function which is 
          # called in this expression
          row_access_func_call_index <- NA
          func_name <-
            function_call.get_function_name_token(expr, tokens)$text
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
          
          gl_row_function <- globals.get_row_function(func_name)
          func_arg_expr_tokens <-
            function_call.get_function_arg_expr_tokens(expr, tokens)

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
              getChildTokensForParent(row_func_arg_expr_tokens[i, ], tokens)[1, ]$text
            define_function_pmml_str <- gsub(
              paste("\\{", row_param_name, "\\}", sep = ""),
              globals.get_pmml_str_for_row_var_name(row_arg_name),
              define_function_pmml_str
            )
          }
          define_function_pmml_strs <<- paste(define_function_pmml_strs, define_function_pmml_str, sep = '')
          
          func_args_pmml_str <- function_call.get_pmml_str_for_arg_exprs(non_row_func_arg_expr_tokens, tokens)
          
          return(glue::glue('<Apply function="{new_func_name}">{func_args_pmml_str}</Apply>'))
        }
      
      get_pmml_str_for_row_access <- function(expr, tokens) {
        row_var_name <- dollar_op.get_var(expr, tokens)
        
        return(dollar_op.get_pmml_node(expr, tokens, globals.get_pmml_str_for_row_var_name(row_var_name)))
      }
      
      transformations_pmml_str <- getPmmlStringForExpr(tokenWithAssignmentCode, tokens, 
                                                       get_pmml_str_for_func_call_row_access = get_pmml_str_for_func_call_row_access,
                                                       get_pmml_str_for_row_access = get_pmml_str_for_row_access)
      transformationsPmmlString <- paste(
        define_function_pmml_strs,
        glue::glue('<DerivedField name="{var_name}" optype="continuous">{transformations_pmml_str}</DerivedField>'),
        sep = ''
      )
    } else if(tokenWithAssignmentCode$token == NUM_CONST_TOKEN | tokenWithAssignmentCode$token == STR_CONST_TOKEN | tokenWithAssignmentCode$token == NULL_CONST_TOKEN) {
      transformationsPmmlString <- getPmmlStringForConstant(tokenWithAssignmentCode)
    } else if(tokenWithAssignmentCode$token == SYMBOL_TOKEN) {
      transformationsPmmlString <- getPmmlStringForSymbol(tokenWithAssignmentCode)
    } else {
      stop(glue::glue('Unhandled token type {tokenWithAssignmentCode$token} for field {derivedFieldName}'))
    }
    
    return(transformationsPmmlString)
  }
}