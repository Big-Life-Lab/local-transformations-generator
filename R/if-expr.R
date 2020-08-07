if_expr_is <- function(expr, tokens) {
  child_tokens <- get_child_tokens_for_parent(expr, tokens)

  return(child_tokens[1, "token"] == IF_TOKEN)
}

get_pmml_string_for_if_expr <- function(expr, tokens, comment_tokens, evaluated_variables) {
  derived_fields_set <- data.frame();
  # The index of the dataframe is the derived field name
  # conditionExprIds: String which has the ids of the expr that holds the condition code which when true the corresponsing true code sets this derived field. Each expr id is seperated by a comma
  # exprBlockIds: String which has the ids of the expr tokens which is run when the corresponsing condition expr is set to true. Each expr id is seperated by a comma and matches with one from the conditionExprIds

  children_for_root_exp <- get_child_tokens_for_parent(expr, tokens)

  # The expr tokens which are run when the condition for this if statement evaluates to true. The expr token which holds this code is always the 5th one in the list
  when_condition_true_exprs <- get_expr_tokens(get_child_tokens_for_parent(children_for_root_exp[5, ], tokens))

  is_else <- children_for_root_exp[1, 'text'] == '{'
  # When the expr is an else statement then the first child is a { and the when_condition_true_exprs are not at the 5 place in the array so get all the expr tokens
  if(is_else) {
    when_condition_true_exprs <- get_expr_tokens(children_for_root_exp)
  }

  for(i in 1:nrow(when_condition_true_exprs)) {
    derived_field_name <- getDerivedFieldNameOrFunctionNameForTokens(get_descendants_of_token(when_condition_true_exprs[i, ], tokens))

    if(!is_else) {
      derived_fields_set[derived_field_name, 'conditionExprId'] <- children_for_root_exp[3, 'id']
    }
    # If this is an else statement then there is no condition expr so set it to NA
    else {
      derived_fields_set[derived_field_name, 'conditionExprId'] <- NA
    }

    derived_fields_set[derived_field_name, 'exprBlockId'] <- when_condition_true_exprs[i, 'id']
  }

  else_pmml_strings <- NA
  if(!is.na(children_for_root_exp[6, ]) & children_for_root_exp[6, 'token'] == 'ELSE') {
    else_pmml_strings <- get_pmml_string_for_if_expr(
      children_for_root_exp[7, ],
      tokens,
      comment_tokens,
      evaluated_variables,
    )
  }

  derived_field_name_with_pmml_string <- list()
  for(derived_field_name in row.names(derived_fields_set)) {
    pmml_str <- ''

    condition_pmml_string <- getPmmlStringForExpr(getExprWithIdInTokens(derived_fields_set[derived_field_name, 'conditionExprId'], tokens), tokens)
    when_true_pmml_string <- getDerivedFieldPmmlStringForTokens(
      get_descendants_of_token(getExprWithIdInTokens(derived_fields_set[derived_field_name, 'exprBlockId'], tokens), tokens),
      derived_field_name,
      comment_tokens,
      evaluated_variables,
      FALSE
    )
    when_false_pmml_string <- glue::glue('<FieldRef field="{derived_field_name}"/>')
    if(!(is.na(else_pmml_strings) | (derived_field_name %in% row.names(else_pmml_strings)) == FALSE)) {
      when_false_pmml_string <- else_pmml_strings[derived_field_name, 'pmmlString']
    }

    if(condition_pmml_string != '') {
      pmml_str <- glue::glue('<Apply function="if">{condition_pmml_string}{when_true_pmml_string}{when_false_pmml_string}</Apply>')
    } else {
      pmml_str <- when_true_pmml_string
    }

    derived_field_name_with_pmml_string[[length(derived_field_name_with_pmml_string) + 1]] <- list(
      derived_field_name = derived_field_name,
      pmml_str = pmml_str
    )
  }

  # This if loop will add the derived fields which have been initialised in the
  # else part of the if loop to the master list if they have not been put in there
  # already
  if(!is.na(else_pmml_strings)) {
    for(derived_field_name in row.names(else_pmml_strings)) {
      not_in_derived_fields_list <- FALSE
      if(length(derived_field_name_with_pmml_string) != 0) {
        for(i in 1:length(derived_field_name_with_pmml_string)) {
          if(derived_field_name != derived_field_name_with_pmml_string[[i]]$derived_field_name) {
            not_in_derived_fields_list <- TRUE
          }
        }
      } else {
        not_in_derived_fields_list <- TRUE
      }

      if(not_in_derived_fields_list) {
        derived_field_name_with_pmml_string[[length(derived_field_name_with_pmml_string) + 1]] <- list(
          derived_field_name = derived_field_name,
          pmml_str = else_pmml_strings[derived_field_name, 'pmmlString']
        )
      }
    }
  }

  return(derived_field_name_with_pmml_string)
}

if_expr_get_cond_expr_to_block_exprs_map <- function(
  expr,
  tokens,
  cond_expr_to_block_exprs_mappings = list()
) {
  child_tokens <- get_child_tokens_for_parent(expr, tokens)
  child_expr_tokens <- get_expr_tokens(child_tokens)

  found_if_token <- FALSE
  for(i in 1:nrow(child_tokens)) {
    cur_child_token <- child_tokens[i, ]

    if(cur_child_token$token == IF_TOKEN) {
      found_if_token <- TRUE

      cond_expr_id <- child_expr_tokens[1, "id"]
      block_expr_ids <- get_expr_tokens(
        get_child_tokens_for_parent(child_expr_tokens[2, ], tokens))$id
      cond_expr_to_block_exprs_mappings[[length(cond_expr_to_block_exprs_mappings) + 1]] <- list(
        cond_expr_id = cond_expr_id,
        block_expr_ids = block_expr_ids
      )
    }

    if(cur_child_token$token == ELSE_TOKEN) {
      cond_expr_to_block_exprs_mappings <- if_expr_get_cond_expr_to_block_exprs_map(
        child_tokens[i + 1, ], tokens, cond_expr_to_block_exprs_mappings
      )
    }
  }

  if(found_if_token == FALSE) {
    cond_expr_to_block_exprs_mappings[[length(cond_expr_to_block_exprs_mappings) + 1]] <- list(
      cond_expr_id = NA,
      block_expr_ids = child_expr_tokens$id
    )
  }

  return(cond_expr_to_block_exprs_mappings)
}
