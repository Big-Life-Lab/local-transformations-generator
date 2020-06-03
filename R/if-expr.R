if_expr.is <- function(expr, tokens) {
  child_tokens <- getChildTokensForParent(expr, tokens)
  
  return(child_tokens[1, "token"] == IF_TOKEN)
}

getPmmlStringForIfExpr <- function(expr, tokens, comment_tokens, evaluated_variables) {
  derivedFieldsSet <- data.frame();
  # The index of the dataframe is the derived field name
  # conditionExprIds: String which has the ids of the expr that holds the condition code which when true the corresponsing true code sets this derived field. Each expr id is seperated by a comma
  # exprBlockIds: String which has the ids of the expr tokens which is run when the corresponsing condition expr is set to true. Each expr id is seperated by a comma and matches with one from the conditionExprIds
  
  childrenForRootExp <- getChildTokensForParent(expr, tokens)
  
  # The expr tokens which are run when the condition for this if statement evaluates to true. The expr token which holds this code is always the 5th one in the list
  whenConditionTrueExprs <- getExprTokens(getChildTokensForParent(childrenForRootExp[5, ], tokens))
  
  isElse <- childrenForRootExp[1, 'text'] == '{'
  # When the expr is an else statement then the first child is a { and the whenConditionTrueExprs are not at the 5 place in the array so get all the expr tokens
  if(isElse) {
    whenConditionTrueExprs <- getExprTokens(childrenForRootExp)  
  }
  
  for(i in 1:nrow(whenConditionTrueExprs)) {
    derivedFieldName <- getDerivedFieldNameOrFunctionNameForTokens(getDescendantsOfToken(whenConditionTrueExprs[i, ], tokens))
    
    if(!isElse) {
      derivedFieldsSet[derivedFieldName, 'conditionExprId'] <- childrenForRootExp[3, 'id']
    }
    # If this is an else statement then there is no condition expr so set it to NA
    else {
      derivedFieldsSet[derivedFieldName, 'conditionExprId'] <- NA
    }
    
    derivedFieldsSet[derivedFieldName, 'exprBlockId'] <- whenConditionTrueExprs[i, 'id']
  }
  
  elsePmmlStrings <- NA
  if(!is.na(childrenForRootExp[6, ]) & childrenForRootExp[6, 'token'] == 'ELSE') {
    elsePmmlStrings <- getPmmlStringForIfExpr(
      childrenForRootExp[7, ],
      tokens, 
      comment_tokens,
      evaluated_variables,
      FALSE
    )
  }
  
  derivedFieldNameWithPmmlString <- list()
  for(derivedFieldName in row.names(derivedFieldsSet)) {
    pmml_str <- ''
    
    conditionPmmlString <- getPmmlStringForExpr(getExprWithIdInTokens(derivedFieldsSet[derivedFieldName, 'conditionExprId'], tokens), tokens)
    whenTruePmmlString <- getDerivedFieldPmmlStringForTokens(
      getDescendantsOfToken(getExprWithIdInTokens(derivedFieldsSet[derivedFieldName, 'exprBlockId'], tokens), tokens), 
      derivedFieldName, 
      comment_tokens,
      evaluated_variables,
      FALSE
    )
    whenFalsePmmlString <- glue::glue('<FieldRef field="{derivedFieldName}"/>')
    if(!(is.na(elsePmmlStrings) | (derivedFieldName %in% row.names(elsePmmlStrings)) == FALSE)) {
      whenFalsePmmlString <- elsePmmlStrings[derivedFieldName, 'pmmlString']
    }
    
    if(conditionPmmlString != '') {
      pmml_str <- glue::glue('<Apply function="if">{conditionPmmlString}{whenTruePmmlString}{whenFalsePmmlString}</Apply>')
    } else {
      pmml_str <- whenTruePmmlString
    }
    
    derivedFieldNameWithPmmlString[[length(derivedFieldNameWithPmmlString) + 1]] <- list(
      derived_field_name = derived_field_name,
      pmml_str = pmml_str
    )
  }
  
  # This if loop will add the derived fields which have been initialised in the
  # else part of the if loop to the master list if they have not been put in there
  # already
  if(!is.na(elsePmmlStrings)) {
    for(derivedFieldName in row.names(elsePmmlStrings)) {
      not_in_derived_fields_list <- FALSE
      if(length(derivedFieldNameWithPmmlString) != 0) {
        for(i in 1:length(derivedFieldNameWithPmmlString)) {
          if(derivedFieldName != derivedFieldNameWithPmmlString[[i]]$derived_field_name) {
            not_in_derived_fields_list <- TRUE
          }
        }
      } else {
        not_in_derived_fields_list <- TRUE
      }
      
      if(not_in_derived_fields_list) {
        derivedFieldNameWithPmmlString[[length(derivedFieldNameWithPmmlString) + 1]] <- list(
          derived_field_name = derivedFieldName,
          pmml_str = elsePmmlStrings[derivedFieldName, 'pmmlString']
        )
      }
    }
  }

  return(derivedFieldNameWithPmmlString)
}

if_expr.get_cond_expr_to_block_exprs_map <- function(
  expr,
  tokens,
  cond_expr_to_block_exprs_mappings = list()
) {
  child_tokens <- getChildTokensForParent(expr, tokens)
  child_expr_tokens <- getExprTokens(child_tokens)
  
  found_if_token <- FALSE
  for(i in 1:nrow(child_tokens)) {
    cur_child_token <- child_tokens[i, ]
    
    if(cur_child_token$token == IF_TOKEN) {
      found_if_token <- TRUE
      
      cond_expr_id <- child_expr_tokens[1, "id"]
      block_expr_ids <- getExprTokens(
        getChildTokensForParent(child_expr_tokens[2, ], tokens))$id
      cond_expr_to_block_exprs_mappings[[length(cond_expr_to_block_exprs_mappings) + 1]] <- list(
        cond_expr_id = cond_expr_id,
        block_expr_ids = block_expr_ids
      )
    }
    
    if(cur_child_token$token == ELSE_TOKEN) {
      cond_expr_to_block_exprs_mappings <- if_expr.get_cond_expr_to_block_exprs_map(
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