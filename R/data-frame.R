# Check whether this expr is for accessing the row in a data frame for eg.,
# table[col1 == "val1", ]
# tabke["val1", "col2"]
data_frame.is_expr <- function(expr, tokens) {
  child_tokens <- getChildTokensForParent(expr, tokens)
  
  if(nrow(child_tokens) == 0) {
    return(FALSE)
  }
  else if(nrow(child_tokens) > 2 & child_tokens[2, 'text'] == '[' & child_tokens[nrow(child_tokens), 'text'] == ']') {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Check whether this data rame expr has a wild card access in it
data_frame.is_wildcard_expr <- function(expr, tokens) {
  child_tokens <- getChildTokensForParent(expr, tokens)
  expr_child_tokens <- getExprTokens(child_tokens)
  
  return(nrow(expr_child_tokens) != 3)
}

# Check whether this expression is of the type table[table$row == var1, ]$col
data_frame.is_col_access <- function(expr, tokens) {
  if(dollar_op.is_expr(expr, tokens) == FALSE) {
    return(FALSE)
  }
  
  child_expr_tokens <- getExprTokens(
    getChildTokensForParent(expr, tokens))
  
  return(data_frame.is_expr(child_expr_tokens[1, ], tokens))
}

# Check whether this expression is of the type table[table$row == var1, ]
data_frame.is_row_access <- function(expr, tokens) {
  return(dollar_op.is_expr(expr, tokens) == TRUE & data_frame.is_col_access(expr, tokens) == FALSE)
}

data_frame.get_pmml_node <- function(expr, tokens) {
  child_tokens <- getChildTokensForParent(expr, tokens)
  expr_child_tokens <- getExprTokens(child_tokens)
  
  if(data_frame.is_wildcard_expr(expr, tokens) == FALSE) {
    outputColumnName <- formatSymbolName(getChildTokensForParent(child_tokens[5, ], tokens)[1, ])
    indexColumnValue <- formatSymbolName(getChildTokensForParent(child_tokens[3, ], tokens)[1, ])
    tableName <- formatSymbolName(getChildTokensForParent(child_tokens[1, ], tokens)[1, ])
    
    fieldColumnPairString <- glue::glue('<FieldColumnPair column="index" constant="{indexColumnValue}"/>')
    return(glue::glue('<MapValues outputColumn="{outputColumnName}">{fieldColumnPairString}<TableLocator location="taxonomy" name="{tableName}"/></MapValues>'))
  } else {
    # The first token in the above child tokens is an expressions which has the name of the table we want to search
    exprTokenWithTableName <- expr_child_tokens[1, ]
    # Get the name of the table
    tableName <- getChildTokensForParent(exprTokenWithTableName, tokens)[1, ]$text

    # Get the expr token which has the table search conditions like tableName$col == 'a' along with the AND between the conditions
    exprTokenWithTableEntireSearchConditions <- expr_child_tokens[2, ]

    # Get the descendants of the expr token with table entires. It will have the information we need for the FieldColumnPairs
    tokensToUseForFieldColumnPairStrings <- getDescendantsOfToken(exprTokenWithTableEntireSearchConditions, tokens)

    # The string which at the end of the following loop will have all the FieldColumnPairs
    fieldColumnPairs <- ''
    
    # Go though the descendants
    for(i in 1:nrow(tokensToUseForFieldColumnPairStrings)) {
      # If the token is op type $
      if(tokensToUseForFieldColumnPairStrings[i, 'token'] == "'$'") {
        # The token  after this is the column referenced in the table
        column <- tokensToUseForFieldColumnPairStrings[i+1, ]
        # The token 2 after this is the field or constant we need to compare the column to
        fieldOrConstant <- tokensToUseForFieldColumnPairStrings[i+3, ]
        
        # Make the column pmml string
        columnString <- glue::glue('column="{column$text}"')
        # Make the field or constant pmml string
        fieldOrConstantString <- ifelse(isSymbolToken(fieldOrConstant), glue::glue('field="{fieldOrConstant$text}"'), glue::glue('constant="{formatConstantTokenText(fieldOrConstant)}"'))
        # Make the FieldColumnPair string and append it to the master list
        fieldColumnPairs <- paste(fieldColumnPairs, glue::glue('<FieldColumnPair {columnString} {fieldOrConstantString}/>'))
      }
    }
    
    return(paste(fieldColumnPairs, glue::glue('<TableLocator location="taxonomy" name="{tableName}"/>'), sep = ''))
  }
}