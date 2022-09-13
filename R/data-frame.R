# Check whether this expr is for accessing the row in a data frame for eg.,
# table[col1 == "val1", ]
# tabke["val1", "col2"]
data_frame_is_expr <- function(expr, tokens) {
  child_tokens <- get_child_tokens_for_parent(expr, tokens)

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
data_frame_is_wildcard_expr <- function(expr, tokens) {
  child_tokens <- get_child_tokens_for_parent(expr, tokens)
  expr_child_tokens <- get_expr_tokens(child_tokens)

  return(nrow(expr_child_tokens) != 3)
}

# Check whether this expression is of the type table[table$row == var1, ]$col
data_frame_is_col_access <- function(expr, tokens) {
  if(dollar_op_is_expr(expr, tokens) == FALSE) {
    return(FALSE)
  }

  child_expr_tokens <- get_expr_tokens(
    get_child_tokens_for_parent(expr, tokens))

  return(data_frame_is_expr(child_expr_tokens[1, ], tokens))
}

# Check whether this expression is of the type table[table$row == var1, ]
data_frame_is_row_access <- function(expr, tokens) {
  return(dollar_op_is_expr(expr, tokens) == TRUE & data_frame_is_col_access(expr, tokens) == FALSE)
}

# This parses data frame access code into PMML.
# table["column1", "output"]
# table[table$sex == "male", ]
data_frame_get_pmml_node <- function(expr, tokens, scope_variables) {
  child_tokens <- get_child_tokens_for_parent(expr, tokens)
  expr_child_tokens <- get_expr_tokens(child_tokens)
  table_name <- data_frame_get_table_name(expr, tokens)
  location <- ifelse(table_name %in% scope_variables, "local", "taxonomy")

  # A wildcard data frame query is when the column names are not specified inside
  # the [], for example table[1, ]
  if(data_frame_is_wildcard_expr(expr, tokens) == FALSE) {
    output_column_name <- format_symbol_name(get_child_tokens_for_parent(child_tokens[5, ], tokens)[1, ])
    index_column_value <- format_symbol_name(get_child_tokens_for_parent(child_tokens[3, ], tokens)[1, ])
    field_column_pair_string <- glue::glue('<FieldColumnPair column="index" constant="{index_column_value}"/>')

    return(glue::glue('<MapValues outputColumn="{output_column_name}">{field_column_pair_string}<TableLocator location="{location}" name="{table_name}"/></MapValues>'))
  } else {
    # The string which at the end of the following loop will have all the FieldColumnPairs
    field_column_pairs <- ''

    data_frame_iterate_column_conditions(expr, tokens, function(column, field_or_constant) {
      # Make the column pmml string
      column_string <- glue::glue('column="{column$text}"')
      # Make the field or constant pmml string
      field_or_constant_string <- ifelse(is_symbol_token(field_or_constant), glue::glue('field="{field_or_constant$text}"'), glue::glue('constant="{format_constant_token_text(field_or_constant)}"'))

      # Make the FieldColumnPair string and append it to the master list
      field_column_pairs <<- paste(field_column_pairs, glue::glue('<FieldColumnPair {column_string} {field_or_constant_string}/>'), sep = "")
    })

    return(glue::glue('{field_column_pairs}<TableLocator location="{location}" name="{table_name}"/>'))
  }
}

# Returns the name of the table accessed by this data frame access expression
data_frame_get_table_name <- function(expr, tokens) {
  child_tokens <- get_child_tokens_for_parent(expr, tokens)

  if(data_frame_is_wildcard_expr(expr, tokens) == FALSE) {
    table_name <- format_symbol_name(get_child_tokens_for_parent(child_tokens[1, ], tokens)[1, ])

    return(table_name)
  } else {
    expr_child_tokens <- get_expr_tokens(child_tokens)

    # The first token in the above child tokens is an expressions which has the name of the table we want to search
    expr_token_with_table_name <- expr_child_tokens[1, ]

    # Get the name of the table
    table_name <- get_child_tokens_for_parent(expr_token_with_table_name, tokens)[1, ]$text

    return(table_name)
  }
}

# For the data frame expression token specified in the expr parameter
# it will call the passed iterator function for each pair of search
# condition passing in the column name as the first argument and the
# field or constant to match to the column value as the second argument
data_frame_iterate_column_conditions <- function(expr, tokens, iterator) {
  child_tokens <- get_child_tokens_for_parent(expr, tokens)
  expr_child_tokens <- get_expr_tokens(child_tokens)

  expr_token_with_table_entire_search_conditions <- expr_child_tokens[2, ]

  # Get the descendants of the expr token with table entires. It will have the information we need for the FieldColumnPairs
  tokens_to_use_for_field_column_pair_strings <- get_descendants_of_token(expr_token_with_table_entire_search_conditions, tokens)

  # Go though the descendants
  for(i in 1:nrow(tokens_to_use_for_field_column_pair_strings)) {
    # If the token is op type $
    if(tokens_to_use_for_field_column_pair_strings[i, 'token'] == "'$'") {
      # The token  after this is the column referenced in the table
      column <- tokens_to_use_for_field_column_pair_strings[i+1, ]
      # The token 2 after this is the field or constant we need to compare the column to
      field_or_constant <- tokens_to_use_for_field_column_pair_strings[i+3, ]

      iterator(column, field_or_constant)
    }
  }
}

get_table_expression <- function(expr, tokens) {
  child_tokens <- get_child_tokens_for_parent(expr, tokens)
  table_expr <- child_tokens[child_tokens$token == EXPR_TOKEN, ][1, ]
  return(table_expr)
}
