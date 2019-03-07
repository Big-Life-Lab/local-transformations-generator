# Checks whether the commentTokens has a row which is a custom pmml function comment
get_custom_pmml_func_comment_token <- function(comment_tokens) {
  custom_pmml_function_regex <- "#[ ]*@pmml_custom_func[ ]*\\(.+\\)"

  return(
    comment_tokens[which(grepl(custom_pmml_function_regex, comment_tokens$text)), ][1, ]
    )
}

Z_SCORE <- 'z_score'
# Returns the PMML node for a custom function
get_pmml_node_for_pmml_func_comment_token <- function(
  comment_token, 
  evaluated_variables
) {
  # Split the pmml custom function comment into a list where each entry corresponds to it's arguments
  custom_function_args <- strsplit(
    gsub("[\\(\\)]", "", strsplit(comment_token$text, '@pmml_custom_func')[[1]][[2]]),
    ","
  )[[1]]
  
  custom_function_name <- custom_function_args[[1]]
  
  if(custom_function_name == Z_SCORE) {
    return(
      get_pmml_node_for_z_score_func(custom_function_args, evaluated_variables)
    )
  } else {
    stop(glue::glue('Unknown pmml custom function {custom_function_name}'))
  }
}

# Returns the PMML node for a Z-score pmml custom function
get_pmml_node_for_z_score_func <- function(
  custom_function_args, evaluated_variables
) {
  variable <- trimws(custom_function_args[[2]])
  reference_table_name <- trimws(custom_function_args[[3]])
  reference_table <- evaluated_variables[[reference_table_name]]
  
  mean <- reference_table[which(reference_table$variable == variable), ]$mean
  sd <- reference_table[which(reference_table$variable == variable), ]$sd
  
  return(glue::glue('<Apply function="zScore"><Constant dataType="double">{mean}</Constant><Constant dataType="double">{sd}</Constant><FieldRef field="{variable}"/></Apply>'))
}