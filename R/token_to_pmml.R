is_boolean_data_type <- function(token) {
  return((token$text == 'TRUE' | token$text == 'FALSE') & token$token == NUM_CONST_TOKEN)
}

get_pmml_string_for_symbol <- function(symbol) {
  field_ref_name <- symbol$text

  return(glue::glue('<FieldRef field="{field_ref_name}"/>'))
}

format_symbol_name <- function(symbol) {
  return(gsub("'|\"", "", symbol$text))
}

format_constant_token_text <- function(constant) {
  formatted_value <- constant$text
  if(constant$token == STR_CONST_TOKEN) {
    formatted_value <- gsub("'", "", constant$text)
    formatted_value <- gsub('"', "", formatted_value)
  }
  else if(constant$text == 'NA' & constant$token == NUM_CONST_TOKEN) {
    formatted_value <- 'NA'
  }
  else if(is_boolean_data_type(constant)) {
    formatted_value <- tolower(constant$text)
  }

  return(formatted_value)
}

get_pmml_string_for_constant <- function(constant) {
  data_type <- 'double'

  formattedValue <- format_constant_token_text(constant)

  if(constant$token == STR_CONST_TOKEN) {
    data_type <- 'string'
  }
  else if(constant$text == 'NA' & constant$token == NUM_CONST_TOKEN) {
    data_type <- 'NA'
  }
  else if(constant$token == NULL_CONST_TOKEN) {
    data_type <- 'NULL'
  }
  else if(is_boolean_data_type(constant)) {
    data_type <- 'boolean'
  }

  return(glue::glue('<Constant dataType="{data_type}">{formattedValue}</Constant>'))
}

get_pmml_string_for_logical_operator <- function(logical_token, nested_pmml_string) {
  function_type <- 'unknown'

  logical_token_token <- logical_token$token

  if(logical_token_token == AND_TOKEN | logical_token_token == AND2_TOKEN) {
    function_type <- 'and'
  } else if(logical_token_token %in% OR_TOKENS) {
    function_type <- 'or'
  } else if(logical_token_token == EQUAL_TO_TOKEN) {
    function_type <- 'equal'
  } else if(logical_token_token == NOT_EQUAL_TO_TOKEN) {
    function_type <- 'notEqual'
  } else if(logical_token_token == LESS_THAN_TOKEN) {
    function_type <- 'lessThan'
  } else if(logical_token_token == LESS_THAN_OR_EQUAL_TO_TOKEN) {
    function_type <- 'lessOrEqual'
  } else if(logical_token_token == GREATER_THAN_TOKEN) {
    function_type <- 'greaterThan'
  } else if(logical_token_token == GREATER_THAN_OR_EQUAL_TO_TOKEN) {
    function_type <- 'greaterOrEqual'
  } else if(logical_token_token == NOT_TOKEN) {
    function_type <- 'not'
  } else {
    stop(glue::glue('Unknown function_type for logical operator {logical_token}'))
  }

  return(glue::glue('<Apply function="{function_type}">{nested_pmml_string}</Apply>'))
}

get_pmml_string_for_math_token <- function(math_token, nested_pmml_string) {
  function_type <- gsub("'", "", math_token$token)
  if(function_type == "^") {
    function_type = "pow"
  }

  return(glue::glue('<Apply function="{function_type}">{nested_pmml_string}</Apply>'))
}

get_pmml_string_for_symbol_function_call <- function(function_name, nested_pmml_string) {
  return(glue::glue('<Apply function="{function_name}">{nested_pmml_string}</Apply>'))
}

get_pmml_string_for_function_arg_tokens <- function(function_arg_tokens) {
  if(nrow(function_arg_tokens) == 0) {
    return('')
  }

  parameters_pmml_string_for_function <- ''
  for(i in 1:nrow(function_arg_tokens)) {
    current_arg_name <- function_arg_tokens[i ,'text']
    parameter_pmml_string_for_current_arg_token <- glue::glue('<ParameterField name="{current_arg_name}" dataType="double"/>')
    parameters_pmml_string_for_function <- paste(parameters_pmml_string_for_function, parameter_pmml_string_for_current_arg_token, sep="")
  }

  return(parameters_pmml_string_for_function)
}

get_pmml_string_for_define_function <- function(function_name, function_args_tokens, function_body_pmml_string, additional_params = '') {
  return(glue::glue('<DefineFunction name="{function_name}">{get_pmml_string_for_function_arg_tokens(function_args_tokens)}{additional_params}{function_body_pmml_string}</DefineFunction>'))
}

get_pmml_string_for_colon_token <- function(nested_pmml_string) {
  return(glue::glue('<Apply function="colonOperator">{nested_pmml_string}</Apply>'))
}

