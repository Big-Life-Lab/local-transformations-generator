getPmmlStringForSymbol <- function(symbol) {
  fieldRefName <- symbol$text

  return(glue::glue('<FieldRef field="{fieldRefName}"/>'))
}

getPmmlStringForConstant <- function(constant) {
  dataType <- 'double'
  value <- constant$text

  if(typeof(constant) == 'character') {
    dataType <- 'string'
  }

  return(glue::glue('<Constant dataType="{dataType}">{value}</Constant>'))
}

getPmmlStringForLogicalOperator <- function(logicalToken, nestedPmmlString) {
  functionType <- 'unknown'

  logicalTokenToken <- logicalToken$token

  if(logicalTokenToken == AND_TOKEN) {
    functionType <- 'and'
  } else if(logicalTokenToken == OR_TOKEN) {
    functionType <- 'or'
  } else if(logicalTokenToken == EQUAL_TO_TOKEN) {
    functionType <- 'equal'
  } else if(logicalTokenToken == NOT_EQUAL_TO_TOKEN) {
    functionType <- 'notEqual'
  } else if(logicalTokenToken == LESS_THAN_TOKEN) {
    functionType <- 'lessThan'
  } else if(logicalTokenToken == LESS_THAN_OR_EQUAL_TO_TOKEN) {
    functionType <- 'lessOrEqual'
  } else if(logicalTokenToken == GREATER_THAN_TOKEN) {
    functionType <- 'greaterThan'
  } else if(logicalTokenToken == GREATER_THAN_OR_EQUAL_TO_TOKEN) {
    functionType <- 'greaterOrEqual'
  } else if(logicalTokenToken == NOT_TOKEN) {
    functionType <- 'not'
  } else {
    stop(glue::glue('Unknown functionType for logical operator {logicalToken}'))
  }

  return(glue::glue('<Apply function="{functionType}">{nestedPmmlString}</Apply>'))
}

getPmmlStringForMathToken <- function(mathToken, nestedPmmlString) {
  functionType <- gsub("'", "", mathToken$token)

  return(glue::glue('<Apply function="{functionType}">{nestedPmmlString}</Apply>'))
}

getPmmlStringForSymbolFunctionCall <- function(symbolFunctionCallToken, nestedPmmlString) {
  functionType <- symbolFunctionCallToken$text

  return(glue::glue('<Apply function="{functionType}">{nestedPmmlString}</Apply>'))
}
