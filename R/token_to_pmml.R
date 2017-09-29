source(file.path(getwd(), 'R', './tokens.R'))

getPmmlStringForSymbol <- function(symbol) {
  fieldRefName <- symbol$text

  return(glue::glue('<FieldRef field="{fieldRefName}"/>'))
}

getPmmlStringForConstant <- function(constant) {
  dataType <- 'double'
  value <- constant$text

  formattedValue <- value
  if(constant$token == STR_CONST_TOKEN) {
    dataType <- 'string'
    formattedValue <- gsub("'", "", value)
    formattedValue <- gsub('"', "", formattedValue)
  }
  else if(constant$text == 'NA' & constant$token == NUM_CONST_TOKEN) {
    dataType <- 'NA'
    formattedValue <- 'NA'
  }

  return(glue::glue('<Constant dataType="{dataType}">{formattedValue}</Constant>'))
}

getPmmlStringForLogicalOperator <- function(logicalToken, nestedPmmlString) {
  functionType <- 'unknown'

  logicalTokenToken <- logicalToken$token

  if(logicalTokenToken == AND_TOKEN | logicalTokenToken == AND2_TOKEN) {
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

getPmmlStringForFunctionArgTokens <- function(functionArgTokens) {
  if(nrow(functionArgTokens) == 0) {
    return('')
  }

  parametersPmmlStringForFunction <- ''
  for(i in 1:nrow(functionArgTokens)) {
    currentArgName <- functionArgTokens[i ,'text']
    parameterPmmlStringForCurrentArgToken <- glue::glue('<ParameterField name="{currentArgName}" dataType="double"/>')
    parametersPmmlStringForFunction <- paste(parametersPmmlStringForFunction, parameterPmmlStringForCurrentArgToken)
  }

  return(parametersPmmlStringForFunction)
}

getPmmlStringForDefineFunction <- function(functionName, functionArgsTokens, functionBodyPmmlString) {
  return(glue::glue('<DefineFunction name="{functionName}">{getPmmlStringForFunctionArgTokens(functionArgsTokens)}{functionBodyPmmlString}</DefineFunction>'))
}

