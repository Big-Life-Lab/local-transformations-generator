# This module provides functions to access the global variable called
# row_vars. This variable keeps track of all the variables in the program that
# store a data frame row, for eg.,
# row <- df[col1 == "a", col2 == b, ]
# as opposed to the staement below which actually retreives a column value,
# col_val <- df[col1 == "a", col2 == b, ]$col3

# The row_vars variable is a list where each item is a named list with the shape,
# row_var_name, string - The name of row variable in the code
# col_symbol_conditions, vector string - A vector that stores all the
# symbols used in the column conditional. In the above example, the value of
# this vector would be c("b")
# pmml_str, string - The PMML string to get this variable

# Retreives an item from the list whose row_var_name is field is the same
# as the passed row_var_name argument.
# Returns NA if nothing is found/
globals_get_row_var <- function(row_var_name) {
  found_row_var <- NA
  for(i in seq_len(length(row_vars))) {
    if(row_vars[[i]]$row_var_name == row_var_name) {
      found_row_var <- row_vars[[i]]
      break
    }
  }

  return(found_row_var)
}

# Adds a new item to the list. Check the comments above this file
# for informations about the parameter types.
globals_add_row_var <- function(row_var_name, col_symbol_conditions, pmml_str) {
    row_vars[[length(row_vars) + 1]] <<- list(
      row_var_name = row_var_name,
      col_symbol_conditions = col_symbol_conditions,
      pmml_str = pmml_str
    )
}

# Returns the PMML string for the row var matched to the passed
# row_var_name parameter
globals_get_pmml_str_for_row_var <- function(row_var_name) {
  row_var <- globals_get_row_var(row_var_name)
  if(is.na(row_var)) {
    return(NA)
  }

  return(row_var$pmml_str)
}

# Constructs ParameterField strings for all col symbols for the row vars
# that match those passed into the row_var_names vector
globals_get_parameter_field_strs_for_row_var <- function(row_var_names) {
  parameter_fields_str <- ''

  for(i in seq_len(length(row_var_names))) {
      current_row_var_name <- row_var_names[[i]]
      row_var <- globals_get_row_var(current_row_var_name)

      for(j in seq_len(length(row_var$col_symbol_conditions))) {
        current_col_symbol_condition <- row_var$col_symbol_conditions[[j]]
        field_name <- globals_format_col_symbol_name(
            current_row_var_name,
            current_col_symbol_condition
        )
        parameter_fields_str <- paste(
          parameter_fields_str,
          get_parameter_field_pmml_str(field_name),
          sep = ""
        )
      }
  }

  return(parameter_fields_str)
}

# Constructs a PMML string of FieldRefs for all the column symbols in
# the row vars matched to those in the row_var_names parameter.
# Ensures there are no duplicates.
# This will be used when constructing a call to a function that takes
# in a row as a parameter.
globals_get_col_symbol_field_refs_for_row_vars <- function(row_var_names) {
  field_refs_str <- ''
  added_col_symbols <- c()
  for(i in seq_len(length(row_var_names))) {
    row_var <- globals_get_row_var(row_var_names[[i]])

    for(j in seq_len(length(row_var$col_symbol_conditions))) {
      current_col_symbol_condition <- row_var$col_symbol_conditions[[j]]
      if(current_col_symbol_condition %in% added_col_symbols == FALSE) {
        field_refs_str <- paste(
          field_refs_str,
          get_field_ref_pmml_str(current_col_symbol_condition),
          sep = ""
        )
      }
    }
  }

  return(field_refs_str)
}

# Replaces the field references in a MapValue nodes with the ones to be used
# within a custom function.
# This is used when making the DefineFunction PMML str and there is a
# row parameter that uses variables for some of its column conditions.
globals_get_map_values_pmml_str_with_fields_replaced <- function(row_var_name) {
  row_var <- globals_get_row_var(row_var_name)

  pmml_str <- row_var$pmml_str
  for(i in seq_len(length(row_var$col_symbol_conditions))) {
    current_col_symbol_condition <- row_var$col_symbol_conditions[[i]]
    pmml_str <- gsub(
      glue::glue('field="{current_col_symbol_condition}"'),
      glue::glue('field="{globals_format_col_symbol_name(row_var, current_col_symbol_condition)}"'),
      pmml_str
    )
  }

  return(pmml_str)
}

globals_format_col_symbol_name <- function(row_var_name, col_symbol_name) {
    return(glue::glue("{row_var_name}A{col_symbol_name}"))
}
