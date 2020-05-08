# This variable keeps tracks of all the variables in the program which are rows

globals.get_pmml_str_for_row_var_name <- function(row_var_name) {
  return(row_vars[row_vars$row_name == row_var_name, "pmml"])
}