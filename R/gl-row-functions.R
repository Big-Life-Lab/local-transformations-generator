# This variable keeps track of all the functions where at least one parameter
# is a row
globals_is_row_function <- function(function_name) {
  found_gl_row_function <- globals_get_row_function(function_name)

  return(is.list(found_gl_row_function))
}

globals_get_row_function <- function(function_name) {
  gl_row_function <- NA
  if(exists("gl_row_functions")) {
    if(length(gl_row_functions) != 0) {
      for(i in 1:length(gl_row_functions)) {
        if(gl_row_functions[[i]]$func_name == function_name) {
          gl_row_function <- gl_row_functions[[i]]
          break
        }
      }
    }
  }

  return(gl_row_function)
}
