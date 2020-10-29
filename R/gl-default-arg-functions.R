# This variable keeps track of all the functions where at least one parameter
# can is defaulted if it is missing
globals_is_default_param_function <- function(function_name) {
  found_gl_default_param_function <- globals_get_default_param_function(function_name)

  return(is.list(found_gl_default_param_function))
}

globals_get_default_param_function <- function(function_name) {
  gl_default_param_function <- NA
  if(exists("gl_default_param_functions")) {
    if(length(gl_default_param_functions) != 0) {
      for(i in 1:length(gl_default_param_functions)) {
        if(gl_default_param_functions[[i]]$func_name == function_name) {
          gl_default_param_function <- gl_default_param_functions[[i]]
          break
        }
      }
    }
  }

  return(gl_default_param_function)
}

globals_add_default_param_function <- function(function_name, num_function_params, num_defaulted_params) {
  if(exists("gl_default_param_functions") == FALSE) {
    gl_default_param_functions <<- list()
  }

  gl_default_param_functions[[length(gl_default_param_functions) + 1]] <<- list(
    func_name = function_name,
    num_function_params = num_function_params,
    num_defaulted_params = num_defaulted_params
  )
}
