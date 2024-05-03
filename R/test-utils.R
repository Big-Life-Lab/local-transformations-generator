#' Run a test for the get_pmml_string_from_r_file function
#'
#' @param code string Contains the code for the R file that will be passed
#' into the function
#' @param expected_pmml string | NULL When testing if the function returns the
#' right PMML, this parameter should be set to the expected PMML
#' @param expected_error string | NULL When testing if the function throws the 
#' right error, this parameter should be set to the expected error
#' @param files named_list optional Use this parameter to set any additional
#' files that the test may need. For example, other R files that the main file
#' sources or CSV files that the main file reads in. The schema is below:
#' [file_name]: list(
#'     type: **R** for R files or **CSV** for csv files
#'     contents: string for R files or a data.frame for csv files
#' )
test_utils_run_generate_pmml_test <- function(code, 
                                              expected_pmml = NULL,
                                              expected_error = NULL,
                                              files = list()
                                              ) {
  # Creates all the files that the test needs and adds them to a test 
  # directory 
  code_dir <- tempdir()
  if(!dir.exists(code_dir)) {
    dir.create(code_dir)
  }
  for(file_name in names(files)) {
    file_info <- files[[file_name]]
    file_contents <- NA
    file_extension <- NA
    if(file_info$type == "R") {
      file_contents <- file_info$contents
      file_extension <- '.R'
    }
    else if(file_info$type == "CSV") {
      file_contents <- csv_stringify_data_frame(file_info$contents)
      file_extension <- '.csv'
    }
    else {
      stop(paste("Unknown file type", file_info$type))
    }
    file_path <- file.path(code_dir, paste(file_name, file_extension, sep = ''))
    file.create(file_path)
    write(file_contents, file = file_path) 
  }
  code_file_path <- file.path(code_dir, "code.R")
  file.create(code_file_path)
  write(code, file = code_file_path)
  
  # Calls the function making sure to run the right assert depending on the
  # expected_pmml and expect_error arguments
  # The code changes the working directory to the temporary directory where
  # the files are stored so that any code that requires other files works
  # without any errors.
  # Make sure to set the working directory to the correct one after
  current_working_directory <- getwd()
  setwd(code_dir)
  if (!is.null(expected_pmml)) {
      formatted_expected_pmml <- prettify_xml(expected_pmml)
      formatted_actual_pmml <- prettify_xml(pmml::get_pmml_string_from_r_file(
        code_file_path,
        src_file = TRUE,
        log = FALSE
      ))

      expect_equal(formatted_actual_pmml, formatted_expected_pmml)
  }
  else if(!is.null(expected_error)) {
      expect_error(
        pmml::get_pmml_string_from_r_file(
            code_file_path,
            src_file = TRUE,
            log = FALSE
        ), 
        expected_error
      )
  } 
  else {
     stop('No assert specified. Either specify the expected_pmml or the 
          expected_error test')
  }
  setwd(current_working_directory)
  
  # Deletes the temporary directory
  unlink(code_dir, recursive = TRUE)
}

#' Converts a data.frame to a CSV string
#'
#' @param data_frame data.frame The data frame to convert 
#'
#' @return string The CSV string
csv_stringify_data_frame <- function(data_frame) {
  return(paste(
    capture.output(write.csv(data_frame, row.names = FALSE)), 
    collapse = "\n"
  ))
}

prettify_xml <- function(xml_str) {
    return(XML::saveXML(XML::xmlParse(xml_str)))
}
