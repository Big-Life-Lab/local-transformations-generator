#' Helper function to run assertions
#'
#' @param file_path The path to a file in the assets/test folder. You only need
#' to provide the part of the path within the assets/test folder.
#' @param expected_pmml Expected PMML to be generated
#'
#' @return
#' @export
#'
#' @examples
test_utils.test_code_file <- function(file_path, expected_pmml) {
  formatted_expected_pmml <- gsub("[\r\n]", "", expected_pmml)
  actual_pmml <- getPmmlStringFromRFile(
    file.path("../../assets/test", file_path), 
    srcFile = TRUE
  )
  
  expect_equal(actual_pmml, formatted_expected_pmml)
}