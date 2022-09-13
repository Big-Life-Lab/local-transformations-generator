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
test_utils_test_code_file <- function(file_path, expected_pmml) {
  formatted_expected_pmml <- gsub("[\r\n]", "", expected_pmml)
  actual_pmml <- pmml::get_pmml_string_from_r_file(
    file.path("../../assets/test", file_path),
    src_file = TRUE,
    log = FALSE
  )

  expect_equal(actual_pmml, formatted_expected_pmml)
}
