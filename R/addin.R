library('rstudioapi')
library('glue')

pmml_addin <- function() {
  r_file_path <- rstudioapi::getActiveDocumentContext()$path

  out_dir_path <- file.path(getwd(), 'out')
  if(dir.exists(out_dir_path) == FALSE) {
    dir.create(out_dir_path)
  }
  out_file_path = paste(out_dir_path, '/out.xml', sep='')
  cat(pmml::get_pmml_string_from_r_file(r_file_path, TRUE), file=out_file_path)

  print(glue::glue('Done. The output file should be in {out_file_path}'))
}
