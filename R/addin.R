library('rstudioapi')
library('glue')
source(file.path(getwd(), 'R', 'local-transformations-generator.R'))

pmmlAddin <- function() {
  rFilePath <- rstudioapi::getActiveDocumentContext()$path

  outDirPath <- file.path(getwd(), 'out')
  if(dir.exists(outDirPath) == FALSE) {
    dir.create(outDirPath)
  }
  outFilePath = paste(outDirPath, '/out.xml', sep='')
  cat(getPmmlStringFromRFile(rFilePath, TRUE), file=outFilePath)

  print(glue::glue('Done. The output file should be in {outFilePath}'))
}
