source(file.path(getwd(), 'R', 'local-transformations-generator.R'))

outDirPath <- file.path(getwd(), 'out')
if(dir.exists(outDirPath) == FALSE) {
  dir.create(outDirPath)
}
# file.path(getwd(), 'Transformation Creation Files', 'MPoRT Male Transformations.R')
# file.path(getwd(), 'R', 'test-R-files', 'test-function.R')
cat(getPmmlStringFromRFile(file.path(getwd(), 'R', 'test-R-files', 'test-function.R'), TRUE), file=paste(outDirPath, '/out.xml', sep=''))
