source(file.path(getwd(), 'R', 'local-transformations-generator.R'))

outDirPath <- file.path(getwd(), 'out')
if(dir.exists(outDirPath) == FALSE) {
  dir.create(outDirPath)
}
cat(getPmmlStringFromRFile(file.path(getwd(), 'R', './test-R-files/test-in.R'), TRUE), file=paste(outDirPath, '/out.xml', sep=''))
