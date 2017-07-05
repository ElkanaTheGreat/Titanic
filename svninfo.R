library(XML)

GetRevision <- function() {
  svninfo <- paste(system("svn info --xml", intern = T), collapse = "")
  doc <- htmlParse(svninfo, asText = T)
  svnpath <- xpathSApply(doc, "//repository/root", xmlValue)
  
  svninfo <- paste(system(paste("svn info", svnpath, "--xml"), intern = T), collapse = "")
  doc <- htmlParse(svninfo, asText = T)
  return (as.integer(xpathSApply(doc, "//commit/@revision")))
}