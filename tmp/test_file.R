setwd('/Users/tiago2/BF/doudpackage/R/Non normal')
library(dplyr)
load('.RData')
setwd('/Users/tiago2/BF/doudpackage/R/Non normal')
sourceEntireFolder <- function(folderName, verbose=FALSE, showWarnings=TRUE) { 
  files <- list.files(folderName, full.names=TRUE)
  
  # Grab only R files
  files <- files[ grepl("\\.[rR]$", files) ]
  
  if (!length(files) && showWarnings)
    warning("No R files in ", folderName)
  
  for (f in files) {
    if (verbose)
      cat("sourcing: ", f, "\n")
    ## TODO:  add caught whether error or not and return that
    try(source(f, local=FALSE, echo=FALSE), silent=!verbose)
  }
  return(invisible(NULL))
}



sourceEntireFolder('.')
bdd.tmpGlobal<-select(bdd.merge, Age, `Charlson`, `Max Temperature`,  `Fréquence.respiratoire.max...min.`, 
                      `Min Lymphocytes (G/L)`, `Max CRP (mg/L)`, `Length of stay`, Corticotherapy, Sex)
bdd.tmpGlobal<-filter(bdd.tmpGlobal, !is.na(Corticotherapy))
ft_parse(univ_CTCGlobal<-ft_desc_tab(bdd.annexe ,group = "Corticotherapy", na.print = T, nonnormal = 2), bdd.annexe, group = "Corticotherapy", col.order = c("1","0"))

