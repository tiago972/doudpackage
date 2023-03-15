
# use_r("univariate")
# use_r("tools")
# use_r("parse")
# use_r("descTab")
# use_r("bivariate")
# use_r("Classes")
# use_gpl_license()
#
# use_package("stringi")
# use_package("dplyr")
# use_package("purrr")
# use_package("tibble")
# use_package("kableExtra")
#
# use_package("stats")
# use_package("methods")
# use_package("tidyr")
# usethis::use_package("parallel")


library(devtools)
setwd("/Users/tiago2/BF/doudpackage")
tryCatch({
  #.rs.restartR()
  devtools::unload(package = "doudpackage")
},
error=function(e){
  print(e)
})
devtools::document()
devtools::check()

devtools::build(pkg = ".", path = ".")

devtools::check_win_release(".")

library(rhub)
devtools::check_rhub()
