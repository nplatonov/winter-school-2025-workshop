# install.packages("C:/platt/R/ursa-package/ursa_3.11.2-1158.zip")
.libPaths()
if (!dir.exists("Rlibs"))
   dir.create("Rlibs")
.libPaths(c("Rlibs",.libPaths()))
cat("------------\n")
.libPaths()
pkgList <- c("png","mapview","adehabitatLT","readxl","tidyr","lwgeom"
            ,"geosphere","ggstatsplot","ursa")
repos <- "https://cloud.r-project.org/"
type <- if (.Platform$OS.type=="windows") "binary" else getOption("pkgType")
available <- sapply(pkgList,function(pkg) {
   if (requireNamespace(pkg))
      return(TRUE)
   install.packages(pkg,repos=repos,type=type)
   requireNamespace(pkg)
})
if (packageVersion("ursa")<"3.11.2")
   install.packages("ursa",repos=repos,type=type)
available
c('Everything is ok?'=all(available))
