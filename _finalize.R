Sys.setenv(R_USER="")
dst <- "D:/github.io/now/20250205"
ret1 <- try(system("cmd /c render.bat cameral.Rmd rebuild www remark      fileout=show.html"))
stopifnot(!inherits(ret1,"try-error"))
unlink("Rlibs",recursive=TRUE)
ret2 <- try(system("cmd /c render.bat cameral.Rmd rebuild www vignet toc2 fileout=view.html"))
stopifnot(!inherits(ret2,"try-error"))
try(unlink("Rlibs",recursive=TRUE))
cond <- 0L
if (file.exists("show.html")) {
   file.copy("show.html",dst,overwrite=T,copy.date=T)
   cond <- cond+1L
}
if (file.exists("view.html")) {
   file.copy("view.html",dst,overwrite=T,copy.date=T)
   cond <- cond+1L
}
if (cond>0)
   system(paste("robocopy","assets",file.path(dst,"assets"),"/MIR"))
