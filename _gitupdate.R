desc <- commandArgs(TRUE)
if (!length(desc)) {
   desc <- c("Initial commit","Ongoing activity")[2]
}
if (T) {
   ret <- c(add=999,commit=999,push=999)
   ret[1] <- system("git add -A")
   if (!(ret[2] <- system(paste0("git commit -m",dQuote(desc)))))
      ret[3] <- system("git push")
   print(ret)
}
