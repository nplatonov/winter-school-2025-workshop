#' ---
#' toc: true
#' toc_depth: 3
#' ---

#+ include=FALSE
b <- readxl::read_excel("Только прошедшие на второй этап.xlsx"
                       ,.name_repair="minimal")[,1:24]
a <- readxl::read_excel("Мск_Морское_биоразнообразие_Итог от МФТИ.xlsx"
                       ,.name_repair="minimal")
colnames(a)[1] <- colnames(b)[1]
aname <- colnames(a)
bname <- colnames(b)
ind <- match(a[[1]],b[[1]])
b <- b[na.omit(ind),]
a <- a[which(!is.na(ind)),]
ind <- which(is.na(match(bname,aname)))
a[[aname[ind]]] <- b[[ind]]
cname <- colnames(a)
options(knitr.kable.NA = ' ')
seed <- if (F) 894 else sample(100:999,1)
c(seed=seed)
set.seed(seed)

#+ child
level2 <- lapply(sample(nrow(a)),function(i) knitr::knit_expand(file="details-lev2.Rmd"))
#+ level2, eval=!ursa:::.isKnitr()
cat(unlist(level2),sep="\n")

#' `r knitr::knit(text = unlist(level2))`
