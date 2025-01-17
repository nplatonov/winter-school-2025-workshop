#'
#+ include=FALSE
a <- readxl::read_excel("documents/Мск_Морское_биоразнообразие_Итог от МФТИ.xlsx"
                       ,.name_repair="minimal")
b <- readxl::read_excel("documents/Только прошедшие на второй этап.xlsx"
                       ,.name_repair="minimal")[,1:24]
a[["Второй этап"]] <- "да"
# is.na(match(a[[1]],b[[1]]))
a[["Второй этап"]][is.na(match(a[[1]],b[[1]]))] <- "нет"
b <- readxl::read_excel("documents/Отбор студентов ЗШ ПУ 2025_фин.xlsx"
                          ,sheet=1,.name_repair="minimal")
d <- readxl::read_excel("documents/Расписание ЗШ ПУ 2025 ИПЭЭ РАН вер.15_01_25.xlsx"
                          ,sheet="Список участников",.name_repair="minimal")
#colnames(a)[1] <- colnames(b)[1]
aname <- colnames(a)
bname <- colnames(b)
fio1 <- paste(d[["Фамилия"]],d[["Имя"]],d[["Отчество"]])
fio2 <- paste(a[["Фамилия"]],a[["Имя"]],a[["Отчество"]])
#match(fio1,fio2)
#match(fio2,fio1)
#substr(aname,1,72)
#substr(bname,1,72)
ind <- match(a[[1]],b[[1]])
#b <- b[na.omit(ind),]
#a <- a[which(!is.na(ind)),]
indA <- which(!is.na(ind))
indB <- c(na.omit(ind))
ind2 <- match(bname,aname)
ind2A <- c(na.omit(ind2))
ind2B <- which(is.na(ind2))
a <- a[indA,]
a2 <- b[indB,ind2B]
a <- cbind(a,a2)
# substr(colnames(a),1,72)
if (FALSE) {
   print(colnames(a))
   a2 <- a[,c(1,3,4,5,9)]
   a2$'R' <- ""
   a2$'GIS' <- ""
   str(a2)
   xlsx::write.xlsx(a2,"background_new.xlsx")
} else {
   a2 <- readxl::read_excel("background.xlsx",.name_repair="minimal")
   a2[[1]] <- NULL
   ind <- match(a2[[1]],a[[1]])
   a[['Бэкграунд']] <- a2[['Бэкграунд']][ind]
  # str(a)
}
#+
if (FALSE) {
   # print(substr(colnames(a),1,72))
   a <- a[,c(3,4,5,30,53)]
   colnames(a)[5] <- "score"
   a$score <- a$score#*3
   a <- a[order(a$score,decreasing=TRUE),]
   rownames(a) <- NULL
   knitr::kable(a,digits=2)
}
