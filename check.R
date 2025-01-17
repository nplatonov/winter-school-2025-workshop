list1 <- dir(path="input",full.names=TRUE)
file.info(list1)[,c("size","isdir","mtime")]


