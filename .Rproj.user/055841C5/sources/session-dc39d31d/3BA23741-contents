library(tidyverse)

var <- c("a", "b", "c")
n <- length(var)
sequence <- seq(1, n)
combs <- purrr::map(sequence, combn, x = sequence, simplify = FALSE)
combs
combs <- combs %>% unlist(recursive = FALSE)

paste("teste ~ ", paste(var[combs[[1]]], collapse = " + "))

map(combs, paste, pastwe)


n <- length(var)
id <- unlist(lapply(1:n, function(i)combn(1:n,i,simplify=FALSE)),recursive=FALSE)
formulas <- sapply(id,function(i) paste("Data~",paste(var[i],collapse="+")))
formulas
