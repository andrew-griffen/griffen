library(tidyverse)
library(griffen)

tbl1 <- tibble(id = c("1","2","3"), drew1 = c(1,2,3), drew2 = c(4,5,6), drew3 = c(7,8,9))
tbl2 <- tibble(id = c(1,2,3), a = c(1,2,3), b = c(4,5,6) , c = c(7,8,9))
tbl3 <- tibble(bla1l128a = c(1,2,3), id = c(1,2,3), ahfka1 = c(4,5,6), fdhsfka = c(7,8,9))
tbl4 <- tibble(id = c(1,2,3), contribution_round1 = c(100,200,300), contribution_round2 = c(300,200,200))

n <- 10
rounds <- 100
tbl5 <- matrix(100*round(runif(n*rounds,0,6)),n,rounds) %>% as.data.frame()
names(tbl5) <- paste(rep("contribution_round",rounds),1:rounds,sep="")
tbl5 <- as_tibble(tbl5)
tbl5 <- tbl5 %>% mutate(id = 1:nrow(tbl5)) %>% left()


usethis::use_data(tbl1,overwrite=TRUE)
usethis::use_data(tbl2,overwrite=TRUE)
usethis::use_data(tbl3,overwrite=TRUE)
usethis::use_data(tbl4,overwrite=TRUE)
usethis::use_data(tbl5,overwrite=TRUE)
