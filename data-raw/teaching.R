library(tidyverse)

tbl1 <- tibble(id = c("1","2","3"), drew1 = c(1,2,3), drew2 = c(4,5,6), drew3 = c(7,8,9))
usethis::use_data(tbl1)

tbl2 <- tibble(id = c(1,2,3), a = c(1,2,3), b = c(4,5,6) , c = c(7,8,9))
usethis::use_data(tbl2)
