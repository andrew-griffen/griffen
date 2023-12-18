library(pacman)
p_load(tidyverse,dplyr,griffen,zoo,fExtremes,modelr,tidyverse,magrittr,sf)

#save datasets into data folder
save_datasets = function(...){
  datasets_list <- lapply(eval(substitute(alist(...))),deparse)
  files = lapply(datasets_list,function(x) paste0("../data/",x,".rda"))
  mapply(save, list = datasets_list, file = files)
  invisible(datasets_list)
}

filter <- dq8plyr::filter

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


state_population <- read_csv("state_population.csv")
state_population <- state_population %>% pivot_longer(-c("state","region"),names_to="year",values_to="population")
region <- state_population %>% select(state,region) %>% distinct()
state_population <- state_population %>% select(-region)
state_population <- state_population %>% mutate(year = as.integer(year))
state_population <- state_population %>% select(state) %>% distinct() %>% crossing(tibble(year=1970:2014)) %>% full_join(state_population) %>% arrange(state,year)
state_population <- state_population %>% dplyr::filter(year<=2010) %>% group_by(state) %>% mutate(population = na.approx(population,year))
state_population <- state_population %>% left_join(region)

n <- 3
form_df <- tibble(y = round(10*runif(3)), x1 = round(10*runif(3)), x2 = round(10*runif(3)), D = c("treated","control","treated") )
form_df <- form_df %>% mutate(D = factor(D))
form_df <- form_df %>% mutate(D = fct_relevel(D,"treated"))

# form_df <- tibble(y = c(1,2,3), x1 = c(1,6,1), x2 = c(6,1,4), D = c("treated","control","treated"))
# form_df <- form_df %>% mutate(D = fct_relevel(D,"treatment"))

#number of individuals
n <- 4000
draw_bart_data <- function(n){

  #draw discrete choice data
  price_car <- rlnorm(n,meanlog=1,sdlog=1)
  price_metro <- rlnorm(n,meanlog=0,sdlog=1)
  price_bus <- rlnorm(n,meanlog=-1,sdlog=1)

  time_car <- rlnorm(n,meanlog=-1,sdlog=1)
  time_metro <- rlnorm(n,meanlog=1,sdlog=1)
  time_bus <- rlnorm(n,meanlog=0,sdlog=1)
  wage <- rlnorm(n,meanlog=5,sdlog=1)

  pre_bart <- tibble(id = 1:n, price_car, price_metro, price_bus,time_car,time_bus,time_metro,wage)

  price <- pre_bart %>% select(id,wage,starts_with("price")) %>% pivot_longer(starts_with("price"),names_to="mode",values_to="price")
  price <- price %>% mutate(mode = str_replace(mode,"price_",""))

  time <- pre_bart %>% select(id,wage,starts_with("time")) %>% pivot_longer(starts_with("time"),names_to="mode",values_to="time")
  time <- time %>% mutate(mode = str_replace(mode,"time_",""))

  bart <- inner_join(price,time) %>% select(id,mode,price,time,wage)

  return(bart)
}

#pre-bart data
formula_bart <- ~ mode + price + time + mode:wage

pre_bart <- draw_bart_data(n)
pre_bart_bus <- pre_bart %>% filter(mode=="bus")
pre_bart <- pre_bart %>% filter(mode!="bus")

set.seed(1028131)
formula_bart <- ~ mode + price + time + mode:wage
b0 <- c(-.1,-.05,-.05,-.002)
design_matrix <- model_matrix( pre_bart , formula_bart ) %>% as.matrix()
design_matrix <- design_matrix[,c(2,3,4,6)]

pre_bart <- pre_bart %>% mutate(e = rgev(n*2,xi=0,mu=0,beta=1)) %>% left()
pre_bart <- pre_bart %>% mutate(v = as.vector(design_matrix %*% b0)) %>% left()
pre_bart <- pre_bart %>% mutate(u = v + e) %>% select(u,v,e,everything())
pre_bart <- pre_bart %>% group_by(id) %>% mutate(max_u = max(u))
pre_bart <- pre_bart %>% group_by(id) %>% mutate(d = ifelse(max_u==u,1L,0L))
pre_bart <- pre_bart %>% select(-u,-v,-e,-max_u) %>% ungroup
pre_bart <- bind_rows(pre_bart,pre_bart_bus) %>% arrange(id)

#post-bart data
post_bart <- draw_bart_data(n)
post_bart <- post_bart %>% mutate(mode = fct_relevel(mode,"car"))
design_matrix <- model_matrix( post_bart , formula_bart ) %>% as.matrix()
design_matrix <- design_matrix[,c(2,3,4,5,7,8)]

b0_post <- c(b0[1],b0,last(b0))
post_bart <- post_bart %>% mutate(e = rgev(n*3,xi=0,mu=0,beta=1)) %>% left()
post_bart <- post_bart %>% mutate(v = as.vector(design_matrix %*% b0_post)) %>% left()
post_bart <- post_bart %>% mutate(u = v + e) %>% select(u,v,e,everything())
post_bart <- post_bart %>% group_by(id) %>% mutate(max_u = max(u))
post_bart <- post_bart %>% group_by(id) %>% mutate(d = ifelse(max_u==u,1L,0L))
post_bart <- post_bart %>% select(-u,-v,-e,-max_u) %>% ungroup

# pre_bart %>% group_by(mode) %>% summarise(mean(d))
# post_bart %>% group_by(mode) %>% summarise(mean(d))

#factors
pre_bart <- pre_bart %>% mutate(mode = factor(mode,levels=c("car","metro","bus")))
post_bart <- post_bart %>% mutate(mode = factor(mode,levels=c("car","metro","bus")))

#cps
cps <- read_csv("cps.csv")
cps <- cps[sample(1:nrow(cps),.2*nrow(cps)),]
cps %<>% mutate(year = as.integer(year))
save_datasets(cps)

#credit scores
credit <- read_csv("credit.csv")
credit <- credit %>% mutate(default = factor(default))

#oj
oj <- read_csv("oj.csv")

#boston
boston <- read_csv("boston.csv")

#heights
heights <- read_csv("heights.csv")

x <- tibble(key = c(1,2,3), val_x = c("x1","x2","x3"))
y <- tibble(key = c(1,2,4), val_y = c("y1","y2","y3"))



japan_shp <- st_read('../../../Japan_Shapefiles/JPN_adm1.shp', stringsAsFactors = FALSE)
japan_shp %<>% mutate(NAME_1=ifelse(NAME_1=="Naoasaki","Nagasaki",NAME_1))
japan_shp %<>% mutate(NAME_1=ifelse(NAME_1=="Hyōgo","Hyogo",NAME_1))
japan_shp %<>% mutate(prefecture = NAME_1)
japan_shp %<>% select(prefecture,geometry)


japan_travel <- read_csv("japan_travel.csv")
japan_travel <- japan_travel %>% mutate(visited = ifelse(is.na(visited),"no","yes"))
japan_travel$visited <- factor(japan_travel$visited,levels=c("yes","no"))


# japan_shp <- japan_shp %>% left_join(japan_travel)
# japan_shp$visited <- factor(japan_shp$visited,levels=c("yes","no"))


whales <- read_csv("whales.csv")
comment(whales) <- "Sperm whales Gulf of California 2007-2008"
# whale <- read_csv("Sperm whales Gulf of California 2007-2008 - Argos data.csv")
whales <- whales %>% mutate(date = as.Date(timestamp)) %>% left()
whales <- whales %>% arrange(timestamp) %>% distinct(date,.keep_all=TRUE)
whales %<>% rename(long = `location-long`, lat = `location-lat`, time = timestamp) %>% select(time,long,lat)

oecd <- read_csv("oecd.csv")

clark = read_csv("clark.csv")
coges = read_csv("coges.csv")


save_datasets(x,y)
save_datasets(boston,credit,form_df,heights,oj,post_bart,pre_bart,state_population,tbl1,tbl2,tbl3,tbl4,tbl5)
save_datasets(oecd)
save_datasets(whales)
save_datasets(clark,coges)
save_datasets(japan_travel,japan_shp)

