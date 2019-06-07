library(fst)
library(socsci)
library(car)


gss <- read.fst("C://gss18.fst")


birth_fun <- function(df, num){
  
  df %>% 
    filter(year == num) %>% 
    mutate(birthyr = num - age)
}


aaa1  <- gss %>% birth_fun(1972)
aaa2  <- gss %>% birth_fun(1973)
aaa3  <- gss %>% birth_fun(1974)
aaa4  <- gss %>% birth_fun(1975)
aaa5  <- gss %>% birth_fun(1976)
aaa6  <- gss %>% birth_fun(1977)
aaa7  <- gss %>% birth_fun(1978)
aaa8  <- gss %>% birth_fun(1980)
aaa9  <- gss %>% birth_fun(1982)
aaa10 <- gss %>% birth_fun(1983)
aaa11 <- gss %>% birth_fun(1984)
aaa12 <- gss %>% birth_fun(1985)
aaa13 <- gss %>% birth_fun(1986)
aaa14 <- gss %>% birth_fun(1987)
aaa15 <- gss %>% birth_fun(1988)
aaa16 <- gss %>% birth_fun(1989)
aaa17 <- gss %>% birth_fun(1990)
aaa18 <- gss %>% birth_fun(1991)
aaa19 <- gss %>% birth_fun(1993)
aaa20 <- gss %>% birth_fun(1994)
aaa21 <- gss %>% birth_fun(1996)
aaa22 <- gss %>% birth_fun(1998)
aaa23 <- gss %>% birth_fun(2000)
aaa24 <- gss %>% birth_fun(2002)
aaa25 <- gss %>% birth_fun(2004)
aaa26 <- gss %>% birth_fun(2006)
aaa27 <- gss %>% birth_fun(2008)
aaa28 <- gss %>% birth_fun(2010)
aaa29 <- gss %>% birth_fun(2012)
aaa30 <- gss %>% birth_fun(2014)
aaa31 <- gss %>% birth_fun(2016)
aaa32 <- gss %>% birth_fun(2018)

gss <- bind_df("aaa")

write.fst(gss, "D://gss18b.fst")