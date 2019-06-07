library(fst)
library(socsci)
library(car)
source("D://theme.R")

gss <- read.fst("C://gss18b.fst")


gss <- gss %>% 
  mutate(cohorts = car::recode(birthyr, "1883:1899 = '1883-1899';
                                      1900:1904 = '1900-1904';
                                      1905:1909 = '1905-1909';
                                      1910:1914 = '1910-1914';
                                      1915:1919 = '1915-1919';
                                      1920:1924 = '1920-1924';
                                      1925:1929 = '1925-1929';
                                      1930:1934 = '1930-1934';
                                      1935:1939 = '1935-1939';
                                      1940:1944 = '1940-1944';
                                      1945:1949 = '1945-1949';
                                      1950:1954 = '1950-1954';
                                      1955:1959 = '1955-1959';
                                      1960:1964 = '1960-1964';
                                      1965:1969 = '1965-1969';
                                      1970:1974 = '1970-1974';
                                      1975:1979 = '1975-1979';
                                      1980:1984 = '1980-1984';
                                      1985:1989 = '1985-1989';
                                      1990:1994 = '1990-1994';
                                      1995:2000 = '1995-2000'; else = NA")) %>% 
  mutate(cohorts = as.factor(cohorts))

regg <- gss %>% 
  mutate(pid7 = car::recode(partyid, "7:99 =NA")) %>% 
  mutate(pid3 = car::recode(pid7, "0:2 = 'Democrat'; 4:6 = 'Republican'; else = NA")) %>% 
  mutate(pid3 = as.factor(pid3)) %>% 
  mutate(male = car::recode(sex, "1='Men'; 2='Women'; else = NA")) %>% 
  mutate(male = as.factor(male)) %>% 
  mutate(years = as.factor(year)) %>% 
  mutate(white = car::recode(race, "1=1; else =0")) %>% 
  mutate(white = as.factor(white)) %>% 
  mutate(ab1 = car::recode(abdefect, "1=0; 2=1; else = NA")) %>% 
  mutate(ab2 = car::recode(abnomore, "1=0; 2=1; else = NA")) %>% 
  mutate(ab3 = car::recode(abhlth, "1=0; 2=1; else = NA")) %>% 
  mutate(ab4 = car::recode(abpoor, "1=0; 2=1; else = NA")) %>% 
  mutate(ab5 = car::recode(abrape, "1=0; 2=1; else = NA")) %>% 
  mutate(ab6 = car::recode(absingle, "1=0; 2=1; else = NA")) %>% 
  mutate(ab_scale = ab1 + ab2 + ab3 + ab4 + ab5 + ab6)

regg2 <- regg %>% 
  filter(cohorts != "1995-2000")

reg1 <- lm(ab_scale ~ year*male*cohorts + educ + partyid + white + attend, data = regg2)

gg2 <- interact_plot(reg1, pred = year, modx = male, mod2 = cohorts, int.width = .76, interval = TRUE, mod2.labels = c("1900-1904",
"1905-1909",
"1910-1914",
"1915-1919",
"1920-1924",
"1925-1929",
"1930-1934",
"1935-1939",
"1940-1944",
"1945-1949",
"1950-1954",
"1955-1959",
"1960-1964",
"1965-1969",
"1970-1974",
"1975-1979",
"1980-1984",
"1985-1989",
"1990-1994",
"1995-2000"
))

gg2 +
  theme_gg("Abel") +
  theme(legend.position = "bottom") +
  theme(panel.spacing = unit(1, "lines")) +
  scale_x_continuous(breaks =c(1970, 1990, 2010)) +
  scale_fill_simpsons() +
  scale_color_simpsons() + 
  labs(y = "<-- More Pro-Choice : More Pro-Life -->", x = "Survey Year", title = "Cohort Analysis of Gender on Abortion Opinion", subtitle = "Includes controls for education, race, party identification, and church attendance", caption = "Data: GSS 1972-2018") +
  ggsave("D://abortion40/full_apc_gss.png", type = "cairo-png", width = 10, height = 10)

dems <- regg2 %>% 
  filter(cohorts != "1995-2000") %>% 
  filter(pid3 == "Democrat")

reg1 <- lm(ab_scale ~ year*male*cohorts + educ + white + attend, data = dems)

gg2 <- interact_plot(reg1, pred = year, modx = male, mod2 = cohorts, int.width = .76, interval = TRUE, mod2.labels = c("1900-1904",
                                                                                                                       "1905-1909",
                                                                                                                       "1910-1914",
                                                                                                                       "1915-1919",
                                                                                                                       "1920-1924",
                                                                                                                       "1925-1929",
                                                                                                                       "1930-1934",
                                                                                                                       "1935-1939",
                                                                                                                       "1940-1944",
                                                                                                                       "1945-1949",
                                                                                                                       "1950-1954",
                                                                                                                       "1955-1959",
                                                                                                                       "1960-1964",
                                                                                                                       "1965-1969",
                                                                                                                       "1970-1974",
                                                                                                                       "1975-1979",
                                                                                                                       "1980-1984",
                                                                                                                       "1985-1989",
                                                                                                                       "1990-1994",
                                                                                                                       "1995-2000"
))

gg2 +
  theme_gg("Abel") +
  theme(legend.position = "bottom") +
  theme(panel.spacing = unit(1, "lines")) +
  scale_x_continuous(breaks =c(1970, 1990, 2010)) +
  scale_fill_simpsons() +
  scale_color_simpsons() + 
  labs(y = "<-- More Pro-Choice : More Pro-Life -->", x = "Survey Year", title = "Cohort Analysis of Gender on Abortion Opinion - Democrats", subtitle = "Includes controls for education, race, party identification, and church attendance", caption = "Data: GSS 1972-2018") +
  ggsave("D://abortion40/full_apc_gss_dem.png", type = "cairo-png", width = 10, height = 10)



rep <- regg2 %>% 
  filter(cohorts != "1995-2000") %>% 
  filter(pid3 == "Republican")

reg1 <- lm(ab_scale ~ year*male*cohorts + educ + white + attend, data = rep)

gg2 <- interact_plot(reg1, pred = year, modx = male, mod2 = cohorts, int.width = .76, interval = TRUE, mod2.labels = c("1900-1904",
                                                                                                                       "1905-1909",
                                                                                                                       "1910-1914",
                                                                                                                       "1915-1919",
                                                                                                                       "1920-1924",
                                                                                                                       "1925-1929",
                                                                                                                       "1930-1934",
                                                                                                                       "1935-1939",
                                                                                                                       "1940-1944",
                                                                                                                       "1945-1949",
                                                                                                                       "1950-1954",
                                                                                                                       "1955-1959",
                                                                                                                       "1960-1964",
                                                                                                                       "1965-1969",
                                                                                                                       "1970-1974",
                                                                                                                       "1975-1979",
                                                                                                                       "1980-1984",
                                                                                                                       "1985-1989",
                                                                                                                       "1990-1994",
                                                                                                                       "1995-2000"
))

gg2 +
  theme_gg("Abel") +
  theme(legend.position = "bottom") +
  theme(panel.spacing = unit(1, "lines")) +
  scale_x_continuous(breaks =c(1970, 1990, 2010)) +
  scale_fill_simpsons() +
  scale_color_simpsons() + 
  labs(y = "<-- More Pro-Choice : More Pro-Life -->", x = "Survey Year", title = "Cohort Analysis of Gender on Abortion Opinion - Republicans", subtitle = "Includes controls for education, race, party identification, and church attendance", caption = "Data: GSS 1972-2018") +
  ggsave("D://abortion40/full_apc_gss_rep.png", type = "cairo-png", width = 10, height = 10)

