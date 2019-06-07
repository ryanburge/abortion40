cces16 <- cces16 %>% 
  mutate(age  = 2016 - birthyr) %>% 
  mutate(age2 = car::recode(age, "18:24 = '18-24'; 25:29 = '25-29'; 30:34 = '30-34'; 35:39 = '35-39'; 40:44 = '40-44'; 45:49 = '45-49'; 50:54 = '50-54'; 55:59 = '55-59'; 60:64 = '60-64'; 65:69 = '65-69'; 70:74 = '70-74'; 75:100 = '75+'")) %>% 
  mutate(male = car::recode(gender, "1=1; 2=0")) %>% 
  mutate(male = as.factor(male)) %>% 
  mutate(ab_imp = car::recode(CC16_301b, "5=1; 2=4; 3=3; 4=2; 5=1; else = NA")) %>% 
  mutate(ab1 = recode(CC16_332a, "2=0; 1=1; else = NA")) %>%
  mutate(ab2 = recode(CC16_332c, "1=1; 2=0; else = NA")) %>%
  mutate(ab3 = recode(CC16_332d, "1=1; 2=0; else = NA")) %>%
  mutate(ab4 = recode(CC16_332e, "1=1; 2=0; else = NA")) %>%
  mutate(ab5 = recode(CC16_332f, "1=1; 2=0; else = NA")) %>% 
  mutate(abort = ab1 + ab2 + ab3 + ab4 + ab5) %>% 
  mutate(white = car::recode(race, "1=1; else =0")) %>% 
  mutate(white = as.factor(white)) %>% 
  mutate(pid = car::recode(pid3, "4:9= NA")) %>% 
  mutate(pid = as.factor(pid)) %>% 
  mutate(kids = car::recode(child18, "1=1; else =0")) %>% 
  mutate(att = car::recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else = NA")) %>% 
  mutate(pid7_new = car::recode(pid7, "8:99 = NA"))

cces16 <- cces16 %>% 
  mutate(age2 = car::recode(birthyr, "1930:1934 = '1930-1934';
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
  mutate(age2 = as.factor(age2))


reg1 <- glm(ab5 ~ pid7_new*male*age2 + educ + kids + att + white, data = cces16, family = "binomial")

gg2 <- interact_plot(reg1, pred= pid7_new, modx = male, mod2 = age2, int.width = .76, interval = TRUE , modx.labels = c("Women", "Men"), mod2.labels = c("1930-1934",
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
                                                                                                                                                         "1995-2000")) 

gg2 +
  labs(x = "Party ID", y = "<-- Less In Favor : More In Favor -->", title = "Make Completely Illegal - By Birth Cohorts", caption = "Data: CCES 2016") +
  theme_gg("Slabo 27px") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(limits = c(1,7), breaks = c(1,2,3,4,5,6, 7), labels = c("Strong\nDem", "", "Lean\nDem", "Ind.", "Lean\nRep", "", "Strong\nRep")) +
  scale_color_manual(values = c("goldenrod", "azure3")) +
  scale_fill_manual(values = c("goldenrod", "azure3")) +
  theme(legend.position = c(.75,.005)) +
  theme(legend.text = element_text(size = 28)) +
  theme(panel.spacing = unit(1, "lines")) +
  ggsave("D://abortion40/cohort_interact.png", width = 12, height = 10)


