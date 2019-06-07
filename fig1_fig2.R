
library(fst)
library(socsci)
library(car)
source("D://theme.R")

cces16 <- read.fst("C://cces16.fst")


cces16 <- cces16 %>% 
  mutate(age  = 2016 - birthyr) %>% 
  mutate(age2 = car::recode(age, "18:24 = '18-24'; 25:29 = '25-29'; 30:34 = '30-34'; 35:39 = '35-39'; 40:44 = '40-44'; 45:49 = '45-49'; 50:54 = '50-54'; 55:59 = '55-59'; 60:64 = '60-64'; 65:69 = '65-69'; 70:74 = '70-74'; 75:100 = '75+'")) %>% 
  mutate(male = car::recode(gender, "1=1; 2=0")) %>% 
  mutate(ab_imp = car::recode(CC16_301b, "5=1; 2=4; 3=3; 4=2; 5=1; else = NA")) %>% 
  mutate(ab1 = recode(CC16_332a, "2=0; 1=1; else = NA")) %>%
  mutate(ab2 = recode(CC16_332c, "1=1; 2=0; else = NA")) %>%
  mutate(ab3 = recode(CC16_332d, "1=1; 2=0; else = NA")) %>%
  mutate(ab4 = recode(CC16_332e, "1=1; 2=0; else = NA")) %>%
  mutate(ab5 = recode(CC16_332f, "1=1; 2=0; else = NA"))


aaa1 <- cces16 %>% 
  filter(age <= 80) %>% 
  filter(age >= 20) %>%
  group_by(gender, pid3, age) %>% 
  filter(pid3 <= 3) %>% 
  mean_ci(ab1, wt = commonweight_vv) %>% 
  ungroup(pid3, gender) %>% 
  mutate(pid3 = as.numeric(pid3)) %>% 
  mutate(pid3 = car::recode(pid3, "1 = 'Democrat'; 2 = 'Republican'; 3 = 'Independent'; else = NA")) %>% 
  mutate(gender = as.numeric(gender)) %>% 
  mutate(gender = car::recode(gender, "1= 'Men'; 2 = 'Women'")) %>% 
  mutate(issue = "Matter of Choice")

aaa2 <- cces16 %>% 
  filter(age <= 80) %>% 
  filter(age >= 20) %>%
  group_by(gender, pid3, age) %>% 
  filter(pid3 <= 3) %>% 
  mean_ci(ab2, wt = commonweight_vv) %>% 
  ungroup(pid3, gender) %>% 
  mutate(pid3 = as.numeric(pid3)) %>% 
  mutate(pid3 = car::recode(pid3, "1 = 'Democrat'; 2 = 'Republican'; 3 = 'Independent'; else = NA")) %>% 
  mutate(gender = as.numeric(gender)) %>% 
  mutate(gender = car::recode(gender, "1= 'Men'; 2 = 'Women'")) %>% 
  mutate(issue = "Prohibit After\n20 Weeks")

aaa3 <- cces16 %>% 
  filter(age <= 80) %>% 
  filter(age >= 20) %>%
  group_by(gender, pid3, age) %>% 
  filter(pid3 <= 3) %>% 
  mean_ci(ab3, wt = commonweight_vv) %>% 
  ungroup(pid3, gender) %>% 
  mutate(pid3 = as.numeric(pid3)) %>% 
  mutate(pid3 = car::recode(pid3, "1 = 'Democrat'; 2 = 'Republican'; 3 = 'Independent'; else = NA")) %>% 
  mutate(gender = as.numeric(gender)) %>% 
  mutate(gender = car::recode(gender, "1= 'Men'; 2 = 'Women'")) %>% 
  mutate(issue = "Employers Deny\nAbortion Ins. Coverage")

aaa4 <- cces16 %>% 
  filter(age <= 80) %>% 
  filter(age >= 20) %>%
  group_by(gender, pid3, age) %>% 
  filter(pid3 <= 3) %>% 
  mean_ci(ab4, wt = commonweight_vv) %>% 
  ungroup(pid3, gender) %>% 
  mutate(pid3 = as.numeric(pid3)) %>% 
  mutate(pid3 = car::recode(pid3, "1 = 'Democrat'; 2 = 'Republican'; 3 = 'Independent'; else = NA")) %>% 
  mutate(gender = as.numeric(gender)) %>% 
  mutate(gender = car::recode(gender, "1= 'Men'; 2 = 'Women'")) %>% 
  mutate(issue = "Prohibit Federal\nFunds")

aaa5 <- cces16 %>% 
  filter(age <= 80) %>% 
  filter(age >= 20) %>%
  group_by(gender, pid3, age) %>% 
  filter(pid3 <= 3) %>% 
  mean_ci(ab5, wt = commonweight_vv) %>% 
  ungroup(pid3, gender) %>% 
  mutate(pid3 = as.numeric(pid3)) %>% 
  mutate(pid3 = car::recode(pid3, "1 = 'Democrat'; 2 = 'Republican'; 3 = 'Independent'; else = NA")) %>% 
  mutate(gender = as.numeric(gender)) %>% 
  mutate(gender = car::recode(gender, "1= 'Men'; 2 = 'Women'")) %>% 
  mutate(issue = "Make Completely\nIllegal")

aaa6 <- cces16 %>% 
  mutate(ab6 = car::recode(CC16_332b, "2=1; 1=0; else = NA")) %>% 
  filter(age <= 80) %>% 
  filter(age >= 20) %>%
  group_by(gender, pid3, age) %>% 
  filter(pid3 <= 3) %>% 
  mean_ci(ab6, wt = commonweight_vv) %>% 
  ungroup(pid3, gender) %>% 
  mutate(pid3 = as.numeric(pid3)) %>% 
  mutate(pid3 = car::recode(pid3, "1 = 'Democrat'; 2 = 'Republican'; 3 = 'Independent'; else = NA")) %>% 
  mutate(gender = as.numeric(gender)) %>% 
  mutate(gender = car::recode(gender, "1= 'Men'; 2 = 'Women'")) %>% 
  mutate(issue = "Only for Rape,\nIncest, Life of Mother")

graph <- bind_df("aaa")


graph %>%
  filter(gender == "Men") %>% 
  ggplot(., aes(y=mean, x= age, color = pid3)) +
  geom_point() +
  geom_smooth(show.legend = FALSE) +
  theme_gg("Slabo 27px") +
  facet_wrap(~ issue, ncol =3) +
  labs(title = "Support for Abortion Policies", subtitle = "Among Men", x = "", y = "", caption = "Data: CCES 2016") +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = c("dodgerblue3", "azure4", "firebrick3")) +
  scale_fill_manual(values = c("dodgerblue3", "azure4", "firebrick3")) +
  theme(legend.position = "bottom") +  
  theme(legend.title=element_blank()) +
  ggsave("D://abortion40/final_images/democrat_5questions_males_ppt.png", type = "cairo-png", width = 10)


graph %>%
  filter(gender == "Women") %>% 
  ggplot(., aes(y=mean, x= age, color = pid3)) +
  geom_point() +
  geom_smooth(show.legend = FALSE) +
  theme_gg("Slabo 27px") +
  facet_wrap(~ issue, ncol =3) +
  labs(title = "Support for Abortion Policies", subtitle = "Among Women", x = "", y = "", caption = "Data: CCES 2016") +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = c("dodgerblue3", "azure4", "firebrick3")) +
  scale_fill_manual(values = c("dodgerblue3", "azure4", "firebrick3")) +
  theme(legend.position = "bottom") +  
  theme(legend.title=element_blank()) +
  ggsave("D://abortion40/final_images/democrat_5questions_females_ppt.png", type = "cairo-png", width = 10)