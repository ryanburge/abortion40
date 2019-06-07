cces18 <- cces18 %>% 
  mutate(age  = 2018 - birthyr) %>% 
  mutate(age2 = car::recode(age, "18:24 = '18-24'; 25:29 = '25-29'; 30:34 = '30-34'; 35:39 = '35-39'; 40:44 = '40-44'; 45:49 = '45-49'; 50:54 = '50-54'; 55:59 = '55-59'; 60:64 = '60-64'; 65:69 = '65-69'; 70:74 = '70-74'; 75:100 = '75+'")) %>% 
  mutate(male = car::recode(gender, "1=1; 2=0")) %>% 
  # mutate(ab_imp = car::recode(CC16_301b, "5=1; 2=4; 3=3; 4=2; 5=1; else = NA")) %>% 
  mutate(ab1 = recode(CC18_321a, "2=0; 1=1; else = NA")) %>%
  mutate(ab2 = recode(CC18_321c, "1=1; 2=0; else = NA")) %>%
  mutate(ab3 = recode(CC18_321d, "1=1; 2=0; else = NA")) %>%
  mutate(ab4 = recode(CC18_321e, "1=1; 2=0; else = NA")) %>%
  mutate(ab5 = recode(CC18_321f, "1=1; 2=0; else = NA")) %>% 
  mutate(abort = ab1 + ab2 + ab3 + ab4 + ab5) %>% 
  mutate(white = car::recode(race, "1=1; else =0")) %>% 
  mutate(white = as.factor(white)) %>% 
  mutate(pid = car::recode(pid3, "4:9= NA")) %>% 
  mutate(pid = as.factor(pid)) %>% 
  mutate(kids = car::recode(child18, "1=1; else =0")) %>% 
  mutate(att = car::recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else = NA"))




male <- cces18 %>% 
  filter(gender ==1)

gg <- glm(ab5 ~ age*pid*white + educ + kids + att, data = male, family = "binomial")

gg2 <- interact_plot(gg, pred= age, modx = pid, mod2 = white, int.width = .76, interval = TRUE, modx.labels = c("Democrat", "Republican", "Independent"), mod2.labels = c("Non-White", "White")) 

ab5m <- gg2 +
  labs(x = "Age", y = "<-- Less In Favor : More In Favor -->", title = "Make Completely Illegal", caption = "Data: CCES 2018", subtitle = "Among Men") +
  theme_gg("Slabo 27px") +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  theme(legend.position = c(.25,.75)) +
  ggsave("D://abortion40/2018/interact_age_pid_white_male_ab5.png", width = 12)

female <- cces18 %>% 
  filter(gender ==2)

gg <- glm(ab5 ~ age*pid*white + educ + kids + att, data = female, family = "binomial")

gg2 <- interact_plot(gg, pred= age, modx = pid, mod2 = white, int.width = .76, interval = TRUE, modx.labels = c("Democrat", "Republican", "Independent"), mod2.labels = c("Non-White", "White")) 

ab5f <- gg2 +
  labs(x = "Age", y = "<-- Less In Favor : More In Favor -->", title = "Make Completely Illegal", caption = "Data: CCES 2018", subtitle = "Among Women") +
  theme_gg("Slabo 27px") +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  theme(legend.position = c(.15,.85)) +
  ggsave("D://abortion40/2018/interact_age_pid_white_female_ab5.png", width = 12)

library(patchwork)
ab5b <- ab5m + ab5f +plot_layout(ncol = 1)
ggsave("D://abortion40/2018/ab5_both.png", ab5b, width = 12, height = 8, type = "cairo-png")