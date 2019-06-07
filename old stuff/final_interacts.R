cces16 <- cces16 %>% 
  mutate(age  = 2016 - birthyr) %>% 
  mutate(age2 = car::recode(age, "18:24 = '18-24'; 25:29 = '25-29'; 30:34 = '30-34'; 35:39 = '35-39'; 40:44 = '40-44'; 45:49 = '45-49'; 50:54 = '50-54'; 55:59 = '55-59'; 60:64 = '60-64'; 65:69 = '65-69'; 70:74 = '70-74'; 75:100 = '75+'")) %>% 
  mutate(male = car::recode(gender, "1=1; 2=0")) %>% 
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
  mutate(att = car::recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else = NA"))

gg <- glm(ab1 ~ age*pid*male + white + educ + kids + att, data = cces16, family = "binomial")

gg2 <- interact_plot(gg, pred= age, modx = pid, mod2 = male, int.width = .76, interval = TRUE, modx.labels = c("Democrat", "Republican", "Independent"), mod2.labels = c("Women", "Men")) 

gg2 +
  labs(x = "Age", y = "<-- Less In Favor : More In Favor -->", title = "Interaction of Party Identification and Gender on Abortion Opinion", caption = "Data: CCES 2016", subtitle = "Abortion as a Matter of Choice") +
  theme_gg("Slabo 27px") +
  scale_y_continuous(labels = percent) +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  theme(legend.position = c(.15,.15)) +
  theme(plot.title = element_text(size = 18)) +
  ggsave("D://abortion40/interact_matterofchoice.png", type = "cairo-png", width = 10)

gg <- glm(ab5 ~ age*pid*male + white + educ + kids + att, data = cces16, family = "binomial")

gg2 <- interact_plot(gg, pred= age, modx = pid, mod2 = male, int.width = .76, interval = TRUE, modx.labels = c("Democrat", "Republican", "Independent"), mod2.labels = c("Women", "Men")) 

gg2 +
  labs(x = "Age", y = "<-- Less In Favor : More In Favor -->", title = "Interaction of Party Identification and Gender on Abortion Opinion", caption = "Data: CCES 2016", subtitle = "Make Completely Illegal") +
  theme_gg("Slabo 27px") +
  scale_y_continuous(labels = percent) +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  theme(legend.position = c(.15,.15)) +
  theme(plot.title = element_text(size = 18)) +
  ggsave("D://abortion40/interact_illegal.png", type = "cairo-png", width = 10)