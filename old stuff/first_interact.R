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

## Using Abortion Scale ####  

male <- cces16 %>% 
  filter(gender ==1)

gg <- lm(abort ~ age*pid*white + educ + kids + att, data = male)

gg2 <- interact_plot(gg, pred= age, modx = pid, mod2 = white, int.width = .76, interval = TRUE, modx.labels = c("Democrat", "Republican", "Independent"), mod2.labels = c("Non-White", "White")) 

gg2 +
  labs(x = "Age", y = "<-- More Pro-Choice : More Pro-Life -->", title = "Interaction of Party ID and Race on Abortion Support", caption = "Data: CCES 2016", subtitle = "Among Men") +
  theme_gg("Slabo 27px") +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  theme(legend.position = c(.15,.75)) +
  ggsave("D://abortion40/interact_age_pid_white_male.png", width = 12)

female <- cces16 %>% 
  filter(gender ==2)

gg <- lm(abort ~ age*pid*white + educ + kids + att, data = female)

gg2 <- interact_plot(gg, pred= age, modx = pid, mod2 = white, int.width = .76, interval = TRUE, modx.labels = c("Democrat", "Republican", "Independent"), mod2.labels = c("Non-White", "White")) 

gg2 +
  labs(x = "Age", y = "<-- More Pro-Choice : More Pro-Life -->", title = "Interaction of Party ID and Race on Abortion Support", caption = "Data: CCES 2016", subtitle = "Among Women") +
  theme_gg("Slabo 27px") +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  theme(legend.position = c(.15,.75)) +
  ggsave("D://abortion40/interact_age_pid_white_female.png", width = 12)

### Matter of Choice ####

male <- cces16 %>% 
  filter(gender ==1)

gg <- glm(ab1 ~ age*pid*white + educ + kids + att, data = male, family = "binomial")

gg2 <- interact_plot(gg, pred= age, modx = pid, mod2 = white, int.width = .76, interval = TRUE, modx.labels = c("Democrat", "Republican", "Independent"), mod2.labels = c("Non-White", "White")) 

ab1m <- gg2 +
  labs(x = "Age", y = "<-- Less In Favor : More In Favor -->", title = "Matter of Choice", caption = "Data: CCES 2016", subtitle = "Among Men") +
  theme_gg("Slabo 27px") +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  theme(legend.position = c(.15,.25)) +
  ggsave("D://abortion40/interact_age_pid_white_male_ab1.png", type = "cairo-png", width = 12)

female <- cces16 %>% 
  filter(gender ==2)

gg <- glm(ab1 ~ age*pid*white + educ + kids + att, data = female, family = "binomial")

gg2 <- interact_plot(gg, pred= age, modx = pid, mod2 = white, int.width = .76, interval = TRUE, modx.labels = c("Democrat", "Republican", "Independent"), mod2.labels = c("Non-White", "White")) 

ab1f <- gg2 +
  labs(x = "Age", y = "<-- Less In Favor : More In Favor -->", title = "Matter of Choice", caption = "Data: CCES 2016", subtitle = "Among Women") +
  theme_gg("Slabo 27px") +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  theme(legend.position = c(.15,.15)) +
  ggsave("D://abortion40/interact_age_pid_white_female_ab1.png", width = 12)

## Rape, Incest, Life of Mother ####

cces16 <- cces16 %>% 
  mutate(ab6 = car::recode(CC16_332b, "1=1; else =0"))


male <- cces16 %>% 
  filter(gender ==1)

gg <- glm(ab6 ~ age*pid*white + educ + kids + att, data = male, family = "binomial")

gg2 <- interact_plot(gg, pred= age, modx = pid, mod2 = white, int.width = .76, interval = TRUE, modx.labels = c("Democrat", "Republican", "Independent"), mod2.labels = c("Non-White", "White")) 

ab6m <- gg2 +
  labs(x = "Age", y = "<-- Less In Favor : More In Favor -->", title = "Prohibit After 20 Weeks", caption = "Data: CCES 2016", subtitle = "Among Men") +
  theme_gg("Slabo 27px") +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  theme(legend.position = c(.15,.15)) +
  ggsave("D://abortion40/interact_age_pid_white_male_ab6.png", width = 12)

female <- cces16 %>% 
  filter(gender ==2)

gg <- glm(ab6 ~ age*pid*white + educ + kids + att, data = female, family = "binomial")

gg2 <- interact_plot(gg, pred= age, modx = pid, mod2 = white, int.width = .76, interval = TRUE, modx.labels = c("Democrat", "Republican", "Independent"), mod2.labels = c("Non-White", "White")) 

ab6f <- gg2 +
  labs(x = "Age", y = "<-- Less In Favor : More In Favor -->", title = "Prohibit After 20 Weeks", caption = "Data: CCES 2016", subtitle = "Among Women") +
  theme_gg("Slabo 27px") +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  theme(legend.position = c(.15,.25)) +
  ggsave("D://abortion40/interact_age_pid_white_female_ab6.png", width = 12)

## Late Term ####

male <- cces16 %>% 
  filter(gender ==1)

gg <- glm(ab2 ~ age*pid*white + educ + kids + att, data = male, family = "binomial")

gg2 <- interact_plot(gg, pred= age, modx = pid, mod2 = white, int.width = .76, interval = TRUE, modx.labels = c("Democrat", "Republican", "Independent"), mod2.labels = c("Non-White", "White")) 

ab2m <- gg2 +
  labs(x = "Age", y = "<-- Less In Favor : More In Favor -->", title = "Prohibit After 20 Weeks", caption = "Data: CCES 2016", subtitle = "Among Men") +
  theme_gg("Slabo 27px") +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  theme(legend.position = c(.15,.25)) +
  ggsave("D://abortion40/interact_age_pid_white_male_ab2.png", width = 12)

female <- cces16 %>% 
  filter(gender ==2)

gg <- glm(ab2 ~ age*pid*white + educ + kids + att, data = female, family = "binomial")

gg2 <- interact_plot(gg, pred= age, modx = pid, mod2 = white, int.width = .76, interval = TRUE, modx.labels = c("Democrat", "Republican", "Independent"), mod2.labels = c("Non-White", "White")) 

ab2f <- gg2 +
  labs(x = "Age", y = "<-- Less In Favor : More In Favor -->", title = "Prohibit After 20 Weeks", caption = "Data: CCES 2016", subtitle = "Among Women") +
  theme_gg("Slabo 27px") +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  theme(legend.position = c(.15,.85)) +
  ggsave("D://abortion40/interact_age_pid_white_female_ab2.png", width = 12)


### Decline Insurance Coverage ####

male <- cces16 %>% 
  filter(gender ==1)

gg <- glm(ab3 ~ age*pid*white + educ + kids + att, data = male, family = "binomial")

gg2 <- interact_plot(gg, pred= age, modx = pid, mod2 = white, int.width = .76, interval = TRUE, modx.labels = c("Democrat", "Republican", "Independent"), mod2.labels = c("Non-White", "White")) 

ab3m <- gg2 +
  labs(x = "Age", y = "<-- Less In Favor : More In Favor -->", title = "Employers Deny Abortion Ins. Coverage", caption = "Data: CCES 2016", subtitle = "Among Men") +
  theme_gg("Slabo 27px") +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  theme(legend.position = c(.15,.85)) +
  ggsave("D://abortion40/interact_age_pid_white_male_ab3.png", width = 12)

female <- cces16 %>% 
  filter(gender ==2)

gg <- glm(ab3 ~ age*pid*white + educ + kids + att, data = female, family = "binomial")

gg2 <- interact_plot(gg, pred= age, modx = pid, mod2 = white, int.width = .76, interval = TRUE, modx.labels = c("Democrat", "Republican", "Independent"), mod2.labels = c("Non-White", "White")) 

ab3f <- gg2 +
  labs(x = "Age", y = "<-- Less In Favor : More In Favor -->", title = "Employers Deny Abortion Ins. Coverage", caption = "Data: CCES 2016", subtitle = "Among Women") +
  theme_gg("Slabo 27px") +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  theme(legend.position = c(.15,.85)) +
  ggsave("D://abortion40/interact_age_pid_white_female_ab3.png", width = 12)



### Prohibit Federal Funds ####


male <- cces16 %>% 
  filter(gender ==1)

gg <- glm(ab4 ~ age*pid*white + educ + kids + att, data = male, family = "binomial")

gg2 <- interact_plot(gg, pred= age, modx = pid, mod2 = white, int.width = .76, interval = TRUE, modx.labels = c("Democrat", "Republican", "Independent"), mod2.labels = c("Non-White", "White")) 

ab4m <- gg2 +
  labs(x = "Age", y = "<-- Less In Favor : More In Favor -->", title = "Prohibit Federal Funds", caption = "Data: CCES 2016", subtitle = "Among Men") +
  theme_gg("Slabo 27px") +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  theme(legend.position = c(.15,.85)) +
  ggsave("D://abortion40/interact_age_pid_white_male_ab4.png", width = 12)

female <- cces16 %>% 
  filter(gender ==2)

gg <- glm(ab4 ~ age*pid*white + educ + kids + att, data = female, family = "binomial")

gg2 <- interact_plot(gg, pred= age, modx = pid, mod2 = white, int.width = .76, interval = TRUE, modx.labels = c("Democrat", "Republican", "Independent"), mod2.labels = c("Non-White", "White")) 

ab4f <- gg2 +
  labs(x = "Age", y = "<-- Less In Favor : More In Favor -->", title = "Prohibit Federal Funds", caption = "Data: CCES 2016", subtitle = "Among Women") +
  theme_gg("Slabo 27px") +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  theme(legend.position = c(.15,.85)) +
  ggsave("D://abortion40/interact_age_pid_white_female_ab4.png", width = 12)



## Make Abortion Illegal ####


male <- cces16 %>% 
  filter(gender ==1)

gg <- glm(ab5 ~ age*pid*white + educ + kids + att, data = male, family = "binomial")

gg2 <- interact_plot(gg, pred= age, modx = pid, mod2 = white, int.width = .76, interval = TRUE, modx.labels = c("Democrat", "Republican", "Independent"), mod2.labels = c("Non-White", "White")) 

ab5m <- gg2 +
  labs(x = "Age", y = "<-- Less In Favor : More In Favor -->", title = "Make Completely Illegal", caption = "Data: CCES 2016", subtitle = "Among Men") +
  theme_gg("Slabo 27px") +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  theme(legend.position = c(.25,.75)) +
  ggsave("D://abortion40/interact_age_pid_white_male_ab5.png", width = 12)

female <- cces16 %>% 
  filter(gender ==2)

gg <- glm(ab5 ~ age*pid*white + educ + kids + att, data = female, family = "binomial")

gg2 <- interact_plot(gg, pred= age, modx = pid, mod2 = white, int.width = .76, interval = TRUE, modx.labels = c("Democrat", "Republican", "Independent"), mod2.labels = c("Non-White", "White")) 

ab5f <- gg2 +
  labs(x = "Age", y = "<-- Less In Favor : More In Favor -->", title = "Make Completely Illegal", caption = "Data: CCES 2016", subtitle = "Among Women") +
  theme_gg("Slabo 27px") +
  # scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_color_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3", "azure3")) +
  theme(legend.position = c(.15,.85)) +
  ggsave("D://abortion40/interact_age_pid_white_female_ab5.png", width = 12)

## Patching ####

library(patchwork)
ab1b <- ab1m + ab1f +plot_layout(ncol = 1)
ggsave("D://abortion40/ab1_both.png", ab1b, width = 12, height = 8, type = "cairo-png")

ab2b <- ab2m + ab2f +plot_layout(ncol = 1)
ggsave("D://abortion40/ab2_both.png", ab2b, width = 12, height = 8, type = "cairo-png")

ab3b <- ab3m + ab3f +plot_layout(ncol = 1)
ggsave("D://abortion40/ab3_both.png", ab3b, width = 12, height = 8, type = "cairo-png")

ab4b <- ab4m + ab4f +plot_layout(ncol = 1)
ggsave("D://abortion40/ab4_both.png", ab4b, width = 12, height = 8, type = "cairo-png")

ab5b <- ab5m + ab5f +plot_layout(ncol = 1)
ggsave("D://abortion40/ab5_both.png", ab5b, width = 12, height = 8, type = "cairo-png")

ab6b <- ab6m + ab6f +plot_layout(ncol = 1)
ggsave("D://abortion40/ab6_both.png", ab5b, width = 12, height = 8, type = "cairo-png")

