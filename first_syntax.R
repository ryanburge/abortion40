library(fst)
library(socsci)
library(car)
source("D://theme.R")

cces16 <- read.fst("C://cces16.fst")

## Recoding Everything ####
cces16 <- cces16 %>% 
  mutate(age  = 2016 - birthyr) %>% 
  mutate(age2 = car::recode(age, "18:24 = '18-24'; 25:29 = '25-29'; 30:34 = '30-34'; 35:39 = '35-39'; 40:44 = '40-44'; 45:49 = '45-49'; 50:54 = '50-54'; 55:59 = '55-59'; 60:64 = '60-64'; 65:69 = '65-69'; 70:74 = '70-74'; 75:100 = '75+'")) %>% 
  mutate(male = car::recode(gender, "1=1; 2=0")) %>% 
  mutate(ab_imp = car::recode(CC16_301b, "5=1; 2=4; 3=3; 4=2; 5=1; else = NA")) %>% 
  mutate(ab1 = recode(CC16_332a, "2=1; 1=0; else = NA")) %>%
  mutate(ab2 = recode(CC16_332c, "1=1; 2=0; else = NA")) %>%
  mutate(ab3 = recode(CC16_332d, "1=1; 2=0; else = NA")) %>%
  mutate(ab4 = recode(CC16_332e, "1=1; 2=0; else = NA")) %>%
  mutate(ab5 = recode(CC16_332f, "1=1; 2=0; else = NA")) %>%
  mutate(abort = ab1 + ab2 + ab3 + ab4 + ab5) %>% 
  mutate(dem = car::recode(pid3, "1=1; else = 0")) %>% 
  mutate(rep = car::recode(pid3, "2=1; else = 0")) %>% 
  mutate(ind = car::recode(pid3, "3=1; else = 0")) %>% 
  mutate(new_pid = car::recode(pid3, "1 = 'Democrat'; 2 = 'Republican'; 3 = 'Independent'; else = NA"))

## Issue Importance by Age Group and Party ID ####

graph <- cces16 %>% 
  filter(male == 0) %>% 
  group_by(new_pid, age2) %>% 
  mean_ci(ab_imp)

graph %>% 
  filter(new_pid != "NA") %>% 
  ggplot(., aes(x = age2, y = mean, group = new_pid, color = new_pid)) +
  geom_line(size = 1) +
  theme_gg("Slabo 27px") +
  geom_ribbon(aes(ymin=lower, ymax=upper, color = new_pid, fill = new_pid), alpha = .4, show.legend = FALSE) +
  scale_color_manual(values=c("dodgerblue3", "azure4", "firebrick1")) +
  scale_fill_manual(values=c("dodgerblue3", "azure4",  "firebrick1")) +
  scale_y_continuous(limits=c(2,4), labels = c("No Importance", "Very Low", "Somewhat Low", "Somewhat High", "Very High")) +
  theme(legend.position = c(.75, .25)) +
  labs(x = "Age Groups", y = "Mean Level of Importance", title = "Importance of Abortion by Age and Party ID", subtitle = "Among Women", caption = "Data: CCES 2016") +
  ggsave("D://abortion40/imp_ribbons.png", width = 8)

## Support for Abortion by Age and Party ID ####

graph <- cces16 %>% 
  mutate(choice = car::recode(CC16_332a, "1=1; 2=0; else = NA")) %>% 
  group_by(pid3, gender, age) %>% 
  mean_ci(choice) %>% 
  ungroup(pid3, gender, age) %>% 
  mutate(pid3 = as.numeric(pid3)) %>% 
  mutate(pid3 = car::recode(pid3, "1 = 'Democrat'; 2 = 'Republican'; 3 = 'Independent'; else = NA")) %>% 
  mutate(gender = as.numeric(gender)) %>% 
  mutate(gender = car::recode(gender, "1= 'Male'; 2 = 'Female'"))


graph %>% 
  filter(pid3 != "NA") %>% 
  filter(age <= 80) %>% 
  filter(age >= 20) %>% 
  ggplot(., aes(x = age, y = mean, color = pid3, group = pid3)) +
  geom_point() +
  geom_smooth() +
  # geom_ribbon(aes(ymin=lower, ymax=upper, color = pid3, fill = pid3), alpha = .005, show.legend = FALSE) +
  scale_color_manual(values = c("dodgerblue3", "azure4", "firebrick3")) +
  scale_fill_manual(values = c("dodgerblue3", "azure4", "firebrick3")) +
  scale_y_continuous(labels = percent) +
  facet_grid(~ gender) +
  theme_gg("Slabo 27px") +
  labs(x = "Age", y = "Percent Who Are Pro-Choice", title = "Abortion Support by Age, Gender, and Party ID", caption = "Data: CCES 2016") +
  ggsave("D://abortion40/facet_gender_pid_prochoice.png", width = 8)



graph <- cces16 %>% 
  mutate(choice = car::recode(CC16_332a, "1=1; 2=0; else = NA")) %>% 
  filter(pid7 <= 7) %>% 
  group_by(pid7, gender, age) %>% 
  mean_ci(choice) %>% 
  ungroup(pid3, gender, age) %>% 
  mutate(gender = as.numeric(gender)) %>% 
  mutate(gender = car::recode(gender, "1= 'Male'; 2 = 'Female'")) %>%  
  filter(pid7 >= 5) %>% 
  mutate(pid7 = car::recode(pid7, "5 = 'Lean Republican'; 6 = 'Not Strong Republican'; 7 = 'Strong Republican'"))


graph %>% 
  filter(age <= 80) %>% 
  filter(age >= 20) %>% 
  ggplot(., aes(x = age, y = mean, color = pid7, group = pid7)) +
  geom_point() +
  geom_smooth(aes(fill = pid7), alpha = .8) +
  scale_color_manual(values = c("#ffb2b2", "#ff4c4c", "#ff0000")) +
  scale_fill_manual(values = c("#ffb2b2", "#ff4c4c", "#ff0000")) +
  scale_y_continuous(labels = percent) +
  facet_grid(~ gender) +
  theme_gg("Slabo 27px") +
  theme(legend.position = c(.25, .15)) +
  labs(x = "Age", y = "Percent Who Are Pro-Choice", title = "Abortion Support by Age, Gender, and Party ID", caption = "Data: CCES 2016") +
  ggsave("D://abortion40/facet_gender_all_rep.png", width = 8)



### White Women vs Non-White ####

graph <- cces16 %>% 
  mutate(white = car::recode(race, "1='White'; else ='Non-White'")) %>% 
  filter(male != 0) %>% 
  filter(pid7 <= 7) %>% 
  group_by(pid7, white) %>% 
  mean_ci(abort) 
  
graph %>% 
  ggplot(., aes(x = pid7, y = mean, color = white)) +
  geom_line() + 
  geom_point() +
  geom_ribbon(aes(ymin=lower, ymax=upper, color = white, fill = white), alpha = .4, show.legend = FALSE) +
  scale_color_manual(values=c("#2a044a", "#fa8e8c")) +
  scale_fill_manual(values=c("#2a044a", "#fa8e8c")) +
  theme_gg("Slabo 27px") +
  theme(legend.position = c(.75, .25)) +
  labs(x = "", y = "<-- More Pro-Choice : More Pro-Life -->", title = "Support for Abortion by Race and Party ID", subtitle = "Among Women", caption = "Data: CCES 2016") +
  scale_x_continuous(limits = c(1,7.1), breaks = c(1,2,3,4,5,6,7), labels = c("Strong\nDemocrat", "Not Strong\nDemocrat", "Lean\nDemocrat", "Independent", "Lean\nRepublican", "Not Strong\nRepublican", "Strong\nRepublican")) +
  ggsave("D://abortion40/white_nonwhite_pid7.png")


### Women With Children ####

graph <- cces16 %>% 
  mutate(kids = car::recode(child18, "1=1; else =0")) %>% 
  mutate(gender = car::recode(gender, "1= 'Male'; 2 = 'Female'")) %>% 
  group_by(gender, age) %>%  
  mean_ci(kids) %>% 
  arrange(-mean)

graph %>% 
  filter(age <= 80) %>% 
  ggplot(., aes(x = age, y = mean, color = gender)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = percent) +
  theme_gg("Roboto Slab") +
  theme(legend.position = c(.75, .75)) +
  scale_color_manual(values=c("#2a044a", "#fa8e8c")) +
  labs(x = "Age", y = "Percent with Children", title = "Are You A Parent/Guardian of Any Children Under 18?", caption = "Data: CCES 2016") + 
  ggsave("D://abortion40/kids_by_gender.png", width = 8)

### Interaction Menopause ###

cces16 <- cces16 %>% 
  mutate(fert = car::recode(age, "18:39 = 2; 40:49 =1; 50:60 = 0")) %>% 
  mutate(pid = car::recode(pid7, "8:99 = NA")) %>% 
  mutate(choice = car::recode(CC16_332a, "1=1; 2=0; else = NA")) %>% 
  mutate(kids = car::recode(child18, "1=1; else =0")) %>% 
  mutate(att = car::recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else = NA"))


reg <- cces16 %>% 
  # filter(catholic ==1) %>% 
  na.omit()

gg <- glm(choice ~ pid*fert*gender + educ + kids, family = "binomial", data = reg)

gg2 <- interact_plot(gg, pred= pid, modx = fert, mod2 = gender, int.width = .76, interval = TRUE, mod2.values = c(1,2),  modx.labels = c("Peak Fertility", "Middle Age", "Menopause"), mod2.labels = c("Male", "Female")) 

gg2 +
  labs(x = "Party ID", y = "<-- More Pro-Life : More Pro-Choice -->", title = "Interaction of Fertility and Party ID on Abortion Support") +
  theme_gg("Slabo 27px") +
  scale_x_continuous(limits = c(1,7.1), breaks = c(1,2,3,4,5,6,7), labels = c("Strong\nDemocrat", "Not Strong\nDemocrat", "Lean\nDemocrat", "Independent", "Lean\nRepublican", "Not Strong\nRepublican", "Strong\nRepublican")) +
  scale_fill_npg() +
  scale_color_npg() +
  scale_y_continuous(labels = percent) +
  theme(legend.position = c(.85,.25)) +
  ggsave("D://abortion40/interact_menopause.png", width = 12)

### Interact Attendance ####

gg <- glm(choice ~ att*fert*gender + educ + kids, family = "binomial", data = reg)

gg2 <- interact_plot(gg, pred= att, modx = fert, mod2 = gender, int.width = .76, interval = TRUE, mod2.values = c(1,2),  modx.labels = c("Peak Fertility", "Middle Age", "Menopause"), mod2.labels = c("Male", "Female")) 

gg2 +
  labs(x = "Attendance", y = "<-- More Pro-Life : More Pro-Choice -->", title = "Interaction of Fertility and Church Attendance on Abortion Support") +
  theme_gg("Slabo 27px") +
  scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_fill_npg() +
  scale_color_npg() +
  theme(legend.position = c(.85,.25)) +
  ggsave("D://abortion40/interact_attendance.png", width = 12)

reg <- cces16 %>% 
  filter(catholic ==1) %>% 
  na.omit()


gg <- glm(choice ~ att*fert*gender + educ + kids, family = "binomial", data = reg)

gg2 <- interact_plot(gg, pred= att, modx = fert, mod2 = gender, int.width = .76, interval = TRUE, mod2.values = c(1,2),  modx.labels = c("Peak Fertility", "Middle Age", "Menopause"), mod2.labels = c("Male", "Female")) 

gg2 +
  labs(x = "Attendance", y = "<-- More Pro-Life : More Pro-Choice -->", title = "Interaction of Fertility and Church Attendance on Abortion Support", subtitle = "Among Catholics") +
  theme_gg("Slabo 27px") +
  scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_fill_npg() +
  scale_color_npg() +
  scale_y_continuous(labels = percent) +
  theme(legend.position = c(.65,.25)) +
  ggsave("D://abortion40/interact_attendance_cath.png", width = 12)


reg <- cces16 %>% 
  filter(evangelical ==1) %>% 
  na.omit()


gg <- glm(choice ~ att*fert*gender + educ + kids, family = "binomial", data = reg)

gg2 <- interact_plot(gg, pred= att, modx = fert, mod2 = gender, int.width = .76, interval = TRUE, mod2.values = c(1,2),  modx.labels = c("Peak Fertility", "Middle Age", "Menopause"), mod2.labels = c("Male", "Female")) 

gg2 +
  labs(x = "Attendance", y = "<-- More Pro-Life : More Pro-Choice -->", title = "Interaction of Fertility and Church Attendance on Abortion Support", subtitle = "Among Evangelicals") +
  theme_gg("Slabo 27px") +
  scale_x_continuous(limits = c(1,6.1), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  scale_fill_npg() +
  scale_color_npg() +
  scale_y_continuous(labels = percent) +
  theme(legend.position = c(.65,.25)) +
  ggsave("D://abortion40/interact_attendance_evan.png", width = 12)
