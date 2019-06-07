library(psych)

#### Bar Graphs #####

cces16 <- cces16 %>% 
  mutate(ab1 = recode(CC16_332a, "2=1; 1=0; else = NA")) %>%
  mutate(ab2 = recode(CC16_332c, "1=1; 2=0; else = NA")) %>%
  mutate(ab3 = recode(CC16_332d, "1=1; 2=0; else = NA")) %>%
  mutate(ab4 = recode(CC16_332e, "1=1; 2=0; else = NA")) %>%
  mutate(ab5 = recode(CC16_332f, "1=1; 2=0; else = NA")) %>%
  mutate(abort = ab1 + ab2 + ab3 + ab4 + ab5)

alpha <- cces16 %>% 
  mutate(ab6 = car::recode(CC16_332b, "2=1; 1=0; else = NA")) %>% 
  select(ab1, ab2, ab3, ab4, ab5, ab6)

psych::alpha(alpha)$total$std.alpha

graph <- cces16 %>% 
  group_by(pid3, gender, age) %>% 
  mean_ci(abort, wt = commonweight) %>% 
  ungroup(pid3, gender) %>% 
  mutate(pid3 = as.numeric(pid3)) %>% 
  mutate(pid3 = car::recode(pid3, "1 = 'Democrat'; 2 = 'Republican'; 3 = 'Independent'; else = NA")) %>% 
  mutate(gender = as.numeric(gender)) %>% 
  mutate(gender = car::recode(gender, "1= 'Men'; 2 = 'Women'")) %>% 
  filter(pid3 != "NA") 

graph %>% 
  filter(age >= 20) %>% 
  filter(age <= 80) %>% 
  ggplot(., aes(y=mean, x= age, color = pid3)) +
  geom_point() +
  geom_smooth(show.legend = FALSE) +
  theme_gg("Slabo 27px") +
  facet_wrap(~ gender, ncol =2) +
  scale_color_manual(values = c("dodgerblue3", "azure4", "firebrick3")) +
  scale_fill_manual(values = c("dodgerblue3", "azure4", "firebrick3")) +
  theme(legend.position = "bottom") +  
  theme(legend.title=element_blank()) +
  labs(y= "<-- More Pro-Choice:More Pro-Life -->", x = "", title = "Average of Five Abortion Questions", caption = "Data: CCES 2016") +
  ggsave("D://abortion40/final_images/abort_dist_lines.png")