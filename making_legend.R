reg1 <- lm(ab_scale ~ year*male + educ + white + attend, data = gss)

gg2 <- interact_plot(reg1, pred = year, modx = male,  int.width = .76, interval = TRUE) 



gg2 +
  theme_gg("Slabo 27px") +
   theme(legend.position = "bottom") +
  # theme(panel.spacing = unit(1, "lines")) +
  scale_x_continuous(breaks =c(1970, 1990, 2010), limits = c(1970, 2018)) +
  scale_y_continuous(limits = c(0,6)) +
  scale_fill_simpsons() +
  scale_color_simpsons() +
  theme(legend.text = element_text(size = 24)) +
  ggsave("D://abortion40/legend_save.png", width =  12)