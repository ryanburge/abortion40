library(fst)
library(socsci)
library(car)
library(interactions)
library(ggsci)
library(png)
library(gridExtra)
library(grid)
library(showtext)

theme_gg <- function(fff, base_size = 20, base_family = "font") 
{
  
  font_add_google(fff, "font")
  showtext_auto()
  
  theme_minimal() +
    theme(legend.position = "none") +
    theme(legend.title = element_blank()) +
    theme(text=element_text(size=20, family="font"))
  
  
}

showtext_opts(dpi = 300)

##run the making_birthyr.R script ####

gss <- read.fst("C://gss18b.fst") %>% 
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
  mutate(ab_scale = ab1 + ab2 + ab3 + ab4 + ab5 + ab6) %>% 
  select(pid7, pid3, male, years, white, ab1, ab2, ab3, ab4, ab5, ab6, ab_scale, birthyr, year, educ, attend)
  
#   
#  
# gss <- gss %>% 
#   mutate(cohorts = car::recode(birthyr, "1883:1899 = '1883-1899';
#                                       1900:1904 = '1900-1904';
#                                       1905:1909 = '1905-1909';
#                                       1910:1914 = '1910-1914';
#                                       1915:1919 = '1915-1919';
#                                       1920:1924 = '1920-1924';
#                                       1925:1929 = '1925-1929';
#                                       1930:1934 = '1930-1934';
#                                       1935:1939 = '1935-1939';
#                                       1940:1944 = '1940-1944';
#                                       1945:1949 = '1945-1949';
#                                       1950:1954 = '1950-1954';
#                                       1955:1959 = '1955-1959';
#                                       1960:1964 = '1960-1964';
#                                       1965:1969 = '1965-1969';
#                                       1970:1974 = '1970-1974';
#                                       1975:1979 = '1975-1979';
#                                       1980:1984 = '1980-1984';
#                                       1985:1989 = '1985-1989';
#                                       1990:1994 = '1990-1994';
#                                       1995:2000 = '1995-2000'; else = NA")) %>% 
#   mutate(cohorts = as.factor(cohorts))



gss <- gss %>% 
  mutate(cohorts = car::recode(birthyr,"1883:1915 = '1883-1915';
                                        1916:1926 = '1916-1926';
                                        1927:1942 = '1927-1942';
                                        1943:1951 = '1943-1951';
                                        1952:1961 = '1952-1961';
                                        1962:1971 = '1962-1971';
                                        1972:1981 = '1972-1981';
                                        1982:1991 = '1982-1991';
                                        1992:2000 = '1992-2000';
                                      else = NA")) %>% 
  mutate(cohorts = as.factor(cohorts))



co_fun <- function(df, coh, file){
  
  test <- gss %>% 
    filter(cohorts == coh) %>% 
    filter(pid3 == "Republican")
  
  reg1 <- lm(ab_scale ~ year*male + educ + white + attend, data = test)
  
  gg2 <- interact_plot(reg1, pred = year, modx = male,  int.width = .76, interval = TRUE) 
  
  filename <- paste('D://abortion40/interactions/rep/',file, '.png', sep = '')

  gg2 +
    theme_gg("Slabo 27px") +
    # theme(legend.position = "bottom") +
    # theme(panel.spacing = unit(1, "lines")) +
    scale_x_continuous(breaks =c(1970, 1990, 2010), limits = c(1970, 2018)) +
    scale_y_continuous(limits = c(0,6)) +
    scale_fill_simpsons() +
    scale_color_simpsons() + 
    theme(plot.subtitle = element_text(size = 54)) +
    theme(legend.text = element_text(size = 54)) +
    theme(axis.text.x = element_text(size = 44)) +
    theme(axis.text.y = element_text(size = 44)) +
    labs(y = "", x = "", title = "", subtitle = glue("{coh}"), caption = "") +
    ggsave(filename, type = "cairo-png", width = 10, height = 10)
  
  test <- gss %>% 
    filter(cohorts == coh) %>% 
    filter(pid3 == "Democrat")
  
  reg1 <- lm(ab_scale ~ year*male + educ + white + attend, data = test)
  
  gg2 <- interact_plot(reg1, pred = year, modx = male,  int.width = .76, interval = TRUE) 
  
  filename <- paste('D://abortion40/interactions/dem/',file, '.png', sep = '')
  
  gg2 +
    theme_gg("Slabo 27px") +
    # theme(legend.position = "bottom") +
    # theme(panel.spacing = unit(1, "lines")) +
    scale_x_continuous(breaks =c(1970, 1990, 2010), limits = c(1970, 2018)) +
    scale_y_continuous(limits = c(0,6)) +
    scale_fill_simpsons() +
    scale_color_simpsons() + 
    theme(plot.subtitle = element_text(size = 54)) +
    theme(legend.text = element_text(size = 54)) +
    theme(axis.text.x = element_text(size = 44)) +
    theme(axis.text.y = element_text(size = 44)) +
    labs(y = "", x = "", title = "", subtitle = glue("{coh}"), caption = "") +
    ggsave(filename, type = "cairo-png", width = 10, height = 10)
}

gss %>% co_fun("1883-1915", "a")
gss %>% co_fun("1916-1926", "b")
gss %>% co_fun("1927-1942", "c")
gss %>% co_fun("1943-1951", "d")
gss %>% co_fun("1952-1961", "e")
gss %>% co_fun("1962-1971", "f")
gss %>% co_fun("1972-1981", "g")
gss %>% co_fun("1982-1991", "h")
gss %>% co_fun("1992-2000", "i")


setwd("D://abortion40/interactions/rep/")

plots <- lapply(ll <- list.files(patt='.*[.]png'),function(x){
  img <- as.raster(readPNG(x))
  rasterGrob(img, interpolate = FALSE)
})


# ggsave("rep_all.pdf",  grid.arrange(grobs=plots, nrow=3, ncol=3, as.table = TRUE))

ggsave("Plots_Combined%03d.png",width=11.5, height=8, 
       grid.arrange(grobs = plots, ncol=3, nrow =3,  as.table = TRUE, top = textGrob("Republicans", gp=gpar(fontsize = 15, font = 8))))


setwd("D://abortion40/interactions/dem/")

plots <- lapply(ll <- list.files(patt='.*[.]png'),function(x){
  img <- as.raster(readPNG(x))
  rasterGrob(img, interpolate = FALSE)
})


# ggsave("dem_all.pdf",  grid.arrange(grobs=plots, nrow=3, ncol=3, as.table = TRUE))
ggsave("Plots_Combined%03d.png",width=11.5, height=8, 
       grid.arrange(grobs = plots, ncol=3, nrow =3,  as.table = TRUE, top = textGrob("Democrats", gp=gpar(fontsize = 15, font = 8))))
