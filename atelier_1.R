library(tidyverse)
library(ratdat)

dataw=complete_old
summary(complete_old)
head(complete_old)# tibble = type of dataframe
str(complete_old)
#dataframe? plusieurs vecteurs (e.g. record_id, month) mis ensemble VS. matrice = vecteurs avec même type de données

## ggplot2
library(ggplot2)

ggplot(complete_old, 
  mapping = aes(x=hindfoot_length, y=weight)) +
  geom_point(alpha=0.1) 
  

dataw_c=filter(dataw, !is.na(weight) & !is.na(hindfoot_length))
  
#après cleaning 
ggplot(dataw_c, 
       mapping = aes(x=hindfoot_length, y=weight, color=plot_type)) +
  geom_point(alpha=0.1) +
  scale_color_viridis_d()+
  scale_x_log10()

#meilleure boxplot
ggplot(dataw_c,
       mapping = aes(x=plot_type, y=weight))+
  geom_jitter(alpha=0.1,aes(color=plot_type))+
  geom_boxplot(outlier.shape = NA, fill=NA)+
  scale_x_discrete(labels=label_wrap_gen(width = 10))

#violin graph
ggplot(dataw_c,
       mapping = aes(x=plot_type, y=weight))+
  geom_jitter(alpha=0.1,aes(color=plot_type))+
  geom_violin(fill=NA)+
  scale_x_discrete(labels=label_wrap_gen(width = 10))

#thème 
plot_final= ggplot(dataw_c,
       mapping = aes(x=plot_type, y=weight))+
  geom_jitter(alpha=0.1,aes(color=plot_type))+
  geom_boxplot(outlier.shape = NA, fill=NA)+
  facet_wrap(vars(sex),ncol = 1)+
  scale_x_discrete(labels=label_wrap_gen(width = 10))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x="plot type", y="weight (kg)")

ggsave(filename="figures/plot_final.png",
       plot = plot_final,
       height = 6,
       width=8)
#### TIDYVERSE ####
install.packages("bit64")
library(bit64)
library(tidyverse)
library(ratdat)
surveys <- read_csv("data/cleaned/surveys_complete_77_89.csv")
surveys <- complete_old



#new dataset with year from 1980 and 1983 & variables: year, month, species_id, plot_id
#ce pipe permet de nettoyer un jeu de données (le premier annoncé au debut), sans le répéter à chque fois
dataw_c %>% #ctrl + shift + m
  filter(year==(1980:1985))%>%
  select(c(year,month,species_id,plot_type))

b=dataw_c %>% 
  mutate(weight_kg=weight/1000,
         weight_lbs=weight_kg*2.2) %>% 
  relocate(weight_kg, .after = record_id) %>% 
  relocate(weight_lbs, .after = weight_kg)

b1= b%>% 
  mutate(date=ymd(paste(year,month,day,sep = "-"))) 
  
b1 %>% 
  group_by(date, sex)

library(lubridate)

dataw_c %>% 
  mutate(date=ymd(paste(year,month,day, sep="-"))) %>% 
  group_by(sex,date) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(date,count,color=sex))+
  geom_line()


                