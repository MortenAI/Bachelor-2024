library(dplyr)
library(tidyverse)
library(vtable)
library(readr)
library(tidyr)
library(car)
library(ggplot2)
library(hrbrthemes)
library(jtools)
library(huxtable)
library(stargazer)
library(sjPlot)
library(effects)
library(glmmTMB)
library(broom)
library(kableExtra)
library(sjPlot)


#Laster inn data##-----------------------------

NSD3127 <- read_csv("C:/Users/morte/Desktop/Samfunnsøkonomi med datavitenskap 2023/6. Semester/Bachelor/Norsk medborgerpanel runde 26, 2023/NSD3127.csv")

# Velger ut variabler og fjerne "ikke svart" og "ikke spurt"

df3 <- NSD3127 %>% 
  dplyr::select(r26_pccoo, r26k2_bginc, r26P4_1, r26P1, r26P5_2, r26_bgciv, r26_bghea, r26_pceco)

df3 <- df3 %>% 
  rename(Tillit = r26_pccoo, Utdanningsnivaa = r26P4_1, Inntekt = r26k2_bginc, Kjoenn = r26P1, Alder = r26P5_2,
         Sivilstatus = r26_bgciv, HelseVurdSubj = r26_bghea, OekonomiVurdSubj = r26_pceco)

df3 <- subset(df3, Tillit != 97 & Tillit != 98)
df3 <- subset(df3, Inntekt != 97 & Inntekt != 98)
df3 <- subset(df3, Utdanningsnivaa != 97 & Utdanningsnivaa != 98)
df3 <- subset(df3, Kjoenn != 97 & Kjoenn != 98)
df3 <- subset(df3, Alder != 97 & Alder != 98)
df3 <- subset(df3, Sivilstatus != 97 & Sivilstatus != 98)
df3 <- subset(df3, HelseVurdSubj != 97 & HelseVurdSubj != 98)
df3 <- subset(df3, OekonomiVurdSubj != 97 & OekonomiVurdSubj != 98)


#UTDANNING

df3 <- df3 %>%
  mutate(Utdanningsnivaa = factor(Utdanningsnivaa, 
                                 levels = c(1, 2, 3),
                                 labels = c("Ingen utdanning/grunnskole", "Videregaaende skole", "Hoegskole/Universitet"),
                                 exclude = 97)) %>%
  mutate(Utdanningsnivaa = fct_relevel(Utdanningsnivaa, "Ingen utdanning/grunnskole"))



#INNTEKT

df3 <- df3 %>%
  mutate(Inntekt = factor(Inntekt, 
                          levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                          labels = c("Mindre enn 150 000 kr", "150 001 kr - 300 000 kr", "300 001 kr - 400 000 kr",
                                     "400 001 kr - 500 000 kr","500 001 kr - 600 000 kr","600 001 kr - 700 000 kr",
                                     "700 001 kr - 1 000 000 kr","mer enn 1 000 000 kr"),
                          exclude = 97)) 


# SLÅR SAMMEN DE TO MINSTE GRUPPENE I INNNTEKT

df3$Inntekt <- as.character(df3$Inntekt)

df3$Inntekt[df3$Inntekt %in% c("Mindre enn 150 000 kr", "150 001 kr - 300 000 kr")] <- "Mindre enn 150 000 kr - 300 000 kr"

df3$Inntekt <- factor(df3$Inntekt)


df3 <- df3 %>% 
  mutate(Inntekt = fct_relevel(Inntekt, "Mindre enn 150 000 kr - 300 000 kr")) 


#KJØNN


df3 <- df3 %>%
  mutate(Kjoenn = factor(Kjoenn, 
                        levels = c(1, 2),
                        labels = c("Mann", "Kvinne"),
                        exclude = 97)) %>% 
  mutate(Kjoenn = fct_relevel(Kjoenn, "Mann"))



#ALDER

df3 <- df3 %>%
  mutate(Alder = factor(Alder, 
                             levels = c(1, 2, 3),
                             labels = c("1959 eller tidligere", "1960-1989", "1990 eller senere"),
                             exclude = 97)) %>% 
  mutate(Alder = fct_relevel(Alder, "1959 eller tidligere"))



#SIVILSTATUS


df3 <- df3 %>%
  mutate(Sivilstatus = factor(Sivilstatus, 
                                  levels = c(1, 2, 3, 4, 5, 6, 7),
                                  labels = c("Singel", "Kjæreste/Særbo", "Samboer", "Gift/juridisk registrert partner",
                                             "Skilt/separert", 'Enke/enkemann', 'Annet'),
                                  exclude = 97)) %>% 
  mutate(Sivilstatus = fct_relevel(Sivilstatus, "Singel"))


# SLÅR SAMMEN GRUPPER TIL 'SINGEL' OG 'IKKE SINGEL'

df3 <- df3 %>%
  mutate(Sivilstatus = ifelse(Sivilstatus %in% c('Singel', 'Skilt/separert/tidligere juridisk registrert partner',
                                                 'Enke/enkemann', 'Annet'), "Singel", "Ikke singel"))



#HELSE SUBJEKTIV VURDERING

df3 <- df3 %>%
  mutate(HelseVurdSubj = factor(HelseVurdSubj, 
                                  levels = c(1, 2, 3, 4, 5),
                                  labels = c("Utmerket", "Meget god", "God", "Noksaa god",
                                             "Daarlig"),
                                  exclude = 97)) %>% 
  mutate(HelseVurdSubj = fct_relevel(HelseVurdSubj, "Daarlig", "Noksaa god", "God", "Meget god", "Utmerket"))




#ØKONOMI SUBJEKTIV VURDERING

df3 <- df3 %>%
  mutate(OekonomiVurdSubj = factor(OekonomiVurdSubj, 
                             levels = c(1, 2, 3, 4, 5, 6, 7),
                             labels = c("Svaert god", "God", "Noe god", "Verken god eller daarlig",
                                        "Noe daarlig", 'Daarlig', 'Svaert daarlig'),
                             exclude = 97)) %>% 
  mutate(OekonomiVurdSubj = fct_relevel(OekonomiVurdSubj, "Svaert daarlig", 'Daarlig', "Noe daarlig", 
                                       "Verken god eller daarlig", "Noe god", "God", "Svaert god"))


# SLÅR SAMMEN DE TO MINSTE GRUPPENE I OEKONOMIVURDSUBJ

df3$OekonomiVurdSubj <- as.character(df3$OekonomiVurdSubj)

df3$OekonomiVurdSubj[df3$OekonomiVurdSubj %in% c("Svaert daarlig", "Daarlig")] <- "Daarlig"

df3$OekonomiVurdSubj <- factor(df3$OekonomiVurdSubj)


df3 <- df3 %>% 
  mutate(OekonomiVurdSubj = fct_relevel(OekonomiVurdSubj, "Daarlig", "Noe daarlig", "Verken god eller daarlig",
                                        "Noe god", "God", "Svaert god")) 



## Deskriptiv statistikk

sumtable(df3)


# REGRESJON

model_1 <- lm(Tillit ~ Inntekt + Utdanningsnivaa + Kjoenn + Alder + Sivilstatus + HelseVurdSubj + OekonomiVurdSubj, data = df3)

model_2 <- lm(Tillit ~ Inntekt + Utdanningsnivaa, data = df3)

model_3 <- lm(Tillit ~ Inntekt, data = df3)

model_4 <- lm(Tillit ~ Utdanningsnivaa, data = df3)


tab_model(model_2, model_1, title = "Regression Model Results")


tab_model(model_4, model_3, model_2, model_1, title = "Regression Model Results")



## GRAFER

#Utdanningsnivå / inntekt

df3 %>% 
  ggplot(aes(x = Inntekt, y = Tillit, group = Utdanningsnivaa, color = Utdanningsnivaa)) +
  stat_smooth(method = "lm", aes(fill = Utdanningsnivaa)) +
  scale_x_discrete(breaks=c("Mindre enn 150 000 kr - 300 000 kr", "300 001 kr - 400 000 kr",
                            "400 001 kr - 500 000 kr","500 001 kr - 600 000 kr","600 001 kr - 700 000 kr",
                            "700 001 kr - 1 000 000 kr","mer enn 1 000 000 kr"),
                   labels=c("0 - 300k", "300k - 400k ", "400k - 500k", "500k - 600k", "600k - 700k", "700k - 1M", "Mer enn 1m"))+
  theme(axis.text.x = element_text(angle = 2, hjust = 1))+
  theme_bw()+
  labs(title = "Figur 3",
       subtitle = "3veis-interaksjonsplot: Inntekt og utdanning",
       x = "Inntekt",
       y = "Tillit")


#Violinplot for inntekt og utdanning

df3 %>%
  group_by(Utdanningsnivaa) %>%
  filter(Tillit > quantile(Tillit, 0.025),
         Tillit < quantile(Tillit, 0.975)) %>%
  ggplot(mapping = aes(x = Utdanningsnivaa, y = Tillit, color = Utdanningsnivaa, fill = Utdanningsnivaa)) +
  geom_violin()  +
  labs(x = "Utdanningsnivå",
       y = "Tillitsnivå") +
  guides(color = "none") +
  viridis::scale_color_viridis(discrete = TRUE, begin = 0.2, end = 0.8, option = "black")+
  scale_x_discrete(breaks=c("Ingen utdanning/grunnskole", "Videregaaende skole", "Hoegskole/Universitet"),
                   labels=c("Ingen utdanning/grunnskole", "Videregående skole", "Høgskole/universitet"))+
  theme_bw()+
  theme(legend.position= 'none')+
  labs(title = "Figur 1",
       subtitle = "Fordeling av observasjoner på tillit per utdanningsnivå",
       x = "Utdanningsnivå",
       y = "Tillit")


  
df3 %>%
  group_by(Inntekt) %>%
  filter(Tillit > quantile(Tillit, 0.025),
         Tillit < quantile(Tillit, 0.975)) %>%
  ggplot(mapping = aes(x = Inntekt, y = Tillit, color = Inntekt, fill = Inntekt)) +
  geom_violin()  +
  labs(x = "Inntekt",
       y = "Tillitsnivå") +
  guides(color = "none") +
  viridis::scale_color_viridis(discrete = TRUE, begin = 0.2, end = 0.8, option = "black")+
  scale_x_discrete(breaks=c("Mindre enn 150 000 kr - 300 000 kr", "300 001 kr - 400 000 kr",
                            "400 001 kr - 500 000 kr","500 001 kr - 600 000 kr","600 001 kr - 700 000 kr",
                            "700 001 kr - 1 000 000 kr","mer enn 1 000 000 kr"),
                   labels=c("0 - 300k", "300k - 400k ", "400k - 500k", "500k - 600k", "600k - 700k", "700k - 1M", "Mer enn 1m"))+
  theme_bw()+
  theme(legend.position= 'none')+
  labs(title = "Figur 2",
       subtitle = "Fordeling av observasjoner på tillit per inntekt gruppe",
       x = "Inntekt",
       y = "Tillit")














