# Pyramid for SARI cases in Brazil

library(tidyverse)
library(lubridate)

# Data available at http://bit.ly/mave-infogripe-dados
info.data <- read.csv2("~/Downloads/dados_semanais_faixa_etaria_sexo_virus_sem_filtro_febre.csv")


info.data.BR <- info.data %>% 
  filter(
    UF == 0, # Brasil 
    escala == "casos",
    dado == "srag",
    Ano.epidemiológico == 2020, 
    (sexo == "M" | sexo == "F"), 
    ) %>% 
  transmute(
    Epiweek = Semana.epidemiológica,
    Sex = as.character(sexo),
    `0 - 9` = X..2.anos +  X2.4.anos + X5.9.anos,
    `10 - 19` = X10.19.anos,
    `20 - 29` = X20.29.anos,
    `30 - 39` = X30.39.anos,
    `40 - 49` = X40.49.anos,
    `50 - 59` = X50.59.anos,
    `60 or more` = X60..anos
  ) %>% gather(key = `Age`, value = Cases, -Sex, -Epiweek)




for(k in 1:max(info.data.BR$Epiweek)){
  aux.k <- info.data.BR %>% filter(Epiweek == k)
  
  p <- ggplot(info.data.BR, aes(x = Age, y = Cases, fill = Sex)) + 
    #geom_bar(stat = "identity") +
    geom_bar(data = subset(aux.k, Sex == "F"), stat = "identity") + 
    geom_bar(data = subset(aux.k, Sex == "M"), aes(y = -Cases), stat = "identity") + 
    scale_y_continuous(breaks = c(-6000, -4500, -3000, -1500, 0, 1500, 4500, 3000, 6000),
                       labels = c(6000, 4500, 3000, 1500, 0, 1500, 4500, 3000, 6000), 
                       limits = c(-6500, 6500)
    ) +
    xlab("Age category") +
    coord_flip() + 
    scale_fill_brewer(palette = "Set1") + 
    theme_bw() +
    labs(caption = "Data source: Infogripe. \n @leosbastos") +
    ggtitle(paste('SARI hospitalized cases in Brazil at epiweek', k))

  ggsave(plot = p, filename = paste0("~/Temp/SRAG/plot",sprintf("%02d",k),".png"), device = "png")
  
}


# Gif file is create using gifski ( https://gif.ski/ )
# gifski -o anim.gif --fps 2 --width 1000 plot*.png



