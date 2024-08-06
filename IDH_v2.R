rm(list = ls())
pacman::p_load(tidyverse, dplyr)


pacman::p_load(readr, tidyverse)
HDR21_22_Composite_indices_complete_time_series <- read_csv("C:/Users/User/Desktop/CURSOS/MESTRADO/DISSERTAÇÃO/CONDIÇÕES CAUSAIS E RESULTADO/DADOS SOCIOECONÔMICOS/IDHAD - IDHI/Dados socioeconômicos/PNUD/R/HDR21-22_Composite_indices_complete_time_series.csv")
glimpse(HDR21_22_Composite_indices_complete_time_series)

HDR_manipulado <- HDR21_22_Composite_indices_complete_time_series %>% 
  select(-c(iso3,hdicode, region,hdi_rank_2021,"le_1990":"mf_2021")) %>% 
  pivot_longer(
    -country, names_to = "Ano", values_to = "IDH") %>% 
  filter(country %in% c("Brazil", "Venezuela (Bolivarian Republic of)")) %>% 
  mutate(country = recode(country, "Brazil" = "Brasil")) %>% 
  mutate(country = recode(country, "Venezuela (Bolivarian Republic of)" = "Venezuela"))
  

HDR_manipulado$Ano <- parse_number(HDR_manipulado$Ano)

HDR_manipulado %>% 
  ggplot(aes(x = Ano,
             y = IDH,
             color = country)) +
  geom_line(size = 1) +
  labs(color = "Países",
       linetype = "Países") +
  scale_colour_manual(
    values = c("Brasil" = "darkgreen", "Venezuela" = "blue")) +
  theme(legend.position = "bottom")
  


# Solução 2

HDR_manipulado_2 <- HDR21_22_Composite_indices_complete_time_series %>% 
  select(-c(iso3,hdicode, region,hdi_rank_2021,"le_1990":"mf_2021")) %>% 
  pivot_longer(
    -country, names_to = "Ano", values_to = "IDH")

HDR_Brazil <- subset(HDR_manipulado_2, country == "Brazil")
HDR_Brazil$Ano <- parse_number(HDR_Brazil$Ano)

HDR_Venezuela <- subset(HDR_manipulado_2, country =="Venezuela (Bolivarian Republic of)")
HDR_Venezuela$Ano <- parse_number(HDR_Venezuela$Ano)


plot(x = HDR_Brazil$Ano, y = tapply(HDR_Brazil$IDH, HDR_Brazil$Ano, mean, 
                           na.rm = TRUE), xlab = "Ano", 
     ylab = "IDH", type = "lines", col = "darkgreen")
lines(x = HDR_Venezuela$Ano, y = tapply(HDR_Venezuela$IDH, HDR_Venezuela$Ano, mean, 
                            na.rm = TRUE), col = "blue")
text(2000, 0.71, "Venezuela", col = "blue")
text(2010, 0.69, "Brasil", col = "darkgreen")


