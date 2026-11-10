# @author:  Diana Feijo
# @author update: Diana Feijo
# @workplace: IPMA
# @date:      29/08/2023
# @update:    10/11/2026
# @subject:   ficha recursos stocks - site IPMA

# 0. PACKAGES ------------------------------------------------

library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)

# conclusao: matosinhos regista SYC na DOCAPESCA e depois na DGRM saem com SYT

# 1. DIRECTORIES ---------------------------------------------

pasta_projeto <- "C:/Users/diana.feijo/Documents/2026_desembarques_GUX"

pasta_saida <- file.path(pasta_projeto, "working data")

pasta_plots <- file.path(pasta_saida,"Figuras")

# 2. LOAD DATA -----------------------------------------------

rm(list=ls())

load("C:/Users/diana.feijo/Documents/2026_desembarques_GUX/raw data/vd2003_2023.Rdata")
load("C:/Users/diana.feijo/Documents/2026_desembarques_GUX/raw data/vd_2024.Rdata")
load("C:/Users/diana.feijo/Documents/2026_desembarques_GUX/raw data/vd_2025.Rdata")

#inclusão coluna estrato
#2024
vd_2024 = vd_2024 %>% mutate(
  estrato = case_when(Loa < 10 ~ '0-10',
                      Loa >= 10 & Loa < 12 ~ '10-12',
                      Loa >= 12 & Loa < 18 ~ '12-18',
                      Loa >= 18 & Loa < 24 ~ '18-24',
                      Loa >= 24 & Loa < 40 ~ '24-40',
                      T ~ '40+'
  )
)

vd_2024$estrato <- as.factor(vd_2024$estrato)

# vd_2024$Construction.Year <- as.numeric(substr(vd_2024$Construction.Year, 1 , 4))
# 
# vd_2024 <- vd_2024 %>%
#   mutate(Construction.Year = as.numeric(as.character(Construction.Year)))

#2025
vd_2025 = vd_2025 %>% mutate(
  estrato = case_when(Loa < 10 ~ '0-10',
                      Loa >= 10 & Loa < 12 ~ '10-12',
                      Loa >= 12 & Loa < 18 ~ '12-18',
                      Loa >= 18 & Loa < 24 ~ '18-24',
                      Loa >= 24 & Loa < 40 ~ '24-40',
                      T ~ '40+'
  )
)

vd_2025$estrato <- as.factor(vd_2025$estrato)

vd_2025 <- vd_2025 %>%
  mutate(
    Construction.Year = as.POSIXct(
      paste0(Construction.Year, "-01-01"),
      format = "%Y-%m-%d",
      tz = "UTC"
    )
  )


# deu problemas com UTF-8
# Número de colunas
ncol(vd_2025)

# Identificar nomes com UTF-8 inválido
which(!validUTF8(names(vd_2025)))

# Verificar o nome da coluna 45, se existir
if (ncol(vd_2025) >= 45) {
  charToRaw(names(vd_2025)[45])
}


problemas_2025 <- sapply(vd_2025, function(x) {
  if (is.character(x)) {
    sum(!is.na(x) & !validUTF8(x))
  } else {
    0L
  }
})

problemas_2025[problemas_2025 > 0]

# Identificar valores inválidos
idx <- which(
  !is.na(vd_2025$Vessel.Name) &
    !validUTF8(vd_2025$Vessel.Name)
)

# Testar a conversão em 10 valores
teste <- iconv(
  vd_2025$Vessel.Name[idx[1:10]],
  from = "Windows-1252",
  to = "UTF-8",
  sub = NA
)

# Mostrar os resultados
teste

# Guardar cópia de segurança
vd_2025_backup <- vd_2025

# Identificar os valores inválidos
idx <- which(
  !is.na(vd_2025$Vessel.Name) &
    !validUTF8(vd_2025$Vessel.Name)
)

# Converter apenas esses valores
convertidos <- iconv(
  vd_2025$Vessel.Name[idx],
  from = "Windows-1252",
  to = "UTF-8",
  sub = NA
)

# Confirmar que não houve perdas na conversão
sum(is.na(convertidos))

# Substituir apenas se não existirem NA
if (!anyNA(convertidos)) {
  vd_2025$Vessel.Name[idx] <- convertidos
}

# Confirmar se ainda existem caracteres inválidos
sum(
  !is.na(vd_2025$Vessel.Name) &
    !validUTF8(vd_2025$Vessel.Name)
)

View(vd_2025)

#uniao dos rdatas
vd_2003_2025 <- bind_rows(vd, vd_2024, vd_2025)

vd_2003_2025 <- vd_2003_2025 %>% 
  filter (!vd_2003_2025$zona == 'O') %>% droplevels()

# solucao p resolver os NA nos gears
vd_2003_2025 = vd_2003_2025 %>%
  mutate(across(starts_with("Gear"),
                ~ if_else(is.na(.), "NA", as.factor(.))))

#limpeza do ambiente
vd = vd_2003_2025
rm(vd_2003_2025, vd_2024, vd_2025, vd_2025_backup)

save(vd,file = "raw data/vd2003_2025.RData")

# 3. ANALYSIS --------------------------

vd %>% 
  filter (EGRUPART == "PS") %>% 
  filter(zona != "Portugal") %>% 
  filter (year_sale %in% c(2022)) %>% 
  group_by(zona, year_sale) %>% 
  summarise (total = sum(QVENDA)/1000)


vd$EESPECIE %>% unique()

ruivos <- c("GUU", "GUM", "GUN", "GUG", "GUR", "CTZ", "LDV", "LEP", "GUX")

df_ruivos <- vd[vd$EESPECIE %in% ruivos,]


df_ruivos %>%
  group_by(EESPECIE) %>%
  # group_by(PORTO, EESPECIE) %>%
  summarise (desemb = sum(QVENDA))

df_ruivos %>%
  group_by(EESPECIE) %>%
  group_by(PORTO, EESPECIE) %>%
  summarise (desemb = sum(QVENDA))

options(pillar.sigfig = 7) # assim mostra mais numeros significativos

df_ruivos %>%
  group_by(year_sale) %>%
  # group_by(PORTO, EESPECIE) %>%
  summarise (desemb = round(sum(QVENDA)/1000, d=2))

#valores max e min
df_ruivos %>%
  group_by(year_sale) %>%
  summarise(
    desemb = sum(QVENDA, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) %>%
  summarise(
    min_desemb = min(desemb),
    ano_min = paste(year_sale[desemb == min(desemb)], collapse = ", "),
    max_desemb = max(desemb),
    ano_max = paste(year_sale[desemb == max(desemb)], collapse = ", ")
  )

# A tibble: 1 × 4
# min_desemb ano_min max_desemb ano_max
# <dbl> <chr>        <dbl> <chr>  
#   1   184.9343 2024      506.2018 2003


# 4. PLOTS --------------------------

# 1. Preparar os dados
desemb_spp <- df_ruivos %>%
  group_by(year_sale, EESPECIE) %>%
  summarise(
    desemb = sum(QVENDA, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) %>%
  mutate(
    year_sale = as.numeric(as.character(year_sale))
  )

# 2. Gráfico de barras empilhadas
p_desemb_spp <- ggplot(
  desemb_spp,
  aes(x = factor(year_sale), y = desemb, fill = EESPECIE)
) +
  geom_col(width = 0.85) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = " "),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Year",
    y = "Landings (tonnes)",
    fill = "Species"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(
      angle = 45, hjust = 1
    ),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.major.y = element_line(
      colour = "grey90", linewidth = 0.3
    )
  )

p_desemb_spp

# save(p_desemb_spp,
#   file = file.path(pasta_plots, "desemb_gux_2003_2025.jpeg")
# )

# POR ZONA
desemb_spp_porto_zona <- df_ruivos %>%
  group_by(year_sale, zona, PORTO, EESPECIE) %>%
  summarise(
    desemb = round(sum(QVENDA, na.rm = TRUE) / 1000, 2),
    .groups = "drop"
  ) %>%
  arrange(year_sale, zona, PORTO, EESPECIE)

desemb_total_spp_porto_zona <- df_ruivos %>%
  group_by(zona, PORTO, EESPECIE) %>%
  summarise(
    desemb = round(sum(QVENDA, na.rm = TRUE) / 1000, 2),
    .groups = "drop"
  ) %>%
  arrange(zona, PORTO, desc(desemb))

p_porto_zona <- ggplot(
  desemb_total_spp_porto_zona,
  aes(x = reorder(PORTO, desemb, FUN = sum),
      y = desemb,
      fill = EESPECIE)
) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ zona, scales = "free_y") +
  labs(
    x = "Port",
    y = "Total landings (tonnes), 2003–2025",
    fill = "Species"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

p_porto_zona

# save(p_porto_zona,
#   file = file.path(pasta_plots, "desemb_gux_zona_2003_2025.jpeg")
# )

# 5. OUTPUTS --------------------------

write.csv(
  df_ruivos,
  file = file.path(pasta_saida, "desemb_gux_2003_2025.csv"),
  row.names = FALSE
)


