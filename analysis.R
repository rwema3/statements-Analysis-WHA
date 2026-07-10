#WHA

##----------load-------------------
invisible(lapply(c("tidyverse", "pheatmap", "countrycode", "sf", "rnaturalearth", 
                   "rnaturalearthdata", "dendextend", "scales", "patchwork", "ggplot2",
                   "nnet", "ggrepel"), library, character.only = TRUE))

df <- read_csv("path/WHA_climate_theme.csv")
theme_cols <- c("ADP", "CAP", "EQU", "FIN", "GOV", "IMP",
                "LND", "MIT", "RSP", "TEC", "TRN")

non_countries <- c("Save the Children", "Secretariat", "Cooperate accountability",
                   "International Federation of medical students associations",
                   "Organization of family doctors", "World Heart Federation",
                   "International lactation consultant association Asian",
                   "Madison, san frontiers international",
                   "Global health council", "ncd alliance", 
                   "Medical Monday international", 
                   "doctor area and the consistent director", 
                   "The public services international",
                   "The world Medical Association", 
                   "Legal", "GAVI", "medical students associations","DR. Islandless, and director general universal health coverage, healthier populations")

df_filtered <- df %>% filter(!speaker %in% non_countries)
df_summary <- df_filtered %>%
  group_by(speaker) %>%
  summarise(across(all_of(theme_cols), sum, na.rm = TRUE)) %>%
  column_to_rownames("speaker")
##END




###-------P2-------------
library(tidyverse)
library(countrycode)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

save_dir <- "path/global attention"
if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

theme_cols <- c("ADP", "CAP", "EQU", "FIN", "GOV", 
                "IMP", "LND", "MIT", "RSP", "TEC", "TRN")

df_theme_iso <- df_filtered %>%
  mutate(speaker = case_when(
    speaker == "Columbia" ~ "Colombia",
    speaker == "Micronesia" ~ "Micronesia (Federated States of)",
    TRUE ~ speaker
  )) %>%
  mutate(iso3 = countrycode(speaker, "country.name", "iso3c")) %>%
  filter(!is.na(iso3))

world <- ne_countries(scale = "medium", returnclass = "sf")
theme_cols <- c("ADP", "CAP", "EQU", "FIN", "GOV", 
                "IMP", "LND", "MIT", "RSP", "TEC", "TRN")

df_total <- df_filtered %>%
  mutate(speaker = case_when(
    speaker == "Columbia" ~ "Colombia",
    speaker == "Micronesia" ~ "Micronesia (Federated States of)",
    TRUE ~ speaker
  )) %>%
  mutate(iso3 = countrycode(speaker, "country.name", "iso3c")) %>%
  filter(!is.na(iso3)) %>%
  mutate(total_mentions = rowSums(across(all_of(theme_cols))))

df_total_allyears = df_total %>%
  group_by(speaker,iso3) %>%
  summarise(
    top_keywords = paste0(top_keywords, collapse = "; "),
    matched_themes = paste0(matched_themes, collapse = "; "),
    ADP = sum(ADP), CAP = sum(CAP), EQU = sum(EQU), FIN = sum(FIN),
    GOV = sum(GOV), IMP = sum(IMP), LND = sum(LND), MIT = sum(MIT),
    RSP = sum(RSP), TEC = sum(TEC),TRN = sum(TRN),
    total_mentions = sum(total_mentions)
  )

world <- ne_countries(scale = "medium", returnclass = "sf")
map_total <- world %>%
  left_join(df_total_allyears[, c("speaker", "iso3", "total_mentions")], 
            by = c("iso_a3" = "iso3"))

P2 <- ggplot(map_total) +
  geom_sf(aes(fill = total_mentions), color = "grey80") +
  scale_fill_gradient(
    low = "#bdd7e7", high = "#08519c", na.value = "grey95",
    breaks = seq(0, 14, by = 2)
  ) +
  labs(title = "", fill = "Total Mentions") +
  theme_void(base_size = 13) +                         
  theme(
    panel.background = element_rect(fill = "white", colour = NA), 
    plot.background  = element_rect(fill = "white", colour = NA),  
    legend.position = "right",
    plot.margin = margin(0, 0, 0, 0)
  ) +
  coord_sf(expand = FALSE)
P2
##END
