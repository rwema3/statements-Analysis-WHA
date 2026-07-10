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

