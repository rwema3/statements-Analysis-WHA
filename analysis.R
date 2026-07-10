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
