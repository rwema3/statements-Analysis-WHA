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

#--------law and policy data clean------------
library(tidyverse)
library(janitor)     
library(countrycode)

data_dir <- "path/disease and policy"
law_raw <- read_csv(file.path(data_dir, "global climate laws.csv"),   show_col_types = FALSE) %>%
  clean_names() 
pol_raw <- read_csv(file.path(data_dir, "global climate policy.csv"), show_col_types = FALSE) %>%
  clean_names()

custom_iso <- c(
  "XKX" = "XKX",     
  "EU"  = "EUU", "EUR" = "EUU",
  "TWN" = "TWN"   
)

process_df <- function(df, date_col){
  df %>% 
    separate_rows(geographies, sep = ";|\\|") %>% 
    mutate(geographies = str_trim(geographies)) %>% 
    mutate(
      iso3 = case_when(str_detect(geographies, "^[A-Z]{3}$") ~ geographies),
      iso3 = coalesce(
        iso3,
        countrycode(geographies, "country.name", "iso3c",
                    custom_match = custom_iso,
                    warn = FALSE)
      )
    ) %>% 
    filter(!is.na(iso3)) %>% 
    mutate(year = as.integer(substr(.data[[date_col]], 1, 4))) %>% 
    group_by(iso3, family_name, .drop = FALSE) %>% 
    slice_min(year, with_ties = FALSE) %>% 
    ungroup() %>% 
    select(iso3,
           country = geographies,
           year,
           file_name = document_title)
}

law_clean <- process_df(law_raw, "family_publication_date")
pol_clean <- process_df(pol_raw, "family_publication_date")

law_count <- law_clean %>% count(iso3, year, name = "law_n")
pol_count <- pol_clean %>% count(iso3, year, name = "policy_n")

summary_tbl <- full_join(law_count, pol_count, by = c("iso3", "year")) %>% 
  replace_na(list(law_n = 0, policy_n = 0)) %>% 
  arrange(iso3, year)

write_csv(law_clean,  file.path(data_dir, "global_climate_laws_clean.csv"))
write_csv(pol_clean,  file.path(data_dir, "global_climate_policy_clean.csv"))
write_csv(summary_tbl, file.path(data_dir, "global_climate_law_policy_summary.csv"))
##END


#---------P3----------------
policy_sum <- read_csv(
  file.path(data_dir, "global_climate_law_policy_summary.csv"),
  show_col_types = FALSE
)
policy_tot <- policy_sum %>% 
  group_by(iso3) %>% 
  summarise(policy_total = sum(policy_n, na.rm = TRUE), .groups = "drop") %>% 
  filter(policy_total > 0) %>%                           
  mutate(                                               
    policy_cat = case_when(
      policy_total >= 40 ~ "≥ 40",
      policy_total >= 20 ~ "20–39",
      policy_total >= 10 ~ "10–19",
      policy_total >=  5 ~ "5–9",
      TRUE               ~ "1–4"
    )
  )   

df_h3 <- map_total |>                     
  st_drop_geometry() |>                 
  select(iso3 = iso_a3,
         total_mentions,
         income_grp) |>                  
  left_join(policy_tot, by = "iso3") |>  
  mutate(across(c(total_mentions, policy_total),
                ~ replace_na(., 0)))   

talk_thr   <- median(df_h3$total_mentions, na.rm = TRUE)
policy_thr <- median(df_h3$policy_total  , na.rm = TRUE)


df_h3 <- df_h3 %>%
  mutate(mismatch = recode(mismatch,
                           "Low-Talk  / Low-Policy"  = "Low-Policy / Low-Talk",
                           "High-Talk / Low-Policy"  = "High-Policy / Low-Talk",
                           "Low-Talk  / High-Policy" = "Low-Policy / High-Talk",
                           "High-Talk / High-Policy" = "High-Policy / High-Talk"
  )) %>%
  mutate(mismatch = factor(mismatch, levels = c(
    "Low-Policy / Low-Talk",
    "High-Policy / Low-Talk",
    "Low-Policy / High-Talk",
    "High-Policy / High-Talk"
  )))
tab_h3   <- table(df_h3$mismatch, df_h3$income_grp)
chisq_h3 <- chisq.test(tab_h3)            

print(tab_h3)
print(chisq_h3)

multi_mod <- multinom(mismatch ~ income_grp, data = df_h3)
summary(multi_mod)

p_h3_blue <- ggplot(df_h3, aes(group, fill = mismatch)) +
  geom_bar(position = "fill",
           colour = "grey40",
           width = .80) +
  scale_x_discrete(
    limits = c("OECD-Donor", "Transition", "Emerging", "Vulnerable"),
    labels = c("OECD\n donors", "Transition\n economies", "Emerging\n emitters", "Vulnerable\n group")
  ) +
  scale_y_continuous(
    labels = percent_format(),
    breaks = seq(0, 1, .25),
    expand = expansion(mult = c(0, .05))
  ) +
  scale_fill_manual(
    values = c(
      "Low-Policy / Low-Talk"  = "#4292c6",
      "High-Policy / Low-Talk" = "#6baed6",
      "Low-Policy / High-Talk" = "#bdd7e7",
      "High-Policy / High-Talk"= "#08519c"
    ),
    name = "Mismatch Type"
  ) +
  labs(
    x = "UNFCCC Group",
    y = "Share of Countries",
    title = ""
  ) +
  theme_classic(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.ticks.length = unit(4, "pt"),
    legend.position = "right",
    axis.title.x = element_text(hjust = 0.5, size = 13, colour = "black", face = "bold"),
    axis.title.y = element_text(hjust = 0.5, size = 13, colour = "black", face = "bold"),
    axis.text.x = element_text(size= 13, colour = "black"),
    axis.text.y = element_text(size= 13, colour = "black"),
    axis.line = element_line(colour = "black")
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
p_h3_blue

P3

##     chisq_h3$p.value < 0.05               ##
##   “Low-Talk / High-Policy”  (RRR↑, p<.05) ##
##   “High-Talk / Low-Policy” (RRR↑, p<.05)  ##
##END


#---------P1-----------
library(ggplot2)
library(gridExtra)
library(grid)
library(Cairo)
library(cowplot)
library(gtable)
library(ggplotify)

UNFCCC_country <- read.csv("path/UNFCCC_4_Group_Classification.csv")
colnames(UNFCCC_country) = c("country","iso3","Group","unfccc_group")

theme_group <- tibble(
  theme = c("ADP", "CAP", "TRN", "GOV", "EQU", "FIN", "TEC", "MIT", "IMP", "RSP", "LND"),
  group = c("System Capacity", "System Capacity", "System Capacity",
            "Governance & Equity", "Governance & Equity",
            "Finance & Technology", "Finance & Technology", "Finance & Technology",
            "Health Risk & Loss", "Health Risk & Loss", "Health Risk & Loss"),
  theme_full = c(
    "Adaptation &\n Resilience", "Capacity-building", "Transparency\n& Stocktake",
    "Governance", "Equity &\n Empowerment",
    "Finance &\n Mobilization", "Technology &\n Innovation", "Mitigation",
    "Impacts &\n Co-benefits", "Response &\n Transition", "Loss & Damage"))

df_long <- df_summary %>%
  rownames_to_column("country") %>%
  pivot_longer(-country, names_to = "theme", values_to = "mention") %>%
  filter(mention > 0) %>%
  left_join(theme_group,   by = "theme") %>%
  left_join(UNFCCC_country, by = "country") %>%
  drop_na(unfccc_group)                       
df_long1 = df_long %>%
  mutate(unfccc_group = case_when(
    unfccc_group == "Emerging"           ~ "Emerging emitters",
    unfccc_group == "OECD_Donor"         ~ "OECD donors",
    unfccc_group == "Transition"         ~ "Transition economies",
    unfccc_group == "Vulnerable"           ~ "Vulnerable group",
    TRUE                         ~ as.character(unfccc_group)
  ))
