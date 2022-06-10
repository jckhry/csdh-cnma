library(netmeta)
library(metafor)
library(meta)
library(rnaturalearth)
library(rnaturalearthdata)
library(revtools)
library(tidyverse)
library(patchwork)
library(ggstatsplot)
library(gtsummary)
library(flextable)
library(readxl)
library(openxlsx)
library(xtable)
library(magicfor)
library(naniar)
library(ggpubr)
library(officer)
library(evaluate)

##import and clean data

table_chars <- read_excel(
  "Data/table_chars.xlsx",
  col_types = c(
    "text",
    "numeric",
    "numeric",
    "text",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "text",
    "text",
    "text",
    "text",
    "text",
    "numeric"
  )
)

recurrence_data <- read_excel("Data/recurrence_data.xlsx")

complications_data <- read_excel("Data/complications_data.xlsx")

mortality_data <- read_excel("Data/mortality_data.xlsx")

rob_nrs <- read_excel("Data/rob_nrs.xlsx")

rob_random <- read_excel("Data/rob_random.xlsx")

mods_surg_treat <- read_excel("Data/mods_surg_treat.xlsx")

mods_surg_comp <- read_excel("Data/mods_surg_comp.xlsx")

mods_med_treat <- read_excel("Data/mods_med_treat.xlsx")

mods_med_comp <- read_excel("Data/mods_med_comp.xlsx")

##rename separator to avoid using an operator (makes naming items messy)
recurrence_data %<>% as_tibble()
complications_data %<>% as_tibble()
mortality_data %<>% as_tibble()
table_chars %<>% as_tibble()


complications_data$treat <-
  complications_data$treat %>%
  str_replace_all(pattern = "-", replacement = "_")

recurrence_data$treat <-
  recurrence_data$treat %>%
  str_replace_all(pattern = "-", replacement = "_")

mortality_data$treat <-
  mortality_data$treat %>% 
  str_replace_all(pattern = "-", replacement = "_")

table_chars_temp <- table_chars %>% rename(studlab = study)


recurrence_data <-
  left_join(recurrence_data, table_chars_temp, by = "studlab")  %>% 
  select(studlab, treat, event, n.x, Design) %>% 
  rename(n = n.x, design = Design)

complications_data <-
  left_join(complications_data, table_chars_temp, by = "studlab") %>%
  select(studlab, treat, event, n.x, Design) %>%
  rename(n = n.x, design = Design)

mortality_data <-
  left_join(mortality_data, table_chars_temp, by = "studlab") %>%
  select(studlab, treat, event, n.x, Design) %>% 
  rename(n = n.x, design = Design)


##bin P-NRS/R-NRS/Randomised
recurrence_data$design <-
  recurrence_data$design %>% 
  str_replace_all("R-NRS", "non-randomised") %>%
  str_replace_all("P-NRS", "non-randomised") %>% 
  str_replace_all("Randomised", "randomised")

complications_data$design <-
  complications_data$design %>% 
  str_replace_all("R-NRS", "non-randomised") %>% 
  str_replace_all("P-NRS", "non-randomised") %>% 
  str_replace_all("Randomised", "randomised")

mortality_data$design <-
  mortality_data$design %>% 
  str_replace_all("R-NRS", "non-randomised") %>% 
  str_replace_all("P-NRS", "non-randomised") %>% 
  str_replace_all("Randomised", "randomised")

##create directories for storing outputs if not present

if(!file.exists("Figures")) {
  dir.create(path = "Figures")
}

if(!file.exists("Tables")) {
  dir.create(path = "Tables")
}

if(!file.exists("Other outputs")) {
  dir.create(path = "Other outputs")
}
