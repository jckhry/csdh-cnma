##installs dependencies

packages <- c(
  "netmeta",
  "metafor",
  "meta",
  "tidyverse",
  "rnaturalearth",
  "rnaturalearthdata",
  "revtools",
  "patchwork",
  "ggstatsplot",
  "gtsummary",
  "flextable",
  "readxl",
  "openxlsx",
  "xtable",
  "maps",
  "magicfor",
  "naniar",
  "ggpubr",
  "officer",
  "evaluate",
  "devtools",
  "renv",
  "nloptr",
  "lme4"
)

install.packages(packages)

devtools::install_github("EmilHvitfeldt/miscpalettes")


