##estimates of proportions, reproduces table 2

source("R/import.R")

vars_surgery_recurrence <- data.frame(
  labels = c(
    "burrhole",
    "burrhole_drain",
    "burrhole_nodrain",
    "burrhole_sd",
    "burrhole_sg",
    "craniotomy",
    "craniotomy_drain",
    "craniotomy_nodrain",
    "craniotomy_sd",
    "craniotomy_sg",
    "endoscopy",
    "endoscopy_drain",
    "endoscopy_sd",
    "endoscopy_sg",
    "MIS",
    "SEPS",
    "surgery",
    "twist",
    "twist_drain",
    "twist_nodrain",
    "twist_sd",
    "YL1"
  ),
  
  names = c(
    "burrhole",
    "burrhole with drain",
    "burrhole without drain",
    "burrhole with subdural drain",
    "burrhole with subgaleal drain",
    "craniotomy",
    "craniotomy with drain",
    "craniotomy without drain",
    "craniotomy with subdural drain",
    "craniotomy with subgaleal drain",
    "endoscopy",
    "endoscopy with drain",
    "endoscopy with subdural drain",
    "endoscopy with subgaleal drain",
    "other (non-SEPS/YL1) MIS",
    "SEPS",
    "surgery (any form)",
    "twist drill",
    "twist drill with drain",
    "twist drill without drain",
    "twist drill with subdural drain",
    "YL1 needle"
  )
)

vars_surgery_complications <- data.frame(
  labels = c(
    "burrhole",
    "burrhole_drain",
    "burrhole_nodrain",
    "burrhole_sd",
    "burrhole_sg",
    "craniotomy",
    "craniotomy_drain",
    "craniotomy_sd",
    "craniotomy_sg",
    "endoscopy",
    "endoscopy_drain",
    "endoscopy_sd",
    "endoscopy_sg",
    "MIS",
    "SEPS",
    "surgery",
    "twist",
    "twist_drain",
    "twist_nodrain",
    "twist_sd",
    "YL1"
  ),
  
  names = c(
    "burrhole",
    "burrhole with drain",
    "burrhole without drain",
    "burrhole with subdural drain",
    "burrhole with subgaleal drain",
    "craniotomy",
    "craniotomy with drain",
    "craniotomy with subdural drain",
    "craniotomy with subgaleal drain",
    "endoscopy",
    "endoscopy with drain",
    "endoscopy with subdural drain",
    "endoscopy with subgaleal drain",
    "other (non-SEPS/YL1) MIS",
    "SEPS",
    "surgery (any form)",
    "twist drill",
    "twist drill with drain",
    "twist drill without drain",
    "twist drill with subdural drain",
    "YL1 needle"
  )
)

vars_surgery_mortality <- data.frame(
  labels = c(
    "burrhole",
    "burrhole_drain",
    "burrhole_nodrain",
    "burrhole_sd",
    "burrhole_sg",
    "craniotomy",
    "craniotomy_drain",
    "craniotomy_sd",
    "craniotomy_sg",
    "endoscopy",
    "endoscopy_drain",
    "endoscopy_sd",
    "endoscopy_sg",
    "MIS",
    "SEPS",
    "surgery",
    "twist",
    "twist_drain",
    "twist_nodrain",
    "twist_sd",
    "YL1"
  ),
  
  names = c(
    "burrhole",
    "burrhole with drain",
    "burrhole without drain",
    "burrhole with subdural drain",
    "burrhole with subgaleal drain",
    "craniotomy",
    "craniotomy with drain",
    "craniotomy with subdural drain",
    "craniotomy with subgaleal drain",
    "endoscopy",
    "endoscopy with drain",
    "endoscopy with subdural drain",
    "endoscopy with subgaleal drain",
    "other (non-SEPS/YL1) MIS",
    "SEPS",
    "surgery (any form)",
    "twist drill",
    "twist drill with drain",
    "twist drill without drain",
    "twist drill with subdural drain",
    "YL1 needle"
  )
)

vars_medical_recurrence <- data.frame(
  labels = c(
    "atorvastatin",
    "atorvastatin_dexamethasone",
    "conservative",
    "dexamethasone",
    "medical",
    "surgery_atorvastatin",
    "surgery_goreisan",
    "surgery_steroid",
    "surgery_TXA",
    "TXA"
  ),
  
  names = c(
    "atorvastatin alone",
    "atorvastatin with dexamethasone",
    "conservative management",
    "dexamethasone alone",
    "pharmacotherapy (any form)",
    "atorvastatin with surgery",
    "goreisan with surgery",
    "steroids with surgery",
    "tranexamic acid with surgery",
    "tranexamic acid alone"
  )
)

vars_medical_complications <- data.frame(
  labels = c(
    "atorvastatin",
    "conservative",
    "dexamethasone",
    "medical",
    "surgery_atorvastatin",
    "surgery_goreisan",
    "surgery_steroid",
    "surgery_TXA",
    "TXA"
  ),
  
  names = c(
    "atorvastatin alone",
    "conservative management",
    "dexamethasone alone",
    "pharmacotherapy (any form)",
    "atorvastatin with surgery",
    "goreisan with surgery",
    "steroids with surgery",
    "tranexamic acid with surgery",
    "tranexamic acid alone"
  )
)

vars_medical_mortality <- data.frame(
  labels = c(
    "atorvastatin",
    "atorvastatin_dexamethasone",
    "conservative",
    "dexamethasone",
    "medical",
    "surgery_atorvastatin",
    "surgery_steroid",
    "TXA"
  ),
  
  names = c(
    "atorvastatin alone",
    "atorvastatin with dexamethasone",
    "conservative management",
    "dexamethasone alone",
    "pharmacotherapy (any form)",
    "atorvastatin with surgery",
    "steroids with surgery",
    "tranexamic acid alone"
  )
)

vars_embolization_recurrence <- data.frame(
  labels = c(
    "embolization",
    "surgery_MMA"
  ),
  
  names = c(
    "embolization alone",
    "embolization with surgery"
  )
)

vars_embolization_mortality <- vars_embolization_recurrence
vars_embolization_complications <- vars_embolization_recurrence



##function to take list of labels,names and data and
##summarise into a data frame

##data must be arranged as columns in order study,
##treatment, event, n in arm based format

summarise_proportions <- function(var_list, data, output) {
  
  require(metafor)
  require(meta)
  require(tidyverse)
  require(xtable)
  require(magicfor)
  
  data <- as_tibble(data)
  
  data$treat <- data$treat %>% 
    str_replace_all(pattern = "surgery_placebo", replacement = "surgery_alone")
  
  data$treat <- data$treat %>%
    str_replace_all(pattern = "placebo", replacement = "conservative")
  
  data <- data %>% arrange(studlab)
  
  ##initialise output dataframe
  y = tibble(
    label = NULL,
    analysis = NULL,
    studies = NULL,
    event = NULL,
    n = NULL,
    prop = NULL,
    cilb = NULL,
    ciub = NULL,
    I2 = NULL,
    tau2 = NULL
  )
  
  objs <- list()
  
  for(i in var_list$labels) {
    
    ##filter dataframe
    tempdat <- data %>% filter(treat == i)
    
    ##perform meta-analysis
    meta <- metaprop(
      event,
      n,
      studlab,
      method.tau = "ML",
      sm = "PFT",
      method = "Inverse",
      data = tempdat
    )
    
    ##extract treatment effect/CIs and back transform using harmonic mean
    TE <- transf.ipft.hm(
      xi = meta$TE.random,
      targs = list(ni = meta$n)
    )
    
    cilb <- transf.ipft.hm(
      xi = (meta$TE.random - 1.96*meta$seTE.random),
      targs = list(ni = meta$n)
    )
    
    ciub <- transf.ipft.hm(
      xi = (meta$TE.random + 1.96*meta$seTE.random),
      targs = list(ni = meta$n)
    )
    
    df <- tibble(
      label = i,
      analysis = NA,
      studies = meta$k.study,
      event = sum(meta$event),
      n = sum(meta$n),
      prop = TE,
      cilb = cilb,
      ciub = ciub,
      I2 = meta$I2,
      tau2 = meta$tau2
    )
    
    y <- bind_rows(y, df)
    
    objs[[i]] <- meta
    
    
  }
  
  y$analysis <- var_list$names
  
  ##round and scale
  
  y$prop <- y$prop*100
  y$cilb <- y$cilb*100
  y$ciub <- y$ciub*100
  y$I2 <- y$I2*100
  
  y$prop <- round(y$prop, digits = 1)
  y$cilb <- round(y$cilb, digits = 1)
  y$ciub <- round(y$ciub, digits = 1)
  y$I2 <- round(y$I2, digits = 1)
  y$tau2 <- format(y$tau2, scientific = FALSE)
  y$tau2 <- as.numeric(y$tau2)
  y$tau2 <- round(y$tau2, digits = 4)
  
  
  
  if(output == "df"){
    return(y)
  } else{
    return(objs)
  }
  
}

summary_surgery_recurrence <- summarise_proportions(
  var_list = vars_surgery_recurrence,
  data = recurrence_data, 
  output = "df")

summary_surgery_complications <- summarise_proportions(
  var_list = vars_surgery_complications, 
  data = complications_data, 
  output = "df")

summary_surgery_mortality <- summarise_proportions(
  var_list = vars_surgery_mortality, 
  data = mortality_data, output = "df")

summary_medical_complications <- summarise_proportions(
  var_list = vars_medical_complications,
  data = complications_data, 
  output = "df")

summary_medical_mortality <- summarise_proportions(
  var_list = vars_medical_mortality, 
  data = mortality_data, output = "df")

summary_medical_recurrence <- summarise_proportions(
  var_list = vars_medical_recurrence,
  data = recurrence_data, output = "df")

summary_embolization_recurrence <- summarise_proportions(
  var_list = vars_embolization_recurrence,
  data = recurrence_data, output = "df")

summary_embolization_complications <- summarise_proportions(
  var_list = vars_embolization_complications,
  data = complications_data, output = "df")

summary_embolization_mortality <- summarise_proportions(
  var_list = vars_embolization_mortality, data = mortality_data,
  output = "df")

proportions_summary_recurrence <- rbind(
  summary_surgery_recurrence,
  summary_medical_recurrence,
  summary_embolization_recurrence
)

proportions_summary_complications <- rbind(
  summary_surgery_complications,
  summary_medical_complications,
  summary_embolization_complications
)

proportions_summary_mortality <- rbind(
  summary_surgery_mortality,
  summary_medical_mortality,
  summary_embolization_mortality
)

##results in summary dataframes

##create table of estimates

prop.recur <- proportions_summary_recurrence %>%
  mutate(esci = paste0(prop, " (", cilb, " - ", ciub, ")")) %>%
  select(label, studies, event, n, esci) %>%
  rename(k.recur = studies, e.recur = event, n.recur = n, es.recur = esci)

prop.complications <- proportions_summary_complications %>%
  mutate(esci = paste0(prop, " (", cilb, " - ", ciub, ")")) %>%
  select(label, studies, event, n, esci) %>%
  rename(k.comp = studies, e.comp = event, n.comp = n, es.comp = esci)

prop.mortality <- proportions_summary_mortality %>%
  mutate(esci = paste0(prop, " (", cilb, " - ", ciub, ")")) %>% 
  select(label, studies, event, n, esci) %>%
  rename(k.mort = studies, e.mort = event, n.mort = n, es.mort = esci)

prop.tab <- prop.recur %>%
  left_join(prop.complications, by = "label") %>%
  left_join(prop.mortality, by = "label")

##generate page layout for table

sec <- prop_section(
  page_size = page_size(width = 21/2.54, height = 29.7/2.54, orient = "landscape"),
  page_margins = page_mar(top = 1.27, bottom = 1.27, left = 1.27, right = 1.27)
)

##reproduce table 2

flextable(prop.tab) %>% save_as_docx(path = "Tables/table 2.docx",
                                     pr_section = sec)
