##pairwise estimates for all comparisons with ≥2 studies

source("R/import.R")

level2.treats <- c(
  "burrhole",
  "craniotomy",
  "SEPS",
  "twist",
  "endoscopy",
  "YL1",
  "SEPS",
  "MIS"
)

level3.treats <- c(
  "burrhole_drain",
  "burrhole_nodrain",
  "craniotomy_drain",
  "craniotomy_nodrain",
  "twist_drain",
  "twist_nodrain",
  "endoscopy_drain",
  "endoscopy_nodrain",
  "SEPS",
  "YL1",
  "MIS"
)

level4.treats <- c(
  "burrhole_sd",
  "burrhole_sg",
  "craniotomy_sd",
  "craniotomy_sg",
  "twist_sd",
  "twist_sg",
  "endoscopy_sd",
  "endoscopy_sg"
)

medical.treats <- c(
  "atorvastatin",
  "atorvastatin_dexamethasone",
  "surgery_atorvastatin",
  "dexamethasone",
  "surgery_steroid",
  "surgery_goreisan",
  "TXA",
  "surgery_TXA",
  "medical",
  "conservative",
  "surgery_streptokinase",
  "surgery_alone",
  "surgery_placebo"
)

embolization.treats <- c(
  "embolization",
  "surgery_MMA",
  "surgery_alone"
)



summarise_pairs <- function(data, treats) {
  require(netmeta)
  require(meta)
  require(tidyverse)
  require(stringr)
  
  data <- as_tibble(data)
  
  data$treat <- data$treat %>% str_replace_all(pattern = "surgery_placebo", replacement = "surgery_alone")
  data$treat <- data$treat %>% str_replace_all(pattern = "placebo", replacement = "conservative")
  
  
  data <- data %>% filter(treat %in% treats)
  data <- subset(data, duplicated(data$studlab) | duplicated(data$studlab, fromLast = TRUE))
  data <- data %>% arrange(treat) %>% arrange(studlab)
  
  dat <- data
  
  
  ##pairwise
  dat <- pairwise(
    treat = treat,
    event = event,
    n = n,
    studlab = studlab,
    sm = "RR",
    data = dat
  )
  
  
  
  ##add comparison column
  
  dat$comparison <- str_c(dat$treat1, " vs ", dat$treat2)
  
  ##create datframe of vars needed
  
  dat <- tibble(
    studlab = dat$studlab,
    comparison = dat$comparison,
    treat1 = dat$treat1,
    treat2 = dat$treat2,
    event.e = dat$event1,
    n.e = dat$n1,
    event.c = dat$event2,
    n.c = dat$n2,
    design = dat$design
  )
  
  
  ##find comparisons with ≥2 studies
  
  df <- dat %>% count(comparison)
  df <- df %>% filter(n >= 2) %>% rename(studies = n)
  
  
  ##subset dat
  
  dat <- dat %>% filter(comparison %in% df$comparison)
  
  ##initialise datframe for meta-analysis output
  
  y <- tibble(
    comp = NULL,
    analysis = NULL,
    studies = NULL,
    RR = NULL,
    cilb = NULL,
    ciub = NULL,
    pval = NULL,
    I2 = NULL,
    tau2 = NULL
  )
  
  for(i in df$comparison) {
    
    temp <- dat %>% filter(comparison == i)
    
    meta <- metabin(
      event.e = event.e,
      event.c = event.c,
      n.e = n.e,
      n.c = n.c,
      studlab = studlab,
      data = temp,
      byvar = temp$design,
      sm = "RR",
      method = "MH",
      comb.fixed = FALSE,
      comb.random = TRUE
    )
    
    ##extract sub-group estimates
    
    TE.groups <- exp(meta$TE.random.w)
    cilb.groups <- exp(meta$TE.random.w-1.96*meta$seTE.random.w)
    ciub.groups <- exp(meta$TE.random.w+1.96*meta$seTE.random.w)
    I2.groups <- meta$I2.w*100
    pval.groups <- meta$pval.random.w
    studies.groups <- meta$k.w
    tau2.groups <- meta$tau2.1.w
    
    df.groups <- tibble(
      comp = i,
      analysis = meta$bylevs,
      studies = studies.groups,
      RR = TE.groups,
      cilb = cilb.groups,
      ciub = ciub.groups,
      pval = pval.groups,
      I2 = I2.groups,
      tau2 = tau2.groups
    )
    
    ##extract variables for overall
    TE.overall <- exp(meta$TE.random)
    cilb.overall <- exp(meta$TE.random - 1.96*meta$seTE.random)
    ciub.overall <- exp(meta$TE.random + 1.96*meta$seTE.random)
    pval.overall <- meta$pval.random
    I2.overall <- meta$I2*100
    k.overall <- meta$k.all
    tau2.overall <- meta$tau2
    
    ##dat frame of overall
    df.overall <- tibble(
      comp = i,
      analysis = "overall",
      studies = k.overall,
      RR = TE.overall,
      cilb = cilb.overall,
      ciub = ciub.overall,
      pval = pval.overall,
      I2 = I2.overall,
      tau2 = tau2.overall
    )
    
    ##combine with above
    
    y <- bind_rows(y, df.overall, df.groups)
  }
  
  ##round and scale
  
  y$RR <- round(y$RR, digits = 2)
  y$cilb <- round(y$cilb, digits = 2)
  y$ciub <- round(y$ciub, digits = 2)
  y$pval <- round(y$pval, digits = 6)
  y$I2 <- round(y$I2, digits = 1)
  y$tau2 <- round(y$tau2, digits = 2)
  
  return(y)
  
  
  
}

surgery.recurrence <- rbind(
  summarise_pairs(recurrence_data, treats = level2.treats),
  summarise_pairs(recurrence_data, treats = level3.treats),
  summarise_pairs(recurrence_data, treats = level4.treats))

surgery.complications <- rbind(
  summarise_pairs(complications_data, treats = level2.treats),
  summarise_pairs(complications_data, treats = level3.treats),
  summarise_pairs(complications_data, treats = level4.treats))

medical.recurrence <- summarise_pairs(
  recurrence_data, treats = medical.treats)

medical.complications <- summarise_pairs(
  complications_data, treats = medical.treats)

embolization.recurrence <- summarise_pairs(
  recurrence_data, treats = embolization.treats)

embolization.complications <- summarise_pairs(
  complications_data, treats = embolization.treats)

pairwise_summary_recurrence <- rbind(
  surgery.recurrence,
  medical.recurrence,
  embolization.recurrence
)

pairwise_summary_complications <- rbind(
  surgery.complications,
  medical.complications,
  embolization.complications
)

##summary tables

recurrence.pair <- pairwise_summary_recurrence %>%
  mutate(estimate = paste0(RR, " (", cilb, " - ", ciub, ")")) %>% 
  select(comp, analysis, studies, estimate, pval, I2, tau2) %>% 
  mutate(pval = round(pval, digits = 4))

complications.pair <- pairwise_summary_complications %>% 
  mutate(estimate = paste0(RR, " (", cilb, " - ", ciub, ")")) %>% 
  select(comp, analysis, studies, estimate, pval, I2, tau2) %>% 
  mutate(pval = round(pval, digits = 4))

##set page layout for tables

sec <- prop_section(
  page_size = page_size(width = 21/2.54, height = 29.7/2.54, orient = "landscape"),
  page_margins = page_mar(top = 1.27, bottom = 1.27, left = 1.27, right = 1.27)
)

##export, reproduces Table S2 and Table S8


flextable(recurrence.pair) %>% 
  save_as_docx(path = "Tables/table S2.docx", pr_section = sec)

flextable(complications.pair) %>% 
  save_as_docx(path = "Tables/table S8.docx", pr_section = sec)
