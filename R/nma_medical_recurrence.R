##medical CNMA - primary outcome

source("R/import.R")

##define treatments in our analysis which are eligible for the NMA

sec <- prop_section(
  page_size = page_size(width = 21/2.54, height = 29.7/2.54, orient = "landscape"),
  page_margins = page_mar(top = 1.27, bottom = 1.27, left = 1.27, right = 1.27)
)

sec_portrait <- prop_section(
  page_size = page_size(width = 21/2.54, height = 29.7/2.54, orient = "portrait"),
  page_margins = page_mar(top = 1.27, bottom = 1.27, left = 1.27, right = 1.27)
)


##
treats <- c(
  "atorvastatin",
  "surgery_atorvastatin",
  "dexamethasone",
  "surgery_steroid",
  "atorvastatin_dexamethasone",
  "surgery_goreisan",
  "surgery_streptokinase",
  "TXA",
  "surgery_TXA",
  "placebo",
  "surgery_placebo",
  "conservative",
  "surgery_alone"
)

##find multi-arm studies with these treatments

nma.dat <- recurrence_data %>% filter(treat %in% treats)

##select multi-arm studies
nma.dat <- subset(nma.dat, duplicated(nma.dat$studlab) | duplicated(nma.dat$studlab, fromLast = TRUE))

##clean up treatment names/labels to facilitate cNMA

nma.dat$treat <- nma.dat$treat %>%
  str_replace_all("_alone", "") %>%
  str_replace_all("_", "+") %>%
  str_replace_all("atorvastatin", "ATS") %>%
  str_replace_all("dexamethasone", "ST") %>% 
  str_replace_all("placebo", "Plac") %>%
  str_replace_all("surgery", "Surg") %>% 
  str_replace_all("goreisan", "GRS") %>% 
  str_replace_all("streptokinase", "SK") %>% 
  str_replace_all("conservative", "Cons") %>%
  str_replace_all("steroid", "ST")

##equate placebo and conservative

nma.dat$treat <- nma.dat$treat %>% str_replace_all("\\+Plac", "")

nma.dat$treat <- nma.dat$treat %>% str_replace_all("Plac", "Cons")



##list treatments for NMA

unique(nma.dat$treat)

nma.dat <- nma.dat %>% arrange(treat)

##transform for inclusion in netmeta and calculate effect sizes

nma.pairs <- pairwise(
  treat = treat,
  event = event,
  n = n,
  studlab = studlab,
  data = nma.dat,
  sm = 'RR'
)

nma.pairs <- as_tibble(nma.pairs)

##summary of characteristics of studies in medical CNMA for recurrence
pairs.med.recur <- nma.pairs %>% 
  select(studlab, design, treat1, treat2, TE, seTE) %>%
  rename(es.recur = TE, se.recur = seTE)

pairs.med.recur$design <- pairs.med.recur$design %>%
  str_replace_all(pattern = "non-randomised", replacement = "NRS") %>%
  str_replace_all(pattern = "randomised", replacement = "Randomised")

chars_temp <- table_chars_temp %>% 
  select(studlab, n, `F/U`, M, F, BL, `Age (yrs.)`, Location, Defn.) %>% 
  rename(followup = `F/U`, 
         m = M, f = F,
         bl = BL, 
         age = `Age (yrs.)`,
         defn = Defn.,
         location = Location)

pairs.med.recur <- pairs.med.recur %>% left_join(chars_temp)

random.all <- rob_random %>% 
  select(Study, Overall) %>% 
  rename(studlab = Study, bias = Overall)

nrs.all <- rob_nrs %>%
  select(Study, Overall) %>%
  rename(studlab = Study, bias = Overall)

bias <- bind_rows(random.all, nrs.all)

pairs.med.recur <- pairs.med.recur %>%
  left_join(bias, by = "studlab") %>%
  arrange(studlab) %>%
  mutate(es.recur = round(es.recur, digits = 3),
         se.recur = round(se.recur, digits = 3))

##reproduce part of table S6

flextable(pairs.med.recur) %>%
  save_as_docx(path = "Tables/table S6_1.docx",
               pr_section = sec)

##naive NMA - NRS have equal weight

nma.naive <- netmeta(
  TE,
  seTE,
  treat1,
  treat2,
  studlab,
  data = nma.pairs,
  sm = "RR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "ML"
)

##design adjusted analyses

##disconntected network, therefore use separated subnetworks with various assumptions

##for details see Figure S18

nma.dat.random <- nma.pairs %>% filter(design == "randomised")
nma.dat.nrs <- nma.pairs %>% filter(design == "non-randomised")

netcon <- netconnection(
  treat1 = treat1,
  treat2 = treat2,
  studlab = studlab, 
  data = nma.dat.random)


##reproduce figure S18.1

##dashed lines were added post-hoc in Adobe Illustrator

pdf(file = "Figures/figure S18_1.pdf")

netgraph(netcon)

dev.off()

##next we do design-adjusted analyses on subnetworks of randomized studies separately

treats.random1 <- c("ST", "ATS+ST", "ATS", "Cons")
treats.random2 <- c("Surg", "Surg+ST", "Surg+GRS", "Surg+TXA")

nma.dat.random1 <- nma.dat %>% 
  filter(treat %in% treats.random1) %>%
  filter(design == "randomised")

nma.dat.random2 <- nma.dat %>%
  filter(treat %in% treats.random2) %>%
  filter(design == "randomised")

nma.dat.random1 <- pairwise(
  treat = treat,
  event = event,
  n = n,
  studlab = studlab,
  data = nma.dat.random1,
  sm = 'RR'
) %>% as_tibble()

nma.dat.random2 <- pairwise(
  treat = treat,
  event = event,
  n = n,
  studlab = studlab,
  data = nma.dat.random2,
  sm = 'RR'
) %>% as_tibble()

nma.random1 <- netmeta(
  TE,
  seTE,
  treat1,
  treat2,
  studlab,
  data = nma.dat.random1,
  sm = "RR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "ML"
)

nma.random2 <- netmeta(
  TE,
  seTE,
  treat1,
  treat2,
  studlab,
  data = nma.dat.random2,
  sm = "RR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "ML"
)

##subnetwork 1 (surg, surg+st, surg+grs, surg+txa)

treat.net1 <- c("Surg", "Surg+ST", "Surg+GRS", "Surg+TXA")

nma.nrs.net1 <- nma.dat %>%
  filter(design == "non-randomised") %>%
  filter(treat %in% treat.net1)

nma.nrs.net1 <- subset(
  nma.nrs.net1, duplicated(
    nma.nrs.net1$studlab) | duplicated(
      nma.nrs.net1$studlab, fromLast = TRUE))

nma.nrs.net1 <- pairwise(
  treat = treat, 
  event = event,
  n = n, 
  studlab = studlab, 
  sm = "RR", 
  data = nma.nrs.net1)

nma.nrs.net1 <- nma.nrs.net1 %>% as_tibble()


random1.netmeta <- netmeta(
  TE,
  seTE, 
  treat1,
  treat2, 
  studlab, 
  data = nma.dat.random2,
  comb.fixed = FALSE, 
  comb.random = T, 
  sm = "RR")

nma.pairs.d1 <- nma.dat %>% 
  filter(treat %in% treat.net1)

nma.pairs.d1 <- subset(
  nma.pairs.d1, duplicated(
    nma.pairs.d1$studlab) | duplicated(
      nma.pairs.d1$studlab, fromLast = TRUE))

nma.pairs.d1 <- pairwise(
  treat = treat, 
  event = event, 
  n = n, 
  studlab = studlab,
  sm = "RR", 
  data = nma.pairs.d1) %>% 
  as_tibble()

nma.d1 <- netmeta(
  TE, 
  seTE,
  treat1,
  treat2, 
  studlab,
  data = nma.pairs.d1,
  sm = "RR",
  comb.fixed = F,
  comb.random = T)

adjusted.analyses4 <- list()

##define weights for nrs

weights <- c(0.2,0.4,0.6, 0.8)

##set colours for forest plot points

cols <- c("#32CD32", "#4682B4", "#4682B4", "#4682B4", "#4682B4", "black")

##loop through weights
for(i in weights) {
  
  ##first, divide study standard errors by sqrt(weight)
  
  
  ##this will have the effect of variance/weight when
  ##netmeta() calculates V as seTE^2
  nrs.weighted3 <- nma.nrs.net1
  nrs.weighted3$seTE <- nrs.weighted3$seTE/sqrt(i)
  
  
  adjusted.pairs3 <- bind_rows(nrs.weighted3, nma.dat.random2)
  
  nma.weighted3 <- netmeta(
    TE = TE,
    seTE = seTE,
    treat1 = treat1,
    treat2 = treat2,
    studlab = studlab,
    data = adjusted.pairs3,
    comb.fixed = FALSE,
    comb.random = TRUE,
    sm = "RR",
    method.tau = "ML"
  )
  
  lab = paste0("w", i)
  
  ##store weighted NMA
  
  adjusted.analyses4[[lab]] <- nma.weighted3
}

design.adjusted4 <- netbind(
  random1.netmeta,
  adjusted.analyses4$w0.2,
  adjusted.analyses4$w0.4,
  adjusted.analyses4$w0.6,
  adjusted.analyses4$w0.8,
  nma.d1,
  name = c("w = 0 (randomised studies only)",
           "w = 0.2",
           "w = 0.4",
           "w = 0.6",
           "w = 0.8",
           "w = 1 (naive NMA)"),
  col.study = cols,
  col.inside = cols,
  col.square = cols,
  col.square.lines = cols,
  reference.group = "Surg")

##subnetwork 2 (ST, ATS+ST, ATS, Cons)

treat.net2 <- c("ATS", "ATS+ST", "Cons", "ST")

nma.nrs.net1 <- nma.dat %>% 
  filter(design == "non-randomised") %>%
  filter(treat %in% treat.net2)

nma.nrs.net1 <- subset(
  nma.nrs.net1, duplicated(
    nma.nrs.net1$studlab) | duplicated(
      nma.nrs.net1$studlab, fromLast = TRUE))

nma.nrs.net1 <- pairwise(
  treat = treat,
  event = event,
  n = n, 
  studlab = studlab, 
  sm = "RR", 
  data = nma.nrs.net1)

nma.nrs.net1 <- nma.nrs.net1 %>%
  as_tibble()


random2.netmeta <- netmeta(
  TE,
  seTE,
  treat1, 
  treat2,
  studlab,
  data = nma.dat.random1,
  comb.fixed = FALSE,
  comb.random = T, 
  sm = "RR")

nma.pairs.d2 <- nma.dat %>%
  filter(treat %in% treat.net2)

nma.pairs.d2 <- subset(
  nma.pairs.d2, duplicated(
    nma.pairs.d2$studlab) | duplicated(
      nma.pairs.d2$studlab, fromLast = TRUE))

nma.pairs.d2 <- pairwise(
  treat = treat, 
  event = event, 
  n = n,
  studlab = studlab,
  sm = "RR", 
  data = nma.pairs.d2) %>% 
  as_tibble()

nma.d2 <- netmeta(
  TE, 
  seTE,
  treat1,
  treat2,
  studlab,
  data = nma.pairs.d2,
  sm = "RR", 
  comb.fixed = F,
  comb.random = T)

adjusted.analyses5 <- list()

for(i in weights) {
  
  ##first, divide study standard errors by sqrt(weight)
  
  
  ##this will have the effect of variance/weight when 
  ##netmeta() calculates V as seTE^2
  nrs.weighted3 <- nma.nrs.net1
  nrs.weighted3$seTE <- nrs.weighted3$seTE/sqrt(i)
  
  
  adjusted.pairs3 <- bind_rows(nrs.weighted3, nma.dat.random1)
  
  nma.weighted3 <- netmeta(
    TE = TE,
    seTE = seTE,
    treat1 = treat1,
    treat2 = treat2,
    studlab = studlab,
    data = adjusted.pairs3,
    comb.fixed = FALSE,
    comb.random = TRUE,
    sm = "RR",
    method.tau = "ML"
  )
  
  lab = paste0("w", i)
  
  ##store weighted NMA
  
  adjusted.analyses5[[lab]] <- nma.weighted3
}

design.adjusted5 <- netbind(
  random2.netmeta,
  adjusted.analyses5$w0.2,
  adjusted.analyses5$w0.4,
  adjusted.analyses5$w0.6,
  adjusted.analyses5$w0.8,
  nma.d2,
  name = c("w = 0 (randomised studies only)",
           "w = 0.2",
           "w = 0.4",
           "w = 0.6",
           "w = 0.8",
           "w = 1 (naive NMA)"),
  col.study = cols,
  col.inside = cols,
  col.square = cols,
  col.square.lines = cols,
  reference.group = "Cons")

forest(design.adjusted5, col.by = "black")

##reproduce first panel of figure s18.2
dev.off()

pdf(file = "Figures/figure S18_2_1.pdf")

forest(design.adjusted4, col.by = "black")

dev.off()

##reproduce second panel


pdf(file = "Figures/figure S18_2_2.pdf")

forest(design.adjusted5, col.by = "black")

dev.off()


##next we connect the network using the non-randomized study by Miah 2020
##first fit NMA for all values of weights

adjusted.analyses <- list()

for(i in weights) {
  
  ##first, divide study standard errors by sqrt(weight)
  
  
  ##this will have the effect of variance/weight when 
  ##netmeta() calculates V as seTE^2
  nrs.weighted <- nma.dat.nrs
  nrs.weighted$seTE <- nrs.weighted$seTE/sqrt(i)
  
  
  adjusted.pairs <- bind_rows(nrs.weighted, nma.dat.random)
  
  nma.weighted <- netmeta(
    TE = TE,
    seTE = seTE,
    treat1 = treat1,
    treat2 = treat2,
    studlab = studlab,
    data = adjusted.pairs,
    comb.fixed = FALSE,
    comb.random = TRUE,
    sm = "RR",
    method.tau = "ML"
  )
  
  lab = paste0("w", i)
  
  ##store weighted NMA
  
  adjusted.analyses[[lab]] <- nma.weighted
}


##fit NMA using one NRS to connect
nma.dat.miah <- nma.dat.nrs %>%
  filter(studlab == "Miah 2020") %>% 
  bind_rows(nma.dat.random)

nma.miah <- netmeta(
  TE,
  seTE, 
  treat1,
  treat2, 
  studlab, 
  data = nma.dat.miah,
  comb.fixed = F,
  comb.random = T,
  sm = "RR", 
  method.tau = "ML")

##create forest plot to compare findings

design.adjusted6 <- netbind(
  nma.miah,
  adjusted.analyses$w0.2,
  adjusted.analyses$w0.4,
  adjusted.analyses$w0.6,
  adjusted.analyses$w0.8,
  nma.naive,
  name = c("w = 0 (randomised studies + Miah 2020)",
           "w = 0.2",
           "w = 0.4",
           "w = 0.6",
           "w = 0.8",
           "w = 1 (naive NMA)"),
  col.study = cols,
  col.inside = cols,
  col.square = cols,
  col.square.lines = cols,
  reference.group = "Cons")

##reproduce figure S18.3

dev.off()

pdf(file = "Figures/figure S18_3.pdf", height = 20, width = 10)

forest(design.adjusted6, col.by = "black")

dev.off()


##connect network using Ban 2017

nma.dat.ban <- nma.dat.nrs %>%
  filter(studlab == "Ban 2017") %>% 
  bind_rows(nma.dat.random)

nma.ban <- netmeta(
  TE,
  seTE,
  treat1,
  treat2, 
  studlab, 
  data = nma.dat.ban, 
  comb.fixed = F,
  comb.random = T,
  sm = "RR", 
  method.tau = "ML")

##create forest plot to compare findings

design.adjusted7 <- netbind(
  nma.ban,
  adjusted.analyses$w0.2,
  adjusted.analyses$w0.4,
  adjusted.analyses$w0.6,
  adjusted.analyses$w0.8,
  nma.naive,
  name = c("w = 0 (randomised studies + Ban 2017)",
           "w = 0.2",
           "w = 0.4",
           "w = 0.6",
           "w = 0.8",
           "w = 1 (naive NMA)"),
  col.study = cols,
  col.inside = cols,
  col.square = cols,
  col.square.lines = cols,
  reference.group = "Cons")

##reproduce figure S18.4

pdf(file = "Figures/figure S18_4.pdf", height = 20, width = 10)

forest(design.adjusted7, col.by = "black")

dev.off()

##connect network using both NRS

studlabs <- c("Miah 2020", "Ban 2017")

nma.dat.both <- nma.dat.nrs %>% 
  filter(studlab %in% studlabs) %>% 
  bind_rows(nma.dat.random)

nma.both <- netmeta(
  TE, 
  seTE, 
  treat1, 
  treat2,
  studlab, 
  data = nma.dat.both, 
  comb.fixed = F,
  comb.random = T,
  sm = "RR", 
  method.tau = "ML")

##create forest plot to compare findings

design.adjusted8 <- netbind(
  nma.both,
  adjusted.analyses$w0.2,
  adjusted.analyses$w0.4,
  adjusted.analyses$w0.6,
  adjusted.analyses$w0.8,
  nma.naive,
  name = c("w = 0 (randomised studies + 2 NRS)",
           "w = 0.2",
           "w = 0.4",
           "w = 0.6",
           "w = 0.8",
           "w = 1 (naive NMA)"),
  col.study = cols,
  col.inside = cols,
  col.square = cols,
  col.square.lines = cols,
  reference.group = "Cons")

##reproduce figure S18.5

pdf(file = "Figures/figure S18_5.pdf", height = 20, width = 10)

forest(design.adjusted8, col.by = "black")

dev.off()

##compare effect modifiers across treatments

mods_med_treat <- mods_med_treat %>%
  rename(studlab = study) %>%
  filter(studlab %in% unique(nma.naive$studlab))

mods_med_treat <- mods_med_treat %>% 
  left_join(table_chars_temp, by = "studlab") %>%
  select(studlab, treat, age, bilat, male, Design, Defn.) %>%
  replace_with_na(replace = list(Defn. =  "NR")) %>%
  rename(design = Design, defn = Defn.)

mods_med_treat$design <- mods_med_treat$design %>%
  str_replace_all("R-NRS", "Non-randomised") %>% 
  str_replace_all("P-NRS", "Non-randomised")

tbl_mods_treat <- mods_med_treat %>% 
  select(-studlab) %>% 
  tbl_summary(by = treat, missing = "no")

##reproduce table S19.1

tbl_mods_treat %>%
  as_flex_table() %>%
  save_as_docx(path = "Tables/table S19_1.docx",
               pr_section = sec)


##compare effect modifiers across components

mods_med_comp <- mods_med_comp %>%
  rename(treat = component) %>% 
  rename(studlab = study) %>% 
  filter(studlab %in% unique(nma.naive$studlab))

mods_med_comp <- mods_med_comp %>% 
  left_join(table_chars_temp, by = "studlab") %>%
  select(studlab, treat, age, bilat, male, Design, Defn.) %>%
  replace_with_na(replace = list(Defn. =  "NR")) %>%
  rename(design = Design, defn = Defn.)

mods_med_comp$design <- mods_med_comp$design %>% 
  str_replace_all("R-NRS", "Non-randomised") %>% 
  str_replace_all("P-NRS", "Non-randomised")

tbl_mods_comp <- mods_med_comp %>% 
  select(-studlab) %>%
  tbl_summary(by = treat, missing = "no")

##reproduce table S19.2

tbl_mods_comp %>%
  as_flex_table() %>%
  save_as_docx(path = "Tables/table S19_2.docx",
               pr_section = sec)

##assess additivity in same manner as surgical NMA

all.pairs <- netpairwise(
  nma.naive,
  comb.fixed = FALSE,
  comb.random = TRUE, 
  backtransf = FALSE,
  reference.group = "Cons")

all.pairs

all.pairs <- tibble(
  studlab = all.pairs$studlab,
  TE = all.pairs$TE, 
  seTE = all.pairs$seTE,
  comp = all.pairs$byvar)

all.pairs %>% count(comp)

st.comps <- c("ATS:ATS+ST",
              "ST:Cons",
              "Surg:Surg+ST")

st.comps <- all.pairs %>% 
  filter(comp %in% st.comps) %>% 
  mutate(TE = if_else(
    comp == "ST:Cons",
    TE,
    TE*-1
  )) %>%
  mutate(comp = "ST")

ats.comps <- c("ATS:Cons",
               "Surg:Surg+ATS")

ats.comps <- all.pairs %>%
  filter(comp %in% ats.comps)

ats.comps <- ats.comps %>% 
  mutate(TE = if_else(comp == "Surg:Surg+ATS", TE*-1, TE))  %>% 
  mutate(comp = "ATS")

tests <- bind_rows(st.comps, ats.comps)

tests <- metagen(
  TE,
  seTE,
  studlab, 
  data = tests, 
  sm = "RR", 
  comb.fixed = FALSE,
  comb.random = TRUE, 
  method.tau = "ML", 
  byvar = comp, 
  backtransf = FALSE)

##reproduce figure S25.1

pdf(file = "Figures/figure S25_1.pdf", height = 10, width = 10)

forest(
  tests,
  print.byvar = FALSE,
  overall = FALSE,
  subgroup = FALSE, 
  weight.study = "same", 
  hetstat = FALSE, 
  col.square = "black",
  col.inside = "black", 
  col.by = "black", 
  col.square.lines = "black",
  print.Q.subgroup = F,
  test.subgroup = F,
  print.subgroup.name = F) 

dev.off()

##assess inconsistency in naive NMA

##global inconsistency
decomp <- decomp.design(nma.naive) ##reproduce inconsistency values from
##main text and Table S16 (Q = 1.54, df = 5, p = 0.91)

##local inconsistency using node splitting

split <- netsplit(nma.naive)

##reproduce table S16.2

tables16 <- tibble(split$random)

tables16 <- tibble(
  comparison = tables16$comparison,
  k = split$k,
  prop = split$prop.random,
  NMA = exp(tables16$TE),
  direct = exp(split$direct.random$TE),
  indir = exp(split$indirect.random$TE),
  ror = direct/indir,
  pval = split$compare.random$p
)

tables16 %>%
  flextable() %>%
  save_as_docx(path = "Tables/table S16_2.docx")

##heat plot

##reproduce figure S22
dev.off()

pdf(file = "Figures/figure S22.pdf")

netheat(nma.naive)

dev.off()

##cnma

cnma <- netcomb(nma.naive, inactive = "Cons", sep.comps = "+")

##reproduce figure 5B

hold <- metagen(
  TE = summary(cnma)[["components.random"]]$TE,
  seTE = summary(cnma)[["components.random"]]$seTE,
  studlab = cnma$comps,
  sm = "RR"
)

pdf("Figures/figure 5B.pdf")

forest(hold,
       layout = "JAMA",
       col.square = "black",
       col.inside = "black",
       overall = FALSE,
       weight.study = "same",
       overall.hetstat = FALSE)

dev.off()

##reproduce table S4.2

rankd <- netrank(cnma)

rankd$ranking.random %>%
  as_tibble() %>% 
  cbind(comp = cnma$trts) %>%
  arrange(desc(value)) %>%
  mutate(value = signif(value, digits = 2)) %>%
  mutate(rank = 1:9) %>%
  relocate(rank, comp, value) %>%
  rename(Rank = rank, Treatment = comp, `P-Score` = value) %>%
  flextable() %>%
  save_as_docx(path = "Tables/table S4_2.docx")

##we then generate the league table of pairwise estimates with p-value shading
##Figure 6

logrr.cnma <- cnma$TE.random

logrr.cnma <- logrr.cnma %>% 
  as_tibble() %>% 
  mutate(newcol = colnames(logrr.cnma), .before = "ATS")

logrr.cnma <- logrr.cnma %>%
  pivot_longer(
    cols = c("ATS", "ATS+ST","Cons", 
             "ST", "Surg", "Surg+ATS", 
             "Surg+GRS", "Surg+ST", "Surg+TXA"),
    names_to = "comp",
    values_to = "val"
  )

sete.cnma <- cnma$seTE.random

sete.cnma <- sete.cnma %>%
  as_tibble() %>% 
  mutate(newcol = colnames(sete.cnma), .before = "ATS")

sete.cnma <- sete.cnma %>% 
  pivot_longer(
    cols = c("ATS", "ATS+ST","Cons", 
             "ST", "Surg", "Surg+ATS", 
             "Surg+GRS", "Surg+ST", "Surg+TXA"),
    names_to = "comp",
    values_to = "val"
  ) %>% rename(sete = val)

pval.cnma <- cnma$pval.random

pval.cnma <- pval.cnma %>%
  as_tibble() %>%
  mutate(newcol = colnames(pval.cnma), .before = "ATS")

pval.cnma <- pval.cnma %>% 
  pivot_longer(
    cols = c("ATS", "ATS+ST","Cons",
             "ST", "Surg", "Surg+ATS",
             "Surg+GRS", "Surg+ST", "Surg+TXA"),
    names_to = "comp",
    values_to = "val"
  ) %>% rename(pval = val)

leaguedat <- logrr.cnma %>% 
  add_column(sete = sete.cnma$sete) %>% 
  add_column(pval = pval.cnma$pval)

leaguedat <- leaguedat %>% 
  mutate(rr = exp(val)) %>% 
  mutate(cilb = exp(val-(1.96*sete))) %>%
  mutate(ciub = exp(val+(1.96*sete))) %>% 
  mutate(cilb = round(cilb, digits = 2)) %>%
  mutate(ciub = round(ciub, digits = 2)) %>% 
  mutate(rr = round(rr, digits = 2)) %>%
  mutate(rr.c = as.character(rr)) %>% 
  mutate(cilb.c = as.character(cilb)) %>% 
  mutate(ciub.c = as.character(ciub)) %>%
  mutate(expr = paste0(
    rr," (",cilb," - ",ciub,")"
  ))


##get rank data

rank <- netrank(cnma)

df.rank <- data.frame(
  rank$Pscore.random,
  treat = rownames(data.frame(rank$Pscore.random)))

df.rank <- df.rank %>% 
  as_tibble() %>% 
  rename(score = rank.Pscore.random) %>% 
  arrange(score) %>%
  arrange(score)

##treatment order

factor_surg_recur_x <- factor(
  leaguedat$comp, 
  levels = c("Surg+TXA", "Surg+ST", "Surg+GRS",
             "Surg+ATS", "Surg", "ATS+ST", "ST",
             "ATS", "Cons"))

factor_surg_recur_y <- factor(
  leaguedat$newcol, 
  levels = c("Surg+TXA", "Surg+ST", "Surg+GRS",
             "Surg+ATS", "Surg", "ATS+ST", "ST",
             "ATS", "Cons"))


##reproduce figure 6

league_med_recurrence <- ggplot(data = leaguedat) +
  geom_tile(aes(x = factor_surg_recur_x,
                y = fct_rev(factor_surg_recur_y),
                fill = pval)) + 
  scale_fill_continuous(low = "#87CEEB", 
                        high = "white", 
                        trans = "sqrt",
                        breaks = c(0.75, 0.5, 0.25, 0.05)) + 
  geom_text(aes(x = factor_surg_recur_x, 
                y = fct_rev(factor_surg_recur_y),
                label = expr)) + 
  theme_ggstatsplot() + 
  theme(panel.grid = element_line(colour = "black"), 
        axis.ticks = element_blank())

pdf(file = "Figures/figure 6.pdf", height = 12, width = 20)

league_med_recurrence

dev.off()

##sensitivity analyses - interactions

##pvalues are calculated in same manner as for surgical CNMAs

##interaction between surgery and steroids

c.surgdex <- cbind(cnma$C.matrix,
                   surg.dex = cnma$C.matrix[, "Surg"] * cnma$C.matrix[, "ST"])

cnma.surgdex <- netcomb(nma.naive, inactive = "Cons", C.matrix = c.surgdex)

##additive CNMA - Q = 25.44, df = 22, p = 0.2766 (can check using print(cnma))

##p for difference from additive cnma
1-pchisq((cnma$Q.additive-cnma.surgdex$Q.additive),
         df = (cnma$df.Q.additive-cnma.surgdex$df.Q.additive))

##interaction between surgery and atorvastatin

c.surgats <- cbind(cnma$C.matrix, 
                   surg.ats = cnma$C.matrix[, "Surg"] * cnma$C.matrix[, "ATS"])

cnma.surgats <- netcomb(nma.naive, inactive = "Cons", C.matrix = c.surgats)

1-pchisq((cnma$Q.additive-cnma.surgats$Q.additive),
         df = (cnma$df.Q.additive-cnma.surgats$df.Q.additive))

##reproduce table S23

tables23.1 <- cbind(
  cnma$comps,
  tibble(summary(cnma)$components.random)
) %>% as_tibble() %>%
  mutate(TE = exp(TE)) %>%
  mutate(lower = exp(lower)) %>%
  mutate(upper = exp(upper)) %>%
  mutate(ci = paste0("[", round(lower,4), "; ", round(upper, 4), "]")) %>%
  rename(comp = `cnma$comps`) %>%
  select(comp, TE, ci, statistic, p)

tables23.2 <- cbind(
  cnma.surgdex$comps,
  tibble(summary(cnma.surgdex)$components.random)
) %>% as_tibble() %>%
  mutate(TE = exp(TE)) %>%
  mutate(lower = exp(lower)) %>%
  mutate(upper = exp(upper)) %>%
  mutate(ci = paste0("[", round(lower,4), "; ", round(upper, 4), "]")) %>%
  rename(comp = `cnma.surgdex$comps`) %>%
  select(comp, TE, ci, statistic, p)

tables23.3 <- cbind(
  cnma.surgats$comps,
  tibble(summary(cnma.surgats)$components.random)
) %>% as_tibble() %>%
  mutate(TE = exp(TE)) %>%
  mutate(lower = exp(lower)) %>%
  mutate(upper = exp(upper)) %>%
  mutate(ci = paste0("[", round(lower,4), "; ", round(upper, 4), "]")) %>%
  rename(comp = `cnma.surgats$comps`) %>%
  select(comp, TE, ci, statistic, p)

flextable(tables23.1) %>%
  save_as_docx(path = "Tables/table s23_1.docx",
               pr_section = sec_portrait)

flextable(tables23.2) %>%
  save_as_docx(path = "Tables/table s23_2.docx",
               pr_section = sec_portrait)

flextable(tables23.3) %>%
  save_as_docx(path = "Tables/table s23_3.docx",
               pr_section = sec_portrait)

##network plot

pts.trts <- nma.dat %>%
  filter(studlab %in% nma.naive$studlab) %>%
  group_by(treat) %>%
  summarise(pts = sum(n))


##reproduce figure 5A

pdf(file = "Figures/figure 5A.pdf")

netgraph(
  cnma,
  plastic = FALSE,
  lwd = 8, 
  thickness = "number.of.studies", 
  points = TRUE,
  cex.points = cnma$k.trts, dim = "2d")

dev.off()


##comparison-adjusted funnel plot

##reproduce figure S15.2

pdf(file = "Figures/figure S15_2.pdf")

funnel(
  nma.naive,
  order = c("Surg+TXA",
            "Surg+ST",
            "Surg+GRS",
            "Surg+ATS",
            "Surg",
            "ATS+ST",
            "ST",
            "ATS",
            "Cons"),
  linreg = T,
  pch = 19,
  col = miscpalettes::mschart$pastel[1:11]
)

dev.off()

##contour enhanced funnel for surgery with medical therapy versus
##surgery alone

treats <- c("surgery_alone",
            "surgery_placebo",
            "surgery_steroid",
            "surgery_atorvastatin",
            "surgery_TXA",
            "surgery_goreisan"
)

embolization.temp <- recurrence_data %>% filter(treat %in% treats)

embolization.temp$treat <- embolization.temp$treat %>% 
  str_replace_all("surgery_alone", "control") %>%
  str_replace_all("surgery_placebo", "control") %>%
  str_replace_all("surgery_steroid", "expr") %>%
  str_replace_all("surgery_atorvastatin", "expr") %>%
  str_replace_all("surgery_TXA", "expr") %>%
  str_replace_all("surgery_goreisan", "expr")

embolization.temp <- subset(
  embolization.temp, duplicated(
    embolization.temp$studlab) | duplicated(
      embolization.temp$studlab, fromLast = TRUE)) %>% 
  arrange(treat) %>% 
  arrange(studlab)

embolization.temp <- embolization.temp %>%
  arrange(treat) %>% 
  arrange(studlab)

embolization.temp[37,1] <- "Yamada 2020-2"


embolization.temp <- embolization.temp %>% add_row(
  studlab = "Yamada 2020-2",
  treat = "control",
  event = 8,
  n = 82,
  design = "non-randomised"
)


embolization.temp <- embolization.temp %>% 
  arrange(treat) %>% 
  arrange(studlab)

embolization.temp <- pairwise(
  data = embolization.temp,
  treat = treat, 
  event = event,
  n = n, 
  studlab = studlab, 
  sm = "RR")


bin.r <- metabin(
  event.e = event1, 
  n.e = n1, 
  event.c = event2,
  n.c = n2,
  studlab = studlab, 
  sm = "RR", 
  comb.fixed = F,
  comb.random = T, 
  method = "MH", 
  data = embolization.temp)

##reproduce figure s16.2

pdf(file = "Figures/figure S16_2.pdf")

funnel(bin.r, comb.fixed = F,
       level = 0.95, contour = c(0.9, 0.95, 0.99),
       col.contour = c("darkgray", "gray", "lightgray"),
       lwd = 2, cex = 2, pch = 16, studlab = F, cex.studlab = 1.25)
legend(11, 0.05,
       c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
       fill = c("darkgray", "gray", "lightgray"))

dev.off()






