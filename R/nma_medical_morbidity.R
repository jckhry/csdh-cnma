##medical CNMA - secondary outcome

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

cols <- c("#32CD32", "#4682B4", "#4682B4", "#4682B4", "#4682B4", "black")

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

nma.dat <- complications_data %>% filter(treat %in% treats)

##select multi-arm studies
nma.dat <- subset(
  nma.dat, duplicated(
    nma.dat$studlab) | duplicated(
      nma.dat$studlab, fromLast = TRUE))

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

nma.dat$treat <- nma.dat$treat %>%
  str_replace_all("\\+Plac", "")

nma.dat$treat <- nma.dat$treat %>% 
  str_replace_all("Plac", "Cons")

nma.dat

nma.dat %>% count(treat)

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

pairs.med.comps <- nma.pairs %>%
  select(studlab, design, treat1, treat2, TE, seTE) %>%
  rename(es.comp = TE, se.comp = seTE)

pairs.med.comps$design <- pairs.med.comps$design %>% 
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

pairs.med.comps <- pairs.med.comps %>% left_join(chars_temp)

random.all <- rob_random %>% 
  select(Study, Overall) %>% 
  rename(studlab = Study, bias = Overall)

nrs.all <- rob_nrs %>%
  select(Study, Overall) %>%
  rename(studlab = Study, bias = Overall)

bias <- bind_rows(random.all, nrs.all)

pairs.med.comps <- pairs.med.comps %>%
  left_join(bias, by = "studlab") %>%
  arrange(studlab) %>%
  mutate(es.comp = round(es.comp, digits = 3),
         se.comp = round(se.comp, digits = 3))

##reproduce remainder of table S6

pairs.med.comps %>%
  flextable() %>%
  save_as_docx(path = "Tables/table S6_2.docx",
               pr_section = sec)

##naive NMA

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

##assess impact of NRS - design-adjusted analyses

##separate input data

nma.dat.random <- nma.pairs %>% filter(design == "randomised")
nma.dat.nrs <- nma.pairs %>% filter(design == "non-randomised")


netcon <- netconnection(
  treat1 = treat1,
  treat2 = treat2,
  studlab = studlab, 
  data = nma.dat.random)

##reproduce figure S20.1

pdf(file = "Figures/figure S20_1.pdf")

netgraph(netcon,
                       points = T, 
                       col.points = "grey60", 
                       cex.points = 3.5)

dev.off()


##subnetwork 1 (surg, surg+st, surg+grs, surg+txa)

##no analysis of black subnetwork because all studies were randomized in this 
##subnetwork - for more information see Supplemental Digital Content Figure S20

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

##design-adjusted analysis linking network using Miah 2020

##define nrs weights

weights <- c(0.2,0.4,0.6,0.8)

adjusted.analyses4 <- list()

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

##reproduce figure S20.2

pdf(file = "Figures/figure S20_2.pdf")

forest(design.adjusted4, col.by = "black")

dev.off()

##design adjusted analysis linking network using Miah 2020

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

##reproduce figure S20.3

pdf(file = "Figures/figure S20_3.pdf", height = 20, width = 8)

forest(design.adjusted6, col.by = "black")

dev.off()

##compare effect modifiers across treatments

mods_med_treat <- read_excel("Data/mods_med_treat.xlsx")

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

##reproduce table S21.1

tbl_mods_treat %>%
  as_flex_table() %>%
  save_as_docx(path = "Tables/table S21_1.docx",
               pr_section = sec)

##compare effect modifiers across components

mods_med_comp <- read_excel("Data/mods_med_comp.xlsx")

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

##reproduce table S21.2

tbl_mods_comp %>%
  as_flex_table() %>%
  save_as_docx(path = "Tables/table S21_2.docx",
               pr_section = sec)

##assess additivity as previous

all.pairs <- netpairwise(
  nma.naive,
  comb.fixed = FALSE,
  comb.random = TRUE, 
  backtransf = FALSE)

all.pairs

all.pairs <- tibble(
  studlab = all.pairs$studlab,
  TE = all.pairs$TE, 
  seTE = all.pairs$seTE, 
  comp = all.pairs$byvar)

all.pairs %>% count(comp)

st.comps <- c("Cons:ST",
              "Surg:Surg+ST")

st.comps <- all.pairs %>% 
  filter(comp %in% st.comps) %>%
  mutate(TE = TE*-1) %>% 
  mutate(comp = "ST")

ats.comps <- c("Cons:ATS",
               "Surg:Surg+ATS")

ats.comps <- all.pairs %>% 
  filter(comp %in% ats.comps)

ats.comps <- ats.comps %>% 
  mutate(TE = TE*-1)  %>% mutate(comp = "ATS")

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

##reproduce Figure S27.1

pdf(file = "Figures/figure S27_1.pdf")

forest(
  tests,
  print.byvar = FALSE, 
  overall = FALSE, 
  subgroup = FALSE,
  weight.study = "same", 
  hetstat = F, 
  test.subgroup = F, 
  col.by = "black", 
  col.square = "black",
  col.inside = "black",
  print.subgroup.name = F)

dev.off()

##assess inconsistency

##unable to assess because there were no closed loops

split <- netsplit(nma.naive)

##reproduce table S14

tables14 <- tibble(split$random)

tables14 <- tibble(
  comparison = tables14$comparison,
  k = split$k,
  prop = split$prop.random,
  NMA = exp(tables14$TE),
  direct = exp(split$direct.random$TE),
  indir = exp(split$indirect.random$TE),
  ror = direct/indir,
  pval = split$compare.random$p
)

tables14 %>%
  flextable() %>%
  save_as_docx(path = "Tables/table S14.docx")


##CNMA

cnma <- netcomb(nma.naive, inactive = "Cons", sep.comps = "+")

##reproduce figure 8B

hold <- metagen(
  TE = summary(cnma)[["components.random"]]$TE,
  seTE = summary(cnma)[["components.random"]]$seTE,
  studlab = cnma$comps,
  sm = "RR"
)

pdf("Figures/figure 8B.pdf")

forest(hold,
       layout = "JAMA",
       col.square = "black",
       col.inside = "black",
       overall = FALSE,
       weight.study = "same",
       overall.hetstat = FALSE)

dev.off()

##reproduce table S4.4

rankd <- netrank(cnma)

rankd$ranking.random %>%
  as_tibble() %>% 
  cbind(comp = cnma$trts) %>%
  arrange(desc(value)) %>%
  mutate(value = signif(value, digits = 2)) %>%
  mutate(rank = 1:8) %>%
  relocate(rank, comp, value) %>%
  rename(Rank = rank, Treatment = comp, `P-Score` = value) %>%
  flextable() %>%
  save_as_docx(path = "Tables/table S4_4.docx")

##league table plot with p-value shading - Figure S11


logrr.cnma <- cnma$TE.random

logrr.cnma <- logrr.cnma %>% 
  as_tibble() %>% 
  mutate(newcol = colnames(logrr.cnma), .before = "ATS")

logrr.cnma <- logrr.cnma %>%
  pivot_longer(
    cols = c("ATS","Cons", "ST",
             "Surg", "Surg+ATS", "Surg+GRS", 
             "Surg+ST", "Surg+TXA"),
    names_to = "comp",
    values_to = "val"
  )

sete.cnma <- cnma$seTE.random

sete.cnma <- sete.cnma %>% 
  as_tibble() %>% 
  mutate(newcol = colnames(sete.cnma), .before = "ATS")

sete.cnma <- sete.cnma %>% 
  pivot_longer(
    cols = c("ATS","Cons", "ST",
             "Surg", "Surg+ATS", "Surg+GRS",
             "Surg+ST", "Surg+TXA"),
    names_to = "comp",
    values_to = "val"
  ) %>% rename(sete = val)

pval.cnma <- cnma$pval.random

pval.cnma <- pval.cnma %>% 
  as_tibble() %>% 
  mutate(newcol = colnames(pval.cnma), .before = "ATS")

pval.cnma <- pval.cnma %>% 
  pivot_longer(
    cols = c("ATS","Cons", "ST", 
             "Surg", "Surg+ATS", "Surg+GRS",
             "Surg+ST", "Surg+TXA"),
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
  arrange(score) %>% arrange(score)

##treatment order

factor_surg_recur_x <- factor(
  leaguedat$comp, levels = c("Surg", "Cons", "Surg+GRS",
                             "Surg+ST", "Surg+ATS", "ATS",
                             "ST","Surg+TXA"))

factor_surg_recur_y <- factor(
  leaguedat$newcol, 
  levels = c("Surg", "Cons", "Surg+GRS",
             "Surg+ST", "Surg+ATS", "ATS",
             "ST","Surg+TXA"))


##create league table plot

league_med_complications <- ggplot(data = leaguedat) + 
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

##reproduce Figure S11

pdf(file = "Figures/figure S11.pdf", height = 12, width  =20)

league_med_complications

dev.off()


##sensitivity analysis - imputation of Dex-CSDH adverse events

##assess effect of hutchinson 2020 exclusion

## serious adverse events in 60/375 in dxm group

## serious adverse events in 24/373 in placebo

## AESI in 41/375 in dxm group

## AESI in 12/373 in placebo

##n surgery+plac = 350

##n surgery+st = 349

##n dxm alone = 22

##n plac alone = 16

##n surgical infections in dxm group = 

prop.ae.dxm = 60/375

prop.ae.plac = 24/373

prop.aesi.dxm = 41/375

prop.aesi.plac = 12/373

##scenario 1 - all patients with sae were in conservatively managed group

hutch.dat <- tibble(
  scenario = c("s1", "s1"),
  studlab = "Hutchinson 2020",
  treat = c("Surg+ST", "Surg"),
  event = c((60-22), (24-16)),
  n = c(349, 350)
)

##scenario 2 - no patients with sae were in conservatively managed group

hutch.dat <- hutch.dat %>% add_row(scenario = c("s2", "s2"),
                                   studlab = "Hutchinson 2020",
                                   treat = c("Surg+ST", "Surg"),
                                   event = c((60), (24)),
                                   n = c(349, 350)
)


##scenario 3 - adverse events are evenly distributed 
##between patients with vs without surgery

hutch.dat <- hutch.dat %>% add_row(scenario = c("s3", "s3"),
                                   studlab = "Hutchinson 2020",
                                   treat = c("Surg+ST", "Surg"),
                                   event = c(60*(349/375), 24*(350/373)),
                                   n = c(349, 350)
)

hutch.dat

##scenario 4 - serious adverse events distributed
##relative to surgical infections (2:1 ratio)

ae.total = 60+24
ae.dex = (84/3)*2
ae.plac = 84 - ae.dex

hutch.dat <- hutch.dat %>% add_row(scenario = c("s4", "s4"),
                                   studlab = "Hutchinson 2020",
                                   treat = c("Surg+ST", "Surg"),
                                   event = c(ae.dex, ae.plac),
                                   n = c(349, 350)
)

##scenario 5 - all patients experiencing adverse 
##events in the dexamethasone group received surgery
## but no patients experiencing adverse events 
##in the placebo group received surgery

hutch.dat <- hutch.dat %>% add_row(scenario = c("s5", "s5"),
                                   studlab = "Hutchinson 2020",
                                   treat = c("Surg+ST", "Surg"),
                                   event = c(60, 24-16),
                                   n = c(349,350)
)

##scenario 6 - all patients experiencing adverse events in 
##the dexamethasone group received surgery
## but no patients experiencing adverse events in the 
##placebo group received surgery

hutch.dat <- hutch.dat %>% add_row(scenario = c("s6", "s6"),
                                   studlab = "Hutchinson 2020",
                                   treat = c("Surg+ST", "Surg"),
                                   event = c((60-22), 24),
                                   n = c(349,350)
)

hutch.out <- pairwise(data = hutch.dat, 
                      treat = treat, 
                      event = event, 
                      n = n, 
                      studlab = scenario, 
                      sm = "RR") %>%
  as_tibble() %>% select(scenario, treat1, treat2, TE, seTE)

##reproduce table S10

hutch.out %>%
  flextable() %>%
  save_as_docx(path = "Tables/table S10.docx",
               pr_section = sec)

hutch.sens <- list()

for(i in hutch.dat$scenario) {
  
  dat <- hutch.dat %>% filter(scenario == i)
  
  lab.nma <- paste("nma.", i)
  lab.cnma <- paste("cnma.", i)
  
  dat <- bind_rows(dat, nma.dat)
  
  dat <- pairwise(
    treat = treat,
    event = event,
    n = n,
    studlab = studlab, 
    sm = "RR",
    data = dat)
  
  nma <- netmeta(
    TE, 
    seTE,
    treat1,
    treat2,
    studlab,
    data = dat, 
    sm = "RR", 
    comb.fixed = FALSE, 
    comb.random = TRUE,
    method.tau = "ML")
  
  cnma <- netcomb(nma,
                  sep.comps = "+", 
                  inactive = "Cons")
  
  hutch.sens[[lab.nma]] <- nma
  hutch.sens[[lab.cnma]] <- cnma
  
}

hutch.comps <- tibble(
  scenario = "Primary analysis",
  studlab = cnma$comps,
  TE = cnma$Comp.random,
  seTE = cnma$seComp.random
)

hutch.comps

hutch.comps <- hutch.comps %>% 
  
  add_row(scenario = "Scenario 1",
          studlab = hutch.sens[["cnma. s1"]][["comps"]],
          TE = hutch.sens[["cnma. s1"]][["Comp.random"]],
          seTE = hutch.sens[["cnma. s1"]][["seComp.random"]]
  ) %>%
  
  
  add_row(scenario = "Scenario 2",
          studlab = hutch.sens[["cnma. s2"]][["comps"]],
          TE = hutch.sens[["cnma. s2"]][["Comp.random"]],
          seTE = hutch.sens[["cnma. s2"]][["seComp.random"]]) %>%
  
  add_row(scenario = "Scenario 3",
          studlab = hutch.sens[["cnma. s3"]][["comps"]],
          TE = hutch.sens[["cnma. s3"]][["Comp.random"]],
          seTE = hutch.sens[["cnma. s3"]][["seComp.random"]]) %>%
  
  add_row(scenario = "Scenario 4",
          studlab = hutch.sens[["cnma. s4"]][["comps"]],
          TE = hutch.sens[["cnma. s4"]][["Comp.random"]],
          seTE = hutch.sens[["cnma. s4"]][["seComp.random"]]) %>%
  
  add_row(scenario = "Scenario 5",
          studlab = hutch.sens[["cnma. s5"]][["comps"]],
          TE = hutch.sens[["cnma. s5"]][["Comp.random"]],
          seTE = hutch.sens[["cnma. s5"]][["seComp.random"]]) %>%
  
  add_row(scenario = "Scenario 6",
          studlab = hutch.sens[["cnma. s6"]][["comps"]],
          TE = hutch.sens[["cnma. s6"]][["Comp.random"]],
          seTE = hutch.sens[["cnma. s6"]][["seComp.random"]])


hutch.comps

hutch.m <- metagen(TE,
                   seTE, 
                   studlab, 
                   data = hutch.comps,
                   byvar = scenario,
                   sm = "RR")

##reproduce Figure S13

pdf(file = "Figures/figure S13.pdf", height = 15, width = 8)

forest(hutch.m,
       overall = FALSE, 
       subgroup = FALSE,
       weight.study = "same",
       hetstat = F, 
       test.subgroup = F,
       print.byvar = F, 
       col.by = "black",
       col.square = "black",
       col.inside = "black", 
       leftlabs = c("Component", "logRR", "SE(logRR)"),
       print.subgroup.name = F)

dev.off()

##network plot - Figure 8A

pts.trts <- nma.dat %>%
  filter(studlab %in% nma.naive$studlab) %>%
  group_by(treat) %>%
  summarise(pts = sum(n))

##reproduce Figure 8A

pdf(file = "Figures/figure 8A.pdf")

netgraph(
  cnma, 
  plastic = FALSE,
  lwd = 10, 
  thickness = "number.of.studies",
  points = TRUE, 
  cex.points = cnma$k.trts*2, 
  dim = "2d")

dev.off()

##comparison-adjusted funnel plot - Figure S15.4

##reproduce Figure S15.4

pdf(file = "Figures/figure S15_4.pdf")

funnel(
  nma.naive,
  order = c("Surg",
            "Cons",
            "Surg+GRS",
            "Surg+ATS",
            "Surg+ST",
            "ATS",
            "ST",
            "Surg+TXA"),
  linreg = F,
  pch = 19,
  col = miscpalettes::mschart$pastel[1:7]
)

dev.off()


