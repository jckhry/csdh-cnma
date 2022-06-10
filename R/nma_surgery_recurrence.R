##surgical CNMA for primary outcome

source("R/import.R")

##page layouts

sec_portrait <- prop_section(
  page_size = page_size(width = 21/2.54, height = 29.7/2.54, orient = "portrait"),
  page_margins = page_mar(top = 1.27, bottom = 1.27, left = 1.27, right = 1.27)
)

sec <- prop_section(
  page_size = page_size(width = 21/2.54, height = 29.7/2.54, orient = "landscape"),
  page_margins = page_mar(top = 1.27, bottom = 1.27, left = 1.27, right = 1.27)
)

##primary outcome - recurrence

##define treatments in our analysis which are eligible for the NMA

##
treats <- c(
  "burrhole_drain",
  "burrhole_nodrain",
  "endoscopy_drain",
  "endoscopy_nodrain",
  "twist_drain",
  "twist_nodrain",
  "craniotomy_drain",
  "craniotomy_nodrain",
  "SEPS",
  "YL1",
  "embolization",
  "craniotomy_drain_MMA",
  "SEPS_MMA"
)

##find multi-arm studies with these treatments

nma.dat <- recurrence_data %>% filter(treat %in% treats)
##select multi-arm studies

nma.dat <- subset(
  nma.dat, duplicated(
    nma.dat$studlab) | duplicated(
      nma.dat$studlab, fromLast = TRUE))

##clean up treatment names/labels to facilitate cNMA

nma.dat$treat <- nma.dat$treat %>% 
  str_replace_all("_MMA", "+MMA") %>%
  str_replace_all("_drain", "+D") %>% 
  str_replace_all("_nodrain", "") %>% 
  str_replace_all("embolization", "MMA") %>%
  str_replace_all("burrhole", "BH") %>% 
  str_replace_all("endoscopy", "End") %>%
  str_replace_all("twist", "TD") %>%
  str_replace_all("craniotomy", "Cr")


##list treatments for NMA

unique(nma.dat$treat)

##transform to contrast-based format for inclusion in netmeta and calculate effect sizes

nma.dat <- nma.dat %>% arrange(treat)

nma.pairs <- pairwise(
  treat = treat,
  event = event,
  n = n,
  studlab = studlab,
  data = nma.dat,
  sm = 'RR'
)

nma.pairs <- as_tibble(nma.pairs)

##characteristics of studies included in the surgical CNMA of recurrence
pairs.surgery.recur <- nma.pairs %>%
  select(studlab, design, treat1, treat2, TE, seTE) %>%
  rename(es.recur = TE, se.recur = seTE)

pairs.surgery.recur$design <- pairs.surgery.recur$design %>%
  str_replace_all(pattern = "non-randomised", replacement = "NRS") %>%
  str_replace_all(pattern = "randomised", replacement = "Randomised")

chars_temp <- table_chars_temp %>% 
  select(studlab, n, `F/U`, M, F, BL, `Age (yrs.)`, Location, Defn.) %>% 
  rename(followup = `F/U`, m = M, f = F, bl = BL, age = `Age (yrs.)`,
         defn = Defn., location = Location)

pairs.surgery.recur <- pairs.surgery.recur %>%
  left_join(chars_temp)

random.all <- rob_random %>% 
  select(Study, Overall) %>% 
  rename(studlab = Study, bias = Overall)

nrs.all <- rob_nrs %>% 
  select(Study, Overall) %>%
  rename(studlab = Study, bias = Overall)

bias <- bind_rows(random.all, nrs.all)

pairs.surgery.recur <- pairs.surgery.recur %>%
  left_join(bias, by = "studlab") %>% 
  mutate(es.recur = round(es.recur, digits = 3),
         se.recur = round(se.recur, digits = 3)) %>%
  arrange(studlab)

pairs.surgery.recur ##forms part of Table S3

pairs.surgery.recur %>%
  flextable() %>%
  save_as_docx(path = "Tables/table S3_1.docx",
               pr_section = sec)

##naive NMA (NRS have full weight)

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

##design-adjusted analyses

##separate input data

nma.dat.random <- nma.pairs %>% filter(design == "randomised")
nma.dat.nrs <- nma.pairs %>% filter(design == "non-randomised")

##first, fit NMA on randomised studies only

nma.random <- netmeta(
  TE,
  seTE,
  treat1,
  treat2,
  studlab,
  data = nma.dat.random,
  sm = "RR",
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "ML"
)

##define weights for nrs

weights = c(0.2, 0.4, 0.6, 0.8)

##fit NMA for all values of weights


adjusted.analyses <- list()

for(i in weights) {
  
  ##first, divide study standard errors by sqrt(weight)
  
  
  ##this will have the effect of variance/weight when netmeta() calculates V as seTE^2
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

##set colours

cols <- c("#32CD32", "#4682B4", "#4682B4", "#4682B4", "#4682B4", "black")

##create forest plot to compare findings

design.adjusted <- netbind(
  nma.random,
  adjusted.analyses$w0.2,
  adjusted.analyses$w0.4,
  adjusted.analyses$w0.6,
  adjusted.analyses$w0.8,
  nma.naive,
  name = c("w = 0 (randomised studies only)",
           "w = 0.2",
           "w = 0.4",
           "w = 0.6",
           "w = 0.8",
           "w = 1 (naive NMA)"),
  col.square = cols,
  col.square.lines = cols,
  col.inside = cols,
  col.study = cols
)

##reproduce figure S17

pdf("Figures/figure S17.pdf")

forest(
  design.adjusted, 
  col.by = "black", 
  subset.treatments = c("BH+D", "TD+D"))

dev.off()

##assess inconsistency

##design by treatment test

decomp <- decomp.design(nma.naive)

decomp ##reproduces global inconsistency findings from main text 
##and table S15.1 (Q = 1.36, df = 3, p = 0.71)

##node splitting

split <- netsplit(nma.naive)

split

##reproduce table S15.2

tables15 <- tibble(split$random)

tables15 <- tibble(
  comparison = tables15$comparison,
  k = split$k,
  prop = split$prop.random,
  NMA = exp(tables15$TE),
  direct = exp(split$direct.random$TE),
  indir = exp(split$indirect.random$TE),
  ror = direct/indir,
  pval = split$compare.random$p
)


flextable(tables15) %>% save_as_docx(path = "Tables/table s15_2.docx",
                                     pr_section = sec_portrait)

##reproduce heat plot for this analysis (Figure S21)

pdf(file = "Figures/figure S21.pdf")

netheat(nma.naive)

dev.off()

##compare effect modifiers across treatments

mods_surg_treat <- mods_surg_treat %>%
  rename(treat = treatment) %>% 
  filter(studlab %in% unique(nma.naive$studlab))

mods_surg_treat <- mods_surg_treat %>%
  left_join(table_chars_temp, by = "studlab") %>% 
  select(studlab, treat, age, bilat, male, Design, Defn.) %>%
  replace_with_na(replace = list(Defn. =  "NR")) %>%
  rename(design = Design, defn = Defn.)

mods_surg_treat$design <- mods_surg_treat$design %>%
  str_replace_all("R-NRS", "Non-randomised") %>%
  str_replace_all("P-NRS", "Non-randomised")

tbl_mods_treat <- mods_surg_treat %>%
  select(-studlab) %>%
  tbl_summary(by = treat, missing = "no")

##reproduce table S18.1

tbl_mods_treat %>%
  as_flex_table() %>%
  save_as_docx(path = "Tables/table S18_1.docx",
               pr_section = sec)


##fit CNMA

cnma <- netcomb(nma.naive, sep.comps = "+")

##reproduce figure 3B

hold <- metagen(
  TE = summary(cnma)[["components.random"]]$TE,
  seTE = summary(cnma)[["components.random"]]$seTE,
  studlab = cnma$comps,
  sm = "RR"
)

pdf("Figures/figure 3b.pdf")

forest(hold,
       layout = "JAMA",
       col.square = "black",
       col.inside = "black",
       overall = FALSE,
       weight.study = "same",
       overall.hetstat = FALSE)
       
dev.off()

##reproduce table S4.1

rankd <- netrank(cnma)

rankd$ranking.random %>%
  as_tibble() %>% 
  cbind(comp = cnma$trts) %>%
  arrange(desc(value)) %>%
  mutate(value = signif(value, digits = 2)) %>%
  mutate(rank = 1:11) %>%
  relocate(rank, comp, value) %>%
  rename(Rank = rank, Treatment = comp, `P-Score` = value) %>%
  flextable() %>%
  save_as_docx(path = "Tables/table S4_1.docx")
  

##assess additivity by comparing study effects expected to estimate
##the same component

all.pairs <- netpairwise(
  nma.naive,
  comb.fixed = FALSE,
  comb.random = TRUE, 
  backtransf = FALSE,
  reference.group = "BH+D")

all.pairs

all.pairs <- tibble(
  studlab = all.pairs$studlab,
  TE = all.pairs$TE, 
  seTE = all.pairs$seTE,
  comp = all.pairs$byvar)

all.pairs

##show number of studies for each component
all.pairs %>% count(comp)

##comparisons expected to estimate effect of drains
comps.d <- c("BH:BH+D", "Cr:Cr+D")

drain.comps <- all.pairs %>% 
  filter(comp %in% comps.d)

drain.comps$TE <- drain.comps$TE*(-1)

drain.comps <- drain.comps %>% 
  mutate(comp = "Drain")

mmasurg.t <- c("surgery_MMA", "surgery_alone")

mma.surg <- recurrence_data %>% 
  filter(treat %in% mmasurg.t)

mma.surg <- subset(
  mma.surg, duplicated(
    mma.surg$studlab) | duplicated(
      mma.surg$studlab, fromLast = TRUE))

mma.surg <- pairwise(
  treat = treat,
  event = event,
  n = n, 
  studlab = studlab,
  sm = "RR",
  data = mma.surg)

mma.surg <- mma.surg %>% 
  as_tibble() %>% 
  select(studlab, TE, seTE) %>% 
  mutate(comp = "MMA") %>%
  mutate(TE = TE*(-1))

mmacons.t <- c("embolization", "conservative")

mma.cons <- recurrence_data %>%
  filter(treat %in% mmacons.t)

mma.cons <- subset(
  mma.cons, duplicated(
    mma.cons$studlab) | duplicated(
      mma.cons$studlab, fromLast = T))

mma.cons <- pairwise(
  treat = treat, 
  event = event, 
  n = n, 
  studlab = studlab,
  sm = "RR",
  data = mma.cons)

mma.cons <- mma.cons %>%
  as_tibble() %>%
  select(studlab, TE, seTE) %>%
  mutate(comp = "MMA") %>% 
  mutate(TE = TE*(-1))

mma.surg <- mma.surg %>%
  bind_rows(mma.cons)

fdat <- drain.comps %>% 
  bind_rows(mma.surg)

add.meta <- metagen(TE,
                    seTE,
                    studlab,
                    data = fdat, 
                    byvar = comp,
                    sm = "RR", 
                    backtransf = F, 
                    comb.fixed = F,
                    comb.random = T)

##reproduce Figure S24

pdf("Figures/figure S24_1.pdf", width = 10, height = 10)

forest(add.meta, 
       overall = F, 
       hetstat = F,
       col.square = "black",
       weight.study = "same", 
       print.byvar = F, 
       col.by = "black", 
       subgroup = F,
       test.subgroup = F,
       print.subgroup.name = F)

dev.off()

##create league table with shading by pval (Figure 4)

logrr.cnma <- cnma$TE.random

logrr.cnma <- logrr.cnma %>% 
  as_tibble() %>% 
  mutate(newcol = colnames(logrr.cnma), .before = "BH")

logrr.cnma <- logrr.cnma %>%
  pivot_longer(
    cols = c("BH", "BH+D", "Cr", 
             "Cr+D", "Cr+D+MMA", "End+D", 
             "MMA", "SEPS", "SEPS+MMA", 
             "TD+D", "YL1"),
    names_to = "comp",
    values_to = "val"
  )

sete.cnma <- cnma$seTE.random

sete.cnma <- sete.cnma %>%
  as_tibble() %>% 
  mutate(newcol = colnames(sete.cnma), .before = "BH")

sete.cnma <- sete.cnma %>% 
  pivot_longer(
    cols = c("BH", "BH+D", "Cr",
             "Cr+D", "Cr+D+MMA", "End+D",
             "MMA", "SEPS", "SEPS+MMA",
             "TD+D", "YL1"),
    names_to = "comp",
    values_to = "val"
  ) %>% rename(sete = val)

pval.cnma <- cnma$pval.random

pval.cnma <- pval.cnma %>% 
  as_tibble() %>%
  mutate(newcol = colnames(pval.cnma), .before = "BH")

pval.cnma <- pval.cnma %>% 
  pivot_longer(
    cols = c("BH", "BH+D", "Cr", 
             "Cr+D", "Cr+D+MMA",
             "End+D", "MMA", "SEPS", 
             "SEPS+MMA", "TD+D", "YL1"),
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
  levels = c("MMA", "Cr+D+MMA", "End+D",
             "SEPS+MMA","YL1", "BH+D", "Cr+D", 
             "TD+D", "SEPS","BH", "Cr"))

factor_surg_recur_y <- factor(
  leaguedat$newcol,
  levels = c("MMA", "Cr+D+MMA", "End+D", "SEPS+MMA",
             "YL1", "BH+D", "Cr+D", "TD+D", "SEPS",
             "BH", "Cr"))


##create league table plot

league_surg_recur <- ggplot(data = leaguedat) +
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

pdf(file = "Figures/figure 4.pdf", width = 20, height = 12)

league_surg_recur

dev.off()

##compare effect modifiers across components

mods_surg_comp <- mods_surg_comp %>%
  rename(treat = component) %>% 
  filter(studlab %in% unique(nma.naive$studlab))

mods_surg_comp <- mods_surg_comp %>% 
  left_join(table_chars_temp, by = "studlab") %>%
  select(studlab, treat, age, bilat, male, Design, Defn.) %>%
  replace_with_na(replace = list(Defn. =  "NR")) %>% 
  rename(design = Design, defn = Defn.)

mods_surg_comp$design <- mods_surg_comp$design %>% 
  str_replace_all("R-NRS", "Non-randomised") %>%
  str_replace_all("P-NRS", "Non-randomised")

tbl_mods_comp <- mods_surg_comp %>% 
  select(-studlab) %>% 
  tbl_summary(by = treat, missing = "no")

##reproduce table S18.2

tbl_mods_comp %>%
  as_flex_table() %>%
  save_as_docx(path = "Tables/table S18_2.docx",
               pr_section = sec)

##network plot, nodes by n patients, edges by n studies

pts.trts <- nma.dat %>% 
  filter(studlab %in% nma.naive$studlab) %>%
  group_by(treat) %>% 
  summarise(pts = sum(n))

##reproduce figure 3A

pdf("Figures/figure 3A.pdf")

netgraph(cnma,
                 plastic = FALSE,
                 lwd = 7, 
                 thickness = "number.of.studies",
                 points = TRUE, 
                 cex.points = cnma$k.trts/2.5, dim = "2d")

dev.off()

##sensitivity analysis - interaction terms

##CNMA including interaction between SEPS and embolization
c.sepsmma <- cbind(cnma$C.matrix, 
                   seps.mma = cnma$C.matrix[, "SEPS"] * cnma$C.matrix[, "MMA"])

cnma.sepsmma <- netcomb(nma.naive, sep.comps = "+", C.matrix = c.sepsmma)

##difference in variability

##fully additive CNMA:
##Q = 43.55, df = 40, p = 0.32 (can check by viewing "cnma" object)

1- pchisq((cnma$Q.additive-cnma.sepsmma$Q.additive),
          df = (cnma$df.Q.additive-cnma.sepsmma$df.Q.additive))##p for difference in Q for 
##fully additive versus CNMA with interaction term
##for details see Rucker G, et al. Biom J 2021. doi: 10.1002/bimj201900339

##interaction between embolisation and craniotomy

c.crmma <- cbind(cnma$C.matrix, 
                 cr.mma = cnma$C.matrix[, "Cr"] * cnma$C.matrix[, "MMA"])

cnma.crmma <- netcomb(nma.naive, sep.comps = "+", C.matrix = c.crmma)

1- pchisq((cnma$Q.additive-cnma.crmma$Q.additive), 
          df = (cnma$df.Q.additive-cnma.crmma$df.Q.additive))##p for difference as above

##interaction between BH and D

c.bhd <- cbind(cnma$C.matrix,
               bh.d = cnma$C.matrix[, "BH"] * cnma$C.matrix[, "D"])

cnma.bhd <- netcomb(nma.naive, sep.comps = "+", C.matrix = c.bhd)

##Q bhd; 42.29, df = 39

1 - pchisq((cnma$Q.additive-cnma.bhd$Q.additive),
           df = cnma$df.Q.additive-cnma.bhd$df.Q.additive)##p for difference as above


##reproduce table S22.1

tables22.1.1 <- cbind(
  cnma$comps,
  tibble(summary(cnma)$components.random)
) %>% as_tibble() %>%
  mutate(TE = exp(TE)) %>%
  mutate(lower = exp(lower)) %>%
  mutate(upper = exp(upper)) %>%
  mutate(ci = paste0("[", round(lower,4), "; ", round(upper, 4), "]")) %>%
  rename(comp = `cnma$comps`) %>%
  select(comp, TE, ci, statistic, p)

tables22.1.2 <- cbind(
  cnma.sepsmma$comps,
  tibble(summary(cnma.sepsmma)$components.random)
) %>% as_tibble() %>%
  mutate(TE = exp(TE)) %>%
  mutate(lower = exp(lower)) %>%
  mutate(upper = exp(upper)) %>%
  mutate(ci = paste0("[", round(lower,4), "; ", round(upper, 4), "]")) %>%
  rename(comp = `cnma.sepsmma$comps`) %>%
  select(comp, TE, ci, statistic, p)

tables22.1.3 <- cbind(
  cnma.crmma$comps,
  tibble(summary(cnma.crmma)$components.random)
) %>% as_tibble() %>%
  mutate(TE = exp(TE)) %>%
  mutate(lower = exp(lower)) %>%
  mutate(upper = exp(upper)) %>%
  mutate(ci = paste0("[", round(lower,4), "; ", round(upper, 4), "]")) %>%
  rename(comp = `cnma.crmma$comps`) %>%
  select(comp, TE, ci, statistic, p)

tables22.1.4 <- cbind(
  cnma.bhd$comps,
  tibble(summary(cnma.bhd)$components.random)
) %>% as_tibble() %>%
  mutate(TE = exp(TE)) %>%
  mutate(lower = exp(lower)) %>%
  mutate(upper = exp(upper)) %>%
  mutate(ci = paste0("[", round(lower,4), "; ", round(upper, 4), "]")) %>%
  rename(comp = `cnma.bhd$comps`) %>%
  select(comp, TE, ci, statistic, p)

flextable(tables22.1.1) %>%
  save_as_docx(path = "Tables/table s22_1_1.docx",
               pr_section = sec_portrait)

flextable(tables22.1.2) %>%
  save_as_docx(path = "Tables/table s22_1_2.docx",
               pr_section = sec_portrait)

flextable(tables22.1.3) %>%
  save_as_docx(path = "Tables/table s22_1_3.docx",
               pr_section = sec_portrait)

flextable(tables22.1.4) %>%
  save_as_docx(path = "Tables/table s22_1_4.docx",
               pr_section = sec_portrait)

##sensitivity analysis - impute results of embolization studies


##embolization imputations


embolization.temp <- recurrence_data %>% 
  filter(treat %in% c("surgery_alone", "surgery_MMA", "embolization"))

##get multi arm studies
embolization.temp <- subset(
  embolization.temp, duplicated(
    embolization.temp$studlab) | duplicated(
      embolization.temp$studlab, fromLast = TRUE)) %>% 
  arrange(treat) %>% 
  arrange(studlab)

##remove study already in NMA
embolization.temp <- embolization.temp %>%
  filter(!studlab == "Carpenter 2021")

##assume all surgery is BH+D
embolization.temp$treat <- embolization.temp$treat %>%
  str_replace_all(pattern = "embolization", replacement = "MMA") %>%
  str_replace_all(pattern = "surgery_alone", replacement = "BH+D") %>%
  str_replace_all("surgery_MMA", "BH+D+MMA")

##convert to contrast
embolization.temp <- pairwise(
  data = embolization.temp, 
  treat = treat, 
  event = event, 
  studlab = studlab,
  n = n, sm = "RR") %>% 
  as_tibble()


##remove non-surgery comparisons for ban 2017
embolization.temp <- embolization.temp[-1:-2,]


embosens.pairs <- bind_rows(embolization.temp, nma.pairs) ##imputed data

##new NMA wih imputed data
nma.embosens <- netmeta(
  TE,
  seTE,
  treat1,
  treat2,
  studlab,
  data = embosens.pairs,
  sm = "RR",
  comb.fixed = F,
  comb.random = T,
  method.tau = "ML"
)

##CNMA with imputed data
cnma.embosens <- netcomb(nma.embosens, sep.comps = "+")

##reproduce table S22.2

##part1

embolization.temp %>%
  left_join(table_chars_temp) %>%
  select(studlab, treat1, treat2, TE, seTE, Design) %>%
  flextable() %>%
  save_as_docx(path = "Tables/table S22_2_1.docx",
               pr_section = sec_portrait)

##part2

cbind(
  cnma.embosens$comps,
  tibble(summary(cnma.embosens)$components.random)
) %>% as_tibble() %>%
  mutate(TE = exp(TE)) %>%
  mutate(lower = exp(lower)) %>%
  mutate(upper = exp(upper)) %>%
  mutate(ci = paste0("[", round(lower,4), "; ", round(upper, 4), "]")) %>%
  rename(comp = `cnma.embosens$comps`) %>%
  select(comp, TE, ci, statistic, p) %>%
  flextable() %>%
  save_as_docx(path = "Tables/table S22_2_2.docx",
               pr_section = sec_portrait)

##next we generate the comparison adjusted funnel plot for the analysis
##Figure S15.1


##reproduce figure S15.1

pdf("Figures/figure S15_1.pdf")

funnel(
  nma.naive,
  order = c("MMA",
            "Cr+D+MMA",
            "End+D",
            "SEPS+MMA",
            "YL1",
            "BH+D",
            "Cr+D",
            "TD+D",
            "SEPS",
            "BH",
            "Cr"),
  linreg = T,
  pch = 19,
  col = miscpalettes::mschart$pastel[1:13]
)

dev.off()

##contour enhanced funnel for novel surgical treatments vs. burrhole with drain

treats <- c(
  "burrhole",
  "twist",
  "endoscopy",
  "YL1",
  "embolization",
  "SEPS"
)

funnel.temp <- recurrence_data %>% filter(treat %in% treats)

funnel.temp$treat <- funnel.temp$treat %>% 
  str_replace_all("burrhole_drain", "control") %>%
  str_replace_all("twist", "expr") %>%
  str_replace_all("endoscopy", "expr") %>%
  str_replace_all("YL1", "expr") %>%
  str_replace_all("embolization", "expr") %>%
  str_replace_all("SEPS", "expr")

funnel.temp <- subset(
  funnel.temp, duplicated(
    funnel.temp$studlab) | duplicated(
      funnel.temp$studlab, fromLast = TRUE)) %>%
  arrange(treat) %>%
  arrange(studlab)

funnel.temp <- funnel.temp %>%
  arrange(treat) %>%
  arrange(studlab)

funnel.temp <- pairwise(
  data = funnel.temp,
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
  data = funnel.temp)

##reproduce figure S16.1

pdf(file = "Figures/figure S16_1.pdf")

funnel(bin.r,
       comb.fixed = F,
       level = 0.95, 
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("darkgray", "gray", "lightgray"),
       lwd = 2, 
       cex = 2, 
       pch = 16, 
       studlab = F)

legend(11, 0.05,
       c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
       fill = c("darkgray", "gray", "lightgray"))

dev.off()








