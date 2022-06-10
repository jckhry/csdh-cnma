##surgical CNMA - secondary outcome

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

nma.dat <- complications_data %>% 
  filter(treat %in% treats)

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

pairs.surgery.complications <- nma.pairs %>% 
  select(studlab, design, treat1, treat2, TE, seTE) %>%
  rename(es.comp = TE, se.comp = seTE)

pairs.surgery.complications$design <- pairs.surgery.complications$design %>% str_replace_all(pattern = "non-randomised", replacement = "NRS") %>%
  str_replace_all(pattern = "randomised", replacement = "Randomised")

chars_temp <- table_chars_temp %>%
  select(studlab, n, `F/U`, M, F, BL, `Age (yrs.)`, Location, Defn.) %>% 
  rename(followup = `F/U`, 
         m = M,
         f = F,
         bl = BL,
         age = `Age (yrs.)`,
         defn = Defn., 
         location = Location)

pairs.surgery.complications <- pairs.surgery.complications %>%
  left_join(chars_temp)

random.all <- rob_random %>% 
  select(Study, Overall) %>%
  rename(studlab = Study, bias = Overall)

nrs.all <- rob_nrs %>% 
  select(Study, Overall) %>% 
  rename(studlab = Study, bias = Overall)

bias <- bind_rows(random.all, nrs.all)

pairs.surgery.complications <- pairs.surgery.complications %>% 
  left_join(bias, by = "studlab") %>%
  mutate(es.comp = round(es.comp, digits = 3),
         se.comp = round(se.comp, digits = 3)) %>% 
  arrange(studlab)

##reproduce rest of table S3
flextable(pairs.surgery.complications) %>% 
  save_as_docx(path = "Tables/table S3_2.docx",
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

##reproduce figure S19

pdf(file = "Figures/figure S19.pdf")

forest(design.adjusted,
       col.by = "black",
       subset.treatments = c("BH+D", "TD+D"))

dev.off()


##assess inconsistency

decomp <- decomp.design(nma.naive)

##this reproduces the inconsistency values reported in the main text
##and in Table S17.1 (Q = 2.4, df = 2, p = 0.31)

##local inconsistency using node splitting

split <- netsplit(nma.naive)

##reproduce table S17.2
tables17 <- tibble(split$random)

tables17 <- tibble(
  comparison = tables17$comparison,
  k = split$k,
  prop = split$prop.random,
  NMA = exp(tables17$TE),
  direct = exp(split$direct.random$TE),
  indir = exp(split$indirect.random$TE),
  ror = direct/indir,
  pval = split$compare.random$p
)

flextable(tables17) %>% save_as_docx(path = "Tables/table S17_2.docx",
                                     pr_section = sec_portrait)

##heat plot - reproduces Figure S23

pdf(file = "Figures/figure S23.pdf")

netheat(nma.naive)

dev.off()

##assess distribution of effect modifiers across treatments

mods_surg_treat <- read_excel("Data/mods_surg_treat.xlsx")

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

##reproduce table S20.1
tbl_mods_treat %>% 
  as_flex_table() %>% 
  save_as_docx(path = "Tables/table S20_1.docx",
               pr_section = sec)

##assess distribution of effect modifiers across components

mods_surg_comp <- read_excel("Data/mods_surg_comp.xlsx")

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

tbl_mods_comp %>% 
  as_flex_table() %>% 
  save_as_docx(path = "Tables/table S20_2.docx")

##assess additivity as in previous NMAs

all.pairs <- netpairwise(nma.naive,
                         comb.fixed = FALSE,
                         comb.random = TRUE,
                         backtransf = FALSE,
                         reference.grop = "BH")

all.pairs

all.pairs <- tibble(
  studlab = all.pairs$studlab,
  TE = all.pairs$TE, 
  seTE = all.pairs$seTE,
  comp = all.pairs$byvar)

all.pairs

all.pairs %>% count(comp)


comps.d <- c("BH+D:BH")

drain.comps <- all.pairs %>%
  filter(comp %in% comps.d)

drain.comps <- drain.comps %>% 
  mutate(comp = "Drain")

mmasurg.t <- c("surgery_MMA", "surgery_alone")

mma.surg <- complications_data %>% 
  filter(treat %in% mmasurg.t)

mma.surg <- subset(
  mma.surg, duplicated(
    mma.surg$studlab) | duplicated(
      mma.surg$studlab, fromLast = TRUE))

mma.surg <- pairwise(
  treat = treat, 
  event = event, n = n, 
  studlab = studlab, 
  sm = "RR", data = mma.surg)

mma.surg <- mma.surg %>%
  as_tibble() %>% 
  select(studlab, TE, seTE) %>%
  mutate(comp = "MMA") %>% 
  mutate(TE = TE*(-1))

fdat <- drain.comps %>% 
  bind_rows(mma.surg)

add.meta <- metagen(
  TE, 
  seTE, 
  studlab,
  data = fdat,
  byvar = comp,
  sm = "RR",
  backtransf = F,
  comb.fixed = F,
  comb.random = T)

##reproduce Figure S26.1

pdf(file = "Figures/figure S26_1.pdf")

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

##CNMA

cnma <- netcomb(nma.naive, sep.comps = "+")

##reproduce Figure 7B

hold <- metagen(
  TE = summary(cnma)[["components.random"]]$TE,
  seTE = summary(cnma)[["components.random"]]$seTE,
  studlab = cnma$comps,
  sm = "RR"
)

pdf("Figures/figure 7B.pdf")

forest(hold,
       layout = "JAMA",
       col.square = "black",
       col.inside = "black",
       overall = FALSE,
       weight.study = "same",
       overall.hetstat = FALSE)

dev.off()

##reproduce table S4.3

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
  save_as_docx(path = "Tables/table S4_3.docx")

##league table plot - reproduces Figure S9

logrr.cnma <- cnma$TE.random

logrr.cnma <- logrr.cnma %>% 
  as_tibble() %>%
  mutate(newcol = colnames(logrr.cnma), .before = "BH")

logrr.cnma <- logrr.cnma %>% 
  pivot_longer(
    cols = c("BH", "BH+D","Cr+D", 
             "End+D", "MMA", "SEPS", 
             "SEPS+MMA", "TD+D", "YL1"),
    names_to = "comp",
    values_to = "val"
  )

sete.cnma <- cnma$seTE.random

sete.cnma <- sete.cnma %>%
  as_tibble() %>% 
  mutate(newcol = colnames(sete.cnma), .before = "BH")

sete.cnma <- sete.cnma %>% 
  pivot_longer(
    cols = c("BH", "BH+D","Cr+D",
             "End+D", "MMA", "SEPS",
             "SEPS+MMA", "TD+D", "YL1"),
    names_to = "comp",
    values_to = "val"
  ) %>% rename(sete = val)

pval.cnma <- cnma$pval.random

pval.cnma <- pval.cnma %>%
  as_tibble() %>% 
  mutate(newcol = colnames(pval.cnma), .before = "BH")

pval.cnma <- pval.cnma %>% 
  pivot_longer(
    cols = c("BH", "BH+D","Cr+D", 
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
  levels = c("YL1", "End+D", "SEPS+MMA",
             "BH+D", "SEPS", "BH", "TD+D",
             "MMA", "Cr+D"))

factor_surg_recur_y <- factor(
  leaguedat$newcol, levels = c("YL1", "End+D", "SEPS+MMA",
                               "BH+D", "SEPS", "BH", "TD+D",
                               "MMA", "Cr+D"))


##create league table plot

league_surg_complications <- ggplot(data = leaguedat) +
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

pdf(file = "Figures/figure S9.pdf", width = 20, height = 12)

league_surg_complications

dev.off()

##network plot - figure 7A

pts.trts <- nma.dat %>%
  filter(studlab %in% nma.naive$studlab) %>% 
  group_by(treat) %>% 
  summarise(pts = sum(n))

##reproduce Figure 7A

pdf(file = "Figures/figure 7A.pdf")

netgraph(cnma,
                 plastic = FALSE,
                 lwd = 10, 
                 thickness = "number.of.studies",
                 points = TRUE, 
                 cex.points = cnma$k.trts/1.5,
                 dim = "2d")

dev.off()

##comparison-adjusted funnel plot - Figure 15.3

##reproduce figure S15.3

pdf(file = "Figures/figure S15_3.pdf")

funnel(
  nma.naive,
  order = c("YL1",
            "End+D",
            "SEPS+MMA",
            "BH+D",
            "SEPS",
            "BH",
            "TD+D",
            "MMA",
            "Cr+D"),
  linreg = T,
  pch = 19,
  col = miscpalettes::mschart$pastel[1:10]
)

dev.off()

##sensitivity analysis - interaction between burrholes and drains

c.bhd <- cbind(cnma$C.matrix,
               bh.d = cnma$C.matrix[, "BH"] * cnma$C.matrix[, "D"])

cnma.bhd <- netcomb(nma.naive, sep.comps = "+", C.matrix = c.bhd)

##p for difference from additive cnma
1 - pchisq((cnma$Q.additive-cnma.bhd$Q.additive),
           df = cnma$df.Q.additive-cnma.bhd$df.Q.additive)##p for difference as above

##reproduce table S24

tables24 <- cbind(
  cnma.bhd$comps,
  tibble(summary(cnma.bhd)$components.random)
) %>% as_tibble() %>%
  mutate(TE = exp(TE)) %>%
  mutate(lower = exp(lower)) %>%
  mutate(upper = exp(upper)) %>%
  mutate(ci = paste0("[", round(lower,4), "; ", round(upper, 4), "]")) %>%
  rename(comp = `cnma.bhd$comps`) %>%
  select(comp, TE, ci, statistic, p)

flextable(tables24) %>%
  save_as_docx(path = "Tables/table s24.docx",
               pr_section = sec_portrait)

