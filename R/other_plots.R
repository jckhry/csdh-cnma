##other plots/figures/tables

source("R/import.R")

##world map - Figure S1

##get world data

world <- map_data("world")

##create temp offshoot of table_chars

table_chars_w <- table_chars

##replace hong kong with China

table_chars_w$Location <- table_chars_w$Location %>% 
  str_replace_all("Hong Kong", "China")

##replace multinational study (Sweden, Norway) with 
##just sweden (dont forget to add norway + 1 later!)

table_chars_w$Location <- table_chars_w$Location %>%
  str_replace_all("Sweden, Norway", "Sweden")

##change countries to match names in world map data

table_chars_w$Location <- table_chars_w$Location %>%
  str_replace_all("S. Korea", "South Korea")

table_chars_w$Location <- table_chars_w$Location %>%
  str_replace_all("United Kingdom", "UK")

##separate freq tables for randomised and non-randomised

countries_random <- table_chars_w %>% 
  filter(Design == "Randomised") %>% 
  count(Location)

countries_nrs <- table_chars_w %>%
  filter(!Design == "Randomised") %>% count(Location)

##add 1 to norway nrs

countries_nrs[33,2] = countries_nrs[33,2] + 1

countries_nrs <- countries_nrs %>% 
  rename(region = Location)

countries_random <- countries_random %>% 
  rename(region = Location)

##add studies column to map data

world_nrs <- world %>% 
  left_join(countries_nrs)

worldplot_nrs <- ggplot() + 
  geom_map(data = world_nrs,
           map = world, 
           aes(long, lat, map_id = region, fill = n)) + 
  scale_fill_gradient(na.value = "grey80",
                      low = "#A2D9FB",
                      high = "#135D8C", 
                      name = "Studies") +
  theme(axis.line = element_blank(), 
        axis.ticks = element_blank(),
        panel.background = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank())



##add studies column to map data

world_random <- world %>%
  left_join(countries_random)

worldplot_random <- ggplot() + 
  geom_map(data = world_random, 
           map = world, 
           aes(long, lat, map_id = region, fill = n)) +
  scale_fill_gradient(na.value = "grey80",
                      low = "#A2D9FB",
                      high = "#135D8C",
                      name = "Studies") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank())

##reproduce Figure S1

pdf(file = "Figures/figure S1.pdf", height = 20, width = 12)

ggarrange(worldplot_nrs, worldplot_random, labels = "AUTO", nrow = 2, ncol = 1)

dev.off()

##table of study characteristics - Table 1

##summary of study characteristics

##Calculate total patients for whom gender was reported
gend.total <- sum(na.omit(table_chars$M) + na.omit(table_chars$F))

##Calculate total patients for whom bilateral/unilateral was reported
bl.total <- table_chars %>% select(BL, n) %>% na.omit() %>% select(n) %>% sum()

##Calculate total patients for whom history of trauma was reported
tr.total <- table_chars %>% select(Tr, n) %>% na.omit() %>% select(n) %>% sum()

##Define functions to calculate percentage of patients from sum
mfunc <- function(vector){
  a <- sum(vector)/gend.total
  a <- a*100
  return(a)
}

blfunc <- function(vector) {
  a <- sum(vector)/bl.total
  a <- a*100
  return(a)
}

trfunc <- function(vector){
  a <- sum(vector)/tr.total
  a <- a*100
  return(a)
}


##Generate table

table_chars_summary <- table_chars %>% 
  select(n, "F/U", M, "F", BL, Tr, Design, Defn., Age) %>%
  
  tbl_summary(
    label = list(
      n ~ "Cases",
      "F/U" ~ "Follow-up",
      M ~ "Male",
      F ~ "Female",
      BL ~ "Bilateral",
      Tr ~ "History of trauma",
      Design ~ "Design",
      Defn. ~ "Definition",
      Age ~ "Age"
    ),
    
    statistic = list(
      n ~ "{sum}",
      M ~ "{sum} ({mfunc}%)",
      F ~ "{sum} ({mfunc}%)",
      BL ~ "{sum} ({blfunc}%)",
      Tr ~ "{sum} ({trfunc}%)"
    ),
    
    missing_text = "Not reported (studies)"
  ) 

##reproduce table 1

table_chars_summary %>%
  as_flex_table() %>%
  save_as_docx(path = "Tables/table 1.docx")


##distribution of effect modifiers across treatments - Figure S4

table_chars <- read_excel("Data/table_chars.xlsx", 
                          col_types = c("text", "numeric", "numeric", 
                                        "text", "numeric", "numeric", "numeric", 
                                        "numeric", "text", "text", "text", 
                                        "text", "text", "numeric"))


table_chars_temp <- table_chars %>% rename(studlab = study) 

##distribution of effect modifiers in included studies

table_chars_temp <- table_chars_temp %>% 
  mutate(prop.male = M/n) %>% mutate(prop.bilat = BL/n) %>%
  mutate(prop.tr = Tr/n)

surg.treats <- c("burrhole", "craniotomy",
                 "twist", "YL1", 
                 "percutaneous", "endoscop",
                 "SEPS", "emboliza")

surg.mods <- table_chars_temp %>% 
  filter(str_detect(Interventions, "burrhol|craniot|twist|YL1|percutan|endoscop|SEPS|emboliz")) %>%
  select(studlab, Interventions, Age, prop.male, prop.bilat, prop.tr) %>% rename(interventions = Interventions, age = Age)

med.mods <- table_chars_temp %>% filter(str_detect(Interventions, "atorv|steroi|dex|pred|gorei|tranex|txa|TXA|cons")) %>%
  select(studlab, Interventions, Age, prop.male, prop.bilat, prop.tr) %>% rename(interventions = Interventions, age = Age)


burr.dat <- surg.mods %>% 
  filter(str_detect(interventions, "burr")) %>% 
  mutate(group = "BH") %>% 
  select(-interventions)

cranio.dat <- surg.mods %>%
  filter(str_detect(interventions, "craniot")) %>%
  mutate(group = "Cr") %>% select(-interventions)

twist.dat <- surg.mods %>% 
  filter(str_detect(interventions, "twist")) %>% 
  mutate(group = "TD") %>% select(-interventions)

mis.dat <- surg.mods %>% 
  filter(str_detect(interventions, "YL1|percut|osteoplas")) %>% 
  mutate(group = "MIS") %>% select(-interventions)

endoscopy.dat <- surg.mods %>% 
  filter(str_detect(interventions, "endoscop")) %>% 
  mutate(group = "End.") %>% select(-interventions)

seps.dat <- surg.mods %>% 
  filter(str_detect(interventions, "SEPS")) %>% 
  mutate(group = "SEPS") %>% select(-interventions)

embo.dat <- surg.mods %>% 
  filter(str_detect(interventions, "emboliza")) %>%
  mutate(group = "MMA") %>% select(-interventions)

surgmods.dat <- bind_rows(
  burr.dat, 
  cranio.dat, 
  twist.dat, 
  mis.dat, 
  endoscopy.dat, 
  seps.dat, 
  embo.dat)

atorv.dat <- med.mods %>% 
  filter(str_detect(interventions, "atorva")) %>%
  mutate(group = "ATS") %>% select(-interventions)

steroid.dat <- med.mods %>% 
  filter(str_detect(interventions, "steroi|dexa|predn")) %>%
  mutate(group = "ST") %>% select(-interventions)

goreisan.dat <- med.mods %>% 
  filter(str_detect(interventions, "gorei")) %>% 
  mutate(group = "GRS") %>% select(-interventions)

txa.dat <- med.mods %>% 
  filter(str_detect(interventions, "txa|TXA|tranexam")) %>%
  mutate(group = "TXA") %>% select(-interventions)

cons.dat <- med.mods %>% 
  filter(str_detect(interventions, "cons")) %>% 
  mutate(group = "Cons") %>% select(-interventions)

medmods.dat <- bind_rows(
  atorv.dat, 
  steroid.dat,
  goreisan.dat,
  txa.dat,
  cons.dat)

surgmods.dat <- surgmods.dat %>% bind_rows(medmods.dat)


multi_format <- function() {
  function(x) format(100*x,digits = 2) 
}

plot_age_surg <- surgmods.dat %>% 
  ggbetweenstats(
    x = group,
    y = age,
    plot.type = "box",
    type = "nonparametric", 
    package = "miscpalettes",
    palette = "pastel", 
    results.subtitle = F,
    pairwise.comparisons = F) + 
  theme(axis.line = element_line(colour = "black"), 
        panel.background = element_blank(), 
        panel.grid = element_blank()) + 
  xlab("Treatment") + 
  ylab("Age") + 
  scale_y_continuous(limits = c(0, 100))

plot_bilat_surg <- surgmods.dat %>%
  ggbetweenstats(
    x = group,
    y = prop.bilat, 
    plot.type = "box",
    type = "nonparametric", 
    package = "miscpalettes",
    palette = "pastel",
    results.subtitle = F, 
    pairwise.comparisons = F) + 
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(), 
        panel.grid = element_blank()) + 
  xlab("Treatment") + 
  ylab("% Bilateral") +
  scale_y_continuous(limits = c(0, 1), labels = multi_format())

plot_male_surg <- surgmods.dat %>% 
  ggbetweenstats(x = group,
                 y = prop.male,
                 plot.type = "box",
                 type = "nonparametric", 
                 package = "miscpalettes", 
                 palette = "pastel",
                 results.subtitle = F,
                 pairwise.comparisons = F) + 
  theme(axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        panel.grid = element_blank()) + 
  xlab("Treatment") + 
  ylab("% Male") + 
  scale_y_continuous(limits = c(0, 1), labels = multi_format())

plot_trauma_surg <- surgmods.dat %>% 
  ggbetweenstats(
    x = group, 
    y = prop.tr, 
    plot.type = "box", 
    type = "nonparametric", 
    package = "miscpalettes",
    palette = "pastel",
    results.subtitle = F,
    pairwise.comparisons = F) + 
  theme(axis.line = element_line(colour = "black"), 
        panel.background = element_blank(),
        panel.grid = element_blank()) + 
  xlab("Treatment") + ylab("% History of trauma") +
  scale_y_continuous(limits = c(0, 1), labels = multi_format())

##reproduce Figure S4

pdf(file = "Figures/figure S4.pdf", height = 20, width = 8)

ggarrange(plot_age_surg, 
          plot_bilat_surg, 
          plot_male_surg, 
          plot_trauma_surg,
          nrow = 4, ncol = 1, 
          labels = "AUTO")

dev.off()

##forest plot for pairwise analysis of burrhole + drain vs. burrhole (Figure S5)

##filter recurrence data to burrhole and burrhole with drain,
##then find studies with >1 arm

slabs <- recurrence_data %>%
  filter(treat %in% c("burrhole_drain", "burrhole_nodrain")) %>%
  count(studlab) %>%
  filter(n > 1)

dat <- recurrence_data %>%
  filter(treat %in% c("burrhole_drain", "burrhole_nodrain")) %>%
  filter(studlab %in% slabs$studlab) %>%
  arrange(treat) %>%
  arrange(studlab)

##convert to contrast format
dat <- pairwise(
  treat = treat,
  event = event,
  n = n,
  sm = "RR",
  studlab = studlab,
  data = dat
)

dat$design <- dat$design %>%
  str_replace_all("randomised", "Randomised") %>%
  str_replace_all("non-Randomised", "Non-randomised")

##meta-analysis
out.meta <- metabin(
  event.e = event1,
  n.e = n1,
  event.c = event2,
  n.c = n2,
  byvar = design,
  studlab = studlab,
  sm = "RR",
  method = "MH",
  comb.random = TRUE,
  comb.fixed = FALSE,
  data  = dat
)

##reproduce Figure S5
pdf(file = "Figures/figure S5.pdf", height = 10, width = 10)

forest(out.meta,
       col.square = "black",
       col.inside = "black",
       col.square.lines = "black",
       lab.e = "BH+D",
       lab.c = "BH",
       col.by = "black",
       col.diamond = "black",
       print.subgroup.name = F)

dev.off()

##forest plot for pairwise analysis of burrhole with subdural vs. subgaleal drain

##same methods as previous

slabs <- recurrence_data %>%
  filter(treat %in% c("burrhole_sd", "burrhole_sg")) %>%
  count(studlab) %>%
  filter(n > 1)

dat <- recurrence_data %>%
  filter(treat %in% c("burrhole_sd", "burrhole_sg")) %>%
  filter(studlab %in% slabs$studlab) %>%
  arrange(treat) %>%
  arrange(studlab)

##convert to contrast format
dat <- pairwise(
  treat = treat,
  event = event,
  n = n,
  sm = "RR",
  studlab = studlab,
  data = dat
)

dat$design <- dat$design %>%
  str_replace_all("randomised", "Randomised") %>%
  str_replace_all("non-Randomised", "Non-randomised")

##meta-analysis
out.meta <- metabin(
  event.e = event1,
  n.e = n1,
  event.c = event2,
  n.c = n2,
  byvar = design,
  studlab = studlab,
  sm = "RR",
  method = "MH",
  comb.random = TRUE,
  comb.fixed = FALSE,
  data  = dat
)

##reproduce Figure S6
pdf(file = "Figures/figure S6.pdf", height = 8, width = 10)

forest(out.meta,
       col.square = "black",
       col.inside = "black",
       col.square.lines = "black",
       lab.e = "Subdural",
       lab.c = "Subgaleal",
       col.by = "black",
       col.diamond = "black",
       print.subgroup.name = F)

dev.off()

##forest plot for surgery vs surgery with steroids

slabs <- complications_data %>%
  filter(treat %in% c("surgery_steroid", "surgery_alone", "surgery_placebo")) %>%
  count(studlab) %>%
  filter(n > 1)

dat <- complications_data %>%
  filter(treat %in% c("surgery_steroid", "surgery_alone", "surgery_placebo")) %>%
  filter(studlab %in% slabs$studlab) %>%
  arrange(treat) %>%
  arrange(studlab)

dat$treat <- dat$treat %>%
  str_replace_all("surgery_placebo", "surgery_alone")

##convert to contrast format
dat <- pairwise(
  treat = treat,
  event = event,
  n = n,
  sm = "RR",
  studlab = studlab,
  data = dat
)

dat$design <- dat$design %>%
  str_replace_all("randomised", "Randomised") %>%
  str_replace_all("non-Randomised", "Non-randomised")

##meta-analysis
out.meta <- metabin(
  event.e = event2,
  n.e = n2,
  event.c = event1,
  n.c = n1,
  byvar = design,
  studlab = studlab,
  sm = "RR",
  method = "MH",
  comb.random = TRUE,
  comb.fixed = FALSE,
  data  = dat
)

##reproduce Figure S12
pdf(file = "Figures/figure S12.pdf", height = 8, width = 10)

forest(out.meta,
       col.square = "black",
       col.inside = "black",
       col.square.lines = "black",
       lab.e = "Surg+ST",
       lab.c = "Surg",
       col.by = "black",
       col.diamond = "black",
       print.subgroup.name = F)

dev.off()














