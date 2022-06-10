##meta-regressions

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

##calculate regression variables in table

table_chars_temp <- table_chars %>%
  rename(studlab = study) 

table_chars_temp <- table_chars_temp %>%
  mutate(prop.bilat = BL/n, prop.trauma = Tr/n, prop.male = M/n) %>%
  rename(followup = `F/U`) 

table_chars_temp <- table_chars_temp %>% 
  separate(studlab, c("study", "year"), 
           extra = "merge", remove = FALSE) 

table_chars_temp <- table_chars_temp %>%
  separate(year, c("year1", "year2"), 
           extra = "merge", 
           remove = FALSE)

table_chars_temp$year1 <- table_chars_temp$year1 %>%
  as.numeric()

table_chars_temp <- table_chars_temp %>%
  unite(year1, year2, col = "years")

table_chars_temp$years <- table_chars_temp$years %>%
  str_replace_all("_NA", "") %>% 
  str_replace_all("NA_", "") %>% 
  str_replace_all("_1", "") %>%
  str_replace_all("_2", "") %>%
  str_replace_all("_3", "") %>% 
  str_replace_all("Veken 2014", "2014") %>%
  as.numeric()

table_chars_temp <- table_chars_temp %>%
  select(-study, -year) %>% 
  rename(pubyear = years) %>% 
  rename(age = Age)

#####Primary outcome

##calculate regression data for surgery, burrhole and burrhole with drain

regdat.surg <- recurrence_data %>% 
  filter(treat == "surgery") %>%
  left_join(table_chars_temp, by = "studlab") %>%
  select(studlab, treat, event, n.x,age, prop.bilat, prop.trauma, prop.male, followup, pubyear) %>%
  rename(n = n.x)

regdat.surg <- escalc(measure = "PFT", xi = event, ni = n, data = regdat.surg)

regdat.surg %<>% as_tibble()

regdat.burr <- recurrence_data %>% 
  filter(treat == "burrhole") %>%
  left_join(table_chars_temp, by = "studlab") %>% 
  select(studlab, treat, event, n.x, age, prop.bilat, prop.trauma,
         prop.male, followup, pubyear) %>%
  rename(n = n.x) 

regdat.burr <- escalc(measure = "PFT", xi = event, ni = n, data = regdat.burr)

regdat.burr %<>% as_tibble()

regdat.drain <- recurrence_data %>%
  filter(treat == "burrhole_drain") %>%
  left_join(table_chars_temp, by = "studlab") %>%
  select(studlab, treat, event, n.x, Design, age,
         prop.bilat, prop.trauma, prop.male, followup, pubyear) %>%
  rename(n = n.x)

regdat.drain <- escalc(measure = "PFT", xi = event, ni = n, data = regdat.drain)

regdat.drain %<>% as_tibble()

regs <- c("age", "prop.bilat", 
          "prop.trauma", "prop.male",
          "followup", "pubyear")

##univariable regressions

univar.surg <- tibble(var = NA,
                      k = NA,
                      coef = NA,
                      sei = NA,
                      cilb = NA,
                      ciub = NA,
                      pval = NA,
                      R2 = NA,
                      p.perm = NA)

for(i in regs) {
  
  form <- as.formula(paste("~", i))
  
  r <- rma(yi = yi, 
           vi = vi,
           measure = "PFT",
           data = regdat.surg, 
           method = "ML",
           mods = form)
  
  coef = r$b[2]
  sei = r$se[2]
  k = r$k
  cilb = coef - 1.96*sei
  ciub = coef + 1.96*sei
  pval = r$pval[2]
  al = paste(i)
  
  
  univar.surg <- univar.surg %>% add_row(
    var = al,
    k = k,
    coef = coef,
    sei = sei,
    cilb = cilb,
    ciub = ciub,
    pval = pval,
    R2 = r$R2
  )
  
}

univar.surg$coef <- round(univar.surg$coef, digits = 4)
univar.surg$sei <- round(univar.surg$sei, digits = 4)
univar.surg$cilb <- round(univar.surg$cilb, digits = 4) 
univar.surg$ciub <- round(univar.surg$ciub, digits = 4)
univar.surg$pval <- round(univar.surg$pval, digits = 4)
univar.surg$R2 <- round(univar.surg$R2, digits = 4)

univar.surg <- univar.surg %>% select(-p.perm) %>% na.omit() %>% as_tibble()


univar.burr <- tibble(var = NA,
                      k = NA,
                      coef = NA,
                      sei = NA,
                      cilb = NA,
                      ciub = NA,
                      pval = NA,
                      R2 = NA,
                      p.perm = NA)

for(i in regs) {
  
  form <- as.formula(paste("~", i))
  
  r <- rma(yi = yi,
           vi = vi, 
           measure = "PFT",
           data = regdat.burr, 
           method = "ML",
           mods = form)
  
  coef = r$b[2]
  sei = r$se[2]
  k = r$k
  cilb = coef - 1.96*sei
  ciub = coef + 1.96*sei
  pval = r$pval[2]
  al = paste(i)
  
  univar.burr <- univar.burr %>% add_row(
    var = al,
    k = k,
    coef = coef,
    sei = sei,
    cilb = cilb,
    ciub = ciub,
    pval = pval,
    R2 = r$R2
  )
  
  
}

univar.burr$coef <- round(univar.burr$coef, digits = 4)
univar.burr$sei <- round(univar.burr$sei, digits = 4)
univar.burr$cilb <- round(univar.burr$cilb, digits = 4) 
univar.burr$ciub <- round(univar.burr$ciub, digits = 4)
univar.burr$pval <- round(univar.burr$pval, digits = 4)
univar.burr$R2 <- round(univar.burr$R2, digits = 4)

univar.burr <- univar.burr %>% select(-p.perm) %>% na.omit() %>% as_tibble()


univar.drain <- tibble(var = NA,
                       k = NA,
                       coef = NA,
                       sei = NA,
                       cilb = NA,
                       ciub = NA,
                       pval = NA,
                       R2 = NA,
                       p.perm = NA)

for(i in regs) {
  
  form <- as.formula(paste("~", i))
  
  r <- rma(yi = yi,
           vi = vi, 
           measure = "PFT",
           data = regdat.drain,
           method = "ML",
           mods = form)
  
  coef = r$b[2]
  sei = r$se[2]
  k = r$k
  cilb = coef - 1.96*sei
  ciub = coef + 1.96*sei
  pval = r$pval[2]
  al = paste(i)
  
  univar.drain <- univar.drain %>% add_row(
    var = al,
    k = k,
    coef = coef,
    sei = sei,
    cilb = cilb,
    ciub = ciub,
    pval = pval,
    R2 = r$R2
  )
  
  
  
}

univar.drain$coef <- round(univar.drain$coef, digits = 4)
univar.drain$sei <- round(univar.drain$sei, digits = 4)
univar.drain$cilb <- round(univar.drain$cilb, digits = 4) 
univar.drain$ciub <- round(univar.drain$ciub, digits = 4)
univar.drain$pval <- round(univar.drain$pval, digits = 4)
univar.drain$R2 <- round(univar.drain$R2, digits = 4)

univar.drain <- univar.drain %>%
  select(-p.perm) %>%
  na.omit() %>%
  as_tibble()

univar.surg <- univar.surg %>% 
  mutate(analysis = "surgery", .before = var) %>%
  mutate(ci = paste0("[", cilb, "; ", ciub, "]"))

univar.burr <- univar.burr %>% 
  mutate(analysis = "burrhole", .before = var) %>%
  mutate(ci = paste0("[", cilb, "; ", ciub, "]"))

univar.drain <- univar.drain %>% 
  mutate(analysis = "drain", .before = var) %>%
  mutate(ci = paste0("[", cilb, "; ", ciub, "]"))

univar <- bind_rows(univar.surg, univar.burr, univar.drain) %>%
  select(analysis, var, k, coef, ci, pval, R2)

size <- page_size(
  width = 8.3,
  height = 11.7,
  orient = "landscape"
)

sec <- prop_section(page_size = size)

##reproduce table S12.1

univar %>%
  flextable() %>%
  save_as_docx(path = "Tables/table S12_1.docx",
               pr_section = sec) 

univar_primary <- univar

##permutation testing for strong associations

regs.perm <- c("age", "prop.bilat", "prop.male")

perm.surg <- tibble(
  analysis = NA,
  var = NA,
  pval = NA
)

for(i in regs.perm) {
  
  form <- as.formula(paste("~", i))
  
  r <- rma(yi = yi, vi = vi, measure = "PFT", data = regdat.surg, method = "ML",
           mods = form)
  
  pr <- permutest(r)
  
  pval <- pr$pval[2]
  
  perm.surg <- perm.surg %>%
    add_row(analysis = "surgery",
            var = paste(i),
            pval = pval)
  
}

perm.burr <- tibble(
  analysis = NA,
  var = NA,
  pval = NA
)

for(i in regs.perm) {
  
  form <- as.formula(paste("~", i))
  
  r <- rma(yi = yi, vi = vi, measure = "PFT", data = regdat.burr, method = "ML",
           mods = form)
  
  pr <- permutest(r)
  
  pval <- pr$pval[2]
  
  perm.burr <- perm.burr %>%
    add_row(analysis = "burrhole",
            var = paste(i),
            pval = pval)
  
}

perm.drain <- tibble(
  analysis = NA,
  var = NA,
  pval = NA
)

for(i in regs.perm) {
  
  form <- as.formula(paste("~", i))
  
  r <- rma(yi = yi, vi = vi, measure = "PFT", data = regdat.drain, method = "ML",
           mods = form)
  
  pr <- permutest(r)
  
  pval <- pr$pval[2]
  
  perm.drain <- perm.drain %>%
    add_row(analysis = "drain",
            var = paste(i),
            pval = pval)
  
}

permutests <- list()

permutests[["surgery"]] <- perm.surg

permutests[["burrhole"]] <- perm.burr

permutests[["drain"]] <- perm.drain

##multivariable regressions

surg.multi <- rma(yi = yi,
                  vi = vi,
                  measure = "PFT", 
                  method = "ML", 
                  slab = studlab, 
                  mods = ~ age + prop.bilat + prop.trauma + 
                    prop.male + followup + pubyear, 
                  data = regdat.surg, 
                  test = "knha")

burr.multi <- rma(yi = yi,
                  vi = vi, 
                  measure = "PFT", 
                  method = "ML", 
                  slab = studlab, 
                  mods = ~ age + prop.bilat + prop.trauma +
                    prop.male + followup + pubyear,
                  data = regdat.burr,
                  test = "knha")

drain.multi <- rma(yi = yi,
                   vi = vi,
                   measure = "PFT", 
                   method = "ML",
                   slab = studlab, 
                   mods = ~ age + prop.bilat + prop.trauma + 
                     prop.male + followup + pubyear, 
                   data = regdat.drain, test = "knha")

surg.multi <- tibble(analysis = "surgery",
                     var = c("intercept", regs),
                     k = surg.multi$k,
                     coef = surg.multi$b,
                     sei = surg.multi$se,
                     cilb = surg.multi$ci.lb,
                     ciub = surg.multi$ci.ub,
                     pval = surg.multi$pval,
)

surg.multi$coef <- round(surg.multi$coef, digits = 4)
surg.multi$sei <- round(surg.multi$sei, digits = 4)
surg.multi$cilb <- round(surg.multi$cilb, digits = 4)
surg.multi$ciub <- round(surg.multi$ciub, digits = 4)
surg.multi$pval <- round(surg.multi$pval, digits = 4)

burr.multi <- tibble(analysis = "burrhole",
                     var = c("intercept", regs),
                     k = burr.multi$k,
                     coef = burr.multi$b,
                     sei = burr.multi$se,
                     cilb = burr.multi$ci.lb,
                     ciub = burr.multi$ci.ub,
                     pval = burr.multi$pval,
)

burr.multi$coef <- round(burr.multi$coef, digits = 4)
burr.multi$sei <- round(burr.multi$sei, digits = 4)
burr.multi$cilb <- round(burr.multi$cilb, digits = 4)
burr.multi$ciub <- round(burr.multi$ciub, digits = 4)
burr.multi$pval <- round(burr.multi$pval, digits = 4)

drain.multi <- tibble(analysis = "drain",
                      var = c("intercept", regs),
                      k = drain.multi$k,
                      coef = drain.multi$b,
                      sei = drain.multi$se,
                      cilb = drain.multi$ci.lb,
                      ciub = drain.multi$ci.ub,
                      pval = drain.multi$pval,
)

drain.multi$coef <- round(drain.multi$coef, digits = 4)
drain.multi$sei <- round(drain.multi$sei, digits = 4)
drain.multi$cilb <- round(drain.multi$cilb, digits = 4)
drain.multi$ciub <- round(drain.multi$ciub, digits = 4)
drain.multi$pval <- round(drain.multi$pval, digits = 4)

multi <- bind_rows(surg.multi, burr.multi, drain.multi) %>%
  mutate(ci = paste0("[",cilb,"; ",ciub,"]"))

##reproduce table S12.2

multi %>%
  select(analysis, var, k, coef, ci, pval) %>%
  flextable() %>%
  save_as_docx(pr_section = sec, path = "Tables/table S12_2.docx")

multi_primary <- multi

#####Secondary outcome

table_chars_temp <- table_chars %>%
  rename(studlab = study) 

table_chars_temp <- table_chars_temp %>%
  mutate(prop.bilat = BL/n, prop.trauma = Tr/n, prop.male = M/n) %>%
  rename(followup = `F/U`) 

table_chars_temp <- table_chars_temp %>% 
  separate(studlab, c("study", "year"), 
           extra = "merge", remove = FALSE) 

table_chars_temp <- table_chars_temp %>%
  separate(year, c("year1", "year2"), 
           extra = "merge", 
           remove = FALSE)

table_chars_temp$year1 <- table_chars_temp$year1 %>%
  as.numeric()

table_chars_temp <- table_chars_temp %>%
  unite(year1, year2, col = "years")

table_chars_temp$years <- table_chars_temp$years %>%
  str_replace_all("_NA", "") %>% 
  str_replace_all("NA_", "") %>% 
  str_replace_all("_1", "") %>%
  str_replace_all("_2", "") %>%
  str_replace_all("_3", "") %>% 
  str_replace_all("Veken 2014", "2014") %>%
  as.numeric()

table_chars_temp <- table_chars_temp %>%
  select(-study, -year) %>% 
  rename(pubyear = years) %>% 
  rename(age = Age)

##calculate regression data for surgery, burrhole and burrhole with drain

regdat.surg <- complications_data %>% 
  filter(treat == "surgery") %>%
  left_join(table_chars_temp, by = "studlab") %>%
  select(studlab, treat, event, n.x,age, prop.bilat, prop.trauma, prop.male, followup, pubyear) %>%
  rename(n = n.x)

regdat.surg <- escalc(measure = "PFT", xi = event, ni = n, data = regdat.surg)

regdat.surg %<>% as_tibble()

regdat.burr <- complications_data %>% 
  filter(treat == "burrhole") %>%
  left_join(table_chars_temp, by = "studlab") %>% 
  select(studlab, treat, event, n.x, age, prop.bilat, prop.trauma,
         prop.male, followup, pubyear) %>%
  rename(n = n.x) 

regdat.burr <- escalc(measure = "PFT", xi = event, ni = n, data = regdat.burr)

regdat.burr %<>% as_tibble()

regdat.drain <- complications_data %>%
  filter(treat == "burrhole_drain") %>%
  left_join(table_chars_temp, by = "studlab") %>%
  select(studlab, treat, event, n.x, Design, age,
         prop.bilat, prop.trauma, prop.male, followup, pubyear) %>%
  rename(n = n.x)

regdat.drain <- escalc(measure = "PFT", xi = event, ni = n, data = regdat.drain)

regdat.drain %<>% as_tibble()

regs <- c("age", "prop.bilat", 
          "prop.trauma", "prop.male",
          "followup", "pubyear")

##univariable - secondary outcome

univar.surg <- tibble(var = NA,
                      k = NA,
                      coef = NA,
                      sei = NA,
                      cilb = NA,
                      ciub = NA,
                      pval = NA,
                      R2 = NA,
                      p.perm = NA)

for(i in regs) {
  
  form <- as.formula(paste("~", i))
  
  r <- rma(yi = yi, 
           vi = vi,
           measure = "PFT",
           data = regdat.surg, 
           method = "ML",
           mods = form)
  
  coef = r$b[2]
  sei = r$se[2]
  k = r$k
  cilb = coef - 1.96*sei
  ciub = coef + 1.96*sei
  pval = r$pval[2]
  al = paste(i)
  
  
  univar.surg <- univar.surg %>% add_row(
    var = al,
    k = k,
    coef = coef,
    sei = sei,
    cilb = cilb,
    ciub = ciub,
    pval = pval,
    R2 = r$R2
  )
  
}

univar.surg$coef <- round(univar.surg$coef, digits = 4)
univar.surg$sei <- round(univar.surg$sei, digits = 4)
univar.surg$cilb <- round(univar.surg$cilb, digits = 4) 
univar.surg$ciub <- round(univar.surg$ciub, digits = 4)
univar.surg$pval <- round(univar.surg$pval, digits = 4)
univar.surg$R2 <- round(univar.surg$R2, digits = 4)

univar.surg <- univar.surg %>% select(-p.perm) %>% na.omit() %>% as_tibble()


univar.burr <- tibble(var = NA,
                      k = NA,
                      coef = NA,
                      sei = NA,
                      cilb = NA,
                      ciub = NA,
                      pval = NA,
                      R2 = NA,
                      p.perm = NA)

for(i in regs) {
  
  form <- as.formula(paste("~", i))
  
  r <- rma(yi = yi,
           vi = vi, 
           measure = "PFT",
           data = regdat.burr, 
           method = "ML",
           mods = form)
  
  coef = r$b[2]
  sei = r$se[2]
  k = r$k
  cilb = coef - 1.96*sei
  ciub = coef + 1.96*sei
  pval = r$pval[2]
  al = paste(i)
  
  univar.burr <- univar.burr %>% add_row(
    var = al,
    k = k,
    coef = coef,
    sei = sei,
    cilb = cilb,
    ciub = ciub,
    pval = pval,
    R2 = r$R2
  )
  
  
}

univar.burr$coef <- round(univar.burr$coef, digits = 4)
univar.burr$sei <- round(univar.burr$sei, digits = 4)
univar.burr$cilb <- round(univar.burr$cilb, digits = 4) 
univar.burr$ciub <- round(univar.burr$ciub, digits = 4)
univar.burr$pval <- round(univar.burr$pval, digits = 4)
univar.burr$R2 <- round(univar.burr$R2, digits = 4)

univar.burr <- univar.burr %>% select(-p.perm) %>% na.omit() %>% as_tibble()


univar.drain <- tibble(var = NA,
                       k = NA,
                       coef = NA,
                       sei = NA,
                       cilb = NA,
                       ciub = NA,
                       pval = NA,
                       R2 = NA,
                       p.perm = NA)

for(i in regs) {
  
  form <- as.formula(paste("~", i))
  
  r <- rma(yi = yi,
           vi = vi, 
           measure = "PFT",
           data = regdat.drain,
           method = "ML",
           mods = form)
  
  coef = r$b[2]
  sei = r$se[2]
  k = r$k
  cilb = coef - 1.96*sei
  ciub = coef + 1.96*sei
  pval = r$pval[2]
  al = paste(i)
  
  univar.drain <- univar.drain %>% add_row(
    var = al,
    k = k,
    coef = coef,
    sei = sei,
    cilb = cilb,
    ciub = ciub,
    pval = pval,
    R2 = r$R2
  )
  
  
  
}

univar.drain$coef <- round(univar.drain$coef, digits = 4)
univar.drain$sei <- round(univar.drain$sei, digits = 4)
univar.drain$cilb <- round(univar.drain$cilb, digits = 4) 
univar.drain$ciub <- round(univar.drain$ciub, digits = 4)
univar.drain$pval <- round(univar.drain$pval, digits = 4)
univar.drain$R2 <- round(univar.drain$R2, digits = 4)

univar.drain <- univar.drain %>%
  select(-p.perm) %>%
  na.omit() %>%
  as_tibble()

univar.surg <- univar.surg %>% 
  mutate(analysis = "surgery", .before = var) %>%
  mutate(ci = paste0("[", cilb, "; ", ciub, "]"))

univar.burr <- univar.burr %>% 
  mutate(analysis = "burrhole", .before = var) %>%
  mutate(ci = paste0("[", cilb, "; ", ciub, "]"))

univar.drain <- univar.drain %>% 
  mutate(analysis = "drain", .before = var) %>%
  mutate(ci = paste0("[", cilb, "; ", ciub, "]"))

univar <- bind_rows(univar.surg, univar.burr, univar.drain) %>%
  select(analysis, var, k, coef, ci, pval, R2)

##reproduce table S13.1

univar %>%
  flextable() %>%
  save_as_docx(pr_section = sec, 
               path = "Tables/table S13_1.docx")

univar_secondary <- univar

##multivariable

surg.multi <- rma(yi = yi,
                  vi = vi,
                  measure = "PFT", 
                  method = "ML", 
                  slab = studlab, 
                  mods = ~ age + prop.bilat + prop.trauma + 
                    prop.male + followup + pubyear, 
                  data = regdat.surg, 
                  test = "knha")

burr.multi <- rma(yi = yi,
                  vi = vi, 
                  measure = "PFT", 
                  method = "ML", 
                  slab = studlab, 
                  mods = ~ age + prop.bilat + prop.trauma +
                    prop.male + followup + pubyear,
                  data = regdat.burr,
                  test = "knha")

drain.multi <- rma(yi = yi,
                   vi = vi,
                   measure = "PFT", 
                   method = "ML",
                   slab = studlab, 
                   mods = ~ age + prop.bilat + prop.trauma + 
                     prop.male + followup + pubyear, 
                   data = regdat.drain, test = "knha")

surg.multi <- tibble(analysis = "surgery",
                     var = c("intercept", regs),
                     k = surg.multi$k,
                     coef = surg.multi$b,
                     sei = surg.multi$se,
                     cilb = surg.multi$ci.lb,
                     ciub = surg.multi$ci.ub,
                     pval = surg.multi$pval,
)

surg.multi$coef <- round(surg.multi$coef, digits = 4)
surg.multi$sei <- round(surg.multi$sei, digits = 4)
surg.multi$cilb <- round(surg.multi$cilb, digits = 4)
surg.multi$ciub <- round(surg.multi$ciub, digits = 4)
surg.multi$pval <- round(surg.multi$pval, digits = 4)

burr.multi <- tibble(analysis = "burrhole",
                     var = c("intercept", regs),
                     k = burr.multi$k,
                     coef = burr.multi$b,
                     sei = burr.multi$se,
                     cilb = burr.multi$ci.lb,
                     ciub = burr.multi$ci.ub,
                     pval = burr.multi$pval,
)

burr.multi$coef <- round(burr.multi$coef, digits = 4)
burr.multi$sei <- round(burr.multi$sei, digits = 4)
burr.multi$cilb <- round(burr.multi$cilb, digits = 4)
burr.multi$ciub <- round(burr.multi$ciub, digits = 4)
burr.multi$pval <- round(burr.multi$pval, digits = 4)

drain.multi <- tibble(analysis = "drain",
                      var = c("intercept", regs),
                      k = drain.multi$k,
                      coef = drain.multi$b,
                      sei = drain.multi$se,
                      cilb = drain.multi$ci.lb,
                      ciub = drain.multi$ci.ub,
                      pval = drain.multi$pval,
)

drain.multi$coef <- round(drain.multi$coef, digits = 4)
drain.multi$sei <- round(drain.multi$sei, digits = 4)
drain.multi$cilb <- round(drain.multi$cilb, digits = 4)
drain.multi$ciub <- round(drain.multi$ciub, digits = 4)
drain.multi$pval <- round(drain.multi$pval, digits = 4)

multi <- bind_rows(surg.multi, burr.multi, drain.multi) %>%
  mutate(ci = paste0("[",cilb,"; ",ciub,"]"))

##reproduce table S13.2

multi %>%
  select(analysis, var, k, coef, ci, pval) %>%
  flextable() %>%
  save_as_docx(path = "Tables/table S13_2.docx",
               pr_section = sec)

multi_secondary <- multi













