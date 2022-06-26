##run entire analysis

##outputs of analysis will be stored in list item "analysis_output"

##all tables and figures will be reproduced in created directories
##figures and tables

##install packages

source("R/installpkgs.R")

##match versions to analysis versions

renv::restore(exclude = c("lme4", "nloptr"))

source("R/import.R")

analysis_output <- list()

##proportions

evaluate::evaluate(file("R/proportions.R"))

analysis_output[["proportions"]] <- prop.tab

##pairwise

evaluate::evaluate(file("R/pairwise.R"))

analysis_output[["pairwise_recurrence"]] <- recurrence.pair

analysis_output[["pairwise_morbidity"]] <- complications.pair

##network meta-analysis

##surgery - primary outcome

evaluate::evaluate(file("R/nma_surgery_recurrence.R"))

analysis_output[["NMA_surgery_recurrence"]][["CNMA"]] <- cnma

analysis_output[["NMA_surgery_recurrence"]][["naive NMA"]] <- nma.naive

##surgery - secondary outcome

evaluate::evaluate(file("R/nma_surgery_morbidity.R"))

analysis_output[["NMA_surgery_morbidity"]][["CNMA"]] <- cnma

analysis_output[["NMA_surgery_morbidity"]][["naive NMA"]] <- nma.naive

##medical - primary outcome

evaluate::evaluate(file("R/nma_medical_recurrence.R"))

analysis_output[["NMA_medical_recurrence"]][["CNMA"]] <- cnma

analysis_output[["NMA_medical_recurrence"]][["naive NMA"]] <- nma.naive

##medical - secondary outcome

evaluate::evaluate(file("R/nma_medical_morbidity.R"))

analysis_output[["NMA_medical_morbidity"]][["CNMA"]] <- cnma

analysis_output[["NMA_medical_morbidity"]][["naive NMA"]] <- nma.naive

##meta-regression

evalate::evaluate(file("R/meta-regression.R"))

analysis_output[["meta-regression"]][["univariable_recurrence"]] <- univar_primary

analysis_output[["meta-regression"]][["multivariable_recurrence"]] <- multi_primary

analysis_output[["meta-regression"]][["univariable_morbidity"]] <- univar_secondary

analysis_output[["meta-regression"]][["multivariable_morbidity"]] <- multi_secondary

##other plots

evaluate::evaluate(file("R/other_plots.R"))

