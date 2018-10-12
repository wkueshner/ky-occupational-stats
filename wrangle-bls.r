library(rio)
library(tidyverse)

#importing the BLS general labor statistics data
filename <- "us-bls-labor-statistics.csv"
bls <- import(filename)

#using basic regex to convert numeric data to numeric fields
bls$OCC_CODE <- as.numeric(gsub("-", "", bls$OCC_CODE))
bls$A_MEAN <- as.numeric(gsub(",", "", bls$A_MEAN))
bls$TOT_EMP <- as.numeric(gsub(",", "", bls$TOT_EMP))
bls$location_quotient <- as.numeric(bls$location_quotient)
bls$lq <- bls$location_quotient
#getting rid of duplicates
bls$location_quotient <- NULL

#saving data frame as an R data file
save(bls, file = "rdas/bls.rda")

#importing the BLS Educational data
ed <- import("education-2017.csv")

#Clean up
ed$a_mean <- as.numeric(gsub(",", "", ed$a_mean))
ed$tot_emp <- as.numeric(gsub(",", "", ed$tot_emp))
ed$pct_total <- as.numeric(ed$pct_total)

#Creating lex_ed
lex_ed <- ed %>% filter(area_name == "Lexington-Fayette, KY")
save(lex_ed, file = "rdas/lex_ed.rda")
