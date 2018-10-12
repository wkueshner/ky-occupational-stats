library(tidyverse)
library(ggthemes)
library(ggrepel)

load(file = "rdas/bls.rda")

#mathematical and computer professions data
blstech <- bls %>% 
  filter(bls$OCC_CODE > 150000, bls$OCC_CODE < 160000,
         PRIM_STATE == "KY", AREA_NAME == "Lexington-Fayette, KY", 
         OCC_TITLE != "Operations Research Analysts") 

#note that geom_text_repel from ggrepel package does the work of fitting text on the plot panel
blstech %>%
  ggplot(aes(A_MEAN, location_quotient)) +
  geom_point() +
  geom_text_repel(aes(label = OCC_TITLE), vjust = -1, size = 2.5) +
  labs(x= "annual salary", y = "location quotient", 
       title = "Location quotient vs. Annual Salary for Math & 
       Computer Occupations Lexington-Fayette") +
  theme_economist()  + 
  xlim(40000, 90000) + ylim(.2, 1.05)


#lexington general data
blslex <- bls %>% filter(PRIM_STATE == "KY", AREA_NAME == "Lexington-Fayette, KY", 
                         OCC_GROUP == "major") %>%
  arrange(desc(location_quotient))

fit <- lm(blslex$location_quotient~blslex$A_MEAN)

#here we plot the average annual salary against the location quotient for the major occ. groups
blslex %>% ggplot(aes(A_MEAN, location_quotient)) +
  geom_point() +
  geom_abline(intercept = fit$coefficients[1], slope = fit$coefficients[2]) +
  labs(x= "annual salary", y = "location quotient", 
       title = "Location quotient vs. Annual Salary In Lexington-Fayette") +
  geom_vline(xintercept = 44564)
  theme_economist()

#The upper outliers we see here are "Farming, Fishing, and Forestry Occupations" at a 1.94 
#location quotient and "Production Occupations" at a 1.63 location quotient. 


