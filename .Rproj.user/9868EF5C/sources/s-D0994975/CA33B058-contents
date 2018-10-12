#In this R file, we perform an analysis and visualization of 1) general occupational data
#for lexington, and 2) mathematical and computer occupations data for lexington

library(tidyverse)
library(ggthemes)
library(ggrepel)

load(file = "rdas/bls.rda")

##part 1) Lexington general occupational data

#Preliminary: what's the location quotient (lq)?
#The location quotient for occupations are calculated by dividing the regional concentration
#of a particular occupation by the national concentration of that same occupation. Thus,
#an occupation with an lq>1 for a given regionis more concentrated than to the national
#average, and with lq<1 is less concentrated than the national average.

#High-LQ Occupations are particularly notable because they provide a work-force oriented
#perspective of a regions economic base.


blslex<- bls %>% filter(PRIM_STATE == "KY", AREA_NAME == "Lexington-Fayette, KY", 
                         OCC_GROUP == "major",
                         lq <= mean(lq, na.rm = TRUE) + 4*sd(lq, na.rm = TRUE),
                         lq >= mean(lq, na.rm = TRUE) - 4*sd(lq, na.rm = TRUE),
                         OCC_TITLE != "Farming, Fishing, and Forestry Occupations") %>% #note this
  arrange(desc(lq))

blslex_farm <- bls %>% filter(PRIM_STATE == "KY", AREA_NAME == "Lexington-Fayette, KY", 
                        OCC_GROUP == "major",
                        lq <= mean(lq, na.rm = TRUE) + 4*sd(lq, na.rm = TRUE),
                        lq >= mean(lq, na.rm = TRUE) - 4*sd(lq, na.rm = TRUE)) %>% #note this
  arrange(desc(lq))

mean(blslex_farm$lq) - mean(blslex$lq) #gives us difference in the aggregate location quotient
#give this to john along with the different graph

#Remove last world "Occupations" (1 space + 11 letters) from every character string in occ_title
blslex$OCC_TITLE <- (gsub('.{12}$', '', blslex$OCC_TITLE))

fit <- lm(blslex$lq~blslex$A_MEAN)

#here we plot the average annual salary against the location quotient for the major occ. groups

blslex %>% ggplot(aes(A_MEAN, lq)) +
  geom_point() +
  geom_abline(intercept = fit$coefficients[1], slope = fit$coefficients[2]) +
  labs(x= "annual salary", y = "location quotient", 
       title = "Location quotient vs. Annual Salary in Lexington-Fayette \n (All Occupations)") +
  #geom_vline(xintercept = 44564) +
  geom_text_repel(aes(label = OCC_TITLE), size = 2) +
  theme_hc()

ggsave("plots/annual_salary_lq_farm_removed.png")

#The upper outliers we see here are "Farming, Fishing, and Forestry Occupations" at a 1.94 
#location quotient and "Production Occupations" at a 1.63 location quotient. 
#The vertical linerepresents the American average income of 44,564 (Q4 2017).


##Part 2) math and computer occupations analysis

#creating a mathematical and computer professions-focused data frame
blstech <- bls %>% 
  filter(bls$OCC_CODE > 150000, bls$OCC_CODE < 160000,
         PRIM_STATE == "KY", AREA_NAME == "Lexington-Fayette, KY",
         ) 

#note that geom_text_repel from ggrepel package does the work of fitting text on the plot panel
blstech %>%
  ggplot(aes(A_MEAN, lq)) +
  geom_point() +
  geom_text_repel(aes(label = OCC_TITLE), vjust = -1, size = 3) +
  theme_economist()  + 
  labs(x= "annual salary", y = "location quotient", 
       title = "Location quotient vs. Annual Salary for Math & 
       Computer Occupations Lexington-Fayette") +
  xlim(40000, 90000) + ylim(.2, 1.05)

ggsave("plots/math_computer_salaries_vs_lq.png")

#job growth estimates graph--figuring out how to plot an outside image appropriately
library(png)
im <- readPNG("plots/annualized_job_growth_estimates.png")
grid::grid.raster(im)
#Note that operations research analysts are not visualized 
#(upper outlier: they have a lq of 1.91 and a mean annual salary of $54,410)

#Pictures first, then report
#Census data on rent prices
