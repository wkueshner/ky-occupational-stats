library(tidyverse, ggrepel, rio)

load("rdas/lex_ed.rda")

lex_ed %>% ggplot(aes(pct_total)) %>%
  coord_polar()



lex_ed <- mutate(lex_ed, education_category = factor(lex_ed$education_category, labels = c("Doctoral",
                                                     "Master's", "Bachelors", 'Associates',
                                                     "Some college w/ certificate", "Some college",
                                                     "High school diploma", "No formal education")))
                                                    

lex_ed %>% ggplot(aes(x = education_category, y = tot_emp)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))



