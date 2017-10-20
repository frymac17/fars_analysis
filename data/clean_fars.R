
library(dplyr)
library(tidyr)
library(readr)
library(maps)
library(purrr)
library(knitr)


load("data/clean_fars.RData")

dim(clean_fars)

length(unique(clean_fars$unique_id))

summary(clean_fars)


clean_fars %>%
  mutate(year_cat = cut(year, breaks = c(1999, 2002, 2006, 2010),
                        labels = c("1999-2002", "2003-2006",
                                   "2007-2010"),
                        include.lowest = TRUE, right = TRUE)) %>%
  filter(!is.na(sex)) %>%
  group_by(drug_type, sex, year_cat) %>%
  summarize(n_non_missing = sum(!is.na(positive_for_drug)),
            positive_test = sum(positive_for_drug, na.rm = TRUE),
            perc_positive = round(100 * positive_test / n_non_missing, 1)) %>%
  select(drug_type, sex, year_cat, perc_positive) %>%
  unite(sex_year_cat, sex, year_cat) %>%
  spread(sex_year_cat, perc_positive) %>%
  kable(col.names = c("Drug type", "F 1999-2002",
                      "F 2003-2006", "F 2007-2010",
                      "M 1999-2002", "M 2003-2006",
                      "M 2007-2010"))
