library(dplyr)
library(tidyr)
library(readr)
library(maps)
library(purrr)
library(knitr)
library(ggplot2)
library(scales)

load("../data/clean_fars.RData")


###Write a function for proportions and confidence intervals, split by drug class in
### 1999 and 2010
perc_cis<-function (df)
{
  clean_fars %>% 
    filter(year %in% c(1999,2010)) %>% 
    filter(!is.na(sex)) %>%
    group_by(drug_type, year) %>%
    summarize(n_non_missing = sum(!is.na(positive_for_drug)),
              positive_test = sum(positive_for_drug, na.rm = TRUE),
              perc_positive = round(100 * positive_test / n_non_missing, 1)) %>%
    mutate(p = positive_test / n_non_missing)%>% 
    mutate(pre_se = p * (1- p)) %>%
    mutate(pre_se2 = pre_se / n_non_missing) %>% 
    mutate(se = sqrt(pre_se2)) %>%
    mutate(CI_LOWER = (p - 1.96 * se))%>% 
    mutate(CI_UPPER = (p + 1.96 * se))%>%
    mutate(CI_LOWER = round(CI_LOWER, digits = 3)) %>% 
    mutate(CI_UPPER = round(CI_UPPER, digits = 3)) %>% 
    mutate(CI_UPPER = percent(CI_UPPER)) %>%
    mutate(CI_LOWER = percent(CI_LOWER)) %>% 
    mutate(p = round(p, digits = 3)) %>% 
    mutate(p = percent(p)) %>% 
    select(drug_type, year, p, CI_LOWER, CI_UPPER) %>% 
    rename(Estimate = p) %>% 
    unite(col = Confidence_Interval, CI_LOWER, CI_UPPER, sep = ", ") %>% 
    unite(col = Values, Estimate, Confidence_Interval, sep = "  ") %>% 
    spread(key = year, value = Values)
}
perc_cis()




## write function a function that will input the clean_fars data set and drug. The column will output
##a one row table of the Z statistic for the Cochran-Armitage trend over the 12 study years
## and one column for p-value



to_test <- clean_fars %>%
  filter(drug_type == "Alcohol") %>%
  group_by(year) %>%
  summarize(positive = sum(positive_for_drug, na.rm = TRUE),
            trials = sum(!is.na(positive_for_drug)))
ca_alcohol <- prop.trend.test(x = to_test$positive,
                              n = to_test$trials)
sqrt(ca_alcohol$statistic)

test_trend_ca(drug = "Stimulant", data = clean_fars)
test_trend_ca(drug = "Alcohol")
test_trend_ca(drug = "Nonalcohol")

  drug_list <- c("Alcohol", "Nonalcohol", "Narcotic", "Depressant",
                 "Stimulant", "Cannabinoid", "Other")
  drug_trend_tests_ca <- lapply(drug_list, test_trend_ca)
  drug_trend_tests_ca <- dplyr::bind_rows(drug_trend_tests_ca) %>%
    dplyr::mutate(drug = drug_list) %>%
    dplyr::select(drug, Z, p.value)
  drug_trend_tests_ca %>% knitr::kable()
  
  

### write a functionto fit the logistic model shown above to a specific drug category
