library(dplyr)
library(tidyr)
library(readr)
library(maps)
library(purrr)
library(knitr)
library(ggplot2)
library(scales)
library(tidyverse)


load("~/r_prog/data/fars_analysis/data/clean_fars.RData")

###Write a function for proportions and confidence intervals, split by drug class in
### 1999 and 2010###############################################################
perc_cis <- function(x, n) {
  prop <- x / n
  standard_error <- sqrt((prop * (1 - prop)) / n)
  upper_ci <- (prop + (1.96 * standard_error))
  lower_ci <- (prop - (1.96 * standard_error))
  prop_percent <- round((prop * 100), digits = 1)
  upper_ci_percent <- round((upper_ci * 100), digits = 1)
  lower_ci_percent <- round((lower_ci * 100), digits = 1)
  final_results <- paste0(prop_percent, "% (", lower_ci_percent, 
                          "%, ", upper_ci_percent, "%)")
  final_results
}

########################Testing for trend using Cochran-Armitage trend test######################
test_trend_ca <- function(drug, data = clean_fars) {
  if (drug == "Nonalcohol") {
    to_test <- data %>% 
      filter(drug_type != "Alcohol") %>%
      group_by(unique_id, year) %>% 
      summarize(positive_for_drug = any(positive_for_drug))  
  } else {
    to_test <- clean_fars %>%
      filter(drug_type == drug) 
  }
  to_test <- to_test %>% 
    group_by(year) %>%
    summarize(positive = sum(positive_for_drug, na.rm = TRUE),
              total_tests = sum(!is.na(positive_for_drug)))
  
  ca_alcohol <- prop.trend.test(x = to_test$positive,
                                n = to_test$total_tests)
  Z <- round(sqrt(ca_alcohol$statistic), digits = 1)
  p.value <- round(ca_alcohol$p.value, digits = 3)
  final_results <- data.frame(Z, p.value)
  tibble::remove_rownames(final_results)
  return(final_results)
}

test_trend_ca( drug = "Nonalcohol")


drug_list <- c("Alcohol", "Nonalcohol", "Narcotic", "Depressant",
               "Stimulant", "Cannabinoid", "Other")
drug_trend_tests_ca <- lapply(drug_list, test_trend_ca)
drug_trend_tests_ca <- dplyr::bind_rows(drug_trend_tests_ca) %>%
  dplyr::mutate(drug = drug_list) %>%
  dplyr::select(drug, Z, p.value)
drug_trend_tests_ca %>% knitr::kable()



########write a functionto fit the logistic model shown above to a specific drug category##########
test_trend_log_reg <- function(drug, data = clean_fars) {
  if (drug == "Nonalcohol") {
    to_test <- data %>% 
      filter(drug_type != "Alcohol") %>%
      group_by(unique_id, year) %>% 
      summarize(positive_for_drug = any(positive_for_drug))  
  } else {
    to_test <- clean_fars %>%
      filter(drug_type == drug) 
  }
  
  log_reg <- glm(positive_for_drug ~ year, data = to_test,
                 family = binomial(link = "logit"))
  log_reg_sum <- slice(broom::tidy(log_reg), 2)
  Z <- round(log_reg_sum$statistic, digits = 1)
  p.value <- round(log_reg_sum$p.value, digits = 3)
  final_results <- data.frame(Z, p.value)
  tibble::remove_rownames(final_results) 
  return(final_results)
}

drug_list <- c("Alcohol", "Nonalcohol", "Narcotic", "Depressant",
               "Stimulant", "Cannabinoid", "Other")
drug_trend_tests_log_reg <- lapply(drug_list, test_trend_log_reg)
drug_trend_tests_log_reg <- dplyr::bind_rows(drug_trend_tests_log_reg) %>%
  dplyr::mutate(drug = drug_list) %>%
  dplyr::select(drug, Z, p.value)
drug_trend_tests_log_reg %>% 
kable()