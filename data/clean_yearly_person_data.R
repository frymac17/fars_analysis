library(dplyr)
library(tidyr)
library(readr)
library(maps)
library(purrr)
library(knitr)


clean_yearly_person_file <- function(year) {
  # 1. Read data in.
  person_file <- paste0("data-raw/yearly_person_data/person_", year, ".csv")
  df <- read_csv(person_file)
  # 2. Convert all column names to lowercase.
  colnames(df) <- tolower(colnames(df))
  df <- df %>%
    # 3. Limit variables.
    select(st_case, veh_no, per_no, state, per_typ, lag_hrs, lag_mins,
                  inj_sev, age, alc_res, contains("drugres"), sex) %>% 

  %>%
    # 4. Limit to relevant `per_typ` and `inj_sev` values, then remove those variables.
    filter(per_typ == 1 & inj_sev == 4) %>%
    select(-per_typ, -inj_sev) %>%
    # 5. Create a `unique_id`. Note: to be unique, `year` needs to be pasted on.
    unite(unique_id, st_case, veh_no, per_no) %>%
    mutate(year = year,
                  unique_id = paste(unique_id, year, sep = "_")) %>%
    # 6. Limit to study states and then remove the `state` variable.
    filter(state %in% c(6,
                               15,
                               17,
                               33,
                               44,
                               54)) %>%
    select(-state) %>%
    # 7. Convert `sex` to a factor with levels "Male" and "Female".
    mutate(sex = ifelse(sex == 9, NA, sex),
                  sex = factor(sex, levels = c(1, 2),
                               labels = c("Male", "Female"))) %>%
    # 8. Use measured alcohol blood level to create `Alcohol` (logical for whether
    # alcohol was present). Then remove the `alc_res` variable.
   mutate(alc_res = ifelse(alc_res > 94, NA, alc_res / 10),
                  Alcohol = alc_res >= 0.01) %>%
   select(-alc_res) %>%
    # 9. Specify missing values for the lag minutes.
   mutate(lag_mins = ifelse(lag_mins == 99, NA, lag_mins))
  # 10. Save lag hours coded as missing as `NA`.
  if(year <= 2008){
    df <- df %>%
    mutate(lag_hrs = ifelse(lag_hrs %in% c(99, 999), NA, lag_hrs))
  } else {
    df <- df %>%
    mutate(lag_hrs = ifelse(lag_hrs == 999, NA, lag_hrs))
  }
  
  # 11. Limit to deaths within an hour of the accident then remove those variables.
  df <- df %>%
    filter((lag_hrs < 1) | (lag_hrs == 1 & lag_mins == 0)) %>%
    select(-lag_hrs, -lag_mins)
  # 12. Save age values coded as missing as `NA`.
  if(year <= 2008){
    df <- df %>%
     mutate(age = ifelse(age == 99, NA, age))
  } else {
    df <- df %>%
     mutate(age = ifelse(age %in% c(998, 999), NA, age))
  }
  # 13. Use age to create age categories and then remove `age` variable.
  df <- df %>%
    mutate(agecat = cut(age, breaks = c(0, 25, 45, 65, 1000),
                               labels = c("< 25 years",
                                          "25--44 years",
                                          "45--64 years",
                                          "65 years +"),
                               include.lowest = TRUE, right = FALSE)) %>%
    select(-age)
  # 14. Gather all the columns with different drug listings (i.e., `drugres1`,
  # `drugres2`, `drugres3`). Convert from the numeric code listings to
  # drug categories.
  gathered_df <- df %>%
    gather(drug_number, drug_type_raw, contains("drugres")) %>%
    mutate(drug_type = ifelse(drug_type_raw %in% 100:295,
                                     "Narcotic", NA),
                  drug_type = ifelse(drug_type_raw %in% 300:395,
                                     "Depressant", drug_type),
                  drug_type = ifelse(drug_type_raw %in% 400:495,
                                     "Stimulant", drug_type),
                  drug_type = ifelse(drug_type_raw %in% 600:695,
                                     "Cannabinoid", drug_type),
                  drug_type = ifelse(drug_type_raw %in% c(500:595, 700:996),
                                     "Other", drug_type),
                  drug_type = ifelse(drug_type_raw == 1,
                                     "None", drug_type),
                  drug_type = factor(drug_type)) %>%
    select(-drug_type_raw, -drug_number) %>%
    # 15. Filter out any observations where both alcohol and drug data is missing.
    filter(!(is.na(Alcohol) & is.na(drug_type)))
  # 16. Create a subset with only individuals with at least one non-missing
  # listing for drugs. (Write a sentence or two for each step in this pipe chain.)
  non_missing_drugs <- gathered_df %>%
    filter(!is.na(drug_type)) %>%
    group_by(unique_id, drug_type) %>%
    summarize(has_drug = TRUE) %>%
    
    mutate(row_num = 1:n()) %>%
    spread(drug_type, has_drug, fill = FALSE) %>%
    select(-row_num)
  # 17. Join this back into the full dataset. (Write a sentence or two for each
  # step in this pipe chain.)
  df <- df %>%
    select(-contains("drugres")) %>%
    full_join(non_missing_drugs, by = "unique_id") %>%
    select(-None) %>%
    gather(drug_type, positive_for_drug, Alcohol, Cannabinoid,
                  Depressant, Narcotic, Other, Stimulant) %>%
    mutate(drug_type = factor(drug_type)) %>%
    unique()
  return(df)
}



clean_fars <- map_df(1999:2010, clean_yearly_person_file)
save(clean_fars, file = "~/r_prog/data/fars_analysis/data/clean_fars.RData")


clean_fars <- map_df(1999:2010, clean_yearly_person_file)
save(clean_fars, file = "data/clean_fars.RData")

