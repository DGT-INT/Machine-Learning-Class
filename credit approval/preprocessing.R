library(dplyr)
library(readr)

credit1 <- read_csv("credit_application.csv")


factor_to_int <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "Missing"
  as.integer(factor(x, levels = unique(x))) - 1
}

# Aggregate STATUS history into features per client
credit_status_summary <- credit1 %>%
  group_by(ID) %>%
  summarise(
    n_months = n(),
    n_paid_off = sum(STATUS == "C", na.rm = TRUE),
    n_no_loan = sum(STATUS == "X", na.rm = TRUE),
    n_overdue_1_29 = sum(STATUS == "0", na.rm = TRUE),
    n_overdue_30_59 = sum(STATUS == "1", na.rm = TRUE),
    n_overdue_60_89 = sum(STATUS == "2", na.rm = TRUE),
    n_overdue_90_119 = sum(STATUS == "3", na.rm = TRUE),
    n_overdue_120_149 = sum(STATUS == "4", na.rm = TRUE),
    n_overdue_150plus = sum(STATUS == "5", na.rm = TRUE),
    ever_defaulted = as.integer(any(STATUS == "5", na.rm = TRUE)),
    worst_status = max(as.numeric(STATUS[STATUS %in% c("0","1","2","3","4","5")]), na.rm = TRUE),
    recent_status = STATUS[which.max(MONTHS_BALANCE)]
  )

# Merge with most recent demographic record
credit_demo <- credit1 %>%
  group_by(ID) %>%
  filter(MONTHS_BALANCE == max(MONTHS_BALANCE, na.rm = TRUE)) %>%
  ungroup()

credit_demo <- credit_demo %>%
  mutate(
    AGE_YEARS = round(abs(DAYS_BIRTH) / 365.25, 2),
    EMPLOYED_YEARS = round(abs(DAYS_EMPLOYED) / 365.25, 2),
    OCCUPATION_TYPE = factor_to_int(OCCUPATION_TYPE),
    NAME_FAMILY_STATUS = factor_to_int(NAME_FAMILY_STATUS),
    NAME_EDUCATION_TYPE = factor_to_int(NAME_EDUCATION_TYPE),
    NAME_INCOME_TYPE = factor_to_int(NAME_INCOME_TYPE),
    NAME_HOUSING_TYPE = factor_to_int(NAME_HOUSING_TYPE),
    CODE_GENDER = ifelse(CODE_GENDER == "F", 1,
                         ifelse(CODE_GENDER == "M", 0, NA_integer_)),
    FLAG_OWN_CAR = ifelse(FLAG_OWN_CAR == "Y", 1,
                          ifelse(FLAG_OWN_CAR == "N", 0, NA_integer_)),
    FLAG_OWN_REALTY = ifelse(FLAG_OWN_REALTY == "Y", 1,
                             ifelse(FLAG_OWN_REALTY == "N", 0, NA_integer_))
  )

# Join summary features with demographics
credit_final1 <- credit_demo %>%
  select(ID, CODE_GENDER, FLAG_OWN_CAR, FLAG_OWN_REALTY, NAME_INCOME_TYPE, NAME_EDUCATION_TYPE, NAME_FAMILY_STATUS, NAME_HOUSING_TYPE, OCCUPATION_TYPE, AGE_YEARS, EMPLOYED_YEARS, , everything()) %>%
  left_join(credit_status_summary, by = "ID")

# View the processed data
head(credit_final1)
