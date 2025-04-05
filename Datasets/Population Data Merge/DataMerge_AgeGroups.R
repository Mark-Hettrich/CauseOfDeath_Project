library(readr)
library(dplyr)
library(stringr)
library(here)

file_path <- here("Datasets", "Population Data Merge", "cps_00006.csv")
df <- read_csv(file_path)
unique(df$YEAR)
table(df$EDUC)

#####
###Adjusting IPUMS Dataset
#####
ipums_asec <- df %>% 
  filter(ASECFLAG == 1)
ipums_asec <- ipums_asec %>%
  mutate(Sex = ifelse(SEX == 1, "Male", "Female"))

edu_grouped <- c(
  # 8th grade or less
  `1` = "8th grade or less",
  `2` = "8th grade or less",
  `10` = "8th grade or less",
  `20` = "8th grade or less",
  `30` = "8th grade or less",
  
  # 9th through 12th grade with no diploma
  `40` = "9th through 12th grade with no diploma",
  `50` = "9th through 12th grade with no diploma",
  `60` = "9th through 12th grade with no diploma",
  `71` = "9th through 12th grade with no diploma",
  
  # High school graduate or GED completed
  `73` = "High school graduate or GED completed",
  
  # Some college credit, but not a degree
  `81` = "Some college credit, but not a degree",
  
  # Associate degree
  `91` = "Associate degree (AA,AS)",
  `92` = "Associate degree (AA,AS)",
  
  # Bachelor’s degree
  `111` = "Bachelor’s degree (BA, AB, BS)",
  
  # Master’s degree
  `123` = "Master’s degree (MA, MS, MEng, MEd, MSW, MBA)",
  
  # Doctorate or professional degree
  `124` = "Doctorate (PhD, EdD) or Professional Degree (MD, DDS, DVM, LLB, JD)",
  `125` = "Doctorate (PhD, EdD) or Professional Degree (MD, DDS, DVM, LLB, JD)"
)

ipums_asec <- ipums_asec %>%
  mutate(Education = recode(as.character(EDUC), !!!edu_grouped))

ipums_asec %>% pull(ASECWT) %>% mean()

pop_by_group <- ipums_asec %>%
  mutate(age_group = case_when(
    AGE == 0 ~ "< 1 year",
    AGE >= 1 & AGE <= 4 ~ "1-4 years",
    AGE >= 5 & AGE <= 14 ~ "5-14 years",
    AGE >= 15 & AGE <= 24 ~ "15-24 years",
    AGE >= 25 & AGE <= 34 ~ "25-34 years",
    AGE >= 35 & AGE <= 44 ~ "35-44 years",
    AGE >= 45 & AGE <= 54 ~ "45-54 years",
    AGE >= 55 & AGE <= 64 ~ "55-64 years",
    AGE >= 65 & AGE <= 74 ~ "65-74 years",
    AGE >= 75 & AGE <= 84 ~ "75-84 years",
    AGE >= 85 ~ "85+ years",
    TRUE ~ "Not Stated"
  )) %>%
  group_by(age_group, Sex, Education) %>%
  summarise(population = sum(ASECWT, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(factor(age_group, levels = c(
    "< 1 year", "1-4 years", "5-14 years", "15-24 years",
    "25-34 years", "35-44 years", "45-54 years", "55-64 years",
    "65-74 years", "75-84 years", "85+ years", "Not Stated"
  )))

####Population Comparison & Adjustments
file_path <- here("Datasets", "Population Data Merge", "Age Group Data", "Population_Age_Sex.csv")
Population_Age_Sex <- read_csv(file_path)

cdc_age_sex <- Population_Age_Sex %>% filter(!is.na(Sex)) %>% filter(!(`Ten-Year Age Groups`== "Not Stated"))

sum(pop_by_group$population) # ~987 Mil
cdc_age_sex %>% # ~1000 Mil
  summarise(total_population = sum(as.numeric(Population), na.rm = TRUE))

####Adjusting Discrepancies

# Step 1: Summarise target population totals (cdc)
cdc_totals <- cdc_age_sex %>%
  group_by(`Ten-Year Age Groups`, Sex) %>%
  summarise(target_population = sum(as.numeric(Population), na.rm = TRUE), .groups = "drop")

# Step 2: Get current totals from pop_by_group
pop_by_group_totals <- pop_by_group %>%
  group_by(age_group, Sex) %>%
  summarise(current_population = sum(population, na.rm = TRUE), .groups = "drop")

# Step 3: Join and compute adjustment factors
adjustment_factors <- left_join(
  cdc_totals,
  pop_by_group_totals,
  by = c("Ten-Year Age Groups" = "age_group", "Sex")
) %>%
  mutate(adjustment_factor = target_population / current_population)

# Step 4: Apply adjustment to each row in pop_by_group
pop_by_group_adjusted <- pop_by_group %>%
  left_join(
    adjustment_factors %>% select(`Ten-Year Age Groups`, Sex, adjustment_factor),
    by = c("age_group" = "Ten-Year Age Groups", "Sex")
  ) %>%
  mutate(adjusted_population = population * adjustment_factor) %>%
  select(age_group, Sex, Education, adjusted_population)

######Control 
pop_by_group_adjusted %>% filter(age_group == "15-24 years" & Sex == "Female") %>% pull(adjusted_population) %>% sum() 
cdc_totals %>% filter(`Ten-Year Age Groups`== "15-24 years" & Sex == "Female")
sum(cdc_totals$target_population) ==sum(pop_by_group_adjusted$adjusted_population)

pop_summary <- pop_by_group_adjusted %>%
  group_by(age_group, Sex) %>%
  summarise(population = sum(adjusted_population, na.rm = TRUE), .groups = "drop")

all.equal(
  pop_summary %>%
    arrange(age_group, Sex) %>%
    select(age_group, Sex, population),
  
  cdc_totals %>%
    rename(age_group = `Ten-Year Age Groups`) %>%
    arrange(age_group, Sex) %>%
    select(age_group, Sex, target_population) %>%
    rename(population = target_population),
  
  tolerance = 1e-6
) # :)



########## Import & adjust Datasets
#Total Deaths
file_path <- here("Datasets", "Population Data Merge", "Age Group Data", "Totals.csv")
totals_df <- read_csv(file_path)
totals_df <- totals_df[-1]


totals_df <- totals_df %>%
  mutate(Deaths = ifelse(Deaths == "Suppressed", 5, Deaths),
         Deaths = as.numeric(Deaths)) %>%
  mutate(Sex = as.factor(Sex)) %>%
  filter(!(Education %in% c("Unknown or Not Stated", "Not Available"))) %>%
  filter(!(`Ten-Year Age Groups` == "Not Stated"))

totals_df <- totals_df %>%
  mutate(
    Education = str_replace_all(Education, fixed("?"), "’"),  # Fix apostrophes
    Education = str_replace_all(Education, "\\.$", ""),       # Remove trailing periods
    Education = str_trim(Education)                           # Remove extra white space
  )
pop_adjusted_renamed <- pop_by_group_adjusted %>%
  rename(
    `Ten-Year Age Groups` = age_group
  )
totals_df <- totals_df %>% 
  left_join(
  pop_adjusted_renamed,
  by = c("Ten-Year Age Groups", "Sex", "Education")
  )

# Heart
file_path <- here("Datasets", "Population Data Merge", "Age Group Data", "Heart.csv")
heart_df <- read_csv(file_path)[-1]

heart_df <- heart_df %>%
  mutate(Deaths = ifelse(Deaths == "Suppressed", 5, Deaths),
         Deaths = as.numeric(Deaths),
         Sex = as.factor(Sex)) %>%
  filter(!(Education %in% c("Unknown or Not Stated", "Not Available"))) %>%
  filter(!(`Ten-Year Age Groups` == "Not Stated")) %>%
  mutate(
    Education = str_replace_all(Education, fixed("?"), "’"),
    Education = str_replace_all(Education, "\\.$", ""),
    Education = str_trim(Education)
  ) %>%
  left_join(pop_by_group_adjusted %>% rename(`Ten-Year Age Groups` = age_group),
            by = c("Ten-Year Age Groups", "Sex", "Education"))

# Neoplasms
file_path <- here("Datasets", "Population Data Merge", "Age Group Data", "Neoplasms.csv")
neoplasms_df <- read_csv(file_path)[-1]

neoplasms_df <- neoplasms_df %>%
  mutate(Deaths = ifelse(Deaths == "Suppressed", 5, Deaths),
         Deaths = as.numeric(Deaths),
         Sex = as.factor(Sex)) %>%
  filter(!(Education %in% c("Unknown or Not Stated", "Not Available"))) %>%
  filter(!(`Ten-Year Age Groups` == "Not Stated")) %>%
  mutate(
    Education = str_replace_all(Education, fixed("?"), "’"),
    Education = str_replace_all(Education, "\\.$", ""),
    Education = str_trim(Education)
  ) %>%
  left_join(pop_by_group_adjusted %>% rename(`Ten-Year Age Groups` = age_group),
            by = c("Ten-Year Age Groups", "Sex", "Education"))

# Accidents
file_path <- here("Datasets", "Population Data Merge", "Age Group Data", "Accidents.csv")
accidents_df <- read_csv(file_path)[-1]

accidents_df <- accidents_df %>%
  mutate(Deaths = ifelse(Deaths == "Suppressed", 5, Deaths),
         Deaths = as.numeric(Deaths),
         Sex = as.factor(Sex)) %>%
  filter(!(Education %in% c("Unknown or Not Stated", "Not Available"))) %>%
  filter(!(`Ten-Year Age Groups` == "Not Stated")) %>%
  mutate(
    Education = str_replace_all(Education, fixed("?"), "’"),
    Education = str_replace_all(Education, "\\.$", ""),
    Education = str_trim(Education)
  ) %>%
  left_join(pop_by_group_adjusted %>% rename(`Ten-Year Age Groups` = age_group),
            by = c("Ten-Year Age Groups", "Sex", "Education"))

# Covid
file_path <- here("Datasets", "Population Data Merge", "Age Group Data", "Covid.csv")
covid_df <- read_csv(file_path)[-1]

covid_df <- covid_df %>%
  mutate(Deaths = ifelse(Deaths == "Suppressed", 5, Deaths),
         Deaths = as.numeric(Deaths),
         Sex = as.factor(Sex)) %>%
  filter(!(Education %in% c("Unknown or Not Stated", "Not Available"))) %>%
  filter(!(`Ten-Year Age Groups` == "Not Stated")) %>%
  mutate(
    Education = str_replace_all(Education, fixed("?"), "’"),
    Education = str_replace_all(Education, "\\.$", ""),
    Education = str_trim(Education)
  ) %>%
  left_join(pop_by_group_adjusted %>% rename(`Ten-Year Age Groups` = age_group),
            by = c("Ten-Year Age Groups", "Sex", "Education"))

# Cerebrovascular
file_path <- here("Datasets", "Population Data Merge", "Age Group Data", "Cerebrovascular.csv")
cerebrovascular_df <- read_csv(file_path)[-1]

cerebrovascular_df <- cerebrovascular_df %>%
  mutate(Deaths = ifelse(Deaths == "Suppressed", 5, Deaths),
         Deaths = as.numeric(Deaths),
         Sex = as.factor(Sex)) %>%
  filter(!(Education %in% c("Unknown or Not Stated", "Not Available"))) %>%
  filter(!(`Ten-Year Age Groups` == "Not Stated")) %>%
  mutate(
    Education = str_replace_all(Education, fixed("?"), "’"),
    Education = str_replace_all(Education, "\\.$", ""),
    Education = str_trim(Education)
  ) %>%
  left_join(pop_by_group_adjusted %>% rename(`Ten-Year Age Groups` = age_group),
            by = c("Ten-Year Age Groups", "Sex", "Education"))

# Others
file_path <- here("Datasets", "Population Data Merge", "Age Group Data", "Others.csv")
others_df <- read_csv(file_path)[-1]

others_df <- others_df %>%
  mutate(Deaths = ifelse(Deaths == "Suppressed", 5, Deaths),
         Deaths = as.numeric(Deaths),
         Sex = as.factor(Sex)) %>%
  filter(!(Education %in% c("Unknown or Not Stated", "Not Available"))) %>%
  filter(!(`Ten-Year Age Groups` == "Not Stated")) %>%
  mutate(
    Education = str_replace_all(Education, fixed("?"), "’"),
    Education = str_replace_all(Education, "\\.$", ""),
    Education = str_trim(Education)
  ) %>%
  left_join(pop_by_group_adjusted %>% rename(`Ten-Year Age Groups` = age_group),
            by = c("Ten-Year Age Groups", "Sex", "Education"))



sum(totals_df$adjusted_population, na.rm = TRUE)
sum(heart_df$adjusted_population, na.rm = TRUE)
sum(neoplasms_df$adjusted_population, na.rm = TRUE)
sum(accidents_df$adjusted_population, na.rm = TRUE)
sum(covid_df$adjusted_population, na.rm = TRUE)
sum(cerebrovascular_df$adjusted_population, na.rm = TRUE)
sum(others_df$adjusted_population, na.rm = TRUE)
#yay

#Add Causes
totals_df <- totals_df %>% mutate(Cause = "All Deaths")
heart_df <- heart_df %>% mutate(Cause = "Diseases of Heart")
neoplasms_df <- neoplasms_df %>% mutate(Cause = "Neoplasms")
accidents_df <- accidents_df %>% mutate(Cause = "Accidents")
covid_df <- covid_df %>% mutate(Cause = "COVID")
cerebrovascular_df <- cerebrovascular_df %>% mutate(Cause = "Cerebrovascular Diseases")
others_df <- others_df %>% mutate(Cause = "Others")


all_causes_df <- bind_rows(
  totals_df,
  heart_df,
  neoplasms_df,
  accidents_df,
  covid_df,
  cerebrovascular_df,
  others_df
)

all_causes_df <- all_causes_df %>%
  select(`Ten-Year Age Groups`, Sex, Education, Deaths, adjusted_population, Cause)

write.csv(all_causes_df, file = here("Datasets", "Population Data Merge", "Age Group Data", "All_Causes_Age_Groups10.csv"), row.names = FALSE)
