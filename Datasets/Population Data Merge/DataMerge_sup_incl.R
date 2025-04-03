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

pop_by_group <- ipums_asec %>%
  group_by(AGE, Sex, Education) %>%
  summarise(population = sum(ASECWT, na.rm = TRUE)) %>%
  ungroup()

View(pop_by_group)
#####
###Compariosn with CDC Data to validate
#####

pop_by_age_sex <- pop_by_group %>%
  group_by(AGE, Sex) %>%
  summarise(population = sum(population, na.rm = TRUE)) %>%
  ungroup()

sum(pop_by_age_sex$population) # 987,413,064 vs 1,000,096,197

#Individual Row Differences: 
file_path <- here("Datasets", "Population Data Merge", "Population_Age&Sex.csv")
Population_Age_Sex <- read_csv(file_path)
cdc_age_sex <- Population_Age_Sex %>% 
  mutate(`Single-Year Ages Code` = as.numeric(`Single-Year Ages Code`)) %>%
  filter(!is.na(Sex))


cdc_age_sex <- cdc_age_sex %>%
  mutate(
    `Single-Year Ages Code` = case_when(
      `Single-Year Ages Code` >= 80 & `Single-Year Ages Code` < 85 ~ 80,
      `Single-Year Ages Code` >= 85 ~ 85,
      TRUE ~ `Single-Year Ages Code`
    ),
    Population = as.numeric(Population)  # <-- fix is here
  ) %>%
  group_by(`Single-Year Ages Code`, Sex) %>%
  summarise(population = sum(Population, na.rm = TRUE)) %>%
  ungroup()

#Remove incomparable Datapoints
cdc_age_sex <- cdc_age_sex %>% filter(`Single-Year Ages Code` <= 80)
pop_by_age_sex <- pop_by_age_sex %>% filter(AGE <= 80)


round(cdc_age_sex$population - pop_by_age_sex$population)
mean(cdc_age_sex$population - pop_by_age_sex$population) #mean difference is 71k, cdc seems to overestimate Population number


#####
###Adding Population to cdc Data:
#####

# We will only include age >= 79, due to IPUMS Ages being coded as: 80 for ages 80-84 & 85 for ages 85+
pop_by_group <- pop_by_group %>% filter(AGE < 80)

file_path <- here("Data_Yicheng", "Death data 21_23", "Ages_Sex_Edu_2021_2023", "Diseases of heart.csv")
heart_df <- read_csv(file_path)
heart_df <- heart_df[-1]
heart_df <- heart_df %>%
  mutate(AGE = as.numeric(`Single-Year Ages Code`)) %>%
  mutate(Deaths = ifelse(Deaths == "Suppressed", 5, Deaths), Deaths = as.numeric(Deaths)) %>%
  mutate(Sex = as.factor(Sex)) %>%
  filter(AGE < 80)
heart_df <- heart_df %>% filter(!(Education %in% c("Unknown or Not Stated", "Not Available")))

heart_df <- heart_df %>%
  mutate(
    Education = str_replace_all(Education, fixed("?"), "’"),  # Fix apostrophes
    Education = str_replace_all(Education, "\\.$", ""),       # Remove trailing periods
    Education = str_trim(Education)                           # Remove extra white space
  )

heart_df <- heart_df %>%
  left_join(pop_by_group, by = c("AGE", "Sex", "Education"))

## Controlling process
sum(heart_df$population, na.rm = TRUE)
sum(pop_by_group$population)

missing_df <- full_join(
  pop_by_group,
  heart_df,
  by = c("AGE", "Sex", "Education"),
  suffix = c("_pop", "_heart")
) %>%
  filter(is.na(Deaths)) %>%
  arrange(desc(population_pop))

sum(missing_df$population_pop)
sum(heart_df$population, na.rm = TRUE) - sum(pop_by_group$population)
#mismatch fixed when if suppressed rows are included

# Neoplasms
neoplasms_df <- read_csv(here("Data_Yicheng", "Death data 21_23", "Ages_Sex_Edu_2021_2023", "Malignant neoplasms.csv"))
neoplasms_df <- neoplasms_df[-1]
neoplasms_df <- neoplasms_df %>%
  mutate(AGE = as.numeric(`Single-Year Ages Code`)) %>%
  mutate(`Single-Year Ages Code` = as.numeric(`Single-Year Ages Code`)) %>%
  mutate(Deaths = ifelse(Deaths == "Suppressed", 5, Deaths), Deaths = as.numeric(Deaths)) %>%
  mutate(Sex = as.factor(Sex)) %>%
  filter(AGE < 80) %>%
  filter(!(Education %in% c("Unknown or Not Stated", "Not Available"))) %>%
  mutate(
    Education = str_replace_all(Education, fixed("?"), "’"),
    Education = str_replace_all(Education, "\\.$", ""),
    Education = str_trim(Education)
  ) %>%
  left_join(pop_by_group, by = c("AGE", "Sex", "Education"))

# Accidents
accidents_df <- read_csv(here("Data_Yicheng", "Death data 21_23", "Ages_Sex_Edu_2021_2023", "Accidents.csv"))
accidents_df <- accidents_df[-1]
accidents_df <- accidents_df %>%
  mutate(AGE = as.numeric(`Single-Year Ages Code`)) %>%
  mutate(Deaths = ifelse(Deaths == "Suppressed", 5, Deaths), Deaths = as.numeric(Deaths)) %>%
  mutate(Sex = as.factor(Sex)) %>%
  filter(AGE < 80) %>%
  filter(!(Education %in% c("Unknown or Not Stated", "Not Available"))) %>%
  mutate(
    Education = str_replace_all(Education, fixed("?"), "’"),
    Education = str_replace_all(Education, "\\.$", ""),
    Education = str_trim(Education)
  ) %>%
  left_join(pop_by_group, by = c("AGE", "Sex", "Education"))

# COVID
covid_df <- read_csv(here("Data_Yicheng", "Death data 21_23", "Ages_Sex_Edu_2021_2023", "Covid-19.csv"))
covid_df <- covid_df[-1]
covid_df <- covid_df %>%
  mutate(AGE = as.numeric(`Single-Year Ages Code`)) %>%
  mutate(Deaths = ifelse(Deaths == "Suppressed", 5, Deaths), Deaths = as.numeric(Deaths)) %>%
  mutate(Sex = as.factor(Sex)) %>%
  filter(AGE < 80) %>%
  filter(!(Education %in% c("Unknown or Not Stated", "Not Available"))) %>%
  mutate(
    Education = str_replace_all(Education, fixed("?"), "’"),
    Education = str_replace_all(Education, "\\.$", ""),
    Education = str_trim(Education)
  ) %>%
  left_join(pop_by_group, by = c("AGE", "Sex", "Education"))

# Cerebro
cerebro_df <- read_csv(here("Data_Yicheng", "Death data 21_23", "Ages_Sex_Edu_2021_2023", "Cerebrovascular diseases.csv"))
cerebro_df <- cerebro_df[-1]
cerebro_df <- cerebro_df %>%
  mutate(AGE = as.numeric(`Single-Year Ages Code`)) %>%
  mutate(Deaths = ifelse(Deaths == "Suppressed", 5, Deaths), Deaths = as.numeric(Deaths)) %>%
  mutate(Sex = as.factor(Sex)) %>%
  filter(AGE < 80) %>%
  filter(!(Education %in% c("Unknown or Not Stated", "Not Available"))) %>%
  mutate(
    Education = str_replace_all(Education, fixed("?"), "’"),
    Education = str_replace_all(Education, "\\.$", ""),
    Education = str_trim(Education)
  ) %>%
  left_join(pop_by_group, by = c("AGE", "Sex", "Education"))

# Others
others_df <- read_csv(here("Data_Yicheng", "Death data 21_23", "Ages_Sex_Edu_2021_2023", "Other DeathCauses.csv"))
others_df <- others_df[-1]
others_df <- others_df %>%
  mutate(AGE = as.numeric(`Single-Year Ages Code`)) %>%
  mutate(Deaths = ifelse(Deaths == "Suppressed", 5, Deaths), Deaths = as.numeric(Deaths)) %>%
  mutate(Sex = as.factor(Sex)) %>%
  filter(AGE < 80) %>%
  filter(!(Education %in% c("Unknown or Not Stated", "Not Available"))) %>%
  mutate(
    Education = str_replace_all(Education, fixed("?"), "’"),
    Education = str_replace_all(Education, "\\.$", ""),
    Education = str_trim(Education)
  ) %>%
  left_join(pop_by_group, by = c("AGE", "Sex", "Education"))

#####
## Total population in pop_by_group for reference
#####
pop_ref <- sum(pop_by_group$population)

# Heart
heart_total <- sum(heart_df$population, na.rm = TRUE)
heart_diff <- heart_total - pop_ref

# Neoplasms
neoplasms_total <- sum(neoplasms_df$population, na.rm = TRUE)
neoplasms_diff <- neoplasms_total - pop_ref

# Accidents
accidents_total <- sum(accidents_df$population, na.rm = TRUE)
accidents_diff <- accidents_total - pop_ref

# COVID
covid_total <- sum(covid_df$population, na.rm = TRUE)
covid_diff <- covid_total - pop_ref

# Cerebro
cerebro_total <- sum(cerebro_df$population, na.rm = TRUE)
cerebro_diff <- cerebro_total - pop_ref

# Others
others_total <- sum(others_df$population, na.rm = TRUE)
others_diff <- others_total - pop_ref

# Display results
cat("Population differences by cause (cause total - pop_by_group total):\n")
cat("Heart     :", heart_diff, "\n")
cat("Neoplasms :", neoplasms_diff, "\n")
cat("Accidents :", accidents_diff, "\n")
cat("COVID     :", covid_diff, "\n")
cat("Cerebro   :", cerebro_diff, "\n")
cat("Others    :", others_diff, "\n")
# All avaliable Populations used

heart_df <- heart_df %>% mutate(Cause = "Diseases of Heart")
neoplasms_df <- neoplasms_df %>% mutate(Cause = "Neoplasms")
accidents_df <- accidents_df %>% mutate(Cause = "Accidents")
covid_df <- covid_df %>% mutate(Cause = "COVID")
cerebro_df <- cerebro_df %>% mutate(Cause = "Cerebrovascular Diseases")
others_df <- others_df %>% mutate(Cause = "Others")

######
## Final Data
#####
all_causes_df <- bind_rows(
  heart_df,
  neoplasms_df,
  accidents_df,
  covid_df,
  cerebro_df,
  others_df
)
write.csv(all_causes_df, file = here("Datasets", "Population Data Merge", "All_Causes_Combined_sup_incl.csv"), row.names = FALSE)
