library(dplyr)
library(readxl)
Death_age_sex_race <- read_excel("Top_5_Death/Death_age_sex_race.xlsx")
View(Death_age_sex_race)
df <- Death_age_sex_race

df_Diseases_of_heart <- df %>%
  filter(`ICD Sub-Chapter Code` %in% c("I05-I09", "I11", "I13", "I20-I25", "I26-I28", "I30-I51")) %>%
  group_by(`Single-Year Ages Code`, `Sex`, `Sex Code`,`Single Race 6` ) %>%
  summarise(Deaths = sum(Deaths), Population = sum(Population),.groups = "drop") %>%
  mutate(`Cause of Death` = "Diseases of heart")%>%
  relocate(`Cause of Death`, .before = 1) 