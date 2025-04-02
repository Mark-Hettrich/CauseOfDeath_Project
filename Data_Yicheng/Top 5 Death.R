library(dplyr)
library(readxl)
Death_Data_ICD_Age_Sex_Urban_ <- read_excel("Top_5_Death/Death Data(ICD_Age_Sex_Urban).xlsx")
View(Death_Data_ICD_Age_Sex_Urban_)
df <- Death_Data_ICD_Age_Sex_Urban_

df_Diseases_of_heart <- df %>%
  filter(`ICD Sub-Chapter Code` %in% c("I05-I09", "I11", "I13", "I20-I25", "I26-I28", "I30-I51")) %>%
  group_by(`Single-Year Ages Code`, `Sex`, `Sex Code`,`Urbanization` ,`Urbanization Code`) %>%
  summarise(Deaths = sum(Deaths), .groups = "drop") %>%
  mutate(`Cause of Death` = "Diseases of heart")%>%
  relocate(`Cause of Death`, .before = 1) 

df_Malignant_neoplasms <- df %>%
  filter(`ICD Sub-Chapter Code` %in% c("C00-C97")) %>%
  group_by(`Single-Year Ages Code`, `Sex`, `Sex Code`,`Urbanization` ,`Urbanization Code`) %>%
  summarise(Deaths = sum(Deaths), .groups = "drop") %>%
  mutate(`Cause of Death` = "Malignant neoplasms")%>%
  relocate(`Cause of Death`, .before = 1) 

df_Accidents <- df %>%
  filter(`ICD Sub-Chapter Code` %in% c("V01-V99", "W00-X59")) %>%
  group_by(`Single-Year Ages Code`, `Sex`, `Sex Code`,`Urbanization` ,`Urbanization Code`) %>%
  summarise(Deaths = sum(Deaths), .groups = "drop") %>%
  mutate(`Cause of Death` = "Accidents (unintentional injuries)")%>%
  relocate(`Cause of Death`, .before = 1) 

df_COVID <- df %>%
  filter(`ICD Sub-Chapter Code` %in% c("U00-U49")) %>%
  group_by(`Single-Year Ages Code`, `Sex`, `Sex Code`,`Urbanization` ,`Urbanization Code`) %>%
  summarise(Deaths = sum(Deaths), .groups = "drop") %>%
  mutate(`Cause of Death` = "COVID-19")%>%
  relocate(`Cause of Death`, .before = 1) 

df_Cerebrovascular <- df %>%
  filter(`ICD Sub-Chapter Code` %in% c("I60-I69")) %>%
  group_by(`Single-Year Ages Code`, `Sex`, `Sex Code`,`Urbanization` ,`Urbanization Code`) %>%
  summarise(Deaths = sum(Deaths), .groups = "drop") %>%
  mutate(`Cause of Death` = "Cerebrovascular diseases")%>%
  relocate(`Cause of Death`, .before = 1) 

Top_5_Death <- rbind(df_Diseases_of_heart, df_Malignant_neoplasms, df_Accidents, df_COVID, df_Cerebrovascular)
