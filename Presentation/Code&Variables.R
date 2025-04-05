others <- 18912824 - sum(top5$Deaths)
new_row <- data.frame()
Death_Causes <- rbind(top5, new_row)

death_causes <- tibble(
  `Causes of Death` = c(
    "Diseases of heart (I00-I09,I11,I13,I20-I51)",
    "Malignant neoplasms (C00-C97)",
    "Accidents (unintentional injuries) (V01-X59,Y85-Y86)",
    "COVID-19 (U07.1)",
    "Cerebrovascular diseases (I60-I69)",
    "Others"
  ),
  `Cause of Death Codes` = c(
    "GR113-054",
    "GR113-019",
    "GR113-112",
    "GR113-137",
    "GR113-070",
    "All others"
  ),
  Deaths = c(4090792, 3628161, 1215794, 1004208, 949001, 8024868),
  Population = rep(1984987277, 6),
  `Crude Rate` = round(c(206.0, 183.0, 61.2, 50.6, 47.8, 404.3), 1)
)
death_causes$`Crude Rate` <- sprintf("%.1f", as.numeric(df$`Crude Rate`))

#cat("Population differences by cause (cause total - pop_by_group total):")
#cat("Heart     :", heart_diff, "\n")


model_df <- df
