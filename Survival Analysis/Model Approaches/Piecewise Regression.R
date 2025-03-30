file_path <- here("Survival Analysis", "test_data2.csv")
df <- read_csv(file_path)
df <- df[-1]
df
summary(df) #keine Population Anzahl über 85

# Prep data
df <- df %>%
  mutate(
    age = as.numeric(`Single-Year Ages Code`),                        
    Sex = factor(Sex),
    ICD_Chapter = factor(`ICD Chapter`),
    Population = as.numeric(Population)
  )


agg <- aggregate(cbind(Deaths, Population) ~ ICD_Chapter, data = df, sum)
agg$DeathRate <- agg$Deaths / agg$Population
ref_rate <- agg$DeathRate[agg$ICD_Chapter == "Codes for special purposes"]
agg$RateRatio_vs_Special <- agg$DeathRate / ref_rate



## Piecewise Exponential Model / Poisson Regression for Grouped Survival Data, maybe with age groups?
model1 <- glm(Deaths ~ Sex + ICD_Chapter + age, 
              family = poisson(link = "log"), 
              offset = log(as.numeric(Population)), #problem with using Population as offset: we dont have the population for ages 85-100+
              data = df)
summary(model1) # Very high Dispersion (overdispersion), --> we switch to negative binomial model

round(exp(coefficients(model1)), 1)
# Compare with:
agg


model2 <- glm.nb(Deaths ~ Sex + ICD_Chapter + age + offset(log(Population)), data = df)
summary(model2) 
round(exp(coefficients(model2)), 1) # high jump for external causes, more young people die compared to other causes, so baseline is higher
model2.2 <- glm.nb(Deaths ~ Sex + ICD_Chapter * age + offset(log(Population)), data = df) #interaction included
summary(model2.2)
round(exp(coefficients(model2.2)), 1) # Worse. Need to fix overreaction, maybe center age variable?

# Split up Death causes

causes <- unique(df$ICD_Chapter)
# Empty list to store models
nb_models <- list()

for (cause in causes) {
  # Filter data for this cause
  df_cause <- subset(df, ICD_Chapter == cause)
  
  # Fit NB model (log-rate of deaths)
  nb_models[[cause]] <- glm.nb(
    Deaths ~ Sex + age + offset(log(Population)),
    data = df_cause
  )
  
  # Output summary
  cat("\n---", cause, "---\n")
  print(summary(nb_models[[cause]]))
}
