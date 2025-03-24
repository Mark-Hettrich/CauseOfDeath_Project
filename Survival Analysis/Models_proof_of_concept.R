#Packages:
library(survival)
library(survminer)
library(dplyr)
library(readr)
library(here)
library(nnet)
library(ggplot2)
library(MASS)
library(flexsurv)

file_path <- here("Survival Analysis", "test_data2.csv")
df <- read_csv(file_path)
df <- df[-1]
df
View(df)


# Prep data
df <- df %>%
  filter(!is.na(Deaths), !is.na(Population)) %>%
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


# Piecewise Exponential Model / Poisson Regression for Grouped Survival Data
model1 <- glm(Deaths ~ Sex + ICD_Chapter + age, 
    family = poisson(link = "log"), 
    offset = log(as.numeric(Population)),
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


# mulitnomial model

#Prepping Comparison:
cause_totals <- aggregate(Deaths ~ ICD_Chapter, data = df, sum)
total_deaths <- sum(cause_totals$Deaths)
cause_totals$Probability <- cause_totals$Deaths / total_deaths

#Model:
multi_model <- multinom(ICD_Chapter ~ Sex + age, data = df, weights = Deaths)

# Predict for values
newdata <- data.frame(Sex = "Male", age = 65)
#Prepping Comparison for example
subset_65M <- subset(df, Sex == "Male" & age == 65)
cause_totals_65M <- aggregate(Deaths ~ ICD_Chapter, data = subset_65M, sum)
total_deaths_65M <- sum(cause_totals_65M$Deaths)
cause_totals_65M$Probability <- cause_totals_65M$Deaths / total_deaths_65M

# Predict probabilities for each cause
predict(multi_model, newdata = newdata, type = "probs")
#Compare with Comparison, all Data:
cause_totals
#Compare with example:
cause_totals_65M



# competing Risk model:


# Survival Analysis Model
#   - Cox: relative hazards: How fast does Group die off compared to another
df$status <- 1
cox_model <- coxph(Surv(age, status) ~ Sex + ICD_Chapter, data = df)
summary(cox_model)

# KM model
survfit(Surv(age, status) ~ Sex + ICD_Chapter, data = df)

#comparison
aggregate(age ~ Sex + ICD_Chapter, data = df, FUN = function(x) round(mean(x), 1))
aggregate(age ~ Sex + ICD_Chapter, data = df, FUN = function(x) round(median(x), 1))

ggplot(df, aes(y = Deaths, x = age, fill = Sex)) +
  geom_smooth()
  labs(
    title = "Age at Death by ICD Chapter and Sex",
    x = "ICD Chapter",
    y = "Age at Death"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10),
    legend.position = "top"
  ) ## :( 
  
  # --> Cox Model doenst include death counts, only counts
  # --> Split up Deathcount into rows, each row = 100 Deaths for easier computation
  reduced_deaths <- df %>%
    mutate(Deaths = Deaths / 100) %>%
    mutate(Deaths = round(Deaths))
  
expanded_deaths <- reduced_deaths %>%
  filter(Deaths > 0) %>%
  rowwise() %>%
  do(data.frame(
    age = rep(.$age, .$Deaths),
    Sex = rep(.$Sex, .$Deaths),
    ICD_Chapter = rep(.$ICD_Chapter, .$Deaths),
    status = 1
  ))
expanded_deaths$Sex_num <- ifelse(expanded_deaths$Sex == "Male", 1, 0)

cox_model <- coxph(Surv(age, status) ~ Sex + ICD_Chapter, data = expanded_deaths)
summary(cox_model) #decent, not great


km_fit <- survfit(Surv(age, status) ~ Sex + ICD_Chapter, data = expanded_deaths)
# Kaplan Meier curves:
ggsurvplot(km_fit, data = expanded_deaths, 
           facet.by = "ICD_Chapter",
           risk.table = TRUE,
           conf.int = TRUE,
           legend.title = "Sex",
           title = "Survival Curves by Cause of Death and Sex")

# Do males die earlier than females within each ICD_Chapter
cox_model2 <- coxph(Surv(age, status) ~ Sex * ICD_Chapter, data = expanded_deaths)
summary(cox_model2)

#Potential Problem with Cox:
#Cox Proportional Hazards Assumptions (PH): The hazard ratio between any two groups is constant over time (i.e., age).
cox.zph(cox_model) # problematic
cox.zph(cox_model2) # problematic
# --> Problem :(

# Possible Solutions: still have to look into it more / havent tested
cox_model3 <- coxph(Surv(age, status) ~ Sex + strata(ICD_Chapter), data = expanded_deaths) # Stratify by Violation prone Variables
cox.zph(cox_model3) #still problematic


cox_model4 <- coxph(Surv(age, status) ~ Sex + ICD_Chapter + tt(Sex),  #doesnt work becasue of log(0) for age = 0
      data = expanded_deaths,
      tt = function(x, t, ...) x * log(t)) #Time Varying Effect / Time variying covariates

expanded_deaths_wo_0 <- expanded_deaths %>% filter(age > 0) 
cox_model4 <- coxph(Surv(age, status) ~ Sex + ICD_Chapter + tt(Sex_num),
                    data = expanded_deaths_wo_0,
                    tt = function(x, t, ...) x * log(t))


expanded_deaths$age_group <- cut(expanded_deaths$age, breaks = c(0, 15, 30, 45, 60, 75, 90, 110)) #Split Age into Groups
cox_model5 <- coxph(Surv(age, status) ~ Sex * ICD_Chapter + strata(age_group), data = expanded_deaths) 
cox.zph(cox_model5)#still problematic



## Accelerated Failure Time

aft_data <- expanded_deaths %>%
  filter(!is.na(age) & age > 0)
# Weibull
aft_weibull <- survreg(Surv(age, status) ~ Sex * ICD_Chapter, data = aft_data, dist = "weibull")

# Log-normal
aft_lognormal <- survreg(Surv(age, status) ~ Sex * ICD_Chapter, data = aft_data, dist = "lognormal")

# Log-logistic
aft_loglogistic <- survreg(Surv(age, status) ~ Sex * ICD_Chapter, data = aft_data, dist = "loglogistic")

# Exponential
aft_exponential <- survreg(Surv(age, status) ~ Sex * ICD_Chapter, data = aft_data, dist = "exponential")

#compare with AIC
AIC(aft_weibull, aft_lognormal, aft_loglogistic, aft_exponential)

#compare log.likelihoods
logLik(aft_weibull)
logLik(aft_lognormal)
logLik(aft_loglogistic)
logLik(aft_exponential)

# interpret coefficients
summary(aft_weibull)
exp(coef(aft_weibull))  # Time ratios

# Use other models: idk which
