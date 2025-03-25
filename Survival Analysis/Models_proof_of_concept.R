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
library(timereg)
library(mgcv)
library(cmprsk)
library(broom)
library(splines)
library(purrr)


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


# Piecewise Exponential Model / Poisson Regression for Grouped Survival Data, maybe with age groups?
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
expanded_deaths <- expanded_deaths %>% filter(!is.na(age))

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

expanded_deaths_wo_0 <- expanded_deaths %>% filter(age > 0) 
cox_model4 <- coxph(Surv(age, status) ~ Sex + ICD_Chapter + tt(Sex_num),
                    data = expanded_deaths_wo_0,
                    tt = function(x, t, ...) x * log(t)) #Time Varying Effect / Time varying covariates


expanded_deaths$age_group <- cut(expanded_deaths$age, breaks = c(0, 15, 30, 45, 60, 75, 90, 110)) #Split Age into Groups
cox_model5 <- coxph(Surv(age, status) ~ Sex * ICD_Chapter + strata(age_group), data = expanded_deaths) 
cox.zph(cox_model5)#still problematic


# Additive Cox model
additive_data <- expanded_deaths
additive_data$status <- ifelse(runif(nrow(additive_data)) < 0.95, 1, 0)
sample_data <- additive_data[sample(nrow(additive_data), 5000), ]
sample_data$status <- ifelse(runif(nrow(sample_data)) < 0.7, 1, 0)
summary(sample_data)

additive_cox <- coxph(Surv(age, status) ~ pspline(age, df = 5) + Sex,
                 data = additive_data) #Model thinks Sex effect can be explained through age at death
seperate_splines <- coxph(Surv(age, status) ~ pspline(age, by = Sex) + Sex, data = additive_data)
summary(seperate_splines) # problematic, we dont have censoring

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


# Aalens additive hazards model
aalen_data <- expanded_deaths

test_data <- aalen_data[sample(nrow(expanded_deaths), 5000), ] #takes ages otherwise

test_fit <- aalen(Surv(age, status) ~ Sex, 
                  data = test_data)

plot(test_fit) 

## Competing Risk models: Include other causes of death as "others"!!

#Cumulative Incident Model (Fine-Gray)
expanded_deaths$ICD_numeric <- as.numeric(factor(expanded_deaths$ICD_Chapter))
cause_labels <- levels(factor(expanded_deaths$ICD_Chapter))

fg_fit <- crr(
  ftime = expanded_deaths$age,
  fstatus = expanded_deaths$ICD_numeric,  # should be numeric: 1 = cancer, 2 = heart...
  cov1 = model.matrix(~ Sex, data = expanded_deaths)[, -1]
) #overall: Sex has no significant effect for probability of death, lets look at the ICD Chapters individually

fg_models <- list()
# covariate matrix with Sex and age
X <- model.matrix(~ Sex + age, data = expanded_deaths)[, -1]  

for (k in 1:4) {
  fg_models[[k]] <- crr(
    ftime = expanded_deaths$age,
    fstatus = ifelse(expanded_deaths$ICD_numeric == k, 1, 2),
    cov1 = X
  )
  cat("\n---", cause_labels[k], "---\n")
  print(summary(fg_models[[k]]))
} #neoplasm interpretation compared with km plot: men dont live long enough to die from cancer, they tend to die first from other causes. Beauty of Competing Risk



# further analysis

# Convert fstatus and group
fstatus <- expanded_deaths$ICD_numeric
ftime <- expanded_deaths$age
group <- expanded_deaths$Sex

# Labels for plot legend
cause_labels <- levels(factor(expanded_deaths$ICD_Chapter))
sex_labels <- levels(expanded_deaths$Sex)

#cause-specific regression results
results <- lapply(seq_along(fg_models), function(k) {
  broom::tidy(fg_models[[k]], conf.int = TRUE) |>
    dplyr::mutate(
      Cause = cause_labels[k],
      term = gsub("cov1", "", term)
    )
})

# Bind and format results
results_df <- dplyr::bind_rows(results) |>
  dplyr::select(Cause, term, estimate, std.error, p.value, conf.low, conf.high) |>
  dplyr::mutate(
    sHR = exp(estimate),
    lower = exp(conf.low),
    upper = exp(conf.high)
  ) |>
  dplyr::select(Cause, term, sHR, lower, upper, p.value)
print(results_df)
# Fit CIF
cif_fit <- cuminc(ftime, fstatus, group = group)
# Number of curves
n_curves <- length(cif_fit)
# PLot Parameters
cause_colors <- c("red", "blue", "green", "purple")  # One per cause
sex_linetypes <- c("solid", "dashed")                # Female = solid, Male = dashed
colors <- rep(cause_colors, each = 2)                # 4 causes × 2 sexes
linetypes <- rep(sex_linetypes, times = 4)           # Female, Male repeated
# Plot CIF
plot(cif_fit, col = colors, lty = linetypes, xlab = "Age", ylab = "Cumulative incidence")
sex_labels <- c("Female", "Male")
cause_labels <- levels(factor(expanded_deaths$ICD_Chapter))
legend_labels <- as.vector(t(outer(cause_labels, sex_labels, paste, sep = " - ")))
legend("topleft", legend = legend_labels, col = colors, lty = linetypes, cex = 0.8)




# Create design matrix with spline for age (e.g., 4 df)
X_spline <- model.matrix(~ Sex + ns(age, df = 4), data = expanded_deaths)[, -1]


## Refit models with splines
fg_models_spline <- list()

for (k in 1:4) {
  fg_models_spline[[k]] <- crr(
    ftime = expanded_deaths$age,
    fstatus = ifelse(expanded_deaths$ICD_numeric == k, 1, 2),
    cov1 = X_spline
  )
  cat("\n---", cause_labels[k], "---\n")
  print(summary(fg_models_spline[[k]]))
}

# Define age range for plotting
age_seq <- seq(0, 100, by = 1)

# Generate spline basis for age (same as used in model)
spline_basis <- ns(age_seq, df = 4)

# Cause labels (adjust if needed)
cause_labels <- c(
  "Codes for Special Purposes",
  "Circulatory Diseases",
  "External Causes",
  "Neoplasms"
)

# Function to extract and compute spline effect
extract_age_effect <- function(model, label) {
  # Get spline coefficients (exclude intercept and sex)
  spline_coefs <- model$coef[grep("^ns\\(age", names(model$coef))]
  age_effect <- as.vector(spline_basis %*% spline_coefs)
  
  data.frame(
    Age = age_seq,
    Effect = age_effect,
    Cause = label
  )
}

# Apply to each model
df_spline_effects <- map2_dfr(
  fg_models_spline,
  cause_labels,
  extract_age_effect
)

# Plot all together with facets
ggplot(df_spline_effects, aes(x = Age, y = Effect)) +
  geom_line(size = 1.2, color = "#2E86C1") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ Cause, scales = "free_y") +
  labs(
    title = "Spline-Based Partial Effects of Age on CIF (Fine–Gray Models)",
    subtitle = "Each line represents the age effect for a specific cause of death",
    x = "Age (years)",
    y = "Effect on Log Subdistribution Hazard"
  ) +
  theme_minimal(base_size = 14)
