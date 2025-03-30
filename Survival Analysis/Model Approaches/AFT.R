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




aft_weibull <- survreg(Surv(age, status) ~ Sex * ICD_Chapter, data = aft_data, dist = "weibull")
newdata <- expand.grid(
  age = seq(min(aft_data$age), max(aft_data$age), by = 1),
  Sex = unique(aft_data$Sex),
  ICD_Chapter = unique(aft_data$ICD_Chapter)
)
# Extract model parameters
lp <- predict(aft_weibull, newdata = newdata, type = "lp")  # linear predictor
scale <- aft_weibull$scale  # this is σ
time <- newdata$age

# Compute survival using survreg's Weibull parameterization
surv_prob <- 1 - pweibull(time, shape = 1/scale, scale = exp(lp))
newdata$surv <- surv_prob
library(ggplot2)

  ggplot(newdata, aes(x = age, y = surv, color = Sex, linetype = ICD_Chapter)) +
  geom_line() +
  labs(
    title = "Survival Curve over Age (Weibull AFT)",
    y = "Survival Probability",
    x = "Age"
  ) +
  theme_minimal()

newdata <- expand.grid(
  age = seq(min(aft_data$age), max(aft_data$age), by = 1),
  Sex = unique(aft_data$Sex),
  ICD_Chapter = unique(aft_data$ICD_Chapter)
)
lp <- predict(aft_weibull, newdata = newdata, type = "lp")  # linear predictor
scale <- aft_weibull$scale
gamma <- 1 / scale
lambda <- exp(lp)
t <- newdata$age
hazard <- (gamma / lambda) * (t / lambda)^(gamma - 1)
newdata$hazard <- hazard
library(ggplot2)

newdata %>%
  filter(ICD_Chapter == "External causes of morbidity and mortality") %>%
  ggplot(aes(x = age, y = hazard, color = Sex, linetype = ICD_Chapter)) +
  geom_line() +
  labs(
    title = "Hazard Curve over Age (Weibull AFT)",
    y = "Hazard",
    x = "Age"
  ) +
  theme_minimal()

df %>%
  filter(ICD_Chapter == "External causes of morbidity and mortality") %>%
  filter(age < 86) %>%
  ggplot(aes(x = age, y = Deaths, color = Sex, linetype = ICD_Chapter)) +
  geom_line() +
  labs(
    title = "Hazard Curve over Age (Weibull AFT)",
    y = "Hazard",
    x = "Age"
  ) +
  theme_minimal()



#####To-Do:
#   - Normalize Deaths by Population --> Problem, we were using Death Counts expanded, how do we normalize & use data --> Detahs per 100K Population --> Crude death rate
#   - Different distributions for different causes of death, confirm for each which one is best
#   - See if we can get Hazard Curve to drop log logistic 1, 2


aft_data_ext <- aft_data %>% 
  filter(ICD_Chapter == "External causes of morbidity and mortality")

aft_model <- survreg(Surv(age, status) ~ Sex, data = aft_data_ext, dist = "lognormal")
# Predict linear predictors and scale (log scale)
lp <- predict(aft_model, type = "lp")
scale <- aft_model$scale
sex_levels <- unique(aft_data_ext$Sex)

# Create a time grid (age range)
time_grid <- seq(from = 0, to = max(aft_data_ext$age), length.out = 300)

# Function to compute survival and hazard
make_surv_haz_df <- function(lp_val, scale, time_grid, group_label) {
  z <- (log(time_grid) - lp_val) / scale
  S <- 1 - pnorm(z)
  f <- dnorm(z) / (time_grid * scale)
  h <- f / S
  tibble(time = time_grid, 
         survival = S,
         hazard = h,
         group = group_label)
}

# Generate data for both sexes
surv_haz_df <- bind_rows(
  make_surv_haz_df(lp_val = predict(aft_model, newdata = data.frame(Sex = sex_levels[1]), type = "lp"),
                   scale = scale, time_grid = time_grid, group_label = sex_levels[1]),
  make_surv_haz_df(lp_val = predict(aft_model, newdata = data.frame(Sex = sex_levels[2]), type = "lp"),
                   scale = scale, time_grid = time_grid, group_label = sex_levels[2])
)

# Survival plot
ggplot(surv_haz_df, aes(x = time, y = survival, color = group)) +
  geom_line(size = 1.2) +
  labs(title = "Survival Function by Sex",
       x = "Age", y = "Survival Probability", color = "Sex") +
  theme_minimal()

# Hazard plot
ggplot(surv_haz_df, aes(x = time, y = hazard, color = group)) +
  geom_line(size = 1.2) +
  labs(title = "Hazard Function by Sex",
       x = "Age", y = "Hazard Rate", color = "Sex") +
  theme_minimal()


aft_weibull <- survreg(Surv(age, status) ~ Sex, data = aft_data_ext, dist = "weibull")
# Compare AIC
AIC(aft_model, aft_weibull)

# Compare BIC
BIC(aft_model, aft_weibull)


logLik(aft_model)
logLik(aft_weibull)
