
library(caret)
library(feather)
library(forcats)
library(modelr)
library(stringr)
library(tidyverse)

raw <- read_feather("./data/decisions.feather")

decisions <- raw %>% 
  select(-c(council_district, fire_district)) %>% 
  filter(tier != "2") %>% 
  rename(outcome = tier)



# Preprocessing -----------------------------------------------------------

num_col_indices <- map_lgl(decisions, is.numeric)
fact_col_inds <- 8:ncol(decisions)

fact_cols <- map(decisions[,fact_col_inds], factor) %>% 
  as_tibble()

decisions[,fact_col_inds] <- fact_cols

na_percs <- map_dbl(fact_cols, ~sum(is.na(.)/length(.)))
na_percs_logical <- na_percs < 0.05

num_levels <- map_int(fact_cols, ~length(levels(.)))
num_levels_logical <- num_levels <= 5

perc_greatest <- map(fact_cols, ~max(table(.)/length(.)))
perc_greatest_logical <- perc_greatest < 0.95

fact_cols <- fact_cols[, na_percs_logical & num_levels_logical & perc_greatest_logical]
complete_indices <- complete.cases(fact_cols)

# diagnose problems with categorical variables

map(fact_cols, ~table(.)/length(.))

sum(complete_indices)

# rejoin numeric variables

num_cols <- decisions[complete_indices, num_col_indices]
fact_cols <- fact_cols[complete_indices,]

with_cats <- bind_cols(decisions['outcome'][complete_indices,], num_cols, fact_cols)

# process numeric variables

mod_dat <- with_cats

mod_dat$outcome <- fct_recode(mod_dat$outcome, high = "1", low = "3")

# set.seed(1234)

in_train <- createDataPartition(mod_dat$outcome, list = FALSE, p = 0.8)

training <- mod_dat[in_train,]
testing <- mod_dat[-in_train,]

# Impute missing numeric values
pp <- preProcess(training[,2:7], method = c("range",
                                            "bagImpute"))

training <- predict(pp, training)
testing <- predict(pp, testing)

# Choose model type
m <- "rf"

train_control <- trainControl(method = "repeatedcv",
                              summaryFunction = twoClassSummary,
                              classProbs = TRUE,
                              number = 5,
                              repeats = 5)

# Train model using cross-validation
fit <- train(form = outcome ~ .,
             data = training,
             method = m,
             metric = 'ROC',
             trControl = train_control)

# Results -----------------------------------------------------------------

print(fit$results)
pred <- predict(fit, newdata = testing)
print(confusionMatrix(data = pred, testing$outcome, positive = "high"))

if (m == "glmnet") {

  coeff_names <- row.names(coef(fit$finalModel, s = fit$bestTune$lambda))
  coeffs <- as.double(coef(fit$finalModel, s = fit$bestTune$lambda))
  
  weights <- data.frame(coeff = coeff_names, weight = coeffs) %>% 
    filter(weight != 0.0) %>%
    arrange(desc(weight))
  
  print(weights)

} else if (m == "glm") {
  print(summary(fit$finalModel))
} else {
  NULL
}

varImp(fit) %>% plot()
varImp(fit)
