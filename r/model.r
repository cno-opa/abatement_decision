
library(caret)
library(feather)
library(modelr)
library(stringr)
library(tidyverse)

raw <- read_feather("./data/decisions.feather")

# Preprocessing -----------------------------------------------------------

# Convert categorical variables to model matrix
mod_mat <- model_matrix(raw[,8:25], ~ .)

# Remove features that are linear combinations of other variables
linear_combos <- mod_mat %>% 
  findLinearCombos() %>% 
  .$remove

mod_dat <- raw %>%
  select(1:7) %>% 
  cbind(mod_mat[,-linear_combos]) %>% 
  select(outcome, everything(), -`(Intercept)`)

names(mod_dat) <- str_replace_all(str_trim(names(mod_dat)), "[:space:]+", "_")

set.seed(1)

in_train <- createDataPartition(mod_dat$outcome, list = FALSE, p = 0.6)

training <- mod_dat[in_train,]
testing <- mod_dat[-in_train,]

# Impute missing numeric values
pp <- preProcess(training[,2:7], method = c("zv",
                                            "nzv",
                                            "center",
                                            "scale",
                                            "knnImpute",
                                            "conditionalX"))

training <- predict(pp, training)
testing <- predict(pp, testing)

# Choose model type
m <- "rpart"

# Train model using cross-validation
tc <- trainControl(method = "cv")
fit <- train(form = outcome ~ .,
             data = training,
             method = m,
             trControl = tc)

# Results -----------------------------------------------------------------

print(fit$results)
pred <- predict(fit, newdata = testing)
print(confusionMatrix(data = pred, testing$outcome, positive = "Sell"))

if (m == "glmnet") {

  coeff_names <- row.names(coef(fit$finalModel, s = fit$bestTune$lambda))
  coeffs <- as.double(coef(fit$finalModel, s = fit$bestTune$lambda))
  
  weights <- data.frame(coeff = coeff_names, weight = coeffs) %>% 
    # filter(weight != 0.0) %>% 
    arrange(desc(weight))
  
  print(weights)

} else if (m == "glm") {
  
  print(summary(fit$finalModel))
  
} else {NULL}