library(ggplot2)
library(GGally)
library(caret)
library(car)

options(scipen = 999)
HospitalDurations <- read.csv("C:\\Users\\moina\\Downloads\\Stat2\\HospitalDurations.csv")
HospitalDurations <- HospitalDurations[, -1] # Take out the ID column
ggpairs(data = HospitalDurations)

HospitalDurations1 <- HospitalDurations[,-c(6,10)]
HospitalDurationsLog <- log(HospitalDurations)
HospitalDurationsLog1 <- HospitalDurationsLog[,-c(6,10)]
ggpairs(data = HospitalDurationsLog)

mlr1 <- lm(Lgth.of.Sty ~ ., data = HospitalDurations)
summary(mlr1)
vif(mlr1)

mlr2 <- lm(Lgth.of.Sty ~ ., data = HospitalDurations1)
summary(mlr2)
vif(mlr2)

mlr3 <- lm(log(Lgth.of.Sty) ~ Inf.Risk + log(Avg.Pat) +
             log(R.Cul.Rat) + log(Age) + log(R.CX.ray.Rat) +
             factor(Med.Sc.Aff) + Pct.Ser.Fac + factor(Region),
             data = HospitalDurations1)


log(Lgth.of.Sty) = B0 + B1 * Inf.Risk + B2 * log(Avg.Pat) + B3 * log(R.Cul.Rat) +
  B4 * log(Age) + B5 * log(R.CX.ray.Rat) + B6 * Med.Sc.Aff_1 + B7 * Med.Sc.Aff_2 +
  B8 * Pct.Ser.Fac + B9 * Region_1 + B10 * Region_2 + B11 * Region_3 + B12 * Region_4

summary(mlr3)
vif(mlr3)
confint(mlr3)

residualPlots(mlr1)
residualPlots(mlr2)
residualPlots(mlr3)

mlr4 <- lm(log(Lgth.of.Sty) ~ log(Age) + Inf.Risk + log(Avg.Pat) +
             log(R.Cul.Rat) + log(R.CX.ray.Rat) + log(Avg.Nur) +
             factor(Region) + factor(Med.Sc.Aff) + Pct.Ser.Fac, 
           data = HospitalDurations)
residualPlots(mlr4)
ggpairs(data = HospitalDurationsLog1)

fit <- lm(log(Lgth.of.Sty) ~ Inf.Risk + log(Avg.Pat) + log(Age) + 
          Pct.Ser.Fac + factor(Region),
          data = HospitalDurations1)
summary(fit)
confint(fit)
rmse_val <- sqrt(mean(fit$residuals^2))
mae_val <- mean(abs(fit$residuals))

set.seed(123)
train_control <- trainControl(method = "cv", number = 10)

# Train the model with bidirectional stepwise selection
step_model <- train(log(Lgth.of.Sty) ~ log(Age) + Inf.Risk + log(Avg.Pat) +
                      log(R.Cul.Rat) + log(R.CX.ray.Rat) + log(Avg.Nur) +
                      factor(Region) + factor(Med.Sc.Aff) + Pct.Ser.Fac +
                      log(N.Beds), 
                    data = HospitalDurations, 
                    method = "glmnet", 
                    trControl = train_control,
                    trace = FALSE) # Set trace = TRUE to see steps

# View the results and selected variables
print(step_model)
#summary(step_model$finalModel)
# 2. Extract the exact coefficients for the best tuned model
best_lambda <- step_model$bestTune$lambda

model_coefficients <- coef(
  step_model$finalModel, 
  s = best_lambda
)

# 3. View coefficients as a matrix
print(model_coefficients)


set.seed(123)
train_control1 <- trainControl(method = "cv", number = 10)

# Train the model with bidirectional stepwise selection
step_model1 <- train(log(Lgth.of.Sty) ~ Inf.Risk + log(Avg.Pat) +
                       log(Age) + Pct.Ser.Fac + factor(Region), 
                    data = HospitalDurations1, 
                    method = "glmnet", 
                    trControl = train_control1,
                    trace = FALSE) # Set trace = TRUE to see steps

# View the results and selected variables
print(step_model1)
#summary(step_model$finalModel)
# 2. Extract the exact coefficients for the best tuned model
best_lambda <- step_model1$bestTune$lambda

model_coefficients <- coef(
  step_model1$finalModel, 
  s = best_lambda
)

# 3. View coefficients as a matrix
print(model_coefficients)

set.seed(123)
train_control2 <- trainControl(method = "cv", number = 10)

# Train the model with bidirectional stepwise selection
step_model2 <- train(log(Lgth.of.Sty) ~ log(Age) + log(Avg.Pat) +
                       log(R.Cul.Rat) + log(R.CX.ray.Rat) + log(Avg.Nur) +
                       poly(Inf.Risk,2) + factor(Region) +
                       factor(Med.Sc.Aff) * Pct.Ser.Fac, 
                     data = HospitalDurations, 
                     method = "glmnet", 
                     trControl = train_control2,
                     trace = FALSE) # Set trace = TRUE to see steps

# View the results and selected variables
print(step_model2)
best_lambda <- step_model2$bestTune$lambda

model_coefficients <- coef(
  step_model2$finalModel, 
  s = best_lambda
)

# 3. View coefficients as a matrix
print(model_coefficients)

set.seed(123)
trControl <- trainControl(method = "cv", number = 10)
tuneGrid=expand.grid(k=c(1:10))
knn_fit <- train(log(Lgth.of.Sty) ~ log(Age) + log(Avg.Pat) +
                   log(R.Cul.Rat) + log(R.CX.ray.Rat) + log(Avg.Nur) +
                   Inf.Risk + factor(Region) +
                   factor(Med.Sc.Aff) + Pct.Ser.Fac + log(N.Beds), 
                 data = HospitalDurations, 
                 method = "knn", 
                 trControl = trControl, 
                 tuneGrid = tuneGrid)
print(knn_fit)
plot(knn_fit)


set.seed(123)
trControl <- trainControl(method = "cv", number = 10)
tuneGrid=expand.grid(k=c(1:10))
knn_fit <- train(log(Lgth.of.Sty) ~ log(Age) * log(Avg.Pat) +
                   log(R.Cul.Rat) + log(R.CX.ray.Rat) + log(Avg.Nur) +
                   poly(Inf.Risk, 2, raw = TRUE) * factor(Region) +
                   factor(Med.Sc.Aff) + Pct.Ser.Fac + log(N.Beds), 
                 data = HospitalDurations, 
                 method = "knn", 
                 trControl = trControl, 
                 tuneGrid = tuneGrid)
print(knn_fit)
plot(knn_fit)

set.seed(123)
trControl <- trainControl(method = "cv", number = 10)
knn_fit <- train(log(Lgth.of.Sty) ~ log(Age) * log(Avg.Pat) +
                   log(R.Cul.Rat) + log(R.CX.ray.Rat) + log(Avg.Nur) +
                   poly(Inf.Risk, 2, raw = TRUE) + factor(Region) +
                   factor(Med.Sc.Aff) * Pct.Ser.Fac + log(N.Beds), 
                 data = HospitalDurations, 
                 method = "kknn", 
                 trControl = trControl, 
                 preProcess = c("center", "scale"))
print(knn_fit)
plot(knn_fit)

set.seed(123)
trControl <- trainControl(method = "cv", number = 10)
rf_fit <- train(log(Lgth.of.Sty) ~ log(Age) * log(Avg.Pat) +
                   log(R.Cul.Rat) + log(R.CX.ray.Rat) + log(Avg.Nur) +
                   poly(Inf.Risk, 2, raw = TRUE) * factor(Region) +
                   factor(Med.Sc.Aff) + Pct.Ser.Fac + log(N.Beds), 
                 data = HospitalDurations, 
                 method = "rf", 
                 trControl = trControl, 
                 ntree = 300)
print(rf_fit)
plot(rf_fit)

set.seed(123)
trControl <- trainControl(method = "cv", number = 10)
rf_fit <- train(log(Lgth.of.Sty) ~ log(Age) * log(Avg.Pat) +
                  log(R.Cul.Rat) + log(R.CX.ray.Rat) + log(Avg.Nur) +
                  poly(Inf.Risk, 2, raw = TRUE) * factor(Region) +
                  factor(Med.Sc.Aff) + Pct.Ser.Fac + log(N.Beds), 
                data = HospitalDurations, 
                method = "rf", 
                trControl = trControl, 
                ntree = 300)
print(rf_fit)
plot(rf_fit)

set.seed(123)
trControl <- trainControl(method = "cv", number = 10)
tree_fit <- train(log(Lgth.of.Sty) ~ log(Age) * log(Avg.Pat) +
                   log(R.Cul.Rat) + log(R.CX.ray.Rat) + log(Avg.Nur) +
                   poly(Inf.Risk, 2, raw = TRUE) + factor(Region) +
                   factor(Med.Sc.Aff) * Pct.Ser.Fac + log(N.Beds), 
                 data = HospitalDurations, 
                 method = "rpart", 
                 trControl = trControl)
print(tree_fit)
plot(tree_fit)
