# Multivariate Logistic Regressions: predicting ATP1A3 using all structures

# libraries
library(ISLR)
library(ggplot2)
library(pROC)

# Control data
LR_con_z <- subset(control_raw_z, select = c(seglist))
LR_con_z$cohort[1:nrow(control_raw_z)] <- 0
head(LR_con_z)

# ATP1A3/cohort data
cohort_raw_z  <- read.csv(paste(growth_chart_path, 
                                "/datasets/ATP1A3_timepoint_rel_z.csv", sep = ""))
LR_ATP1A3_z <- subset(cohort_raw_z, select = c(seglist))
LR_ATP1A3_z$cohort[1:nrow(cohort_raw_z)] <- 1
head(LR_ATP1A3_z)

LR_dat <- rbind(LR_con_z, LR_ATP1A3_z) # bind all
str(LR_dat)
LR_dat$cohort <- as.factor(LR_dat$cohort)

summary(LR_dat) # check all


# -------------------------------------------------------------------------------
# create models - train, predict, and produce confusion matrix

# Set seed for reproducibility
set.seed(1)

# Create a sample for training and testing
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.6, 0.4))
train <- data[sample, ]
test <- data[!sample, ]

# Extract response variable
y <- as.factor(train$cohort)

# Create model matrix
x <- model.matrix(~., data = train)[,-1]

# Standardize the predictor variables
x_scaled <- scale(x)

# Load the glmnet library
library(glmnet)

# Find the best lambda using cross-validation
set.seed(123) 
#cv.lasso <- cv.glmnet(x_scaled, y, alpha = 1, family = "binomial")
cv.lasso <- cv.glmnet(x_scaled, y, alpha = 1, family = "multinomial")
plot(cv.lasso)

# Weaken the Lasso regularization by reducing lambda
weakened_lambda <- cv.lasso$lambda.min / 1000

# Fit an initial model to warm-start (optional binomial family - less effective)
#initial_model <- glmnet(x_scaled, y, alpha = 0.25, family = "binomial", 
#                        thresh = 1e-7,  lambda = weakened_lambda / 100, maxit = 1000000)
initial_model <- glmnet(x_scaled, y, alpha = 0.25, family = "multinomial", 
                        thresh = 1e-7,  lambda = weakened_lambda / 100, maxit = 1000000)
coef(initial_model)

# Use warm start in the final model
#final_model <- glmnet(x_scaled, y, alpha = 0.3, family = "binomial", 
#                   lambda = weakened_lambda, start = initial_model$beta, maxit = 1000000)
final_model <- glmnet(x_scaled, y, alpha = 0.25, family = "multinomial", 
                      lambda = weakened_lambda, start = initial_model$beta, maxit = 1000000)

# Extract coefficients
coef_values <- coef(final_model)
print(coef_values)


# ----------------------------------------------------------------------------
# Evaluate model performance and probabilities

# Extract odds ratios and their confidence intervals
odds_ratios <- exp(as.numeric(coef_values))
conf_intervals <- confint(final_model, parm = "lambda.min")

# Print odds ratios and confidence intervals
print(odds_ratios)
print(conf_intervals)


# Make predictions on the test data
x.test <- model.matrix(~., data = test)[,-1]
probabilities <- predict(final_model, newx = x.test, 
                         s = cv.lasso$lambda.min, type = "response")

# Convert predicted probabilities to predicted classes
predicted.classes <- apply(probabilities, 1, which.max) - 1  
#     (Subtract 1 to convert from 1-based to 0-based indexing)

# Convert to character for consistency with your class labels
predicted.classes <- as.character(predicted.classes)
print(predicted.classes)

# Model accuracy
observed.classes <- as.character(test$cohort)
mean(predicted.classes == observed.classes)

#use model to make predictions on test set
predicted <- predict(model, test, type="response")
print(predicted)

observed.classes <- as.character(test$cohort)

str(train)
str(test)

plot(observed.classes)
print(observed.classes)

#define object to plot
rocobj <- roc(test$cohort, predicted)

#create ROC plot
plot(rocobj) # base R

# Using ggplot2 functions
ggplot(lpp, aes(x = write, y = probability, colour = ses)) + geom_line() + 
  facet_grid(variable ~., scales = "free")


# Confusion matrix
confusionMatrix(as.factor(predicted.classes), as.factor(observed.classes), positive = "True")
