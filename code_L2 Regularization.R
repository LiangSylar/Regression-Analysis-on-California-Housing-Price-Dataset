# This file shows a full procedure of regression anslysis, including: 
# 1. Divide data into train and test  
# 2. Perform model fitting by OLS with L2 regularization 
# 3. Compute metrics for model comparisons

#Load library ------------------------------------------------------------
set.seed(2000727)
install.packages("glmnet")
install.packages("dplyr")
install.packages("psych")
library(glmnet)
library(dplyr)
library(psych) 


#load data, get data & set seed for reproducibility ---------------------
setwd("E:/STA3010/course project")
d = read.csv("cal_housing.csv")
y = d$medianHouseValue
y_log = log(y) #perform transformation according to the requirements
x1 = d$longitude
x2 = d$latitude
x3 = d$housingMedianAge
x4 = d$totalRooms
x5 = d$totalBedrooms
x6 = d$population
x7 = d$households
x8 = d$medianIncome
n = length(y_log); p = 9; k = p - 1
x0 = rep(1, n)
data_0 = c(1:n, y_log, x0, x1, x2, x3, x4, x5, x6, x7, x8)
data_1 = matrix(data_0, nrow = n, ncol = 11)
data_2 = as.data.frame(data_1)
output = matrix(y_log)
input = data_1[, c(-1, -2)]
#divide data into two data set randomly
n_train = 12384; n_test = 8256
seed = set.seed(2000727) #s1[2] = 3606
s1 = sample(n_train)
data_train = data_2[s1,]
data_test = data_2[-s1,]
x = data_train[,c(-1,-2)] #dim(x): n_train, 9
y = y_train

x1 = as.matrix(x[,2]) 
x2 = as.matrix(x[,3])
x3 = as.matrix(x[,4])
x8 = as.matrix(x[,9])
x12 = x1*x2; x18 = x1*x8; x28 = x2*x8
x13 = x1*x3; x38 = x3*x8; x23 = x2*x3
x_poly = cbind(x1, x2, x3, x8, 
               x12, x13, x18, 
               x23, x28, x38)
x = x_poly
y = y_train
data_complete = cbind(y,x)
data_complete = as.data.frame(data_complete)
names(data_complete) = c("y", "x1","x2","x3","x8",
                         "x12", "x13", "x18",
                         "x23", "x28", "x38")

# Center y, X will be standardized in the modelling function
y <- data_complete %>% select(y) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
X <- data_complete %>% select(-y) %>% as.matrix()

#Perform 10-fold cross-validation to select lambda ---------------------------
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
# Setting alpha = 0 implements ridge regression
ridge_cv <- cv.glmnet(X, y, alpha = 0, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)
# Plot cross-validation results
plot(ridge_cv)

# Best cross-validated lambda
lambda_cv <- ridge_cv$lambda.min

# Fit final model, get its sum of squared residuals and multiple R-squared
model_cv <- glmnet(X, y, alpha = 0, lambda = lambda_cv, standardize = TRUE)
y_hat_cv <- predict(model_cv, X)
ssr_cv <- t(y - y_hat_cv) %*% (y - y_hat_cv)
rsq_ridge_cv <- cor(y, y_hat_cv)^2




# Use information criteria to select lambda -----------------------------------
X_scaled <- scale(X)
aic <- c()
bic <- c()
for (lambda in seq(lambdas_to_try)) {
  # Run model
  model <- glmnet(X, y, alpha = 0, lambda = lambdas_to_try[lambda], standardize = TRUE)
  # Extract coefficients and residuals (remove first row for the intercept)
  betas <- as.vector((as.matrix(coef(model))[-1, ]))
  resid <- y - (X_scaled %*% betas)
  # Compute hat-matrix and degrees of freedom
  ld <- lambdas_to_try[lambda] * diag(ncol(X_scaled))
  H <- X_scaled %*% solve(t(X_scaled) %*% X_scaled + ld) %*% t(X_scaled)
  df <- tr(H)
  # Compute information criteria
  aic[lambda] <- nrow(X_scaled) * log(t(resid) %*% resid) + 2 * df
  bic[lambda] <- nrow(X_scaled) * log(t(resid) %*% resid) + 2 * df * log(nrow(X_scaled))
}


# Plot information criteria against tried values of lambdas
plot(log(lambdas_to_try), aic, col = "orange", type = "l",
      ylab = "Information Criterion")
lines(log(lambdas_to_try), bic, col = "skyblue3")
legend("bottomright", lwd = 1, col = c("orange", "skyblue3"), legend = c("AIC", "BIC"))


# Optimal lambdas according to both criteria
lambda_aic <- lambdas_to_try[which.min(aic)]
lambda_bic <- lambdas_to_try[which.min(bic)]


# Fit final models, get their sum of squared residuals and multiple R-squared
model_aic <- glmnet(X, y, alpha = 0, lambda = lambda_aic, standardize = TRUE)
y_hat_aic <- predict(model_aic, X)
ssr_aic <- t(y - y_hat_aic) %*% (y - y_hat_aic)
rsq_ridge_aic <- cor(y, y_hat_aic)^2


model_bic <- glmnet(X, y, alpha = 0, lambda = lambda_bic, standardize = TRUE)
y_hat_bic <- predict(model_bic, X)
ssr_bic <- t(y - y_hat_bic) %*% (y - y_hat_bic)
rsq_ridge_bic <- cor(y, y_hat_bic)^2





# See how increasing lambda shrinks the coefficients --------------------------
# Each line shows coefficients for one variables, for different lambdas.
# The higher the lambda, the more the coefficients are shrinked towards zero.
res <- glmnet(X, y, alpha = 0, lambda = lambdas_to_try, standardize = FALSE)
plot(res, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(X), cex = .7)




# Calculate the weights from univariate regressions----------------------------
weights <- sapply(seq(ncol(X)), function(predictor) {
  uni_model <- lm(y ~ X[, predictor])
  coeff_variance <- summary(uni_model)$coefficients[2, 2]^2
})


# Heteroskedastic Ridge Regression loss function - to be minimized
hridge_loss <- function(betas) {
  sum((y - X %*% betas)^2) + lambda * sum(weights * betas^2)
}


# Heteroskedastic Ridge Regression function
hridge <- function(y, X, lambda, weights) {
  # Use regular ridge regression coefficient as initial values for optimization
  model_init <- glmnet(X, y, alpha = 0, lambda = lambda, standardize = FALSE)
  betas_init <- as.vector(model_init$beta)
  # Solve optimization problem to get coefficients
  coef <- optim(betas_init, hridge_loss)$par
  # Compute fitted values and multiple R-squared
  fitted <- X %*% coef
  rsq <- cor(y, fitted)^2
  names(coef) <- colnames(X)
  output <- list("coef" = coef,
                 "fitted" = fitted,
                 "rsq" = rsq)
  return(output)
}

# Fit model to the data for lambda = 0.001
print(head(y))
hridge_model <- hridge(y, X, lambda = 0.001, weights = weights)
rsq_hridge_0001 <- hridge_model$rsq




#compute scales for comparisons------------------------------------------

#R_2
y_fit = hridge_model$fitted; 
SSres = sum( (y_fit - y)^2 ) #or (n_train - 1)*var(y_fit-y)
SSr = sum( (y_fit - mean(y))^2 )
SST = sum( (y-mean(y))^2 )
SSr + SSres - SST #check the results
R_2 = SSr/SST #0.6163967
(R_adj_2 = 1 - (SSres/(n-p)) / (SST/(n-1))) #0.6161674

#test mean-square-error (i.e.MSE)
#unit length scaling the test data
x = x_test; y = y_test
clean_data = remove_log_y(x, y)
x = clean_data[,-1]
y = clean_data[1]
n = length(y)
data_uls_test = uls(x, y) #perform uls
y_uls_test = data_uls_test[,1]
x_uls_test = data_uls_test[,-1]
#get y_fit_test
y_fit_test = x_uls_test%*%theta
MSE = (1/n) * sum( (y_uls_test - y_fit_test)^2  ) #0.3694697









