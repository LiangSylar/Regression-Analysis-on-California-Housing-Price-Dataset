# This file shows a full procedure of regression anslysis, including: 
# 1. Divide data into train and test 
# 2. Check basic characteristics by scatterplots 
# 3. Check multicolinearity in data 
# 4. Perform model fitting by OLS (ordinary Least Squares)
# 5. Perform model diagnostics
# 6. Compute metrics for model comparisons

#load data
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
n_train = 12384; n_test = n - n_train
seed = set.seed(2000727) #s1[2] = 3606
s1 = sample(n_train)
data_train = data_2[s1,]
data_test = data_2[-s1,]
#check whether overlap data exists
sum(data_train$V1 %in% data_test$V1)
sum(data_test$V1 %in% data_train$V1)
y
#data types
head(data_1)


#check basic characteristics
for(i in 4:11){
  xi = data_2[,i]
  show( mean(xi) )
  show( sqrt(var(xi)) )
  show( quantile(xi) )
}
summary(data_train)  

#histgram
par(mfcol = c(2,2))
lab_names = c("Longitude","Latitude","Housing median age"
              ,"Total rooms","Total bedrooms", "Population",
              "Households", "Median income")

data_2
for(i in 5:8){
  plot(data_train[,i+3], y_train, main = lab_names[i], xlab = lab_names[i]
       , ylab = "ln(House values)")
}



for(i in 5:8){
  hist(data_2[,i+3], col = blues9, xlab = lab_names[i],
       main = lab_names[i])
  box()
  grid()
}

hist(data_2[,2], col = blues9, xlab = "Housing value",
     main = "Housing value")
#scatter between variables
par(mfcol = c(1,1))
plot(x8, y_log, xlab = "Median Income", ylab = "Median House Value")
#discover an obvious straight line in y. May consider removing it later.

#part2: data cleaning: check collinearity
#scaled x to unit length
x_train = data_train[,c(-1,-2)]
y_train = data_train[,2]
x = x_train
y = y_train
data_uls = uls(x, y)
y_uls = data_uls[,1]
x_uls = data_uls[,-1]
data_uls_df = as.data.frame(data_uls)
names(data_uls_df) = c("y","x1", "x2", "x3","x4","x5","x6","x7","x8")
#save as file
#write.csv(data_uls_df,"E:\\STA3010\\course project\\uls_data.csv", row.names = FALSE)


#check multicollinearity problem: method 1
x_uls = as.matrix(x_uls) #need to convert it into matrix
# to apply matrix operations
y_uls = as.matrix(y_uls)
(f1 = t(x_uls)%*%(x_uls) )
#we may consider the multicollinearity exists!!!

#Check multicollinearity issue: method 2 for insurance
(eigen = eigen(t(as.matrix(x_train))%*%as.matrix(x_train)))
(eigen_val = eigen$values)
(eigen_val_max = max(eigen_val))
eigen_val_max / eigen_val
#strong near-linear dependence in the data!!!

#As comparison: after uls, method2 does not show strong collinearty.
(eigen = eigen(t(x_uls)%*%x_uls))
(eigen_val = eigen$values)
(eigen_val_max = max(eigen_val))
eigen_val_max / eigen_val

#use uls data to perform multiple linear 
x = x_uls
y = y_uls
theta = OLS(x, y)
#compute R_2 and R_adj_2
y_fit = x%*%theta
SSres = sum( (y_fit - y)^2 ) #or (n_train - 1)*var(y_fit-y)
SSr = sum( (y_fit - mean(y))^2 )
SST = sum( (y_uls-mean(y_uls))^2 )
SSr + SSres - SST #check the results
(R_2 = SSr/SST) #0.6202259
(R_adj_2 = 1 - (SSres/(n-p)) / (SST/(n-1))) #0.6200786
sst
#test mean-square-error (i.e.MSE)
#unit length scaling the test data
x_test = data_test[, c(-1, -2)]
y_test = data_test[, 2]
data_uls_test = uls(x_test, y_test)
y_uls_test = data_uls_test[,1]
x_uls_test = data_uls_test[,-1]
#get y_fit_test
y_fit_test = x_uls_test%*%theta
MSE = (1/n_test) * sum( (y_uls_test - y_fit_test)^2  ) #3.340203e-05
#MSE = 340203e-05

#compute R-studentized residuals
p = dim(x_uls)[2]
f1 = t(x_uls) %*% x_uls
f2 = solve(f1, diag(p))
H_train = x_uls %*% f2 %*% t(x_uls)
hi_train = diag(H_train)
e_train = y_uls - y_fit
S = c()
MSres = SSres/(n-p)
for(i in 1:n_train) {
  num = (n_train - p)*MSres -( e_train[i])^2/(1-hi_train[i])
  den = (n_train - p - 1)
  si = num / den
  S = c(S, si)
}
t = c()
for(i in 1:n_train) {
  ti = e_train[i] / sqrt(S[i]*(1 - hi_train[i]))
  t = c(t, ti)
}

#plot R-studentized residuals against fitted values
plot(y_fit, t, main = "Plot of R-studentized Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "R-studentized Residuals")
abline(h = 2)
abline(h = -2)
#so strange: a line cutting down???
#Because of the line, may not be able to draw a conclusion ???

#construct normal probability plot
r = c() #compute Studentized Residuals first
for(i in 1:n_train) {
  ri = e_train[i] / sqrt(MSres*(1-hi_train[i]))
  r = c(r, ri)
}
r_sort = sort(r)
p_ = c() #compute cumulative probability
for(i in 1:n_train) {
  pi = (i - 0.5) / n_train
  p_ = c(p_, pi)
}
plot(r_sort, p_, main = "Normal Probability Plot",
     xlab = "Studentized residuals", ylab = "cumulative probability")
#the assumption about the gaussian random error terms is true 
# because of the approximate straight line


#second model match: polynomial model
#decide regressors: chosen x1, x2, x8
#Reason 1: x8 has the highest correlation coefficients with log(y)
cor(x_uls_2) #reason 1

#Reason 2: one's own research interest/ research gap
#it has been stated that position, age and income can have effects on 
# housing value. But current research have not focus on
# interaction term between the three factors. 

#start fitting with chosen regressors:x1, x2, x3, x8
x0 = as.matrix(x_train[,1])
x1 = as.matrix(x_train[,2]) 
x2 = as.matrix(x_train[,3])
x3 = as.matrix(x_train[,4])
x4 = as.matrix(x_train[,5])
x7 = as.matrix(x_train[,8])
x8 = as.matrix(x_train[,9])

x12 = x1*x2; x18 = x1*x8; x28 = x2*x8
x13 = x1*x3; x38 = x3*x8; x23 = x2*x3
x11 = x1^2 ; x22 = x2^2;  x88 = x8^2;
x_poly = cbind(x0,x1, x2, x8,x12, x18, x28, x11,x22,x88)

data_uls = uls(x_poly, y_train)
x_poly = data_uls[,-1]
y_poly = data_uls[,1]

(eigen = eigen(t(x_poly)%*%x_poly))
(eigen_val = eigen$values)
(eigen_val_max = max(eigen_val))
eigen_val_max / eigen_val #reason

data_uls = as.data.frame(data_uls)
names(data_uls) = c("y","x1","x2", "x8", "x12","X18","x28","x11","x22","x88")
poly_ols = lm(formula = y~x1+x2+x8 +x12+x18+x28+x11+x22+x88, data=data_uls)
summary(poly_ols)
plot(poly_ols$model$y, poly_ols$fitted.values)
theta = poly_ols$coefficients

#compute scales for comparisons
x0 = x_test[,1]
x1 = x_test[,2]
x2 = x_test[,3]
x3 = x_test[,4]
x8 = x_test[,9]
x12 = x1*x2; x18 = x1*x8; x28 = x2*x8
x13 = x1*x3; x38 = x3*x8; x23 = x2*x3
x11 = x1^2 ; x22 = x2^2;  x88 = x8^2;
x_poly_test = cbind(x0,x1, x2, x8, 
          x12, x18, x28 ,x11,x22,x88)


data_uls = uls(x_poly_test, y_test)
x_poly_test = data_uls[,-1]
y_poly_test = data_uls[,1]

x_poly_test = cbind(x0, x_poly_test)
k = dim(x_poly_test)[2]
n = n_test
y_fit_test = x_poly_test%*%theta
MSE = (1/n_test) * sum( (y_poly_test - y_fit_test)^2  ) #145.9537




#If some bizzare points are moved from y removed
#remove x8 outliers: 500000, 450000, 350000
x = x_train; y = y_train
clean_data = remove_log_y(x, y)
x_clean = clean_data[,-1]
y_clean = clean_data$y_1
plot(x_clean$V11, y_clean)
#perform uls
x = x_clean
y = y_clean
data_uls_2 = uls(x, y)
x_uls_2 = data_uls_2[,-1]
y_uls_2 = data_uls_2[,1]
#perform ols
theta = OLS(x_uls_2, y_uls_2)
x = x_uls_2
y = y_uls_2
n = length(y)
y_fit = x%*%theta
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

#compute R-studentized residuals
x = x_uls_2
y = y_uls_2
n = length(y_uls_2)
p = dim(x)[2]
f1 = t(x) %*% x
f2 = solve(f1, diag(p))
H_train = x %*% f2 %*% t(x)
hi_train = diag(H_train)
e_train = y - y_fit
S = c()
MSres = SSres/(n-p)
for(i in 1:n_train) {
  num = (n_train - p)*MSres -( e_train[i])^2/(1-hi_train[i])
  den = (n_train - p - 1)
  si = num / den
  S = c(S, si)
}
t = c()
for(i in 1:n) {
  ti = e_train[i] / sqrt(S[i]*(1 - hi_train[i]))
  t = c(t, ti)
}

#plot R-studentized residuals against fitted values
plot(y_fit, t, main = "Plot of R-studentized Residuals against Fitted Values",
     xlab = "Fitted Values", ylab = "R-studentized Residuals")
#the straight line was removed; but the performance of the plot
#   does not seem to improve.



