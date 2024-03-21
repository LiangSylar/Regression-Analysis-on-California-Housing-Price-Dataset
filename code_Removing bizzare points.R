# This file compute about whether removing the bizzare data (outliers)
#   improves the performance of model

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




