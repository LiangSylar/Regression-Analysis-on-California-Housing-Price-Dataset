# This file shows a full procedure of regression anslysis, including: 
# 1. Divide data into train and test  
# 2. Perform model fitting by robust regression models 
# 3. Compute metrics for model comparisons

require(foreign)
require(MASS)

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

names(data_train) = c("num","y","x0","x1", "x2", "x3","x4","x5","x6","x7","x8")
x_train = data_train[,c(-1,-2)] #dim: 12384  9
y_train = data_train[,2]
data_uls = uls(x_train, y_train)
data_uls = as.data.frame(data_uls)
names(data_uls) = c("y","x1", "x2", "x3",
                    "x4","x5","x6","x7","x8")
y_uls = data_uls[,1]
x_uls = data_uls[,-1]

x = x_uls
y = y_uls



ols = lm(formula = y ~ x1+x2+x3+x4+x5+x6+x7+x8, 
         data=data_uls)
summary(ols)


#install.packages("MASS")
library(MASS)

Rlm = rlm(formula = y ~ x1+x2+x3+x4+x5+x6+x7+x8, data=data_uls, psi = psi.huber)
theta_rlm = Rlm$coefficients
y_fit_rlm = Rlm$fitted.values
sumary(Rlm)
plot(y_uls, y_fit_rlm, ylab = "Fitted value", xlab = "Observed Value")


y_fit = y_fit_rlm; y = y_uls
SSres = sum( (y_fit - y)^2 ) #or (n_train - 1)*var(y_fit-y)
SSr = sum( (y_fit - mean(y))^2 )
SST = sum( (y-mean(y))^2 )
SSr + SSres - SST #check the results
R_2 = SSr/SST #0.5893218
(R_adj_2 = 1 - (SSres/(n-p)) / (SST/(n-1))) #0.5890895


