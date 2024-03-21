# This file recognizes outliers points in training data set
#   using 2 methods: 1) Cook's measure; 2) deffict

#load data ------------------------------------
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
names(data_train) = c("num","y","x0","x1", "x2", "x3",
                      "x4","x5","x6","x7","x8")
x_train = data_train[,c(-1,-2)] #dim: 12384  9
y_train = data_train[,2]
data_uls = uls(x_train, y_train)
data_uls = as.data.frame(data_uls)
names(data_uls) = c("y","x1", "x2", "x3",
                      "x4","x5","x6","x7","x8")
y_uls = data_uls[,1]
x_uls = data_uls[,-1]


##Cook's measure ----------------------------------
x = as.matrix(x_uls)
y = as.matrix(y_uls)
p = dim(x)[2]
n = n_train

theta = OLS(x, y)
y_fit = x%*%theta
e = y - y_fit
MSres = sum( (y - y_fit)^2 ) /(n-p) #3.068882e-05

#faster method to do regression
ols = lm(formula = y ~ x1+x2+x3+x4+x5+x6+x7+x8, data=data_uls)
summary(ols)
d1 <- cooks.distance(ols)


#use boxplot to find outliers
outliers = c()
for(i in 1:8){cpar(c(2,2))
  outVals = boxplot(x_uls[,i])$out
  outNum  = length(outVals)
  outliers = c(outliers, outNum)
}
par(mfow = c(2,2))
OutVals = boxplot(x_uls[,8], main = "median income")$out




## deffict ------------------------------------------------
df = dffits(ols)
boundary = 2*sqrt(p/n_train)
sum(df>=boundary)



#would it be helpful to remove all the outliers??
#Go check later~

#Recognize influensial points

#my method of computing di
D = c()
for(i in 1:n){
  x_i = x[-i,]
  y_i = y[-i,]
  y_i = matrix(y_i, ncol = 1)
  theta_i = OLS(x_i, y_i)
  num = t(theta-theta_i) %*% t(x) %*% x %*% (theta-theta_i)
  den = p*MSres
  Di = num/den
  D = c(D, Di)
}


cut_point = qf(0.05, p, n-p)

#Larger than cut_point is outlier
sum(D>cut_point)












