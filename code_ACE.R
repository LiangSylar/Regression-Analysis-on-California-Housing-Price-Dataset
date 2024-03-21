# This file shows a full procedure of regression anslysis, including: 
# 1. Divide data into train and test  
# 2. Check multicolinearity in data 
# 3. Perform model fitting by ACE (a method for Nonlinear model fitting
# 4. Compute metrics for model comparisons

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
n_train = 12384; n_test = 8256
seed = set.seed(2000727) #s1[2] = 3606
s1 = sample(n_train)
data_train = data_2[s1,]
data_test = data_2[-s1,]
names(data_train) = c("num","y","x0","x1", "x2", "x3","x4","x5","x6","x7","x8")


#part2: data cleaning: check collinearity
#scaled x to unit length
par(mfrow=c(1,1)) 
x = data_train[,c(-1,-2)] #dim: 12384  9
y = data_train[,2]
x1 = x$x1
x2 = x$x2
x3 = x$x3
x4 = x$x4
x5 = x$x5
x6 = x$x6
x7 = x$x7
x8 = x$x8
  
a1 = acepack::ace(x,y)
tx = a1$tx
ty = a1$y
ace_data = cbind(tx, ty)
head(ace_data)
ace_data = as.data.frame(ace_data)
ace_ols = lm(formula = ty ~ x1+x2+x3+x4+x5+x6+x7+x8, 
                data=ace_data)
summary(ace_ols)


theta_ace = OLS(tx,ty)
y_fit_ace = tx%*%theta_ace
plot(y_fit_ace, y)

#
x_1 = tx[,2]
x_2 = tx[,3]
x_3 = tx[,4]
x_4 = tx[,5]
x_5 = tx[,6]
x_6 = tx[,7]
x_7 = tx[,8]
x_8 = tx[,9]

xc = seq(-124,-100,0.1)
xc1 = xc[xc< (-118.4)]
xc2 = xc[xc>= -118.4]
plot(x1, x_1)
abline(v = -118.4)
abline(h = -0.2)
abline(h = 1.95)
abline(v = -124)
xf = c(-124, -118.4)
yf = c(1.95, -0.2)
lm(yf~xf)

abline(h = -2.58)
abline(v = -115)
xf = c(-118.4, -115)
yf = c(-0.2, -2.58)
lm(yf~xf)


f1 <- function(x){
  if(x<=(-118.4)){
    y = -45.6571 -0.3839*x
  }
  else{
    y = -83.08-0.70 *x
  }
  return(y)
}
xx1 = c()
xc = -124:-100
for(i in xc){
  xx1 = c(xx1, f1(i))
}
points(xc,xx1,col='red')



plot(x2, x_2)
lm(x_2~x2)
#Coefficients:
#  (Intercept)           x2  
#17.0068      -0.4822
f2 <- function(x){
  return(17.0068 -0.4822*x)
}
xx1 = c()
xc = 32:42
for(i in xc){
  xx1 = c(xx1, f2(i))
}
lines(xc,xx1, col = "red")

#the third transformation------------------------------------
plot(x3, x_3)
abline(v = 15)
abline(h = 0.03115)
abline(v = 2)
abline(h = 0.0388)
abline(v = 20.5) #segment point
xf = c(15, 2)
yf = c(0.03115, 0.0388)
lm(yf~xf)

#right handside 
#select data is a big problem...
data3 = cbind(x_3, x3)
data3 = as.data.frame(data3)
data3 = data3[(data3$x3)>20.5,]
input = cbind(data3$x3,(data3$x3)^2)
input2 = cbind(rep(1, dim(data3)[1]) , input)
output = data3$x_3
theta = OLS(input2, output)
h = input2%*%theta
points(data3$x3, h,col = 'red')

f3 <- function(x){
  if(x<23){
    y = 0.0399769-0.0005885*x
    return(y)
  }
  if(x>=23){
    y = theta[1] + theta[2]*x + theta[3]*x^2
    return(y)
  }
}

xx3 = c()
xc = seq(0,50,0.5)
for(i in xc){
  xx3 = c(xx3, f3(i))
}
plot(x3, x_3)
points(xc,xx3, col = "red")



#the fourth transformation -----------------------------------
plot(x4,x_4)
abline(v = 2500) #segment point

#left handside
plot(x4, x_4, xlim=c(0, 5000),ylim = c(-0.3,0.3))

abline(h = 0.02)
abline(v = 1160)

abline(v = 0)
abline(h = -0.0065)

abline(v = 2350)
abline(h = 0.0526)

xf = c(1160, 0, 2350)
yf = c(0.02, -0.0065, 0.0526)

lm(yf~xf)

#right handside
data4 = cbind(x4, x_4)
data4 = as.data.frame(data4)
data4_left = data4[data4$x4>3000,]
input = data4_left$x4
output = data4_left$x_4
plot(input, output)
lm(output~input)

f4<-function(x){
  if(x<2500){
    y =-7.402e-03 + 2.516e-05*x
  }
  else{
    y =0.2401438-0.0000665*x
  }
  return(y)
}
xc = 0:20000
plot(x4,x_4)
for(i in xc){
  points(i, f4(i), col = 'red')  
}



#the fifth transformation -----------------------------
plot(x5,x_5,xlim=c(0, 1000))
data5 = cbind(x5, x_5)
data5 = as.data.frame(data5)
abline(v = 600)

#left hand side
data5_left = data5[data5$x5<600,]
input = data5_left$x5
output = data5_left$x_5
lm(output~input)

#right handside
data5_right = data5[data5$x5>=600,]
input = data5_right$x5
output = data5_right$x_5
lm(output~input)


f5 <- function(x){
  if(x<=600){
    y = -0.594006+0.001267*x 
  }
  else{
    y = -0.262850+0.000657*x
  }
  return(y)
}
plot(x5,x_5)
for(i in 0:5000){
  points(i, f5(i), col = 'red')
}


#the sixth transformation
plot(x6, x_6, xlim=c(0,5000))
abline(v = 1350)
abline(h = -0.114)
data6 = cbind(x6, x_6)
data6 = as.data.frame(data6)
#left handside
data6_left = data6[data6$x6<1350,]
input = data6_left$x6
output = data6_left$x_6
lm(output~input)

#right handside
data6_right = data6[data6$x6>2000,]
input = data6_right$x6
output = data6_right$x_6
lm(output~input)

f6<-function(x){
  if(x<1350){
    y = 0.9771526 - 0.0008633*x
  }
  else{
    y = 0.0789679 - 0.0002144*x 
  }
  return(y)
}

for(i in 1:5000){
  points(i, f6(i), col='red')
}

#the seventh transformation
plot(x7, x_7,xlim = c(0, 5000))
abline(v = 550)
data7 = cbind(x7,x_7)
data7 = as.data.frame(data7)

#left handside
data7_left = data7[data7$x7<550,]
input = data7_left$x7
output = data7_left$x_7
lm(output~input)

#right handside
data7_right = data7[data7$x7>=550,]
input = data7_right$x7
output = data7_right$x_7
lm(output~input)

f7 <- function(x){
  if(x<550){
    y = -0.3819173 + 0.0009049*x
  }
  else{
    y = -0.0571474 + 0.0003062 *x
  }
  return(y)
}

for(i in 1:5000){
  points(i, f7(i), col = 'red')
}




#the eighth transformation
plot(x8, x_8)
abline(v = 8.6)
data8 = cbind(x8, x_8)
data8 = as.data.frame(data8)

#left handside
data8_left = data8[data8$x8<8.6,]
input = data8_left$x8
output = data8_left$x_8
lm(output~input)

#right handside
data8_right = data8[data8$x8>=8.6, ]
input = data8_right$x8
output = data8_right$x_8
lm(output~input)

f8<-function(x){
  if(x<7.4){
    y = -1.2508 + 0.3289*x
  }
  else{
    y = 0.42186 + 0.09543*x
  }
  return(y)
}
plot(x8, x_8)
cx = seq(0, 15, 0.1)
for(i in cx){
  points(i, f8(i), col = 'red')
}



#complete model------------------------------------------------
#Input x should include constant column
#input x can be either matrix or data.frame
x_test = data_test[,c(-1,-2)]
y_test = data_test$V2
x = x_test
x = as.matrix(x)
p = dim(x)[2]
n = dim(x)[1]
x_ace = matrix(0, nrow = n, ncol = p-1)
  
for(i in 1:n){
  x_ace[i,1] = f1(x[i, 2])
}
  
for(i in 1:n){
  x_ace[i,2] = f2(x[i, 3])
}
plot(x[,3], x_ace[,2])

for(i in 1:n){
  x_ace[i,3] = f3(x[i, 4])
}
plot(x[,4], x_ace[,3], xlim = c(-100, 1000))

for(i in 1:n){
  x_ace[i,4] = f4(x[i, 5])
}
plot(x[,5], x_ace[,4])

for(i in 1:n){
  x_ace[i,5] = f5(x[i, 6])
}
plot(x[,6], x_ace[,5])

for(i in 1:n){
  x_ace[i,6] = f6(x[i, 7])
}
plot(x[,7], x_ace[,6])

for(i in 1:n){
  x_ace[i,7] = f7(x[i, 8])
}
plot(x[,8], x_ace[,7])

for(i in 1:n){
  x_ace[i,8] = f8(x[i, 9])
}
plot(x[,9], x_ace[,8])

x_ace = cbind(rep(1, n), x_ace)


x_test = as.matrix(x_test)
y_fit_ace_test = x_ace%*%theta_ace

plot(y_test, y_fit_ace_test)


#compute scales for comparison-----------------------------------
#R_2

y = y_test
n = n_test
p = 9
y_fit = y_fit_ace_test


(SSres = sum( (y_fit - y)^2 )) #or (n_train - 1)*var(y_fit-y)
(SSr = sum( (y_fit - mean(y))^2 ))
(SST = sum( (y-mean(y) )^2 ))

SSr + SSres - SST #check the results

(R_2 = SSr/SST ) #0.6444509
(R_adj_2 = 1 - (SSres/(n-p)) / (SST/(n-1))) #0.0.6868298

#test mean-square-error (i.e.MSE)
(MSE = (1/n) * sum( (y - y_fit)^2 )) #0.1055837










