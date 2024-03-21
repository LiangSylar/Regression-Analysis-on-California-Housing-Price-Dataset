# This file prepares the data for Nonlinear modeling by Neural Network. 
# The actual modeling of NN is done in Python scripts. 

#load data and save uls data as csv
data_uls_df = as.data.frame(data_uls)
names(data_uls_df) = c("y","x1", "x2", "x3","x4","x5","x6","x7","x8")
#save as file
write.csv(data_uls_df,"E:\\STA3010\\course project\\uls_data.csv", row.names = FALSE)

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

#uls train data
x = data_train[,c(-1,-2)]
y = data_train[,2]
data_uls = uls(x, y)
y_uls = data_uls[,1]
x_uls = data_uls[,-1]
data_uls_df = as.data.frame(data_uls)
#save train ulss data, both input and output
data_uls_df = as.data.frame(data_uls)
names(data_uls_df) = c("y","x1", "x2", "x3","x4","x5","x6","x7","x8")
#save as file
write.csv(data_uls_df,"E:\\STA3010\\course project\\uls_data.csv", row.names = FALSE)


#save test uls input data
x_test = data_test[, c(-1, -2)]
y_test = data_test[, 2]
data_uls_test = uls(x_test, y_test)
y_uls_test = data_uls_test[,1]
x_uls_test = data_uls_test[,-1]
x_uls_test_df = as.data.frame(x_uls_test)
names(data_uls_test) =  c("y","x1", "x2", "x3","x4","x5","x6","x7","x8")
write.csv(x_uls_test_df,"E:\\STA3010\\course project\\test_uls_data.csv", row.names = FALSE)


n_train = 12384
mse = 8.2785e-05
(ssres = mse*n_train)
