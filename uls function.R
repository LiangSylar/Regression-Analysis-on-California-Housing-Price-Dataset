#This file will write a function which will perform uls for data transformation. 
#ULS: unit-length sequence transformation, help mitigate multicollinearity issue.
#input: x and y function;
#   they must be of the same size; dim(x): n, p
#   the x should include the constant column
#   they can be either matrix or data frame type
uls <- function(x, y) 
{
  x = as.matrix(x)
  y = as.matrix(y)
  n = length(y)
  if( n != dim(x)[1] ) 
  {
    return("Invalid input: dimension not match.")
  }
  x = x[,-1] #dim(x): n, p-1
  k = dim(x)[2] #k = p-1
  m = outer(rep(1, n), colMeans(x), '*')
  sjj = colSums( (x - m)^2 )
  Sjj = outer(rep(1, n), sjj, '*')
  x_uls = (x - m)/sqrt(Sjj) #dim(x_uls): n, p-1
  
  SSt = sum( (y - mean(y))^2 )
  y_uls = (y - mean(y))/sqrt(SSt)
  #combine they to a same matrix
  data_uls = cbind(y_uls, x_uls)
  return (data_uls)
}

#this is a function which perform OLS
#x here should involve the constant parameter col
#   unless it has been unit scaled
OLS <- function(x,y){
  x = as.matrix(x)
  y = as.matrix(y)
  p = dim(x)[2]
  f1 = t(x) %*% x
  f2 = solve(f1, diag(p))
  theta = f2%*%t(x)%*%y
  return(theta)
}

#a function to remove bizarre points from log(y)
remove_log_y <- function(x_1, y_1){
  x_1 = x_1[y_1 != log(500001),]
  y_1 = y_1[y_1 != log(500001)]
  plot(x_1$V11, y_1)
  
  x_1 = x_1[y_1 != log(500000),]
  y_1 = y_1[y_1 != log(500000)]
  plot(x_1$V11, y_1)
  
  x_1 = x_1[y_1 != log(450000),]
  y_1 = y_1[y_1 != log(450000)]
  plot(x_1$V11, y_1)
  
  x_1 = x_1[y_1 != log(350000),]
  y_1 = y_1[y_1 != log(350000)]
  plot(x_1$V11, y_1)
  
  clean_data = cbind(y_1, x_1)
  return(clean_data)
}


