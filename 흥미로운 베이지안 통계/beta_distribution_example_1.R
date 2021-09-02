for (i in 1:100){
  result <-integrate(function(x) dbeta(x, 6+i, 1+i), 0.4, 0.6)
  print(i)
  print(result)
}


data <- data.frame(NULL)
for (i in 1:60){
  for (j in 1:30){
    result <-integrate(function(x) dbeta(x, 6+i+j, 1+i), 0.4, 0.6)
    data <- rbind(data, data.frame(i, j, result["value"]))
  }
  
}

str(integrate(function(x) dbeta(x, 6+i, 1+i), 0.4, 0.6))
