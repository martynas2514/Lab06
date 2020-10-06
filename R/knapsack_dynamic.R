knapsack_dynamic <- function(x, W){
  value <- matrix(-1, nrow = length(x$w) + 1, ncol = W +1)
  value[1, ] <- 0
  items <- c()
  m <- function(i,j){
    if(i == 0 || j <= 0){
      value[i, j] <<- 0
      return(0)
    }
    if (value[i-1,j] == -1){
      value[i-1, j] <<-  m(i-1,j)
    }

    if(x$w[i - 1] > j){
      value[i, j] <-  value[i-1, j]
    }
    else{
      if(value[i-1, j- x$w[i - 1]] == -1){
        value[i-1, j- x$w[i - 1]] <<-  m(i-1, j- x$w[i - 1])}

      value[i, j] <<- max(value[i-1, j], value[i-1, j-x$w[i - 1]] + x$v[i - 1])
      print(value[i, j])
    }
  }
 result <-  m(length(x$w)+1, W+1)
 print(items)
 return(result)
}


