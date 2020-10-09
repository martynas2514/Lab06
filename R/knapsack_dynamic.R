#' Dynamic solution for the knapsack problem
#'
#' @param x data structure of weights and values
#' @param W max weight
#'
#' @return most valued combination of elements which fits backpack and the maximum value achieved
#' @export
#'


knapsack_dynamic <- function(x, W){
  w <- x$w
  v <- x$v
  value <- matrix(-1, nrow = length(w) + 1, ncol = W +1)
  value[1, ] <- 0
  value[ , 1] <- 0



  m <- function(i,j){
    weight <- w[i - 1]
    if(i == 0 || j <= 0){
      value[i, j] <<- 0
      return(0)
    }
    if (value[i-1, j] == -1){
      value[i-1, j] <<-  m(i-1,j)
    }

    if(weight > j || j - weight <= 0 ){
      value[i, j] <<-  value[i-1, j]
    }
    else{

      if(value[i, j - weight] == -1){
        value[i, j - weight] <<-  m(i, j - w[i - 1])}

      value[i, j] <<- max(value[i-1, j], value[i -1, j - weight] + v[i - 1])
    }
  }

 result <-  m(length(w)+1, W+1)


 ## lets return vector of items
 items <- c()

 j <- W + 1
 i <- length(w)+1

 while ( i > 1) {
   if(value[i, j] != value[i-1, j]){
     items <- append(items, i-1)
     j <-  j - w[i-1]
   }
   i = i-1
 }
 return(list(value = result , elements = items))
}


