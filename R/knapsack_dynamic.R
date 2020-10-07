knapsack_dynamic <- function(x, W){
  value <- matrix(-1, nrow = length(x$w) + 1, ncol = W +1)
  value[1, ] <- 0
  value[ , 1] <- 0

  m <- function(i,j){

    if(i == 0 || j <= 0){
      value[i, j] <<- 0
      return(0)
    }
    if (value[i-1,j] == -1){
      value[i-1, j] <<-  m(i-1,j)
    }

    weightToCheck <- j - x$w[i - 1] # negative means too heavy
    if(x$w[i - 1] > j || weightToCheck <= 0 ){
      value[i, j] <<-  value[i-1, j]
    }
    else{

      if(value[i, weightToCheck] == -1){
        value[i, weightToCheck] <<-  m(i, weightToCheck)}

      value[i, j] <<- max(value[i-1, j], value[i -1, j - x$w[i - 1]] + x$v[i - 1])
    }
  }
 result <-  m(length(x$w)+1, W+1)



 ## lets return vector of items
 items <- c()

 j <- W + 1
 i <- length(x$w)+1

 while ( i > 1) {
   if(value[i, j] != value[i-1, j]){
     items <- append(items, i-1)
     j <-  j - x$w[i-1]
   }
   i = i-1
 }
 print(items)
 return(result)
}


