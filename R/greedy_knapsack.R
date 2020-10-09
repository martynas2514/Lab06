greedy_knapsack <- function(x, W){
  "Return maximum value of items and their fractional amounts.

    (MaxValue, Fractions) is returned where MaxValue is the maximum value of
    items with total weight not more than capacity.
    Fractions is a list where Fractions[i] is the fraction that should be taken
    of item i, where 0 <= i < total number of items.

    value[i] is the value of item i and weight[i] is the weight of item i
    for 0 <= i < n where n is the number of items.

    capacity is the maximum weight.
    "
  x$ratio <- x$v / x$w

  sorted <- x[order(x$ratio, decreasing = TRUE),]


  MaxValue <- 0
  Fractions <- array(dim=length(sorted$ratio))
  for (row in 1:nrow(sorted)) {
    if (sorted[row,'w'] <= W) {
      Fractions[row] <- 1
      MaxValue <- MaxValue + sorted[row,'v']
      W = W - sorted[row,'w']
    }


    else{
      Fractions[row] <- W/sorted[row,'w']
      MaxValue <- MaxValue + sorted[row,"v"]*W/sorted[row,'w']
      break
      }
  }
    indexes <- which(Fractions==TRUE)
    whole_value <- round(sum(sorted[indexes,"v"]))
    elements <- unlist(lapply(rownames(sorted[indexes,]), as.integer))

    return(list(value = whole_value, elements = elements))

}

