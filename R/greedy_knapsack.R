greedy_knapsack <- function(x, W){
  "Return maximum value of items and their fractional amounts.

    (max_value, fractions) is returned where max_value is the maximum value of
    items with total weight not more than capacity.
    fractions is a list where fractions[i] is the fraction that should be taken
    of item i, where 0 <= i < total number of items.

    value[i] is the value of item i and weight[i] is the weight of item i
    for 0 <= i < n where n is the number of items.

    capacity is the maximum weight.
    "
  x$ratio <- x$v / x$w

  sorted <- x[order(x$ratio, decreasing = TRUE),]


  max_value <- 0
  fractions <- array(dim=length(sorted$ratio))
  for (row in 1:nrow(sorted)) {
    if (sorted[row,'w'] <= W) {
      fractions[row] <- 1
      max_value <- max_value + sorted[row,'v']
      W = W - sorted[row,'w']
    }


    else{
      fractions[row] <- W/sorted[row,'w']
      max_value <- max_value + sorted[row,"v"]*W/sorted[row,'w']
      break
      }
  }
    indexes <- which(fractions==TRUE)
    whole_value <- round(sum(sorted[indexes,"v"]))
    elements <- unlist(lapply(rownames(sorted[indexes,]), as.integer))

    return(list(value = whole_value, elements = elements))

}


