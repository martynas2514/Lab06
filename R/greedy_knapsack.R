#' Greedy solution for the knapsack problem
#'
#' @param x data structure of weights and values
#' @param W max weight
#'
#' @return most valued combination of elements which fits backpack and the maximum value achieved
#' @export
#'

greedy_knapsack <- function(x, W){
  "
  Return maximum value of items and which objects are chosen, the last object
  will be summed in percentage but just whole objects will be selected
    "
  stopifnot("x is not a data frame object"=is.data.frame(x),
            "W is not numeric"=is.numeric(W),
            "W is not positive"= W>0,
            "x does not contain columns with names (w, v)"=colnames(x) %in% c("w","v"))

  # Calculate Ratio
  x$ratio <- x$v / x$w

  # Values
  sorted <- x[order(x$ratio, decreasing = TRUE),]

  # Iterate over weights and update Max Value
  MaxValue <- 0
  Fractions <- array(dim=length(sorted$ratio))
  for (row in 1:nrow(sorted)) {
    if (sorted[row,'w'] <= W) {
      Fractions[row] <- 1
      MaxValue <- MaxValue + sorted[row,'v']
      W = W - sorted[row,'w']
    }

    # If max value is reached stop
    else{
      Fractions[row] <- W/sorted[row,'w']
      MaxValue <- MaxValue + sorted[row,"v"]*W/sorted[row,'w']
      break
      }
  }

    # pick out the entire fractions
    indexes <- which(Fractions==TRUE)
    whole_value <- round(sum(sorted[indexes,"v"]))
    elements <- unlist(lapply(rownames(sorted[indexes,]), as.integer))

    return(list(value = whole_value, elements = elements))

}


