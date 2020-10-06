brute_force_knapsack <- function(x,W){
  stopifnot("x is not a data frame object" =is.data.frame(x),
            "W is not numeric" = is.numeric(W),
            "x does not contain columns with names (w, v) " = colnames(x) %in% c("w","v") )

  # lets use all combinations
  AllCombinations <- data.frame(row.names = c("elements", "weights", "value"))
  for (i in 1:length(x$w)) {
    WeightComb <- unname(colSums(combn(x$w, i)))
    ValueComb <-  unname(colSums(combn(x$v, i)))
    elementComb <- unname(as.list(as.data.frame(combn(row.names(x), i))))
    new <- data.frame(I(elementComb), WeightComb, ValueComb)
    AllCombinations <- rbind(AllCombinations,new)
  }

  filter <- AllCombinations[AllCombinations$WeightComb <= W,]
  result <- filter[filter$ValueComb == max(filter$ValueComb),]
  return(result)
}
