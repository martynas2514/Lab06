brute_force_knapsack <- function(x,W){
  stopifnot("x is not a data frame object"=is.data.frame(x),
            "W is not numeric"=is.numeric(W),
            "W is not positive"= W>0,
            "x does not contain columns with names (w, v)"=colnames(x) %in% c("w","v"))

  # lets use all combinations
  AllCombinations <- data.frame(row.names = c("elements", "weights", "value"))
  for (i in 1:length(x$w)) {
    WeightComb <- unname(colSums(combn(x$w, i)))
    ValueComb <-  unname(colSums(combn(x$v, i)))
    elementComb <- unname(as.list(as.data.frame(combn(row.names(x), i))))
    new <- data.frame(I(elementComb), WeightComb, round(ValueComb))
    AllCombinations <- rbind(AllCombinations, new)
  }

  filter <- AllCombinations[AllCombinations$WeightComb <= W,]
  result <- filter[filter$round.ValueComb == max(filter$round.ValueComb),]
  result <- result[,c('round.ValueComb.','elementComb')]
  colnames(result) <- c('value', 'elements' )
  result_list <- as.list(result)
  result_list$elements <- unlist(lapply(result_list$elements, FUN=as.integer))
  return(result_list)
}
