#' brute force knapsack
#'
#' @param x data structure of weights and values
#' @param W max weight
#' @param parallel TRue if with parallel, False (default if not)
#'
#' @return most valued combination of elements which fits backpack
#' @import parallel
#' @export


brute_force_knapsack <- function(x, W, parallel = FALSE){
  stopifnot("x is not a data frame object"=is.data.frame(x),
            "W is not numeric"=is.numeric(W),
            "W is not positive"= W>0,
            "x does not contain columns with names (w, v)"=colnames(x) %in% c("w","v"))


  #define function
  fi <- function(i){
    WeightComb <- colSums(combn(x$w, i))
    ValueComb <-  colSums(combn(x$v, i))
    elementComb <- combn(row.names(x), i, simplify = FALSE)
    return(data.frame(I(elementComb), WeightComb, round(ValueComb)))
  }

  AllCombinations <- data.frame()
  if(parallel == FALSE){
  # lets use all combinations
  for (i in 1:length(x$w)) {
    AllCombinations <- rbind(AllCombinations, fi(i))
  }}
  else{ # parallel
    cores <- parallel::detectCores()
    cl <- parallel::makeCluster(cores, type = "PSOCK")
    parallel::clusterExport(cl=cl, varlist=c("x"), envir=environment())
    res2 <-  parallel::parLapply(cl, 1:length(x$v), fi)
    parallel::stopCluster(cl)
    AllCombinations <- do.call("rbind", res2)
  }

  filter <- AllCombinations[AllCombinations$WeightComb <= W,]
  result <- filter[filter$round.ValueComb == max(filter$round.ValueComb),]
  result <- result[,c('round.ValueComb.','elementComb')]
  colnames(result) <- c('value', 'elements' )
  result_list <- as.list(result)
  result_list$elements <- unlist(lapply(result_list$elements, FUN=as.integer))
  return(result_list)
}
