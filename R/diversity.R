#' Entropy Based Measures of Data Diversity
#' @description Calculate the diversity of a dataset or vector by determining the degree to which
#' differentiable groups vary from maximum entropy for the number of unique grouping.
#' If the groupings are uniformly distributed, the entropy is maximized.  This functions calculates
#' the ratio of the entropy for the real distribution of groupings to the maximum entropy distribution to determine
#' the overall diversity represented in the data.
#' @importFrom dplyr select group_by_all ungroup summarise n
#' @importFrom entropy entropy
#' @importFrom magrittr `%>%`
#' @param data a vector, data frame, or tibble type object
#' @param ... arguments passed to 'entropy' function
#' @return a named vector containing the percentage diversity,
#' the true entropy of the dataset,
#' and the potential entropy of the dataset
#' ( diversity, entropy, potential.entropy )
#' @export
#' @examples
#' diversity(cars)
#'
#' t <- data.frame(a = rep(0, 100))
#' v <- runif(100)
#' diversity(t)
#' diversity(v)
diversity <- function( data, ... ) try({
  d <- list()
  if( is.data.frame( data ) ) {
    d <- diversity_data_frame( data )
  } else if( is.vector( data ) ) {
    d <- diversity_vector( data )
  } else {
    stop('data must be a data.frame or vector', call. = FALSE)
  }
  return( diversity_plugin( d$fullsize, d$groups ))
})

#' Diversity Vector Helper
#'
#' @param data a vector
#'
#' @return list with fullsize and group to use in diversity plugin
#' @keywords internal
diversity_vector <- function( data ){
  return(
    list(
      'fullsize' = length(data),
      'groups' = as.data.frame(table(data)) %>%
        select('count' = 'Freq') %>%
        unlist()
    )
  )
}

#' Diversity Data Frame Helper
#'
#' @param data a data frame
#'
#' @return list with fullsize and group to use in diversity plugin
#' @keywords internal
diversity_data_frame <- function( data ) {
  return(
    list(
      'fullsize' = nrow(data),
      'groups' = unique_counts(data) %>%
        select('count') %>%
        unlist()
    )
  )
}


#' Counts and proportions for each unique row in a data frame or tibble
#' @description counts all unique row occurrences, returns a data frame with a count for each row
#' @param data a data frame or tibble type object
#' @param include_percent whether a column should be added to show group percentages
#' @return a grouped data frame of unique rows with column count ( and percent )
#' @export
#' @examples
#' unique_counts( cars )
#' unique_counts( iris, include_percent = TRUE )
unique_counts <- function( data, include_percent = TRUE ) try({
  # validation checks
  stopifnot( 'data must be a data.frame' = is.data.frame(data) )

  # get all unique group counts in data
  grouped_data <- data %>% group_by_all()
  if( include_percent == TRUE ) {
    grouped_data <- grouped_data %>% summarise( count = n() , percent = round ( count / nrow( data ), 3 ) )
  } else {
    grouped_data <- grouped_data %>% summarise( count = n() )
  }
  return( grouped_data %>% ungroup() )
})

#' Diversity Work Plugin
#'
#' @param fullSize the total number of entries in the data
#' @param group_counts the count of occurrences for each unique value in the data
#' @param ... arguments to the entropy function
#'
#' @return diversity of data, entropy of true distribution, potential entropy of uniformly distributed set
#' @keywords internal
diversity_plugin <- function( fullSize, group_counts, ... ){

  # count of unique groups
  groupSize <- length(group_counts)

  # count by group for uniform distribution
  unifCount <- floor( fullSize / groupSize )
  unifDist <- rep( unifCount, groupSize )

  # entropy of uniformly distributed groups
  unifEntropy <- entropy( y = unifDist, ... )

  # true entropy of data groupings
  groupEntropy <- entropy( y = group_counts, ... )

  # diversity is the ratio of true entropy to uniform entropy
  groupDiversity <- ifelse( unifEntropy != 0, groupEntropy / unifEntropy, 0)

  return( c( 'diversity' = groupDiversity, 'entropy' = groupEntropy, 'potential_entropy' = unifEntropy ) )
}
