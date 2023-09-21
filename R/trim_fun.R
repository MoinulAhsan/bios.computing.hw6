#' Trimmed mean function
#'
#' @param x A numeric value-
#' @description
#' trimmed mean function that calculates the mean of a numeric vector, ignoring the smallest and largest values
#'
#' @return a number
#' @export
#'
#' @examples
#' x<-c(2,3,0,6,1,22,1,6,88)
#' trim_fun(x)
trim_fun<-function(x)
{
  mis_pos<-which.min(x)
  max_pos<-which.max(x)

  new_vec<-x[-c(mis_pos,max_pos)]

  mean(new_vec)
}
