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
#' s<-2
#' l<-2
#' trim_fun(x,s,l)
trim_fun<-function(x,s,l)
{



    if(length(x)<(s+l+1))
    {
      stop("Input vector must have at least (s+l+1) values to calculate a trimmed mean.")


    }
  else{

    x<-sort(x)
    left_trim<-1:s
    right_trim<-length(x):(length(x)-(l-1))

    trim_mean<- mean(x[-c(left_trim,right_trim)])
    return(trim_mean)

  }

}
