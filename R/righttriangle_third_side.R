#' calculates the length of the third side of right triangle
#'
#' @param a numeric value-
#' @param b numeric value-
#' @param c numeric value-
#' @description
#' he Pythagorean theorem states that the square of the hypotenuse (the side opposite the right angle) is equal to the sum of the squares of the other two side. Given the lengths of two sides of the triangle, this function calculates the length of the third side.
#'
#'
#' @return a number
#' @export
#'
#' @examples
#' righttriangle_third_side(a = 3, b = 4, ) # Calculate c
#' righttriangle_third_side(b = 4, c = 5, ) # Calculate a
#' righttriangle_third_side(a = 3, c = 5, ) # Calculate b



righttriangle_third_side <- function(a=0, b=0, c=0) {
  # Check if all three sides are provided
  if (!missing(a) && !missing(b) && !missing(c)) {
    stop("Please provide only two side lengths or one side length.")
  }

  # Check if only one side is provided
  if (missing(a) && missing(b) && missing(c)) {
    stop("Please provide the lengths of at least two sides.")
  }

  # Check if any of the provided values are not numeric
  if (!is.numeric(a) || !is.numeric(b) || !is.numeric(c)) {
    stop("All side lengths must be numeric values.")
  }

  # Calculate the length of the missing side
  if (missing(a)) {
    return(sqrt(c^2 - b^2))
  } else if (missing(b)) {
    return(sqrt(c^2 - a^2))
  } else if (missing(c)) {
    return(sqrt(a^2 + b^2))
  }
}





