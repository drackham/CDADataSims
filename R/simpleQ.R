#' Simple 2 Attribute Q-matrix
#'
#' @author Dave Rackham \email{ddrackham@gmail.com}
#' @references \url{http://onlinelibrary.wiley.com/doi/10.1002/j.2333-8504.2008.tb02157.x/abstract}
#' @keywords q-matrix
#'
#' @examples
#' q <- simpleQ()
#'
#' @export
simpleQ <- function(){
  q <- matrix (nrow=30, ncol=2) #Column 3 is evidence models
  #           1 2
  q[1,] <- c(1,0)
  q[2,] <- c(1,0)
  q[3,] <- c(1,0)
  q[4,] <- c(1,0)
  q[5,] <- c(1,0)
  q[6,] <- c(1,0)
  q[7,] <- c(1,0)
  q[8,] <- c(1,0)
  q[9,] <- c(1,0)
  q[10,] <- c(1,0)

  #           1 2
  q[11,] <- c(0,1)
  q[12,] <- c(0,1)
  q[13,] <- c(0,1)
  q[14,] <- c(0,1)
  q[15,] <- c(0,1)
  q[16,] <- c(0,1)
  q[17,] <- c(0,1)
  q[18,] <- c(0,1)
  q[19,] <- c(0,1)
  q[20,] <- c(0,1)

  #           1 2
  q[21,] <- c(1,1)
  q[22,] <- c(1,1)
  q[23,] <- c(1,1)
  q[24,] <- c(1,1)
  q[25,] <- c(1,1)
  q[26,] <- c(1,1)
  q[27,] <- c(1,1)
  q[28,] <- c(1,1)
  q[29,] <- c(1,1)
  q[30,] <- c(1,1)

  return(q)
}
