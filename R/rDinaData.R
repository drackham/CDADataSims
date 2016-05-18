#' RDINA data simulation
#'
#' Creates response data for the RDINA model
#'
#' @section \strong{Notation}:
#'  \describe{
#'    \tabular{ll}{
#'      q \tab Q-matrix \cr
#'      I \tab Number of examinees \cr
#'      J \tab Number of items \cr
#'      K \tab Number of skills \cr
#'      f \tab False alarm rate \cr
#'      d \tab Item descrimination (detection) \cr
#'    }
#'  }
#'
#' @author Dave Rackham \email{ddrackham@gmail.com}
#' @references \url{http://onlinelibrary.wiley.com/doi/10.1002/j.2333-8504.2008.tb02157.x/abstract} # TODO: Update
#' @keywords q-matrix
#'
#' @examples
#' data <- rDINA()
#'
#' @export

rDINA <- function(I){
  set.seed(314159)

  q <- simpleQ()
  I <- I # examinees
  J <- nrow(q) # items
  K <- ncol(q) # skill

  f <- runif(30, min = -5, max = -3.75)
  d <- runif(30, min = 7, max = 9)

  # Generate mastery profiles
  alphaJK <- matrix(nrow = I, ncol = K)
  colnames(alphaJK) <- c("alpha1", "alpha2")

  breaks <- c(I*.1,I*.4,I*.7,I)

  for (i in 1:breaks[1]){
    alphaJK[i,] <- c(0.02,0.02)
  }

  for (i in (1+breaks[1]):breaks[2]){
    alphaJK[i,] <- c(0.95,0.02)
  }

  for (i in (1+breaks[2]):breaks[3]){
    alphaJK[i,]  <- c(0.02,.95)
  }

  for (i in (1+breaks[3]):breaks[4]){
    alphaJK[i,] <- c(.95,.95)
  }

  resp <- matrix(nrow = I, ncol = J) # respondents by items
  colnames(resp) <- seq(1:ncol(resp))
  for(i in 1:I){
    for(j in 1:J){
      p <- f[j] + d[j]*prod((alphaJK[i,]^q[j,]))
      resp[i,j] <- rbinom(1, 1, exp(p)/(1+exp(p)))
    }
  }

  out <- list("I" = I, "J" = J, "K" = K, "resp" = resp, "alphaJK" = alphaJK, "f" = f, "d" = d)
  return(out)
}
