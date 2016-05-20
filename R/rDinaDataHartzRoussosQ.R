#' RDINA data simulation with Hartz Roussos Q-matrix (low)
#'
#' Creates response data for the RDINA model and the Hartz Roussos Q-matrix (low)
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
#' @export

rDINAHartzRoussosQ <- function(I){
  set.seed(314159)

  q <- hartzRoussosQLow()
  I <- I # examinees
  J <- nrow(q) # items
  K <- ncol(q) # skill

  f <- runif(J, min = -5, max = -3.75)
  d <- runif(J, min = 7, max = 9)

  alphaK <- c(.3, .4, .45, .5, .55, .6, .65)
  alphaJK <- matrix(nrow = J, ncol = K)
  colnames(alphaJK) <- c("alpha1", "alpha2", "alpha3", "alpha4", "alpha5", "alpha6", "alpha7")

  for (j in 1:J){
    for (k in 1:K){
      alphaJK[j,k] <- rbinom(1,1,alphaK[k])
    }
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
