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

rDINA <- function(){
  set.seed(314159)

  q <- simpleQ()
  I <- 1000 # examinees
  J <- nrow(q) # items
  K <- ncol(q) # skill


  f <- rep(0, 30)
  d <- rep(1, 30)

  # Generate mastery profiles
  alphaK <- c(.7, .5)
  alphaJK <- matrix(nrow = I, ncol = K)
  colnames(alphaJK) <- c("alpha1", "alpha2")
  for (i in 1:I){
    for (k in 1:K){
      alphaJK[i,k] <- rbinom(1,1,alphaK[k])
    }
  }

  resp <- matrix(nrow = I, ncol = J) # respondents x items
  colnames(resp) <- seq(1:ncol(resp))
  for(i in 1:I){
    for(j in 1:J){
      resp[i,j] <- f[i] + d[j]*prod((alphaJK[i,]^q[j,]))
    }
  }

  out <- list("I" = I, "J" = J, "K" = K, "resp" = resp, "alphaJK" = alphaJK)
  return(out)
}
