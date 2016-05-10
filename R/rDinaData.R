#' RDINA data simulation
#'
#' Creates response data for the RDINA model
#'
#' @section \strong{Notation}:
#'  \describe{
#'    \tabular{ll}{
#'      q \tab Q-matrix \cr
#'      I \tab Number of items \cr
#'      K \tab Number of skills \cr
#'      K \tab Number of examinees \cr
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
  I <- nrow(q)
  K <- ncol(q)
  J <- 1000

  f <- rep(0, 30)
  d <- rep(1, 30)

  # Generate mastery profiles
  alphaK <- c(.7, .5)
  alphaJK <- matrix(nrow = J, ncol = K)
  colnames(alphaJK) <- c("alpha1", "alpha2")
  for (j in 1:J){
    for (k in 1:K){
      alphaJK[j,k] <- rbinom(1,1,alphaK[k])
    }
  }

  resp <- matrix(nrow = J, ncol = I) # respondents x items
  colnames(resp) <- seq(1:ncol(resp))
  for(j in 1:J){
    for(i in 1:I){
      resp[j,i] <- f[i] + (prod(d[i]*alphaJK[j,]))
    }
  }

  out <- list("I" = I, "J" = J, "K" = K, "resp" = resp, "alphaJK" = alphaJK)
  return(out)
}
