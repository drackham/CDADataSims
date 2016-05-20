#' Generate a DINA model response vector
#'
#' Creates a DINA model response vector
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
#'

DINAResponse <- function(alpha,q,f,d){
  resp <- c()
  for(j in 1:nrow(q)){
    p <- f[j] + d[j]*prod((alpha^q[j,]))
    resp[j] <- rbinom(1, 1, exp(p)/(1+exp(p)))
  }

  return(resp)
}

# DINAResponse(c(0.98,0.05),q,f,d)

