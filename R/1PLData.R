#' IRT 1PL Data Generator
#'
#' Creates response data for a 1PL IRT Model
#'
#' @section \strong{Notation}:
#'  \describe{
#'    \tabular{ll}{
#'      theta \tab Respondent ability parameter \cr
#'      beta \tab Item difficulty parameter \cr
#'      I \tab Number of respondents \cr
#'      J \tab Number of items \cr
#'    }
#'  }
#' @param I Number of examinees
#' @param J Number of items
#'
#' @author Dave Rackham \email{ddrackham@gmail.com}
#' @keywords IRT 1PL
#'
#' @export
#'
IRT_1PL.DataSim <- function(I, J) {
  set.seed(314159)

  inv_logit = function(u) { 1.0/(1.0 + exp(-u)); }

  I <- I # number of respondents
  J <- J # number of items
  theta <- stats::rnorm(I,0,1)
  beta <- stats::rnorm(J,0,1)

  y_all <- matrix(0,nrow=I,ncol=J)
  for (i in 1:I)
    for (j in 1:J)
      y_all[i,j] <- stats::rbinom(1,1,inv_logit(theta[i] - beta[j] ))

  N <- I * J
  y <- rep(-1,N)
  ii <- rep(-1,N)
  jj <- rep(-1,N)

  n <- 1
  for (i in 1:I) {
    for (j in 1:J) {
      y[n] <- y_all[i,j]
      ii[n] = i
      jj[n] = j
      n <- n + 1
    }
  }

  output <- list()
  output$I <- I
  output$J <- J
  output$N <- N
  output$ii <- ii
  output$jj <- jj
  output$y <- y
  output$theta <- theta
  output$beta <- beta

  output
}

# IRT_1PL.500 <- IRT_1PL.DataSim(I = 500, J = 30)
#
# save(IRT_1PL.500, file="IRT_1PL.500.RData")
