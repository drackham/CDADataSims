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
#' @export
library(boot)

rDINA <- function(I){
  set.seed(314159)

  q <- simpleQ()
  I <- I # examinees
  J <- nrow(q) # items
  K <- ncol(q) # skill

  f <- runif(30, min = -5, max = -3.75)
  d <- runif(30, min = 7, max = 9)

  # Generate mastery profiles
  alphaIK <- matrix(nrow = I, ncol = K)
  colnames(alphaIK) <- c("alpha1", "alpha2")

  breaks <- c(I*.1,I*.4,I*.7,I)

  for (i in 1:breaks[1]){  # 100
    alphaIK[i,] <- c(0,0)
  }

  for (i in (1+breaks[1]):breaks[2]){ # 101-400
    alphaIK[i,] <- c(1,0)
  }

  for (i in (1+breaks[2]):breaks[3]){ # 401 - 700
    alphaIK[i,]  <- c(0,1)
  }

  for (i in (1+breaks[3]):breaks[4]){ # 701 - 1000
    alphaIK[i,] <- c(1,1)
  }

  resp <- matrix(nrow = I, ncol = J) # respondents by items
  colnames(resp) <- seq(1:ncol(resp))
  for(i in 1:I){
    for (j in 1:J){
      resp[i,1] <- rbinom(1, 1, inv.logit( f[1] + ( d[1] * alphaIK[i,1] ) ) )
      resp[i,2] <- rbinom(1, 1, inv.logit( f[2] + ( d[2] * alphaIK[i,1] ) ) )
      resp[i,3] <- rbinom(1, 1, inv.logit( f[3] + ( d[3] * alphaIK[i,1] ) ) )
      resp[i,4] <- rbinom(1, 1, inv.logit( f[4] + ( d[4] * alphaIK[i,1] ) ) )
      resp[i,5] <- rbinom(1, 1, inv.logit( f[5] + ( d[5] * alphaIK[i,1] ) ) )
      resp[i,6] <- rbinom(1, 1, inv.logit( f[6] + ( d[6] * alphaIK[i,1] ) ) )
      resp[i,7] <- rbinom(1, 1, inv.logit( f[7] + ( d[7] * alphaIK[i,1] ) ) )
      resp[i,8] <- rbinom(1, 1, inv.logit( f[8] + ( d[8] * alphaIK[i,1] ) ) )
      resp[i,9] <- rbinom(1, 1, inv.logit( f[9] + ( d[9] * alphaIK[i,1] ) ) )
      resp[i,10] <- rbinom(1, 1, inv.logit( f[10] + ( d[10] * alphaIK[i,1] ) ) )

      resp[i,11] <- rbinom(1, 1, inv.logit( f[11] + ( d[11] * alphaIK[i,2] ) ) )
      resp[i,12] <- rbinom(1, 1, inv.logit( f[12] + ( d[12] * alphaIK[i,2] ) ) )
      resp[i,13] <- rbinom(1, 1, inv.logit( f[13] + ( d[13] * alphaIK[i,2] ) ) )
      resp[i,14] <- rbinom(1, 1, inv.logit( f[14] + ( d[14] * alphaIK[i,2] ) ) )
      resp[i,15] <- rbinom(1, 1, inv.logit( f[15] + ( d[15] * alphaIK[i,2] ) ) )
      resp[i,16] <- rbinom(1, 1, inv.logit( f[16] + ( d[16] * alphaIK[i,2] ) ) )
      resp[i,17] <- rbinom(1, 1, inv.logit( f[17] + ( d[17] * alphaIK[i,2] ) ) )
      resp[i,18] <- rbinom(1, 1, inv.logit( f[18] + ( d[18] * alphaIK[i,2] ) ) )
      resp[i,19] <- rbinom(1, 1, inv.logit( f[19] + ( d[19] * alphaIK[i,2] ) ) )
      resp[i,20] <- rbinom(1, 1, inv.logit( f[20] + ( d[20] * alphaIK[i,2] ) ) )

      resp[i,21] <- rbinom(1, 1, inv.logit( f[21] + d[21] * ( alphaIK[i,1] * alphaIK[i,2] ) ) )
      resp[i,22] <- rbinom(1, 1, inv.logit( f[22] + d[22] * ( alphaIK[i,1] * alphaIK[i,2]) ) )
      resp[i,23] <- rbinom(1, 1, inv.logit( f[23] + d[23] * ( alphaIK[i,1] * alphaIK[i,2]) ) )
      resp[i,24] <- rbinom(1, 1, inv.logit( f[24] + d[24] * ( alphaIK[i,1] * alphaIK[i,2]) ) )
      resp[i,25] <- rbinom(1, 1, inv.logit( f[25] + d[25] * ( alphaIK[i,1] * alphaIK[i,2]) ) )
      resp[i,26] <- rbinom(1, 1, inv.logit( f[26] + d[26] * ( alphaIK[i,1] * alphaIK[i,2]) ) )
      resp[i,27] <- rbinom(1, 1, inv.logit( f[27] + d[27] * ( alphaIK[i,1] * alphaIK[i,2]) ) )
      resp[i,28] <- rbinom(1, 1, inv.logit( f[28] + d[28] * ( alphaIK[i,1] * alphaIK[i,2]) ) )
      resp[i,29] <- rbinom(1, 1, inv.logit( f[29] + d[29] * ( alphaIK[i,1] * alphaIK[i,2]) ) )
      resp[i,30] <- rbinom(1, 1, inv.logit( f[30] + d[30] * ( alphaIK[i,1] * alphaIK[i,2]) ) )
    }
  }

  out <- list("I" = I, "J" = J, "K" = K, "resp" = resp, "alphaIK" = alphaIK, "f" = f, "d" = d)
  return(out)
}
