#' DCM Continuous Mastery
#'
#' Creates response data for an experimental continuous mastery model
#'
#' @section \strong{Notation}:
#'  \describe{
#'    \tabular{ll}{
#'      q \tab Q-matrix \cr
#'      I \tab Number of examinees \cr
#'      J \tab Number of items \cr
#'      K \tab Number of skills \cr
#'      alpha \tab Examinee skill mastery
#'    }
#'  }
#'
#' @param I Number of examinees
#'
#' @author Dave Rackham \email{ddrackham@gmail.com}
#' @references \url{http://onlinelibrary.wiley.com/doi/10.1002/j.2333-8504.2008.tb02157.x/abstract} # TODO: Update
#' @keywords q-matrix
#'
#' @export

continuousDCM <- function(I){
  set.seed(314159)

  q <- simpleQ()
  I <- I # examinees
  J <- nrow(q) # items
  K <- ncol(q) # skill

  b0 <- -5 # intercept, or baseline prob of correct resposne
  b1 <- runif(J, 3, 5) # main effect of skill 1 mastery
  b2 <- runif(J, 3, 5) # main effect of skill2 mastery

  # Generate mastery profiles
  alphaIK <- matrix(nrow = I, ncol = K)
  colnames(alphaIK) <- c("alpha1", "alpha2")

  breaks <- c(I*.1,I*.4,I*.7,I)

  for (i in 1:breaks[1]){  # 100
    alphaIK[i,] <- c(rbeta(1,2,24),rbeta(1,2,24))
  }

  for (i in (1+breaks[1]):breaks[2]){ # 101-400
    alphaIK[i,] <- c(rbeta(1,24,2),rbeta(1,2,24))
  }

  for (i in (1+breaks[2]):breaks[3]){ # 401 - 700
    alphaIK[i,]  <- c(rbeta(1,2,24),rbeta(1,24,2))
  }

  for (i in (1+breaks[3]):breaks[4]){ # 701 - 1000
    alphaIK[i,] <- c(rbeta(1,24,2),rbeta(1,24,2))
  }

  resp <- matrix(nrow = I, ncol = J) # respondents by items
  colnames(resp) <- seq(1:ncol(resp))
  for(i in 1:I){
      resp[i,1] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[1] * alphaIK[i,1] ) )
      resp[i,2] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[2] * alphaIK[i,1] ) )
      resp[i,3] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[3] * alphaIK[i,1] ) )
      resp[i,4] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[4] * alphaIK[i,1] ) )
      resp[i,5] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[5] * alphaIK[i,1] ) )
      resp[i,6] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[6] * alphaIK[i,1] ) )
      resp[i,7] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[7] * alphaIK[i,1] ) )
      resp[i,8] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[8] * alphaIK[i,1] ) )
      resp[i,9] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[9] * alphaIK[i,1] ) )
      resp[i,10] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[10] * alphaIK[i,1] ) )

      resp[i,11] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b2[11] * alphaIK[i,2] ) )
      resp[i,12] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b2[12] * alphaIK[i,2] ) )
      resp[i,13] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b2[13] * alphaIK[i,2] ) )
      resp[i,14] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b2[14] * alphaIK[i,2] ) )
      resp[i,15] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b2[15] * alphaIK[i,2] ) )
      resp[i,16] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b2[16] * alphaIK[i,2] ) )
      resp[i,17] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b2[17] * alphaIK[i,2] ) )
      resp[i,18] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b2[18] * alphaIK[i,2] ) )
      resp[i,19] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b2[19] * alphaIK[i,2] ) )
      resp[i,20] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b2[20] * alphaIK[i,2] ) )

      resp[i,21] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[21] * alphaIK[i, 1] + b2[21] * alphaIK[i,2] ) )
      resp[i,22] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[22] * alphaIK[i, 1] + b2[21] * alphaIK[i,2] ) )
      resp[i,23] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[23] * alphaIK[i, 1] + b2[21] * alphaIK[i,2] ) )
      resp[i,24] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[24] * alphaIK[i, 1] + b2[21] * alphaIK[i,2] ) )
      resp[i,25] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[25] * alphaIK[i, 1] + b2[21] * alphaIK[i,2] ) )
      resp[i,26] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[26] * alphaIK[i, 1] + b2[21] * alphaIK[i,2] ) )
      resp[i,27] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[27] * alphaIK[i, 1] + b2[21] * alphaIK[i,2] ) )
      resp[i,28] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[28] * alphaIK[i, 1] + b2[21] * alphaIK[i,2] ) )
      resp[i,29] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[28] * alphaIK[i, 1] + b2[21] * alphaIK[i,2] ) )
      resp[i,30] <- stats::rbinom(1, 1, boot::inv.logit( b0 + b1[30] * alphaIK[i, 1] + b2[21] * alphaIK[i,2] ) )
  }

  out <- list("I" = I, "J" = J, "K" = K, "resp" = resp, "alphaIK" = alphaIK, "b0" = b0, "b1" = b1, "b2" = b2)
  return(out)
}
