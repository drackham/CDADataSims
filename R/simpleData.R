#' Simplified data simulation
#'
#' Creates response data for a simplified version of the RUM model using the Simple 2 Attribute Q matrix
#'
#' @param J Number of examinees
#' @param jj Student for observation n
#' @param I Number of items
#' @param ii Item for observation n
#' @param K Number of skills
#' @param alphaK Skill mastery population proportion vector
#' @param alphaJK Examinee skill mastery profile
#' @param masteryJK Continuous probability of mastery
#' @param x response matrix
#' @param iParamsLow Matrix of item parameters for ideal low complexity model
#' @param rStar Item discrimination r
#' @param kappa Mastery threshold parameter
#'
#' @author Dave Rackham \email{ddrackham@gmail.com}
#' @references \url{http://onlinelibrary.wiley.com/doi/10.1002/j.2333-8504.2008.tb02157.x/abstract}
#' @keywords q-matrix
#'
#' @examples
#' data <- simpleData()
#'
#' @export

simpleData <- function(){
  set.seed(314159)

  kappa <- .6
  J <- 500
  I <- 30
  K <- 2
  q <- simpleQ()

  # Generate the final mastery proportions
  alphaK <- c(.7, .5)
  alphaJK <- matrix(nrow = J, ncol = K)
  for (j in 1:J){
    for (k in 1:K){
      alphaJK[j,k] <- rbinom(1,1,alphaK[k])
    }
  }

  # Generate random matery probabilities
  masteryJK <- matrix(nrow=J, ncol=K)

  for (j in 1:J){
    for(k in 1:K){
      if(alphaJK[j,k]==0){
        masteryJK[j,k] <- rbeta(1,2,30)
      }
      else{
        masteryJK[j,k] <- rbeta(1,20,2)
      }
    }
  }

  iParamsLow <- matrix(nrow = I, ncol = K) # n skills

  iParamsLow[1,] <-  c(0.10, 1)
  iParamsLow[2,] <-  c(0.05, 1)
  iParamsLow[3,] <-  c(0.25, 1)
  iParamsLow[4,] <-  c(0.30, 1)
  iParamsLow[5,] <-  c(0.45, 1)
  iParamsLow[6,] <-  c(0.10, 1)
  iParamsLow[7,] <-  c(0.15, 1)
  iParamsLow[8,] <-  c(0.20, 1)
  iParamsLow[9,] <-  c(0.33, 1)
  iParamsLow[10,] <- c(0.12, 1)

  iParamsLow[11,] <-  c(1, 0.02)
  iParamsLow[12,] <-  c(1, 0.38)
  iParamsLow[13,] <-  c(1, 0.11)
  iParamsLow[14,] <-  c(1, 0.06)
  iParamsLow[15,] <-  c(1, 0.58)
  iParamsLow[16,] <-  c(1, 0.32)
  iParamsLow[17,] <-  c(1, 0.20)
  iParamsLow[18,] <-  c(1, 0.64)
  iParamsLow[19,] <-  c(1, 0.18)
  iParamsLow[20,] <-  c(1, 0.21)

  iParamsLow[21,] <-  c(0.05, 0.28)
  iParamsLow[22,] <-  c(0.08, 0.03)
  iParamsLow[23,] <-  c(0.21, 0.48)
  iParamsLow[24,] <-  c(0.13, 0.47)
  iParamsLow[25,] <-  c(0.11, 0.19)
  iParamsLow[26,] <-  c(0.26, 0.08)
  iParamsLow[27,] <-  c(0.18, 0.34)
  iParamsLow[28,] <-  c(0.03, 0.08)
  iParamsLow[29,] <-  c(0.13, 0.26)
  iParamsLow[30,] <-  c(0.32, 0.18)

  probCorrect <- matrix (nrow=I, ncol=J)
  for (i in 1:I){ # items
    for (j in 1:J){ # respondents
      rVec <- iParamsLow[i,]
      rStar <- rVec^((1-masteryJK[j,])*q[i,])  # Using mastery
      probCorrect[i,j] <- round(prod(rStar),3)

    }
  }

  # Generate response matrix
  xMat <- matrix (nrow=I, ncol=J)
  for (i in 1:I){
    for (j in 1:J){
      xMat[i,j] <- rbinom(1,1,probCorrect[i,j])
    }
  }

  # Create vectorized response data
  observed <- matrix(1,nrow=I,ncol=J)
  N <- sum(observed)
  # qVec <- rep(-1, N*K)
  x <- rep(-1,N)
  jj <- rep(-1,N)
  ii <- rep(-1,N)
  n <- 1
  for (i in 1:I) {
    for (j in 1:J) {
      if (observed[i,j]) {
        x[n] <- xMat[i,j]
        jj[n] <- j
        ii[n] <- i
        # qVec[n] <- q[j,]
        n <- n + 1
      }
    }
  }

  out <- list("I" = I, "J" = J, "K" = K, "N" = N, "xMat" = xMat, "x" = x, "jj" = jj, "ii" = ii,
              "probCorrect" = probCorrect, "alphaJK" = alphaJK,
              "masteryJK" = masteryJK, "kappa" = kappa, "iParamsLow" = iParamsLow)

  return(out)
}
