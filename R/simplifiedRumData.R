#' Simplified RUM data simulation
#'
#' Creates response data for a simplified version of the RUM model using the Hartz Roussos Q matrix (Low)
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
#' @keywords q-matrix hartz roussos
#'
#' @examples
#' data <- simplifiedRUMData()
#'
#' @export

simplifiedRUMData <- function(){
  set.seed(314159)

  # kappa <- .7
  J <- 1500
  I <- 40
  K <- 7
  q <- hartzRoussosQLow()

  # Generate the final mastery proportions
  alphaK <- c(.3, .4, .45, .5, .55, .6, .65)
  alphaJK <- matrix(nrow = J, ncol = K)
  for (j in 1:J){
    for (k in 1:K){
      alphaJK[j,k] <- rbinom(1,1,alphaK[k])
    }
  }

  # Use alphaJK to randomly generate continuous probablity of mastery
#   masteryJK <- matrix(nrow=J, ncol=K)
#   for (j in 1:J){
#     for(k in 1:K){
#       if(alphaJK[j,k] == 1){
#         masteryJK[j,k] <- round(runif(1, min = kappa, max = 1),3)
#       }
#       else{
#         masteryJK[j,k] <- round(runif(1, min = 0, max = kappa-.01),3)
#       }
#     }
#   }

  iParamsLow <- matrix(nrow = I, ncol = K) # n skills

                        # r1    r2    r3    r4     r5   r6    r7
  iParamsLow[1,] <-  c( NA  , NA  , .447, NA  , .197, NA  , NA  )
  iParamsLow[2,] <-  c( .146, NA  , NA  , NA  , NA  , NA  , NA  )
  iParamsLow[3,] <-  c( .158, NA  , NA  , NA  , .122, NA  , NA  )
  iParamsLow[4,] <-  c( NA  , NA  , NA  , NA  , NA  , NA  , .13 )
  iParamsLow[5,] <-  c( .177, NA  , NA  , .157, NA  , NA  , NA  )
  iParamsLow[6,] <-  c( NA  , .494, NA  , NA  , .442, NA  , .184)
  iParamsLow[7,] <-  c( NA  , NA  , .403, .405, NA  , NA  , .111)
  iParamsLow[8,] <-  c( NA  , NA  , .464, NA  , .132, NA  , NA  )
  iParamsLow[9,] <-  c( NA  , NA  , .493, NA  , .171, NA  , NA  )
  iParamsLow[10,] <- c( NA  , NA  , NA  , .153, NA  , NA  , NA  )

                        # r1    r2    r3    r4     r5   r6    r7
  iParamsLow[11,] <-  c( NA  , NA  , NA  , NA  , NA  , .118, NA  )
  iParamsLow[12,] <-  c( NA  , NA  , NA  , NA  , NA  , NA  , .104)
  iParamsLow[13,] <-  c( .575, NA  , NA  , .167, NA  , NA  , .14 )
  iParamsLow[14,] <-  c( NA  , NA  , NA  , .105, NA  , NA  , NA  )
  iParamsLow[15,] <-  c( NA  , .114, .197, NA  , NA  , NA  , NA  )
  iParamsLow[16,] <-  c( NA  , NA  , NA  , NA  , .104, NA  , .198)
  iParamsLow[17,] <-  c( NA  , NA  , NA  , NA  , .477, NA  , .153)
  iParamsLow[18,] <-  c( .142, NA  , .43 , NA  , NA  , .103, .162)
  iParamsLow[19,] <-  c( .516, NA  , NA  , NA  , NA  , .179, NA  )
  iParamsLow[20,] <-  c( .509, NA  , NA  , NA  , NA  , NA  , NA  )

                        # r1    r2    r3    r4     r5   r6    r7
  iParamsLow[21,] <-  c( NA  , .18 , NA  , NA  , NA  , NA  , NA  )
  iParamsLow[22,] <-  c( NA  , NA  , NA  , NA  , NA  , .191, NA  )
  iParamsLow[23,] <-  c( NA  , NA  , NA  , .192, NA  , NA  , NA  )
  iParamsLow[24,] <-  c( NA  , NA  , NA  , .415, NA  , .183, NA  )
  iParamsLow[25,] <-  c( NA  , .115, NA  , NA  , NA  , .143, NA  )
  iParamsLow[26,] <-  c( NA  , NA  , NA  , NA  , .148, NA  , NA  )
  iParamsLow[27,] <-  c( NA  , NA  , NA  , NA  , NA  , .495, .185)
  iParamsLow[28,] <-  c( NA  , .445, .183, .195, NA  , NA  , NA  )
  iParamsLow[29,] <-  c( NA  , .402, .496, NA  , .104, NA  , .181)
  iParamsLow[30,] <-  c( NA  , NA  , .419, NA  , NA  , NA  , .159)

                        # r1    r2    r3    r4     r5   r6    r7
  iParamsLow[31,] <-  c( .191, NA  , NA  , .14 , NA  , .187, NA  )
  iParamsLow[32,] <-  c( NA  , .455, NA  , .186, NA  , NA  , NA  )
  iParamsLow[33,] <-  c( NA  , NA  , NA  , .157, NA  , NA  , NA  )
  iParamsLow[34,] <-  c( NA  , .141, .404, NA  , NA  , .168, NA  )
  iParamsLow[35,] <-  c( .598, .127, .168, NA  , NA  , NA  , NA  )
  iParamsLow[36,] <-  c( NA  , .46 , NA  , NA  , .149, NA  , NA  )
  iParamsLow[37,] <-  c( .501, NA  , NA  , .175, NA  , NA  , NA  )
  iParamsLow[38,] <-  c( NA  , NA  , NA  , NA  , .434, .167, NA  )
  iParamsLow[39,] <-  c( .13 , NA  , NA  , NA  , NA  , .105, .13 )
  iParamsLow[40,] <-  c( NA  , NA  , NA  , NA  , NA  , .19, .186 )


  probCorrect <- matrix (nrow=I, ncol=J)
  for (i in 1:I){ # respondents
    for (j in 1:J){ # items
      rVec <- iParamsLow[i,]
      rStar <- rVec^((1-alphaJK[j,])*q[i,])
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
              "probCorrect" = probCorrect, "alphaJK" = alphaJK, "kappa" = kappa, "iParamsLow" = iParamsLow)

  return(out)
}
