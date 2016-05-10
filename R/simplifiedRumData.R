#' Simplified RUM data simulation
#'
#' Creates response data for a simplified version of the RUM model using the Hartz Roussos Q matrix (Low)
#'
#'\section{\strong{Notation}}{
#'  \describe{
#'    \tabular{ll}{
#'      JJ \tab Number of examinees \cr
#'      II \tab Number of items \cr
#'      KK \tab Number of skills \cr
#'      j \tab Examinee j \cr
#'      i \tab Item i \cr
#'      k \tab Skill k \cr
#'      alphaK \tab Skill mastery population proportion vector \cr
#'      alphaJK \tab Examinee skill mastery profile \cr
#'      x \tab response matrix \cr
#'      pi \tab Probability that an examinee having mastered all the Q required skills for item i will correctly apply all the skills when solving item i \cr
#'      iParamsLow \tab Matrix of item parameters for ideal low complexity model \cr
#'      rStar \tab Item discrimination \cr
#'      kappa \tab Mastery threshold parameter \cr
#'    }
#'  }
#' }
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

  kappa <- .6
  J <- 5000
  I <- 40
  K <- 7
  q <- hartzRoussosQLow()

  # Generate the final mastery proportions
  alphaK <- c(.3, .4, .45, .5, .55, .6, .65) # These may be too low. Too much noise??
  # alphaK <- c(.4, .5, .55, .6, .65, .7, .75)
  alphaJK <- matrix(nrow = J, ncol = K)
  for (j in 1:J){
    for (k in 1:K){
      alphaJK[j,k] <- rbinom(1,1,alphaK[k])
    }
  }

  # Generate random matery probabilities
  # The problem with this approach below is it prioritizes all latent classes equally
  # Need to figure out how to weight them better?

  # latentClasses <- generateLatentClasses(q)
  masteryJK <- matrix(nrow=J, ncol=K)

  for (j in 1:J){
#     latentClass <- c(latentClasses[sample(nrow(latentClasses),size=1,replace=TRUE),])
#     for(k in 1:K){
#       if(latentClass[k]==0){
#         masteryJK[j,k] <- rbeta(1,2,30)
#       }
#       else{
#         masteryJK[j,k] <- rbeta(1,20,2)
#       }
#     }
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

                        # r1    r2    r3    r4     r5   r6    r7
  iParamsLow[1,] <-  c( 1.0 , 1.0 , .447, 1.0 , .197, 1.0 , 1.0 )
  iParamsLow[2,] <-  c( .146, 1.0 , 1.0 , 1.0 , 1.0 , 1.0 , 1.0 )
  iParamsLow[3,] <-  c( .158, 1.0 , 1.0 , 1.0 , .122, 1.0 , 1.0 )

  iParamsLow[4,] <-  c( 1.0 , 1.0 , 1.0 , 1.0 , 1.0 , 1.0 , .13 )
  iParamsLow[5,] <-  c( .177, 1.0 , 1.0 , .157, 1.0 , 1.0 , 1.0 )
  iParamsLow[6,] <-  c( 1.0 , .494, 1.0 , 1.0 , .442, 1.0 , .184)
  iParamsLow[7,] <-  c( 1.0 , 1.0 , .403, .405, 1.0 , 1.0 , .111)
  iParamsLow[8,] <-  c( 1.0 , 1.0 , .464, 1.0 , .132, 1.0 , 1.0 )
  iParamsLow[9,] <-  c( 1.0 , 1.0 , .493, 1.0 , .171, 1.0 , 1.0 )
  iParamsLow[10,] <- c( 1.0 , 1.0 , 1.0 , .153, 1.0 , 1.0 , 1.0 )

                        # r1    r2    r3    r4     r5   r6    r7
  iParamsLow[11,] <-  c( 1.0 , 1.0 , 1.0 , 1.0 , 1.0 , .118, 1.0 )
  iParamsLow[12,] <-  c( 1.0 , 1.0 , 1.0 , 1.0 , 1.0 , 1.0 , .104)
  iParamsLow[13,] <-  c( .575, 1.0 , 1.0 , .167, 1.0 , 1.0 , .14 )
  iParamsLow[14,] <-  c( 1.0 , 1.0 , 1.0 , .105, 1.0 , 1.0 , 1.0 )
  iParamsLow[15,] <-  c( 1.0 , .114, .197, 1.0 , 1.0 , 1.0 , 1.0 )
  iParamsLow[16,] <-  c( 1.0 , 1.0 , 1.0 , 1.0 , .104, 1.0 , .198)
  iParamsLow[17,] <-  c( 1.0 , 1.0 , 1.0 , 1.0 , .477, 1.0 , .153)
  iParamsLow[18,] <-  c( .142, 1.0 , .43 , 1.0 , 1.0 , .103, .162)
  iParamsLow[19,] <-  c( .516, 1.0 , 1.0 , 1.0 , 1.0 , .179, 1.0 )
  iParamsLow[20,] <-  c( .509, 1.0 , 1.0 , 1.0 , 1.0 , 1.0 , 1.0 )

                        # r1    r2    r3    r4     r5   r6    r7
  iParamsLow[21,] <-  c( 1.0 , .18 , 1.0 , 1.0 , 1.0 , 1.0 , 1.0 )
  iParamsLow[22,] <-  c( 1.0 , 1.0 , 1.0 , 1.0 , 1.0 , .191, 1.0 )
  iParamsLow[23,] <-  c( 1.0 , 1.0 , 1.0 , .192, 1.0 , 1.0 , 1.0 )
  iParamsLow[24,] <-  c( 1.0 , 1.0 , 1.0 , .415, 1.0 , .183, 1.0 )
  iParamsLow[25,] <-  c( 1.0 , .115, 1.0 , 1.0 , 1.0 , .143, 1.0 )
  iParamsLow[26,] <-  c( 1.0 , 1.0 , 1.0 , 1.0 , .148, 1.0 , 1.0 )
  iParamsLow[27,] <-  c( 1.0 , 1.0 , 1.0 , 1.0 , 1.0 , .495, .185)
  iParamsLow[28,] <-  c( 1.0 , .445, .183, .195, 1.0 , 1.0 , 1.0 )
  iParamsLow[29,] <-  c( 1.0 , .402, .496, 1.0 , .104, 1.0 , .181)
  iParamsLow[30,] <-  c( 1.0 , 1.0 , .419, 1.0 , 1.0 , 1.0 , .159)

                        # r1    r2    r3    r4     r5   r6    r7
  iParamsLow[31,] <-  c( .191, 1.0 , 1.0 , .14 , 1.0 , .187, 1.0 )
  iParamsLow[32,] <-  c( 1.0 , .455, 1.0 , .186, 1.0 , 1.0 , 1.0 )
  iParamsLow[33,] <-  c( 1.0 , 1.0 , 1.0 , .157, 1.0 , 1.0 , 1.0 )
  iParamsLow[34,] <-  c( 1.0 , .141, .404, 1.0 , 1.0 , .168, 1.0 )
  iParamsLow[35,] <-  c( .598, .127, .168, 1.0 , 1.0 , 1.0 , 1.0 )
  iParamsLow[36,] <-  c( 1.0 , .46 , 1.0 , 1.0 , .149, 1.0 , 1.0 )
  iParamsLow[37,] <-  c( .501, 1.0 , 1.0 , .175, 1.0 , 1.0 , 1.0 )
  iParamsLow[38,] <-  c( 1.0 , 1.0 , 1.0 , 1.0 , .434, .167, 1.0 )
  iParamsLow[39,] <-  c( .13 , 1.0 , 1.0 , 1.0 , 1.0 , .105, .13 )
  iParamsLow[40,] <-  c( 1.0 , 1.0 , 1.0 , 1.0 , 1.0 , .19, .186 )

  probCorrect <- matrix (nrow=I, ncol=J)
  for (i in 1:I){ # items
    for (j in 1:J){ # respondents
      rVec <- iParamsLow[i,]
      rStar <- rVec^((1-masteryJK[j,])*q[i,])  # Using mastery
      # rStar <- rVec^((1-alphaJK[j,])*q[i,])  # Using alphaJK
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

generateLatentClasses <- function(qmatrix){

  num_att = length(qmatrix[1,])
  max_att = 2^num_att
  latent_classes = matrix (data=NA, max_att, num_att)
  m <- max_att

  for (a in 1:num_att) {
    m = m/2     # Number of repititions of entries 0 or 1 in one cycle
    anf <- 1

    while (anf < max_att) {
      latent_classes[anf : (anf + m -1),a] <- 0
      anf <- anf + m
      latent_classes[anf : (anf + m -1),a] <- 1
      anf <- anf + m
    }
  }
  rownames(latent_classes) = paste("c", 1:max_att, sep= "")
  latent_classes

}
