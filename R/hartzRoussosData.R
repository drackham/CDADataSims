#' Hartz Roussos (2008) Data simulation
#'
#' Simulates the Hartz Roussos data from "The Fusion Model for Skills Diagnosis: Blending Theory with Practicality" (2008)
#'
#' \section{\strong{Notation}}{
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
#'      r \tab Item discrimination \cr
#'      c \tab Item difficulty \cr
#'      eta \tab Latent ability NOT included in the q-matrix \cr
#'    }
#'  }
#' }
#'
#'
#' @author Dave Rackham \email{ddrackham@gmail.com}
#' @references \url{http://onlinelibrary.wiley.com/doi/10.1002/j.2333-8504.2008.tb02157.x/abstract}
#' @keywords hartz roussos
#'
#' @examples
#' data <- hartzRoussosData()
#' @export

hartzRoussosData <- function(){
  set.seed(314159)

  JJ <- 1500
  II <- 40
  KK <- 7
  q <- hartzRoussosQLow()
  alphaK <- c(.3, .4, .45, .5, .55, .6, .65)
  alphaJK <- matrix(nrow = JJ, ncol = KK)
  iParamsLow <- matrix(nrow = II, ncol = KK + 2) # n skills k + 1 r param and + 1 c param

  for (j in 1:JJ){
    for (k in 1:KK){
      alphaJK[j,k] <- rbinom(1,1,alphaK[k])
    }
  }

  # Note 1/13/16: iParamsLow[4,9], [5,25], [6,13] and [6,20] are incorrectly specified in
  # The Fusion Model for Skills Diagnosis: Blending Theory with Practicality" (2008). While
  # values are provided in the paper, QLow is NA for those cells.

                      # pi*   r1    r2    r3    r4     r5   r6    r7     c
  iParamsLow[1,] <-  c(.869, NA  , NA  , .447, NA  , .197, NA  , NA  , 1.128)
  iParamsLow[2,] <-  c(.834, .146, NA  , NA  , NA  , NA  , NA  , NA  , .156)
  iParamsLow[3,] <-  c(.936, .158, NA  , NA  , NA  , .122, NA  , NA  , 1.796)
  iParamsLow[4,] <-  c(.896, NA  , NA  , NA  , NA  , NA  , NA  , .13 , 1.295)
  iParamsLow[5,] <-  c(.957, .177, NA  , NA  , .157, NA  , NA  , NA  , 2.06)
  iParamsLow[6,] <-  c(.889, NA  , .494, NA  , NA  , .442, NA  , .184, 2.476)
  iParamsLow[7,] <-  c(.827, NA  , NA  , .403, .405, NA  , NA  , .111, 1.951)
  iParamsLow[8,] <-  c(.805, NA  , NA  , .464, NA  , .132, NA  , NA  , .647)
  iParamsLow[9,] <-  c(.894, NA  , NA  , .493, NA  , .171, NA  , NA  , .45)
  iParamsLow[10,] <- c(.871, NA  , NA  , NA  , .153, NA  , NA  , NA  , .684)

                      # pi*   r1    r2    r3    r4     r5   r6    r7     c
  iParamsLow[11,] <-  c(.861, NA  , NA  , NA  , NA  , NA  , .118, NA  , .281)
  iParamsLow[12,] <-  c(.907, NA  , NA  , NA  , NA  , NA  , NA  , .104, .642)
  iParamsLow[13,] <-  c(.953, .575, NA  , NA  , .167, NA  , NA  , .14 , 1.872)
  iParamsLow[14,] <-  c(.838, NA  , NA  , NA  , .105, NA  , NA  , NA  , 2.164)
  iParamsLow[15,] <-  c(.965, NA  , .114, .197, NA  , NA  , NA  , NA  , .093)
  iParamsLow[16,] <-  c(.884, NA  , NA  , NA  , NA  , .104, NA  , .198, .686)
  iParamsLow[17,] <-  c(.831, NA  , NA  , NA  , NA  , .477, NA  , .153, .633)
  iParamsLow[18,] <-  c(.952, .142, NA  , .43 , NA  , NA  , .103, .162, 2.406)
  iParamsLow[19,] <-  c(.928, .516, NA  , NA  , NA  , NA  , .179, NA  , .171)
  iParamsLow[20,] <-  c(.962, .509, NA  , NA  , NA  , NA  , NA  , NA  , 1.371)

                      # pi*   r1    r2    r3    r4     r5   r6    r7     c
  iParamsLow[21,] <-  c(.921, NA  , .18 , NA  , NA  , NA  , NA  , NA  , 1.671)
  iParamsLow[22,] <-  c(.911, NA  , NA  , NA  , NA  , NA  , .191, NA  , .057)
  iParamsLow[23,] <-  c(.856, NA  , NA  , NA  , .192, NA  , NA  , NA  , .267)
  iParamsLow[24,] <-  c(.833, NA  , NA  , NA  , .415, NA  , .183, NA  , 1.709)
  iParamsLow[25,] <-  c(.877, NA  , .115, NA  , NA  , NA  , .143, NA  , 1.67)
  iParamsLow[26,] <-  c(.934, NA  , NA  , NA  , NA  , .148, NA  , NA  , 1.755)
  iParamsLow[27,] <-  c(.939, NA  , NA  , NA  , NA  , NA  , .495, .185, 2.331)
  iParamsLow[28,] <-  c(.928, NA  , .445, .183, .195, NA  , NA  , NA  , 1.013)
  iParamsLow[29,] <-  c(.903, NA  , .402, .496, NA  , .104, NA  , .181, 1.006)
  iParamsLow[30,] <-  c(.958, NA  , NA  , .419, NA  , NA  , NA  , .159, 1.429)

                      # pi*   r1    r2    r3    r4     r5   r6    r7     c
  iParamsLow[31,] <-  c(.897, .191, NA  , NA  , .14 , NA  , .187, NA  , 1.831)
  iParamsLow[32,] <-  c(.801, NA  , .455, NA  , .186, NA  , NA  , NA  , 1.819)
  iParamsLow[33,] <-  c(.907, NA  , NA  , NA  , .157, NA  , NA  , NA  , .593)
  iParamsLow[34,] <-  c(.84 , NA  , .141, .404, NA  , NA  , .168, NA  , 1.108)
  iParamsLow[35,] <-  c(.88 , .598, .127, .168, NA  , NA  , NA  , NA  , 2.336)
  iParamsLow[36,] <-  c(.936, NA  , .46 , NA  , NA  , .149, NA  , NA  , 2.258)
  iParamsLow[37,] <-  c(.809, .501, NA  , NA  , .175, NA  , NA  , NA  , 2.43)
  iParamsLow[38,] <-  c(.866, NA  , NA  , NA  , NA  , .434, .167, NA  , .172)
  iParamsLow[39,] <-  c(.826, .13 , NA  , NA  , NA  , NA  , .105, .13 , 2.329)
  iParamsLow[40,] <-  c(.868, NA  , NA  , NA  , NA  , NA  , .19, .186 , 1.356)

  # Generate the item responses

  probCorrect <- matrix (nrow=JJ, ncol=II)
  for (j in 1:JJ){ # respondents
    for (i in 1:II){ # items
      rVec <- iParamsLow[i,2:8]
      rStar <- rVec^((1-alphaJK[j,])*q[i,])
      c <- iParamsLow[i,9]
      eta <- 0 #0 widens, 10 nullifies

      piStar <- iParamsLow[i,1]
      rStar <- prod(rStar)
      rasch <- (1 / (1 + exp (-1.7 * (eta - (-c))))) # -1.7 equates it to normal ogive
      probCorrect[j,i] <- piStar * rStar * rasch

    }
  }

  y <- matrix (nrow=JJ, ncol=II)
  for (j in 1:JJ){
    for (i in 1:II){
      y[j,i] <- rbinom(1,1,probCorrect[j,i])
    }
  }

  out <- list("alphaJK" = alphaJK, "iParamsLow" = iParamsLow, "probCorrect" = probCorrect, "y" = y)
  return(out)
}
