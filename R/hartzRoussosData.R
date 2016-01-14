#' Hartz Roussos (2008) Data simulation
#'
#' Simulates the Hartz Roussos data from "The Fusion Model for Skills Diagnosis: Blending Theory with Practicality" (2008)
#' @param JJ Number of examinees
#' @param j Examinee j
#' @param II Number of ittems
#' @param i Item i
#' @param KK Number of skills
#' @param k Skill k
#' @param alphaK Skill mastery proportion vector
#' @param alphaJK Examinee skill mastery profile
#' @param x response matrix
#' @param pi Probability that an examinee having mastered all the Q required skills for item i will correctly apply all the skills when solving item i.
#' @param r Item discrimination
#' @param c Item difficulty
#' @keywords hartz roussos
#' @export
#' @examples
#'

hartzRoussosData <- function(){
  set.seed(314159)

  JJ <- 1500
  KK <- 7
  alphaK <- c(.3, .4, .45, .5, .55, .6, .65)
  alphaJK <- matrix(nrow=JJ, ncol=KK)

  for (j in 1:JJ){
    for (k in 1:KK){
      alphaJK[j,k] <- rbinom(1,1,alphaK[k])
    }
  }


  out <- list("alphaJK" = alphaJK)
  return(out)
}
