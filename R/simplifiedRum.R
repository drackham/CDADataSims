#' Simplified RUM data simulation
#'
#' Creates response data for a simplified version of the RUM model using the Roussos Q matrix
#'
#' @param JJ Number of examinees
#' @param j Examinee j
#' @param II Number of items
#' @param i Item i
#' @param KK Number of skills
#' @param k Skill k
#' @param alphaK Skill mastery population proportion vector
#' @param alphaJK Examinee skill mastery profile
#' @param x response matrix
#' @param pi Probability that an examinee having mastered all the Q required skills for item i will correctly apply all the skills when solving item i.
#' @param iParamsLow Matrix of item parameters for ideal low complexity model
#' @param r Item discrimination r
#' @param c Item difficulty c
#' @param eta Latent ability NOT included in the q-matrix
#' @author Dave Rackham \email{ddrackham@gmail.com}
#' @references \url{http://onlinelibrary.wiley.com/doi/10.1002/j.2333-8504.2008.tb02157.x/abstract}
#' @keywords q-matrix hartz roussos
#'
#' @examples
#' data <- simplifiedRUM()
#'
#' @export
#'
simplifiedRUM <- function(){

  out <- list()
  return(out)
}
