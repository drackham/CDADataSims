#' RDINA Simple Q Data Analysis
#'
#' Creates a data analysis data frame that can be used to inspect the integrity of the data simulation
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
#' @param data Must be rDINASimpleQ data
#' @param q Must be qLow q
#'
#' @author Dave Rackham \email{ddrackham@gmail.com}
#' @references \url{http://onlinelibrary.wiley.com/doi/10.1002/j.2333-8504.2008.tb02157.x/abstract} # TODO: Update
#' @keywords q-matrix
#'
#' @export

rDINASimpleQDataAnalysis <- function(data, q){

  skill1 <- q[,1]
  skill2 <- q[,2]
  f <- round( data$f, 3)
  d <- round( data$d , 3)
  probCorrectMaster <- round( boot::inv.logit(f + d), 3 )
  probCorrectNonMaster <- round( boot::inv.logit(f) ,3 )
  numMasters <- c(rep(600,20), rep(300,10))
  numNonMasters <- data$I - numMasters
  correct <- colSums(y)
  incorrect <- data$I - correct
  numMisses <- abs(numMasters - correct)

  dataAnalysis <- data.frame(skill1, skill2, f, d, probCorrectMaster, probCorrectNonMaster, numMasters, numNonMasters, correct, incorrect, numMisses)

  return(dataAnalysis)
}
