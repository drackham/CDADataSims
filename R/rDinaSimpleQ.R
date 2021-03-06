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
#'      type \tab simulation type (low_noise, high_noise, uninformative) \cr
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

rDINASimpleQ <- function(I, type){
	set.seed(314159)
	
	q <- simpleQ()
	I <- I # examinees
	J <- nrow(q) # items
	K <- ncol(q) # skill
	
	# specify d and f according to the type of data simulation
	switch(type, 
  			 low_noise = {
					f <- stats::runif(J, min = -5, max = -3.75)
					d <- stats::runif(J, min = 7, max = 9)
  			 },
  			 high_noise = {
  			 	f <- stats::runif(J, min = -1, max = 1)
  			 	d <- stats::runif(J, min = 1, max = 1)  			 	
  			 },
  			 # no variance in item characteristics with lots of noise
				 uninformative = {
  			 	f <- rep(-1, times = J)
  			 	d <- rep(1, times = J)
  			 })
	
  g <- boot::inv.logit(f) # guess
  s <- 1 - boot::inv.logit(f + d) # slip

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
      resp[i,1] <- stats::rbinom(1, 1, boot::inv.logit( f[1] + ( d[1] * alphaIK[i,1] ) ) )
      resp[i,2] <- stats::rbinom(1, 1, boot::inv.logit( f[2] + ( d[2] * alphaIK[i,1] ) ) )
      resp[i,3] <- stats::rbinom(1, 1, boot::inv.logit( f[3] + ( d[3] * alphaIK[i,1] ) ) )
      resp[i,4] <- stats::rbinom(1, 1, boot::inv.logit( f[4] + ( d[4] * alphaIK[i,1] ) ) )
      resp[i,5] <- stats::rbinom(1, 1, boot::inv.logit( f[5] + ( d[5] * alphaIK[i,1] ) ) )
      resp[i,6] <- stats::rbinom(1, 1, boot::inv.logit( f[6] + ( d[6] * alphaIK[i,1] ) ) )
      resp[i,7] <- stats::rbinom(1, 1, boot::inv.logit( f[7] + ( d[7] * alphaIK[i,1] ) ) )
      resp[i,8] <- stats::rbinom(1, 1, boot::inv.logit( f[8] + ( d[8] * alphaIK[i,1] ) ) )
      resp[i,9] <- stats::rbinom(1, 1, boot::inv.logit( f[9] + ( d[9] * alphaIK[i,1] ) ) )
      resp[i,10] <- stats::rbinom(1, 1, boot::inv.logit( f[10] + ( d[10] * alphaIK[i,1] ) ) )

      resp[i,11] <- stats::rbinom(1, 1, boot::inv.logit( f[11] + ( d[11] * alphaIK[i,2] ) ) )
      resp[i,12] <- stats::rbinom(1, 1, boot::inv.logit( f[12] + ( d[12] * alphaIK[i,2] ) ) )
      resp[i,13] <- stats::rbinom(1, 1, boot::inv.logit( f[13] + ( d[13] * alphaIK[i,2] ) ) )
      resp[i,14] <- stats::rbinom(1, 1, boot::inv.logit( f[14] + ( d[14] * alphaIK[i,2] ) ) )
      resp[i,15] <- stats::rbinom(1, 1, boot::inv.logit( f[15] + ( d[15] * alphaIK[i,2] ) ) )
      resp[i,16] <- stats::rbinom(1, 1, boot::inv.logit( f[16] + ( d[16] * alphaIK[i,2] ) ) )
      resp[i,17] <- stats::rbinom(1, 1, boot::inv.logit( f[17] + ( d[17] * alphaIK[i,2] ) ) )
      resp[i,18] <- stats::rbinom(1, 1, boot::inv.logit( f[18] + ( d[18] * alphaIK[i,2] ) ) )
      resp[i,19] <- stats::rbinom(1, 1, boot::inv.logit( f[19] + ( d[19] * alphaIK[i,2] ) ) )
      resp[i,20] <- stats::rbinom(1, 1, boot::inv.logit( f[20] + ( d[20] * alphaIK[i,2] ) ) )

      resp[i,21] <- stats::rbinom(1, 1, boot::inv.logit( f[21] + d[21] * ( alphaIK[i,1] * alphaIK[i,2] ) ) )
      resp[i,22] <- stats::rbinom(1, 1, boot::inv.logit( f[22] + d[22] * ( alphaIK[i,1] * alphaIK[i,2]) ) )
      resp[i,23] <- stats::rbinom(1, 1, boot::inv.logit( f[23] + d[23] * ( alphaIK[i,1] * alphaIK[i,2]) ) )
      resp[i,24] <- stats::rbinom(1, 1, boot::inv.logit( f[24] + d[24] * ( alphaIK[i,1] * alphaIK[i,2]) ) )
      resp[i,25] <- stats::rbinom(1, 1, boot::inv.logit( f[25] + d[25] * ( alphaIK[i,1] * alphaIK[i,2]) ) )
      resp[i,26] <- stats::rbinom(1, 1, boot::inv.logit( f[26] + d[26] * ( alphaIK[i,1] * alphaIK[i,2]) ) )
      resp[i,27] <- stats::rbinom(1, 1, boot::inv.logit( f[27] + d[27] * ( alphaIK[i,1] * alphaIK[i,2]) ) )
      resp[i,28] <- stats::rbinom(1, 1, boot::inv.logit( f[28] + d[28] * ( alphaIK[i,1] * alphaIK[i,2]) ) )
      resp[i,29] <- stats::rbinom(1, 1, boot::inv.logit( f[29] + d[29] * ( alphaIK[i,1] * alphaIK[i,2]) ) )
      resp[i,30] <- stats::rbinom(1, 1, boot::inv.logit( f[30] + d[30] * ( alphaIK[i,1] * alphaIK[i,2]) ) )
    }
  }

  out <- list("I" = I, "J" = J, "K" = K, "resp" = resp, "alphaIK" = alphaIK, "f" = f, "d" = d, "g" = g, 
  						"s" = s)
  return(out)
}
