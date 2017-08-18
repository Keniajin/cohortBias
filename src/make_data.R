#' Create simulated datasets.
#'
#' Create datasets from a hypothetical cohort study. The generated datasets are
#' deemed to have occurred from the collection of two samples collected 1 month
#' apart of 1,000 observations. The first sample (S1) is used to identify observations
#' at risk of being infected at the beginning of the cohort, while the second (S2)
#' is used to identify the outcome.
#'
#' @param n_obs Number of observations.
#' @param Pr Disease prevalence.
#' @param inc Disease incidence.
#' @param se Vector of test sensitivities (Se).
#' @param sp Vector of test specificities (Sp).
#'
#' @return A data frame with variables:
#' \item{obs}{Observation id.}
#' \item{se}{Sensitivity of test.}
#' \item{sp}{Specificity of test.}
#' \item{S1}{First sample true status.}
#' \item{S2}{Second sample true status.}
#' \item{S1i}{Misclassified first sample.}
#' \item{S2i}{Misclassified second sample.}
#' 
#' @examples
#' # Initiate a list to store the n data frames
#' sim_list <- vector("list", 5)
#' # Do not forget to set seed for replication
#' set.seed(123)
#' sim_list <- replicate(n = 5, expr = make_data(100, 0.2, 0.1, seq(.65, .75, .05),
#' seq(.7, .85, .05)), simplify = FALSE)
#' # Or with a progress bar
#' require(pbapply)
#' sim_list <- pbreplicate(n = 5, expr = make_data(100, 0.2, 0.1, seq(.6, 1, .05),
#' seq(.6, 1, .05)), simplify = FALSE)
make_data <- function(n_obs,
                      Pr = NULL,
                      inc = NULL,
                      se = NULL,
                      sp = NULL) {
    if(is.null(Pr) | is.null(inc) | is.null(se) | is.null(sp))
        stop('Missing argument(s)')
    if((Pr < 0.01) | (Pr > 0.9))
        stop('Prevalence should be between 0 and 1')
    if(is.null(se))
        stop('Missing Se.')
    if(is.null(sp))
        stop('Missing Sp.')
    if(!all(se >= 0.5 & se <= 1))
        stop('Sensitivity should be between 0.5 and 1')
    if(!all(sp >= 0.5 & sp <= 1))
        stop('Specificity should be between 0.5 and 1')

    dt <- matrix(NA, nrow = n_obs, ncol = 1)
    colnames(dt) <- "obs"
    dt[, 1] <- 1:nrow(dt)
    
    ## Draw from binomial distribution with pi
    dt <- cbind(dt, S1 = rbinom(nrow(dt), 1, Pr))
    
    ## Divide the data set in observations at risk observations already infected
    atrisk <- dt[dt[, "S1"] == 0, ]
    notatrisk <- dt[dt[, "S1"] == 1, ]
    ## Attribute values to S2 for observations at risk based on Incidence distribution
    ## Draw from binomial distribution
    atrisk <- cbind(atrisk, S2 = rbinom(nrow(atrisk), 1, inc))

    ## No elimination, so notatrisk stay infected
    notatrisk <- cbind(notatrisk, S2 = 1)
    
    dt <- rbind(atrisk, notatrisk)
    dt <- as.data.frame(dt)

    se_sp <- expand.grid(se, sp)

    ## Compute misclassified resuls for S1i and S2i
    for (i in 1:nrow(se_sp)) {
        dt[, 3+i] <- ifelse(dt$S1 == 1,
                            rbinom(length(dt[dt$S1==1, ]$S1), 1, se_sp[i, 1]),
                            rbinom(length(dt[dt$S1==1, ]$S1), 1, 1 - se_sp[i, 2])
                            )
    }
    for (i in 1:nrow(se_sp)) {
        dt[, 3+nrow(se_sp)+i] <- ifelse(dt$S2 == 1,
                                        rbinom(length(dt[dt$S2==1, ]$S2), 1, se_sp[i, 1]),
                                        rbinom(length(dt[dt$S2==1, ]$S2), 1,
                                               1 - se_sp[i, 2]))
    }
    names_S1i <- paste("S1i", se_sp[, 1], se_sp[, 2], sep = "_")
    names_S2i <- paste("S2i", se_sp[, 1], se_sp[, 2], sep = "_")
    colnames(dt) <- c("obs", "S1", "S2", names_S1i, names_S2i)
    return(dt)
}

