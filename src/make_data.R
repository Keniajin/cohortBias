#' Create simulated datasets.
#'
#' Create datasets from a hypothetical cohort study. The generated datasets are
#' deemed to have occurred from the collection of two samples collected 1 month
#' apart of 1,000 observations. The first sample (S1) is used to identify observations
#' at risk of being infected at the beginning of the cohort, while the second (S2)
#' is used to identify the outcome. Exposure is drawn from a binomial distribution
#' with success probability = 0.5. Relative risk is set at 3. Disease incidence is
#' given by the intercept b0_inc, set at -5.25 for an incidence of 0.01, -3.625 for
#' an incidence of 0.05, and at -2.85 for an incidence of 0.1.
#'
#' @param n_obs Number of observations.
#' @param Pr Disease prevalence.
#' @param E_Pr Exposure distribution (= 0.5).
#' @param b0_inc Intercept for disease incidence. Incidence of 0.01 = -5.25; incidence of 0.05 = -3.625; incidence of 0.1 = -2.85.
#' @param RR_inc RR of association between observation variable and disease incidence. Set at 3.
#' @param se Vector of test sensitivities (Se).
#' @param sp Vector of test specificities (Sp).
#'
#' @return A data frame with variables:
#' \item{obs}{Observation id.}
#' \item{exp_var}{Exposure variable.}
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
#' sim_list <- replicate(n = 5, expr = make_data(100, 0.2, 0.5, -3.625, 3,
#' seq(.65, .75, .05), seq(.7, .85, .05)), simplify = FALSE)
#' # Or with a progress bar
#' require(pbapply)
#' sim_list <- pbreplicate(n = 5, expr = make_data(100, 0.2, 0.5, -3.625, 3,
#' seq(.6, 1, .05), seq(.6, 1, .05)), simplify = FALSE)
make_data <- function(n_obs,
                      Pr = NULL,
                      E_Pr = 0.5,
                      b0_inc = NULL,
                      RR_inc = 3,
                      se = NULL,
                      sp = NULL) {
    if(is.null(Pr) | is.null(b0_inc) | is.null(RR_inc) | is.null(se) | is.null(sp))
        stop('Missing argument(s)')
    if((Pr < 0.01) | (Pr > 0.9))
        stop('Prevalence should be between 0 and 1')
    if(RR_inc < 0)
        stop('RR should be > 0')
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
    
    ## S1: Draw from binomial distribution with pi
    dt <- cbind(dt, S1 = rbinom(nrow(dt), 1, Pr))
    ## Exposure
    dt <- cbind(dt, exp_var = rbinom(nrow(dt), 1, E_Pr))
    
    ## Divide the data set in observations at risk observations already infected
    atrisk <- dt[dt[, "S1"] == 0, ]
    notatrisk <- dt[dt[, "S1"] == 1, ]
    ## Attribute values to S2 for observations at risk based on Incidence distribution
    ## Compute probability (pi) that S2 == 1
    atrisk <- cbind(atrisk,
                    pi = (1 / (1 + exp(-1 * (b0_inc + log(RR_inc) * atrisk[, 3])))))
    ## Draw from Bernoulli distribution with pi
    atrisk <- cbind(atrisk, S2 = rbinom(nrow(atrisk), 1, atrisk[, 4]))

    ## Add a fake column pi (for easy binding down the road)
    notatrisk <- cbind(notatrisk, pi = NA)
    ## No elimination, so notatrisk stay infected
    notatrisk <- cbind(notatrisk, S2 = 1)
    
    dt <- rbind(atrisk, notatrisk)
    dt <- as.data.frame(dt)

    se_sp <- expand.grid(se, sp)

    ## Compute misclassified resuls for S1i and S2i
    for (i in 1:nrow(se_sp)) {
        dt[, 5+i] <- ifelse(dt$S1 == 1,
                            rbinom(length(dt[dt$S1==1, ]$S1), 1, se_sp[i, 1]),
                            rbinom(length(dt[dt$S1==1, ]$S1), 1, 1 - se_sp[i, 2])
                            )
    }
    for (i in 1:nrow(se_sp)) {
        dt[, 5+nrow(se_sp)+i] <- ifelse(dt$S2 == 1,
                                        rbinom(length(dt[dt$S2==1, ]$S2), 1, se_sp[i, 1]),
                                        rbinom(length(dt[dt$S2==1, ]$S2), 1,
                                               1 - se_sp[i, 2]))
    }
    names_S1i <- paste("S1i", se_sp[, 1], se_sp[, 2], sep = "_")
    names_S2i <- paste("S2i", se_sp[, 1], se_sp[, 2], sep = "_")
    dt <- subset(dt, select = -c(pi))
    colnames(dt) <- c("obs", "S1", "exp_var", "S2", names_S1i, names_S2i)
    return(dt)
}

