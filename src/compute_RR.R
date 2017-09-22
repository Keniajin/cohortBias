#' Compute relative risk RR
#'
#' Compute relative risk.
#'
#' @param data Data file.
#' @param var Variable identifying observations at risk (==0).
#' @param exposed Exposure variable.
#' @param case Outcome variable.
#'
#' @author Denis Haine
#' @export
compute_RR <- function(data, var, exposed, case) {
    at_risk <- data[which(data[, var] == 0), ]

    tab.df <- table(at_risk[, exposed], at_risk[, case])
    tab <- tab.df[2:1, 2:1]

    a <- tab[1, 1]
    b <- tab[1, 2]
    c <- tab[2, 1]
    d <- tab[2, 2]

    rr <- (a/(a + b)) / (c/(c + d))
    if(rr == Inf) rr <- NA
    return(rr)
}
