#' @include compute_RR.R
#' NULL

#' Compute total, selection, and misclassification biased relative risk RR*
#'
#' Compute relative risk for selection bias (S_1' and S2), misclassification bias
#' (S_1 and S_2'), and total bias (S_1' and S_2').
#'
#' @param data Data file.
#' @param column_names Vector of column names that identifies the combinations of Se
#' and Sp used to create S_1' and S_2'.
#' @param nsimul Number of simulations.
#' 
#' @author Denis Haine
#' @export
#' @importFrom utils setTxtProgressBar txtProgressBar
compute_RRstar <- function(data, column_names, nsimul) {
    ## Collect results
    out_list <- vector("list", nsimul)
    m <- matrix(NA, nrow = 3, ncol = length(column_names))
    colnames(m) <- paste("se_sp", column_names, sep = "_")
    rownames(m) <- c("Total bias", "Selection bias", "Misclassification bias")

    pb <- utils::txtProgressBar(min = 0, max = nsimul, style = 3)

    for (j in 1:nsimul) {

        ## 1. Total bias
        S1i <- paste("S1i_", column_names, sep = "")
        S2i <- paste("S2i_", column_names, sep = "")
        for (n in 1:length(column_names)) {
            RR_star <- compute_RR(data[[j]], S1i[n], "exp_var", S2i[n])
            m[1, n] <- RR_star
        }

        ## 2. Selection bias
        S1i <- paste("S1i_", column_names, sep = "")
        for (n in 1:length(column_names)) {
            RR_star <- compute_RR(data[[j]], S1i[n], "exp_var", "S2")
            m[2, n] <- RR_star
        }

        ## 3. Misclassification bias
        S2i <- paste("S2i_", column_names, sep = "")
        for (n in 1:length(column_names)) {
            RR_star <- compute_RR(data[[j]], "S1", "exp_var", S2i[n])
            m[3, n] <- RR_star
        }
        
        out_list[[j]] <- m

        utils::setTxtProgressBar(pb, j)
    }
    close(pb)
    out_list <- lapply(out_list, function(x) as.data.frame(x))
    out_list
}
