## Selection and Misclassification Biases in Longitudinal Studies
## Source file for bias analysis

## packages load
source("make_data.R")
library(pbapply)
source("compute_risk.R")
source("compute_Rstar.R")
source("compute_RR.R")
source("compute_RRstar.R")
library(data.table)
library(reshape2)
library(stringr)
library(ggplot2)
library(directlabels)
rsession <- sessionInfo()

## Data; Pr 20%; Inc 0.1
sim_list <- vector("list", 1000)
set.seed(123)
sim_list <- pbreplicate(n = 1000, expr = make_data(1000, 0.2, 0.5, -2.85, 3, 
                                                  c(seq(.6, .95, .05), .98, .99, 1),
                                                  c(seq(.6, .95, .05), .98, .99, 1)),
                        simplify = FALSE)

se <- c(seq(.6, .95, .05), .98, .99, 1)
sp <- c(seq(.6, .95, .05), .98, .99, 1)
se_sp <- expand.grid(se, sp)
col_names <- paste(se_sp[, 1], se_sp[, 2], sep = "_")
## Compute R* and RR*
R_star <- compute_Rstar(sim_list, col_names, 1000)
RR_star <- compute_RRstar(sim_list, col_names, 1000)

save(R_star, RR_star, "R_star_20_10")
rm(sim_list, R_star, RR_star)

###############################################################################
## Data; Pr 20%; Inc 0.01
sim_list <- vector("list", 1000)
set.seed(123)
sim_list <- pbreplicate(n = 1000, expr = make_data(1000, 0.2, 0.5, -5.25, 3, 
                                                  c(seq(.6, .95, .05), .98, .99, 1),
                                                  c(seq(.6, .95, .05), .98, .99, 1)),
                        simplify = FALSE)

## Compute R* and RR*
R_star <- compute_Rstar(sim_list, col_names, 1000)
RR_star <- compute_RRstar(sim_list, col_names, 1000)

save(R_star, RR_star, "R_star_20_01")
rm(sim_list, R_star, RR_star)

###############################################################################
## Data; Pr 20%; Inc 0.05
sim_list <- vector("list", 1000)
set.seed(123)
sim_list <- pbreplicate(n = 1000, expr = make_data(1000, 0.2, 0.5, -3.625, 3, 
                                                  c(seq(.6, .95, .05), .98, .99, 1),
                                                  c(seq(.6, .95, .05), .98, .99, 1)),
                        simplify = FALSE)

## Compute R* and RR*
R_star <- compute_Rstar(sim_list, col_names, 1000)
RR_star <- compute_RRstar(sim_list, col_names, 1000)

save(R_star, RR_star, "R_star_20_05")
rm(sim_list, R_star, RR_star)

###############################################################################
## Data; Pr 5%; Inc 0.1
sim_list <- vector("list", 1000)
set.seed(123)
sim_list <- pbreplicate(n = 1000, expr = make_data(1000, 0.05, 0.5, -2.85, 3, 
                                                  c(seq(.6, .95, .05), .98, .99, 1),
                                                  c(seq(.6, .95, .05), .98, .99, 1)),
                        simplify = FALSE)

## Compute R* and RR*
R_star <- compute_Rstar(sim_list, col_names, 1000)
RR_star <- compute_RRstar(sim_list, col_names, 1000)

save(R_star, RR_star, "R_star_05_10")
rm(sim_list, R_star, RR_star)

###############################################################################
## Data; Pr 5%; Inc 0.05
sim_list <- vector("list", 1000)
set.seed(123)
sim_list <- pbreplicate(n = 1000, expr = make_data(1000, 0.05, 0.5, -3.625, 3, 
                                                  c(seq(.6, .95, .05), .98, .99, 1),
                                                  c(seq(.6, .95, .05), .98, .99, 1)),
                        simplify = FALSE)

## Compute R* and RR*
R_star <- compute_Rstar(sim_list, col_names, 1000)
RR_star <- compute_RRstar(sim_list, col_names, 1000)

save(R_star, RR_star, "R_star_05_05")
rm(sim_list, R_star, RR_star)

###############################################################################
## Data; Pr 5%; Inc 0.01
sim_list <- vector("list", 1000)
set.seed(123)
sim_list <- pbreplicate(n = 1000, expr = make_data(1000, 0.05, 0.5, -5.25, 3, 
                                                  c(seq(.6, .95, .05), .98, .99, 1),
                                                  c(seq(.6, .95, .05), .98, .99, 1)),
                        simplify = FALSE)

## Compute R* and RR*
R_star <- compute_Rstar(sim_list, col_names, 1000)
RR_star <- compute_RRstar(sim_list, col_names, 1000)

save(R_star, RR_star, "R_star_05_01")
rm(sim_list, R_star, RR_star)

###############################################################################
## Data; Pr 50%; Inc 0.1
sim_list <- vector("list", 1000)
set.seed(123)
sim_list <- pbreplicate(n = 1000, expr = make_data(1000, 0.5, 0.5, -2.85, 3, 
                                                  c(seq(.6, .95, .05), .98, .99, 1),
                                                  c(seq(.6, .95, .05), .98, .99, 1)),
                        simplify = FALSE)

## Compute R* and RR*
R_star <- compute_Rstar(sim_list, col_names, 1000)
RR_star <- compute_RRstar(sim_list, col_names, 1000)

save(R_star, RR_star, "R_star_50_10")
rm(sim_list, R_star, RR_star)

###############################################################################
## Data; Pr 50%; Inc 0.05
sim_list <- vector("list", 1000)
set.seed(123)
sim_list <- pbreplicate(n = 1000, expr = make_data(1000, 0.5, 0.5, -3.625, 3, 
                                                  c(seq(.6, .95, .05), .98, .99, 1),
                                                  c(seq(.6, .95, .05), .98, .99, 1)),
                        simplify = FALSE)

## Compute R* and RR*
R_star <- compute_Rstar(sim_list, col_names, 1000)
RR_star <- compute_RRstar(sim_list, col_names, 1000)

save(R_star, RR_star, "R_star_50_05")
rm(sim_list, R_star, RR_star)

###############################################################################
## Data; Pr 50%; Inc 0.01
sim_list <- vector("list", 1000)
set.seed(123)
sim_list <- pbreplicate(n = 1000, expr = make_data(1000, 0.5, 0.5, -5.25, 3, 
                                                  c(seq(.6, .95, .05), .98, .99, 1),
                                                  c(seq(.6, .95, .05), .98, .99, 1)),
                        simplify = FALSE)

## Compute R* and RR*
R_star <- compute_Rstar(sim_list, col_names, 1000)
RR_star <- compute_RRstar(sim_list, col_names, 1000)

save(R_star, RR_star, "R_star_50_01")
rm(sim_list, R_star, RR_star)

###############################################################################
sink("sessionInfo.txt")
sessionInfo()
sink()
