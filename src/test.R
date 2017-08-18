source("make_data.R")
sim_list <- vector("list", 100)
set.seed(123)
require(pbapply)
sim_list <- pbreplicate(n = 100, expr = make_data(1000, 0.2, 0.1,
                                                  c(seq(.6, .95, .05), .98, .99, 1),
                                                  c(seq(.6, .95, .05), .98, .99, 1)),
                        simplify = FALSE)

source("compute_risk.R")

se <- c(seq(.6, .95, .05), .98, .99, 1)
sp <- c(seq(.6, .95, .05), .98, .99, 1)
se_sp <- expand.grid(se, sp)
col_names <- paste(se_sp[, 1], se_sp[, 2], sep = "_")

source("compute_Rstar.R")

R_star <- compute_Rstar(sim_list, col_names, 100)

R <- sapply(sim_list, function(x) compute_risk(x, "S1", "S2"))

##Rstar_data <- do.call(rbind, R_star[[1,]])

#dat <- expand.grid(Se = seq(.8, 1, .05), 
#                   Sp = seq(.8, 1, .05))
#dat <- dat[rep(seq_len(nrow(dat)), 20), ]
#dat <- dat[order(dat$Se, dat$Sp), ]

library(data.table)
fastbind.ith.rows <- function(i) rbindlist(lapply(R_star, "[", i, TRUE))
fastbound <- lapply(1:3, fastbind.ith.rows)
names(fastbound) <- c("Total bias", "Selection bias", "Misclassification bias")

total_bias <- t(apply(fastbound[[1]], 2, function(x) quantile(x, c(.025, .5, .0975))))
colnames(total_bias) <- c("q2.5", "q50", "q97.5")
library(reshape2)
library(stringr)
ids <- rownames(total_bias)
ids <- str_sub(ids, start = 7)
total_bias <- cbind(total_bias, colsplit(ids, '_', names = c("Se", "Sp")))

#library(reshape2)
#t <- melt(fastbound[[1]])
#library(stringr)
#t$variable <- as.character(t$variable)
#t$variable <- str_sub(t$variable, start = 7)
#t <- cbind(t, colsplit(t$variable, '_', names = c("Se", "Sp")))

#densify <- function(df, simul) {
#    ncolumns <- ncol(df)
#    dflist <- list()
#    CI <- matrix(NA, nrow = ncolumns, ncol = 3)
#    colnames(CI) <- c("q2.5", "q50", "q97.5")
#    for (i in 1:ncolumns) {
#        pre.dens <- melt(df[, i])
#        pre.dens <- exp(pre.dens)
#        CI[i, ] <- quantile(df[, i], c(.025, .5, .975))
#        dens <- density(pre.dens$value)
#        dens <- data.frame(x = dens$x, y = dens$y)
#        dens$simul <- simul[i]
#        dflist[[i]] <- dens
#    }
#    densified <- do.call(rbind, dflist)
#    out_list <- list(densified, CrI)
#    out_list
#}

library(ggplot2)
library(directlabels)
p <- ggplot(total_bias, aes(x = Se, y = Sp, z = q50)) +
    geom_contour(aes(colour = ..level..)) +
    scale_x_continuous(breaks = seq(0.5, 1, .05), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0.5, 1, .05), expand = c(0,0)) +
    coord_fixed(ylim = c(0.5, 1.025), xlim = c(0.5, 1.025)) +
    scale_colour_gradient(guide = 'none') +
    theme(axis.title = element_text(face="bold"),
          axis.text.y = element_text(colour="grey42"),
          axis.text.x = element_text(colour="grey42"),
          legend.position = "none") +
    xlab("Sensitivity") +
    ylab("Specificity") +
    ggtitle("R*")
direct.label(p, list("far.from.others.borders", "calc.boxes", "enlarge.box", 
      hjust=1, vjust=-.5, box.color = NA, fill = "transparent", "draw.rects"))

selection_bias <- t(apply(fastbound[[2]], 2,
                          function(x) quantile(x, c(.025, .5, .0975))))
colnames(selection_bias) <- c("q2.5", "q50", "q97.5")
ids <- rownames(selection_bias)
ids <- str_sub(ids, start = 7)
selection_bias <- cbind(selection_bias, colsplit(ids, '_', names = c("Se", "Sp")))

p <- ggplot(selection_bias, aes(x = Se, y = Sp, z = q50)) +
    geom_contour(aes(colour = ..level..)) +
    scale_x_continuous(breaks = seq(0.5, 1, .05), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0.5, 1, .05), expand = c(0,0)) +
    coord_fixed(ylim = c(0.5, 1.025), xlim = c(0.5, 1.025)) +
    scale_colour_gradient(guide = 'none') +
    theme(axis.title = element_text(face="bold"),
          axis.text.y = element_text(colour="grey42"),
          axis.text.x = element_text(colour="grey42"),
          legend.position = "none") +
    xlab("Sensitivity") +
    ylab("Specificity") +
    ggtitle("R*")
direct.label(p, list("far.from.others.borders", "calc.boxes", "enlarge.box", 
      hjust=1, vjust=-.5, box.color = NA, fill = "transparent", "draw.rects"))

misclassification_bias <- t(apply(fastbound[[3]], 2,
                          function(x) quantile(x, c(.025, .5, .0975))))
colnames(misclassification_bias) <- c("q2.5", "q50", "q97.5")
ids <- rownames(misclassification_bias)
ids <- str_sub(ids, start = 7)
misclassification_bias <- cbind(misclassification_bias,
                                colsplit(ids, '_', names = c("Se", "Sp")))

p <- ggplot(misclassification_bias, aes(x = Se, y = Sp, z = q50)) +
    geom_contour(aes(colour = ..level..)) +
    scale_x_continuous(breaks = seq(0.5, 1, .05), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0.5, 1, .05), expand = c(0,0)) +
    coord_fixed(ylim = c(0.5, 1.025), xlim = c(0.5, 1.025)) +
    scale_colour_gradient(guide = 'none') +
    theme(axis.title = element_text(face="bold"),
          axis.text.y = element_text(colour="grey42"),
          axis.text.x = element_text(colour="grey42"),
          legend.position = "none") +
    xlab("Sensitivity") +
    ylab("Specificity") +
    ggtitle("R*")
direct.label(p, list("far.from.others.borders", "calc.boxes", "enlarge.box", 
      hjust=1, vjust=-.5, box.color = NA, fill = "transparent", "draw.rects"))



ggplot(t, aes(x = Se, y = Sp)) +
    stat_density2d(geom = "tile", aes(fill = value, alpha = ..density..),
                   contour = FALSE) + 
#  scale_fill_manual(values = c("a"="#FF0000", "b"="#00FF00")) +
    geom_point() +
    theme_minimal()

set.seed(1001)
d <- data.frame(x=rnorm(1000),y=rnorm(1000))
getLevel <- function(x,y,prob=0.95) {
    kk <- MASS::kde2d(x,y)
    dx <- diff(kk$x[1:2])
    dy <- diff(kk$y[1:2])
    sz <- sort(kk$z)
    c1 <- cumsum(sz) * dx * dy
    approx(c1, sz, xout = 1 - prob)$y
}
L95 <- getLevel(d$x,d$y)
library(ggplot2); theme_set(theme_bw())
ggplot(d,aes(x,y)) +
   stat_density2d(geom="tile", aes(fill = ..density..),
                  contour = FALSE)+
   stat_density2d(colour="red",breaks=L95)
kk <- with(d,MASS::kde2d(x,y))
dimnames(kk$z) <- list(kk$x,kk$y)
dc <- melt(kk$z)
ggplot(dc,aes(x=Var1,y=Var2))+
   geom_tile(aes(fill=value))+
    geom_contour(aes(z=value),breaks=L95,colour="red")


ggplot(t, aes(x = Se, y = Sp, z = value)) +
    stat_contour(geom = "polygon", aes(fill = ..level..)) +
    geom_tile(aes(fill = value)) +
    stat_contour(bins = 20)

ggplot(t) +
    geom_tile(aes(x = Se, y = Sp, fill = value))

ggplot(data = t, aes(x = Se, y = Sp, z = value)) +
    geom_tile(aes(fill = value)) +
    stat_contour(bins = 30)

ggplot() +
    theme_bw() +
    stat_contour(data = t, aes(x = Se, y = Sp, z = value, colour = ..level..),
                 breaks = round(quantile(t$value, seq(0, 1, 0.1)), 0), size = 1) +
    scale_color_continuous() +
    theme(legend.justification=c(1, 0), legend.position=c(1, 0))


library(RColorBrewer)
library(ggthemes)
# build color Palette
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
# Plot
ggplot(t, aes(Se, Sp, fill = ..level..)) +
    stat_density_2d(bins = 11, geom = "polygon") +
    scale_fill_gradientn(colours = myPalette(11)) +
    theme_minimal() +
    coord_fixed(ratio = 1)


commonTheme <- list(labs(color = "Density", fill = "Density", x = "Se", y = "Sp"),
                    theme_bw(), theme(legend.position = c(0, 1),
                                      legend.justification = c(0, 1)))
ggplot(data = t, aes(Se, Sp)) + 
    geom_density2d(aes(value, colour = ..level..)) + 
    scale_colour_gradient(low = "green", high = "red") + 
    geom_point() +
    commonTheme

library(MASS)
library(ggplot2)
n <- 1000
x <- mvrnorm(n, mu=c(.5,2.5), Sigma=matrix(c(1,.6,.6,1), ncol=2))
df = data.frame(x); colnames(df) = c("x","y")

commonTheme = list(labs(color="Density",fill="Density",
                        x="RNA-seq Expression",
                        y="Microarray Expression"),
                   theme_bw(),
                   theme(legend.position=c(0,1),
                         legend.justification=c(0,1)))

ggplot(data=df,aes(x,y)) + 
  geom_density2d(aes(colour=..level..)) + 
  scale_colour_gradient(low="green",high="red") + 
    geom_point() + commonTheme
ggplot(data=df,aes(x,y)) + 
  stat_density2d(aes(fill=..level..,alpha=..level..),geom='polygon',colour='black') + 
  scale_fill_continuous(low="green",high="red") +
  geom_smooth(method=lm,linetype=2,colour="red",se=F) + 
  guides(alpha="none") +
  geom_point() + commonTheme
