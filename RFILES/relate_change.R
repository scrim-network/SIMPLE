# In this script the dependence of the equilibrium ice volume and the timescale 'tau' is investigated.

# clean environment and set directory  
rm(list = ls())
graphics.off()

# packages and sourcefiles
require(ggplot2)
require("reshape2")
require("grid")
require("gridExtra")

require(scales)

# set plot_themes
source("set_theme_AB.R")
theme_set(theme_ali(base_size=16))

# function to derive 'change' values from SICOPOLIS runs
get.tau.t <- function(df,a,b){
  df$Veq.v      <- a * as.numeric(as.vector(df$Tt)) + b
  df$Veq        <- ifelse(df$Veq.v < 0, 0, df$Veq.v)
  df$deltaVeq.v <- df$Vt - df$Veq.v
  df$deltaVeq   <- df$Vt - df$Veq
  df$relVdt     <- 100 * df$Vdt/df$Vt
  df$rate       <- ifelse(df$deltaVeq.v==0, NA, -df$Vdt / df$deltaVeq.v) # [mm]
  df$tau        <- ifelse(df$rate==0, 0, 1/df$rate)
  df            <- df[complete.cases(df),]
  return(df)
}

# Read SICOPOLIS data and combine all data sets in trainings data-set
columns <- c("t","Tt","Vt","Vdt","dt")
a       <- read.table("data/params.Veq.txt",header=T)$a
b       <- read.table("data/params.Veq.txt",header=T)$b

# combine data.sets
train <- rbind(
  data.frame(read.table("data/SICOPOLIS_spinup.txt",header=T)[columns], run="spinup"),
  data.frame(read.table("data/SICOPOLIS_hind.txt",  header=T)[columns], run="hind"),
  data.frame(read.table("data/SICOPOLIS_sixdeg.txt",header=T)[columns], run="sixdeg"),
  data.frame(read.table("data/SICOPOLIS_dTX.txt",   header=T)[columns], run="dTX"),
  data.frame(read.table("data/SICOPOLIS_rcp8.5.txt",header=T)[columns], run="rcp8.5"),
  data.frame(read.table("data/SICOPOLIS_equil.txt", header=T)[columns], run="equil"),
  data.frame(read.table("data/SICOPOLIS_grad.txt",  header=T)[columns], run="grad"),
  data.frame(read.table("data/SICOPOLIS_zero.txt",  header=T)[columns], run="zero")
)

#train     <- train[which(train$Tt >= 0),]             # include only simulation data for T>=0
train     <- get.tau.t(train,a=a,b=b)                  # calculate measures of change (Vdot, Vdot.rel, tau, lambda) and predictors (deltaVeq, deltaVea.v, Tt)
train$run <- as.factor(train$run)                      # necessary to distinguish runs by coloring

# plot colors trainingset
cols <- c("spinup" = "black", "hind" = "gray",  "sixdeg" = "yellow", "dTX" = "blue", "rcp8.5" = "orange", "grad" = "red", "equil" = "green", "zero" = "magenta")

# Vdot versus deltaVeq.v with coloring of runs
train.subset <- train[which(train$run != "grad" &
                              train$run != "equil" & 
                              train$run != "zero"),]
train.subset2 <- train[which(train$run == "hind" |
                               train$run == "sixdeg"),]

p <- ggplot() +
  geom_point(data=train, aes(x=deltaVeq.v, y=Vdt, colour=run), size=0.9) +
  geom_point(data=train.subset, aes(x=deltaVeq.v, y=Vdt, colour=run), size=0.5) +
  geom_point(data=train.subset2, aes(x=deltaVeq.v, y=Vdt, colour=run), size=0.5) +
  scale_colour_manual(name="runs",values=cols, breaks=c("spinup","hind","sixdeg","dTX","rcp8.5","equil","grad","zero"),
                      labels=c("Spinup run (Applegate et al., 2012)",
                               "Hindcast 1850-2005",
                               "Gradual temp increase scenario",
                               "Equilibrium runs (Applegate et al., 2015)",
                               "rcp8.5 scenario (Applegate and Keller, 2015)",
                               "Temperature stabilisation scenarios",
                               "Gradual temperature draw-down scenarios",
                               "Instantaneous temperature draw-down scenarios")) +
  theme(legend.justification=c(0.02,0.02), legend.position=c(0,0),
        legend.key = element_rect(fill="white"),
        legend.background = element_rect(fill=alpha('white', 0.0)),
        legend.text = element_text(size = 8),
        legend.key.height = unit(0.7,"lines")) +
  labs(x="Volume - 'virtual' equilibrium volume [m sle]",
       y="Rate of ice volume change [m sle/yr]")
pdf("figures/Vdot_dVeqv_run.pdf", paper="special", width=10, height=10/1.618); print(p); dev.off()

# Vdot
p <- ggplot(data=train, aes(y=Vdt, color=Vt)) +
  scale_color_gradient(low="green", high="blue") +
  theme(legend.justification=c(0.02,0.02), legend.position=c(0,0),
        legend.key = element_rect(fill="white"),
        legend.background = element_rect(fill=alpha('white', 0.0)),
        legend.text = element_text(size = 8),
        legend.key.height = unit(0.7,"lines")) +
  labs(y="Rate of ice volume change [m sle/yr]",
       color="Ice volume \n[m sle]")

# change rate versus deltaVeq
p.Vdot_dVeq   <- p + geom_point(aes(x=deltaVeq),   size=0.8) +
  labs(x="Volume - equilibrium volume [m sle]")

# Change rate versus deltaVeq.v (used in manuscript)
p.Vdot_dVeq.v <- p + geom_point(aes(x=deltaVeq.v), size=0.8) +
  labs(x="Volume - 'virtual' equilibrium volume [m sle]")

# Change rate versus temperature anomaly
p.Vdot_T      <- p + geom_point(aes(x=Tt),         size=0.8) +
  labs(x=expression(paste("Temperature anomaly [",degree,"C]")))

pdf("figures/Vdot_deltaVeqv.pdf", paper="special", width=10, height=10/1.618); print(p.Vdot_dVeq.v); dev.off()
