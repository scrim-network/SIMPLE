# In this script the RMSE of the emulated response with respect to 
# SICOPOLIS is tested for three semi-empirical models and different
# parameter sets trained on different subsets of the geo-engineered
# scenarios

# clean environment and set directory  
rm(list = ls())
graphics.off()

# packages and sourcefiles
require(ggplot2)
require("reshape2")
require("grid")
require("gridExtra")
require("scales")

# set plot_themes
source("set_theme_AB.R")
#source("plot_functions.R")

# Read simulated and emulated datasets
# SICOPOLIS
equil <- read.table("data/SICOPOLIS_equil.txt", header=T)
grad  <- read.table("data/SICOPOLIS_grad.txt",  header=T)

# Quadratic
equil.quadratic.complete <- read.table("data/quadratic_equil.txt",header=T)
equil.quadratic.LOO      <- read.table("data/quadratic.LOO_equil.txt",header=T)
equil.quadratic.L4O      <- read.table("data/quadratic.L4O_equil.txt",header=T)
equil.quadratic.L5O      <- read.table("data/quadratic.L5O_equil.txt",header=T)

grad.quadratic.complete  <- read.table("data/quadratic_grad.txt",header=T)
grad.quadratic.LOO       <- read.table("data/quadratic.LOO_grad.txt",header=T)
grad.quadratic.L4O       <- read.table("data/quadratic.L4O_grad.txt",header=T)
grad.quadratic.L5O       <- read.table("data/quadratic.L5O_grad.txt",header=T)

# Delayed-linear
equil.delayed.linear.complete <- read.table("data/delayed-linear_equil.txt",header=T)
equil.delayed.linear.LOO      <- read.table("data/delayed-linear.LOO_equil.txt",header=T)
equil.delayed.linear.L4O      <- read.table("data/delayed-linear.L4O_equil.txt",header=T)
equil.delayed.linear.L5O      <- read.table("data/delayed-linear.L5O_equil.txt",header=T)

grad.delayed.linear.complete  <- read.table("data/delayed-linear_grad.txt",header=T)
grad.delayed.linear.LOO       <- read.table("data/delayed-linear.LOO_grad.txt",header=T)
grad.delayed.linear.L4O       <- read.table("data/delayed-linear.L4O_grad.txt",header=T)
grad.delayed.linear.L5O       <- read.table("data/delayed-linear.L5O_grad.txt",header=T)

# SIMPLE
equil.SIMPLE.complete <- read.table("data/SIMPLE_equil.txt",header=T)
equil.SIMPLE.LOO      <- read.table("data/SIMPLE.LOO_equil.txt",header=T)
equil.SIMPLE.L4O      <- read.table("data/SIMPLE.L4O_equil.txt",header=T)
equil.SIMPLE.L5O      <- read.table("data/SIMPLE.L5O_equil.txt",header=T)

grad.SIMPLE.complete  <- read.table("data/SIMPLE_grad.txt",header=T)
grad.SIMPLE.LOO       <- read.table("data/SIMPLE.LOO_grad.txt",header=T)
grad.SIMPLE.L4O       <- read.table("data/SIMPLE.L4O_grad.txt",header=T)
grad.SIMPLE.L5O       <- read.table("data/SIMPLE.L5O_grad.txt",header=T)

# SIMPLE_a
equil.SIMPLE_a.complete <- read.table("data/SIMPLE_a_equil.txt",header=T)
equil.SIMPLE_a.LOO      <- read.table("data/SIMPLE_a.LOO_equil.txt",header=T)
equil.SIMPLE_a.L4O      <- read.table("data/SIMPLE_a.L4O_equil.txt",header=T)
equil.SIMPLE_a.L5O      <- read.table("data/SIMPLE_a.L5O_equil.txt",header=T)

grad.SIMPLE_a.complete  <- read.table("data/SIMPLE_a_grad.txt",header=T)
grad.SIMPLE_a.LOO       <- read.table("data/SIMPLE_a.LOO_grad.txt",header=T)
grad.SIMPLE_a.L4O       <- read.table("data/SIMPLE_a.L4O_grad.txt",header=T)
grad.SIMPLE_a.L5O       <- read.table("data/SIMPLE_a.L5O_grad.txt",header=T)


# DICE
equil.DICE.complete <- read.table("data/DICE_equil.txt",header=T)
equil.DICE.LOO      <- read.table("data/DICE.LOO_equil.txt",header=T)
equil.DICE.L4O      <- read.table("data/DICE.L4O_equil.txt",header=T)
equil.DICE.L5O      <- read.table("data/DICE.L5O_equil.txt",header=T)

grad.DICE.complete  <- read.table("data/DICE_grad.txt",header=T)
grad.DICE.LOO       <- read.table("data/DICE.LOO_grad.txt",header=T)
grad.DICE.L4O       <- read.table("data/DICE.L4O_grad.txt",header=T)
grad.DICE.L5O       <- read.table("data/DICE.L5O_grad.txt",header=T)

# DICE_a
equil.DICE_a.complete <- read.table("data/DICE_a_equil.txt",header=T)
equil.DICE_a.LOO      <- read.table("data/DICE_a.LOO_equil.txt",header=T)
equil.DICE_a.L4O      <- read.table("data/DICE_a.L4O_equil.txt",header=T)
equil.DICE_a.L5O      <- read.table("data/DICE_a.L5O_equil.txt",header=T)

grad.DICE_a.complete  <- read.table("data/DICE_a_grad.txt",header=T)
grad.DICE_a.LOO       <- read.table("data/DICE_a.LOO_grad.txt",header=T)
grad.DICE_a.L4O       <- read.table("data/DICE_a.L4O_grad.txt",header=T)
grad.DICE_a.L5O       <- read.table("data/DICE_a.L5O_grad.txt",header=T)

# DICE_b
equil.DICE_b.complete <- read.table("data/DICE_b_equil.txt",header=T)
equil.DICE_b.LOO      <- read.table("data/DICE_b.LOO_equil.txt",header=T)
equil.DICE_b.L4O      <- read.table("data/DICE_b.L4O_equil.txt",header=T)
equil.DICE_b.L5O      <- read.table("data/DICE_b.L5O_equil.txt",header=T)

grad.DICE_b.complete  <- read.table("data/DICE_b_grad.txt",header=T)
grad.DICE_b.LOO       <- read.table("data/DICE_b.LOO_grad.txt",header=T)
grad.DICE_b.L4O       <- read.table("data/DICE_b.L4O_grad.txt",header=T)
grad.DICE_b.L5O       <- read.table("data/DICE_b.L5O_grad.txt",header=T)

# function to calculate RMSE
get_errors <- function(model, complete, LOO, L4O, L5O, label) {
  # define sequence of 'last years' (until) that define the periods
  # over which the RMSE is estimated
  until <- seq(2100,3000,by=100)
  n     <- length(until)
  
  df <- data.frame(t        = as.numeric(as.vector(model$t)),
                   yr0      = as.numeric(as.vector(model$yr0)),
                   complete = complete$Vt - model$Vt,
                   LOO      = LOO$Vt      - model$Vt,
                   L4O      = L4O$Vt      - model$Vt,
                   L5O      = L5O$Vt      - model$Vt)
  df.rmse <- data.frame(model  = rep(label,n),
                        until  = until,
                        RMSE   = rep(NA,n),
                        LOO    = rep(NA,n),
                        L4O    = rep(NA,n),
                        L5O    = rep(NA,n))
  for(i in 1:n) {
    df.rmse[i, -1:-2] <- apply(as.matrix(df[which(df$t<=until[i] & df$yr0<= until[i]), -1:-2]), 2, sd)
  }
  return(df.rmse)
}

# estimate RMSEs
Quadratic <- get_errors(model    = rbind(equil,                   grad               ),
                        complete = rbind(equil.quadratic.complete,    grad.quadratic.complete),
                        LOO      = rbind(equil.quadratic.LOO,         grad.quadratic.LOO     ),
                        L4O      = rbind(equil.quadratic.L4O,         grad.quadratic.L4O     ),
                        L5O      = rbind(equil.quadratic.L5O,         grad.quadratic.L5O     ),
                        label    = "quadratic")

Delayed <-   get_errors(model    = rbind(equil,                   grad               ),
                        complete = rbind(equil.delayed.linear.complete,    grad.delayed.linear.complete),
                        LOO      = rbind(equil.delayed.linear.LOO,         grad.delayed.linear.LOO     ),
                        L4O      = rbind(equil.delayed.linear.L4O,         grad.delayed.linear.L4O     ),
                        L5O      = rbind(equil.delayed.linear.L5O,         grad.delayed.linear.L5O     ),
                        label    = "delayed.linear")

SIMPLE <- get_errors(model    = rbind(equil,                   grad               ),
                        complete = rbind(equil.SIMPLE.complete,    grad.SIMPLE.complete),
                        LOO      = rbind(equil.SIMPLE.LOO,         grad.SIMPLE.LOO     ),
                        L4O      = rbind(equil.SIMPLE.L4O,         grad.SIMPLE.L4O     ),
                        L5O      = rbind(equil.SIMPLE.L5O,         grad.SIMPLE.L5O     ),
                        label    = "SIMPLE")

SIMPLE_a <- get_errors(model    = rbind(equil,                   grad               ),
                     complete = rbind(equil.SIMPLE_a.complete,    grad.SIMPLE_a.complete),
                     LOO      = rbind(equil.SIMPLE_a.LOO,         grad.SIMPLE_a.LOO     ),
                     L4O      = rbind(equil.SIMPLE_a.L4O,         grad.SIMPLE_a.L4O     ),
                     L5O      = rbind(equil.SIMPLE_a.L5O,         grad.SIMPLE_a.L5O     ),
                     label    = "SIMPLE_a")

DICE <- get_errors(model    = rbind(equil,                   grad               ),
                     complete = rbind(equil.DICE.complete,    grad.DICE.complete),
                     LOO      = rbind(equil.DICE.LOO,         grad.DICE.LOO     ),
                     L4O      = rbind(equil.DICE.L4O,         grad.DICE.L4O     ),
                     L5O      = rbind(equil.DICE.L5O,         grad.DICE.L5O     ),
                     label    = "DICE")

DICE_a <- get_errors(model    = rbind(equil,                   grad               ),
                     complete = rbind(equil.DICE_a.complete,    grad.DICE_a.complete),
                     LOO      = rbind(equil.DICE_a.LOO,         grad.DICE_a.LOO     ),
                     L4O      = rbind(equil.DICE_a.L4O,         grad.DICE_a.L4O     ),
                     L5O      = rbind(equil.DICE_a.L5O,         grad.DICE_a.L5O     ),
                     label    = "DICE_a")

DICE_b <- get_errors(model    = rbind(equil,                   grad               ),
                     complete = rbind(equil.DICE_b.complete,    grad.DICE_b.complete),
                     LOO      = rbind(equil.DICE_b.LOO,         grad.DICE_b.LOO     ),
                     L4O      = rbind(equil.DICE_b.L4O,         grad.DICE_b.L4O     ),
                     L5O      = rbind(equil.DICE_b.L5O,         grad.DICE_b.L5O     ),
                     label    = "DICE_b")



write.csv(rbind(Quadratic,
                Delayed,
                SIMPLE,
                SIMPLE_a,
                DICE,
                DICE_a,
                DICE_b), "data/RMSEs.csv")

df <- rbind(Quadratic, Delayed, DICE, SIMPLE)
#levels(df$model)[levels(df$model)=="SIMPLE_DD"] <- "SIMPLE"
levels(df$model)[levels(df$model)=="delayed.linear"]     <- "delayed-linear"
levels(df$model)[levels(df$model)=="DICE"]               <- "GIS-DICE"
names(df)[which(names(df)=="LOO")]                       <- "L1O"

df.melt <- melt(df, id=c("model","until"),variable.name="CV",value.name="RMSE")

p <- ggplot(data=df.melt, aes(x=until, y=RMSE, color=model, shape=CV)) +
  geom_line() +
  geom_point() +
  labs(x="Last year of period over which RMSE is estimated",
       y="RMSE emulated GIS volume [m sle]",
       color="Model",
       shape="Cross-validation ") +
  theme(legend.position=c(0.15,0.62),
        legend.key.height = unit(0.7,"lines"),
        legend.key=element_blank(),
        legend.text = element_text(size = 10),
        legend.background = element_rect(fill=alpha('white', 0.0)) ) +
  guides(colour = guide_legend(order = 1), 
         shape = guide_legend(order = 2))


pdf("figures/RMSE.pdf", paper="special", width=10, height=10/1.618); print(p); dev.off()
