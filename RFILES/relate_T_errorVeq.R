# In this script the dependence of the equilibrium ice volume and the temperature is investigated.

# clean environment and set directory  
rm(list = ls())
graphics.off()

# packages and sourcefiles
require(ggplot2)
require("reshape2")
require("grid")
require("gridExtra")

# set plot_themes
source("set_theme_AB.R")
theme_set(theme_ali(base_size=16))

# functions to fit equilibria
f.lin <- function(x, a, b) {apply(cbind(0,a * x + b),1,max)} # function that linearly relates T and Veq
f.exp <- function(x, a, b) {a * exp(b * -x)}                 # function that exponentially relates T and Veq

# Applegate et al (2014) performed (>60,000 year) equilibrium runs with SICOPOLIS with the 
# 27 'best' parameter settings from the 100 member ensemble of Applegate et al. (2012) for
# 8 different temperature anomalies

# function globally defines variables for plotting for a single parameter setting
# and returns plot for this single parameter setting
makeplots <- function(mem,label=NULL) {
  # get equilibrium volumes
  equilibrium_volume <<- data.frame(
    Tgrl = c(0,1,2,3,4.5,6,9,12),
    Veq  = as.matrix(read.csv("data/SICOPOLIS_equilibrium_runs/V_0.csv"))[,mem] -
      as.matrix(read.csv("data/SICOPOLIS_equilibrium_runs/deltaV.csv"))[,mem] )
  
  # fit linear and exponential models
  fit.exp <- nls( Veq ~ f.exp(Tgrl,a,b), data=equilibrium_volume, start=list(a=1,b=0.0001)) # fits
  fit.lin <- nls( Veq ~ f.lin(Tgrl,a,b), data=equilibrium_volume, start=list(a=1,b=1))
  
  # read fitted parameters
  a    <- coef(fit.lin)[1]
  b    <- coef(fit.lin)[2]
  a1   <- coef(fit.exp)[1]
  b1   <- coef(fit.exp)[2]
  
  # simulate equilibria
  equilibrium_volume$Linear      <- f.lin(equilibrium_volume$Tgrl,a,b)
  equilibrium_volume$Exponential <- f.exp(equilibrium_volume$Tgrl,a1,b1)
  
  # get errors
  eq_volume_error                <- equilibrium_volume
  eq_volume_error$Linear         <- equilibrium_volume$Linear      - equilibrium_volume$Veq
  eq_volume_error$Exponential    <- equilibrium_volume$Exponential - equilibrium_volume$Veq
  
  Tgrl <- seq(0,12,length.out=1000)
  Veq.model <- melt(data=data.frame(Tgrl=Tgrl,Linear=f.lin(Tgrl,a,b),Exponential=f.exp(Tgrl,a1,b1)), id.vars=c("Tgrl"), variable.name="fit", value.name="Veq")

  # labels
  SIClabel <- ifelse(is.null(label),"SICOPOLIS",paste("SICOPOLIS \n",label,sep=""))
  EMlabel  <- ifelse(is.null(label),"Emulator",paste("Emulator of \n",label,sep="")) 
  
  # plot fitting error
  melt.eq <- melt(data=eq_volume_error, id.vars=c("Tgrl","Veq"), variable.name="fit", value.name="error")
  p <- ggplot(data=melt.eq, aes(x=Tgrl, y=error, colour=fit, shape=fit)) + 
    geom_abline(aes(intercept=0, slope=0)) +
    geom_point(size=3) +
    scale_y_continuous(limits=c(-2,2)) +
    labs(x=NULL,
         y=NULL,
         shape=NULL,
         color=NULL) +
    theme(legend.position="none")

  return(p)
}

# FIGURE SUPPLEMENTS
mem=1;  p1  <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=2;  p2  <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=3;  p3  <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=4;  p4  <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=5;  p5  <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=6;  p6  <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=7;  p7  <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=8;  p8  <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=9;  p9  <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=10; p10 <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=11; p11 <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=12; p12 <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=13; p13 <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=14; p14 <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=15; p15 <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=16; p16 <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=17; p17 <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=18; p18 <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=19; p19 <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=20; p20 <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=21; p21 <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=22; p22 <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=23; p23 <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=24; p24 <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=25; p25 <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=26; p26 <- makeplots(mem=mem, label=paste("member",mem)); print(mem)
mem=27; p27 <- makeplots(mem=mem, label=paste("member",mem)); print(mem)

p27 <-  p27 +
  labs(shape = "Emulator",
       color = "Emulator") +
  theme(legend.position=c(0.7,0.9),
        legend.key.height = unit(0.5,"lines"))

pdf("figures/supplement_fig_relation_T_errorVeq.pdf", paper="special", width=10, height=10*1.3)
grid.arrange(
  arrangeGrob(p1,
              p2,
              p3,
              p4,
              p5,
              p6,
              p7,
              p8,
              p9,
              ncol=1),
  arrangeGrob(p10,
              p11,
              p12,
              p13,
              p14,
              p15,
              p16,
              p17,
              p18,
              ncol=1),
  arrangeGrob(p19,
              p20,
              p21,
              p22,
              p23,
              p24,
              p25,
              p26,
              p27,
              ncol=1),
  ncol=3, widths=c(1, 1, 1),
  left = textGrob("Emulation error: difference between simulated and emulated equilibrium ice volume [m SLR]",               rot = 90, vjust = 0.5, 
                  gp = gpar(cex = 1.)),
  sub = textGrob("Temperature with respect to 1976-2005 [deg C]", rot = 0,  vjust = 0,   hjust = 0.5,
                 gp = gpar(cex = 1.))
)
dev.off()

  

