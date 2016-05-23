# PLOT EMULATORS WITH SICOPOLIS

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
source("plot_functions.R")

# Read simulated and emulated datasets
# SICOPOLIS
equil      <- read.table("data/SICOPOLIS_equil.txt", header=T)
grad       <- read.table("data/SICOPOLIS_grad.txt",  header=T)
#rcp8.5     <- read.table("data/SICOPOLIS_rcp8.5.txt",header=T)
equil$yr0  <- as.factor(equil$yr0)
grad$yr0   <- as.factor(grad$yr0)

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

# adjust basis data.sets
#equil.after <- equil[ which(equil$t >= as.numeric(levels(equil$yr0))[equil$yr0]) - 1,]
#grad.after  <- grad[  which(grad$t  >= as.numeric(levels(grad$yr0))[grad$yr0])   - 1,]

# funtion to add emulation layer to existing set of ggplot layers
add.emulation <- function(df.sim, df.em, label) {
  #df$yr0 <- as.factor(df$yr0)
  #df     <- df[which(df$t >= as.numeric(levels(df$yr0))[df$yr0]) - 1,]
  
  df <- df.sim
  df$Vt <- df.em$Vt - df.sim$Vt
  
  p <- ggplot(df, aes(x=t, y=Vt, color=yr0)) +
    geom_line(linetype="11", size=1.5) +    
    #geom_line(data=rcp8.5, aes(x=t, y=Vt), color="black", size=2) +
    
    geom_point(data=df[which(df$t == as.numeric(levels(df$yr0))[df$yr0]) - 1,], size=4) +
    scale_colour_brewer(palette="Spectral") +
    theme(legend.position="none") + labs(x=NULL, y=NULL) +
    scale_y_continuous(limits=c(-2,2)) +
    theme(legend.position="none") +
    geom_hline(yintercept=0) +
    annotate("text", x=2000, y=-1.5, label=label, cex=6, hjust=0)
  
  return(p)
}


legend <- get_legend(ggplot(equil, aes(x=t, y=Vt, color=yr0)) +
                       geom_line(size=1) +
                       labs(color="Starting year \ngeo-\nengineering") +
                       scale_colour_brewer(palette="Spectral")
  )


# equil
p.eq.d.complete <- add.emulation(equil, equil.delayed.linear.complete, "Delayed-linear")
p.eq.d.LOO      <- add.emulation(equil, equil.delayed.linear.LOO     , "Delayed-linear")
p.eq.d.L4O      <- add.emulation(equil, equil.delayed.linear.L4O     , "Delayed-linear")
p.eq.d.L5O      <- add.emulation(equil, equil.delayed.linear.L5O     , "Delayed-linear")

p.eq.S.complete <- add.emulation(equil, equil.SIMPLE.complete, "SIMPLE")
p.eq.S.LOO      <- add.emulation(equil, equil.SIMPLE.LOO     , "SIMPLE")
p.eq.S.L4O      <- add.emulation(equil, equil.SIMPLE.L4O     , "SIMPLE")
p.eq.S.L5O      <- add.emulation(equil, equil.SIMPLE.L5O     , "SIMPLE")

p.eq.q.complete <- add.emulation(equil, equil.quadratic.complete, "Quadratic")
p.eq.q.LOO      <- add.emulation(equil, equil.quadratic.LOO     , "Quadratic")
p.eq.q.L4O      <- add.emulation(equil, equil.quadratic.L4O     , "Quadratic")
p.eq.q.L5O      <- add.emulation(equil, equil.quadratic.L5O     , "Quadratic")


# grad
p.gr.d.complete <- add.emulation(grad, grad.delayed.linear.complete, "Delayed-linear")
p.gr.d.LOO      <- add.emulation(grad, grad.delayed.linear.LOO     , "Delayed-linear")
p.gr.d.L4O      <- add.emulation(grad, grad.delayed.linear.L4O     , "Delayed-linear")
p.gr.d.L5O      <- add.emulation(grad, grad.delayed.linear.L5O     , "Delayed-linear")

p.gr.S.complete <- add.emulation(grad, grad.SIMPLE.complete, "SIMPLE")
p.gr.S.LOO      <- add.emulation(grad, grad.SIMPLE.LOO     , "SIMPLE")
p.gr.S.L4O      <- add.emulation(grad, grad.SIMPLE.L4O     , "SIMPLE")
p.gr.S.L5O      <- add.emulation(grad, grad.SIMPLE.L5O     , "SIMPLE")

p.gr.q.complete <- add.emulation(grad, grad.quadratic.complete, "Quadratic")
p.gr.q.LOO      <- add.emulation(grad, grad.quadratic.LOO     , "Quadratic")
p.gr.q.L4O      <- add.emulation(grad, grad.quadratic.L4O     , "Quadratic")
p.gr.q.L5O      <- add.emulation(grad, grad.quadratic.L5O     , "Quadratic")


## EMULATION (trained by complete set) ##
pdf("figures/emulation_errors_complete.pdf", paper="special", width=10, height=3*5/1.618)
grid.arrange(
  arrangeGrob(p.eq.q.complete,
              p.eq.d.complete,
              p.eq.S.complete,
              ncol=1,
              main=textGrob("Temperature stabilization", rot = 0, vjust = 1,
                            gp = gpar(cex = 0.8))),
  arrangeGrob(p.gr.q.complete,
              p.gr.d.complete,
              p.gr.S.complete,
              ncol=1,
              main=textGrob("Gradual temperature draw-down", rot = 0, vjust = 1,
                            gp = gpar(cex = 0.8))),
  legend, ncol=3, widths=c(2.3, 2.3, 0.8),
  left = textGrob("Ice volume [meter sle]", rot = 90, vjust = 1, hjust=0.5,
                  gp = gpar(cex = 0.9)),
  sub = textGrob("Year", rot = 0, hjust = 2.5, vjust = 0, 
                 gp = gpar(cex = 1))
)
dev.off()

