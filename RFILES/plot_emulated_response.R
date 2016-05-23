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
require("RColorBrewer")

# set plot_themes
source("set_theme_AB.R")
theme_set(theme_ali(base_size=20))

source("plot_functions.R")

# Read simulated and emulated datasets
# SICOPOLIS
equil      <- read.table("data/SICOPOLIS_equil.txt", header=T)
grad       <- read.table("data/SICOPOLIS_grad.txt",  header=T)
rcp8.5     <- read.table("data/SICOPOLIS_rcp8.5.txt",header=T)
equil$yr0  <- as.factor(equil$yr0)
grad$yr0   <- as.factor(grad$yr0)

# number of starting years
nyr0      <- length(levels(equil$yr0))

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

# DICE_b
equil.DICE_b.complete <- read.table("data/DICE_b_equil.txt",header=T)
equil.DICE_b.LOO      <- read.table("data/DICE_b.LOO_equil.txt",header=T)
equil.DICE_b.L4O      <- read.table("data/DICE_b.L4O_equil.txt",header=T)
equil.DICE_b.L5O      <- read.table("data/DICE_b.L5O_equil.txt",header=T)

grad.DICE_b.complete  <- read.table("data/DICE_b_grad.txt",header=T)
grad.DICE_b.LOO       <- read.table("data/DICE_b.LOO_grad.txt",header=T)
grad.DICE_b.L4O       <- read.table("data/DICE_b.L4O_grad.txt",header=T)
grad.DICE_b.L5O       <- read.table("data/DICE_b.L5O_grad.txt",header=T)



# DICE_a
equil.DICE_a.complete <- read.table("data/DICE_a_equil.txt",header=T)
equil.DICE_a.LOO      <- read.table("data/DICE_a.LOO_equil.txt",header=T)
equil.DICE_a.L4O      <- read.table("data/DICE_a.L4O_equil.txt",header=T)
equil.DICE_a.L5O      <- read.table("data/DICE_a.L5O_equil.txt",header=T)

grad.DICE_a.complete  <- read.table("data/DICE_a_grad.txt",header=T)
grad.DICE_a.LOO       <- read.table("data/DICE_a.LOO_grad.txt",header=T)
grad.DICE_a.L4O       <- read.table("data/DICE_a.L4O_grad.txt",header=T)
grad.DICE_a.L5O       <- read.table("data/DICE_a.L5O_grad.txt",header=T)



# adjust basis data.sets
equil.after <- equil[ which(equil$t >= as.numeric(levels(equil$yr0))[equil$yr0]) - 1,]
grad.after  <- grad[  which(grad$t  >= as.numeric(levels(grad$yr0))[grad$yr0])   - 1,]

# funtion to add emulation layer to existing set of ggplot layers
add.emulation <- function(df.sim, df.em, label) {
  df.em$yr0 <- as.factor(df.em$yr0)
  df.em <- df.em[ which(df.em$t >= as.numeric(levels(df.em$yr0))[df.em$yr0]) - 1,]
  
  p <- ggplot(df.sim, aes(x=t, y=Vt, color=yr0)) +
    geom_line(size=1) +
    
    geom_line(data=df.em,  aes(x=t, y=Vt,  color=yr0), linetype="11", size=1.5) +    
    geom_line(data=rcp8.5, aes(x=t, y=Vt), color="black", size=2) +
    
    geom_point(data=df.sim[which(df.sim$t == as.numeric(levels(df.sim$yr0))[df.sim$yr0]) - 1,], aes(colour=yr0), size=4) +
    scale_colour_manual(values = rev(brewer.pal(nyr0,"Spectral"))) +
    theme(legend.position="none") + labs(x=NULL, y=NULL) +
    scale_y_continuous(limits=c(0,7.5)) +
    theme(legend.position="none") +
    annotate("text", x=2000, y=1, label=label, cex=6, hjust=0)  
}


legend <- get_legend(ggplot(equil, aes(x=t, y=Vt, color=yr0)) +
                       geom_line(size=1) +
                       labs(color="Starting year \ngeo-\nengineering") +
                       scale_colour_manual(values = rev(brewer.pal(nyr0,"Spectral")))
)


# equil
p.equil <- ggplot(equil.after, aes(x=t, y=Vt, color=yr0)) +
  geom_line(data=rcp8.5, aes(x=t, y=Vt), color="black", size=1) +
  geom_line(size=1) +
  geom_point(data=equil[which(equil$t == as.numeric(levels(equil$yr0))[equil$yr0]) - 1,], aes(colour=yr0), size=4) +
  scale_colour_manual(values = rev(brewer.pal(nyr0,"Spectral"))) +
  theme(legend.position="none") + labs(x=NULL, y=NULL) +
  scale_y_continuous(limits=c(0,7.5)) +
  theme(legend.position="none")

p.eq.d.complete <- add.emulation(equil.after, equil.delayed.linear.complete, "Delayed-linear")
p.eq.d.LOO      <- add.emulation(equil.after, equil.delayed.linear.LOO     , "Delayed-linear")
p.eq.d.L4O      <- add.emulation(equil.after, equil.delayed.linear.L4O     , "Delayed-linear")
p.eq.d.L5O      <- add.emulation(equil.after, equil.delayed.linear.L5O     , "Delayed-linear")

p.eq.S.complete <- add.emulation(equil.after, equil.SIMPLE.complete, "SIMPLE")
p.eq.S.LOO      <- add.emulation(equil.after, equil.SIMPLE.LOO     , "SIMPLE")
p.eq.S.L4O      <- add.emulation(equil.after, equil.SIMPLE.L4O     , "SIMPLE")
p.eq.S.L5O      <- add.emulation(equil.after, equil.SIMPLE.L5O     , "SIMPLE")

p.eq.Sa.complete <- add.emulation(equil.after, equil.SIMPLE_a.complete, "SIMPLE_a")
p.eq.Sa.LOO      <- add.emulation(equil.after, equil.SIMPLE_a.LOO     , "SIMPLE_a")
p.eq.Sa.L4O      <- add.emulation(equil.after, equil.SIMPLE_a.L4O     , "SIMPLE_a")
p.eq.Sa.L5O      <- add.emulation(equil.after, equil.SIMPLE_a.L5O     , "SIMPLE_a")


p.eq.D.complete <- add.emulation(equil.after, equil.DICE.complete, "GIS-DICE")
p.eq.D.LOO      <- add.emulation(equil.after, equil.DICE.LOO     , "GIS-DICE")
p.eq.D.L4O      <- add.emulation(equil.after, equil.DICE.L4O     , "GIS-DICE")
p.eq.D.L5O      <- add.emulation(equil.after, equil.DICE.L5O     , "GIS-DICE")

p.eq.Da.complete <- add.emulation(equil.after, equil.DICE_a.complete, "DICE_a")
p.eq.Da.LOO      <- add.emulation(equil.after, equil.DICE_a.LOO     , "DICE_a")
p.eq.Da.L4O      <- add.emulation(equil.after, equil.DICE_a.L4O     , "DICE_a")
p.eq.Da.L5O      <- add.emulation(equil.after, equil.DICE_a.L5O     , "DICE_a")

p.eq.Db.complete <- add.emulation(equil.after, equil.DICE_b.complete, "DICE_b")
p.eq.Db.LOO      <- add.emulation(equil.after, equil.DICE_b.LOO     , "DICE_b")
p.eq.Db.L4O      <- add.emulation(equil.after, equil.DICE_b.L4O     , "DICE_b")
p.eq.Db.L5O      <- add.emulation(equil.after, equil.DICE_b.L5O     , "DICE_b")


p.eq.q.complete <- add.emulation(equil.after, equil.quadratic.complete, "Quadratic")
p.eq.q.LOO      <- add.emulation(equil.after, equil.quadratic.LOO     , "Quadratic")
p.eq.q.L4O      <- add.emulation(equil.after, equil.quadratic.L4O     , "Quadratic")
p.eq.q.L5O      <- add.emulation(equil.after, equil.quadratic.L5O     , "Quadratic")


# grad
p.grad <- ggplot(grad.after, aes(x=t, y=Vt, color=yr0)) +
  geom_line(data=rcp8.5, aes(x=t, y=Vt), color="black", size=1) +
  geom_line(size=1) +
  geom_point(data=equil[which(equil$t == as.numeric(levels(equil$yr0))[equil$yr0]) - 1,], aes(colour=yr0), size=4) +
  scale_colour_manual(values = rev(brewer.pal(nyr0,"Spectral"))) +
  theme(legend.position="none") + labs(x=NULL, y=NULL) +
  scale_y_continuous(limits=c(0,7.5)) +
  theme(legend.position="none")

p.gr.d.complete <- add.emulation(grad.after, grad.delayed.linear.complete, "Delayed-linear")
p.gr.d.LOO      <- add.emulation(grad.after, grad.delayed.linear.LOO     , "Delayed-linear")
p.gr.d.L4O      <- add.emulation(grad.after, grad.delayed.linear.L4O     , "Delayed-linear")
p.gr.d.L5O      <- add.emulation(grad.after, grad.delayed.linear.L5O     , "Delayed-linear")

p.gr.S.complete <- add.emulation(grad.after, grad.SIMPLE.complete, "SIMPLE")
p.gr.S.LOO      <- add.emulation(grad.after, grad.SIMPLE.LOO     , "SIMPLE")
p.gr.S.L4O      <- add.emulation(grad.after, grad.SIMPLE.L4O     , "SIMPLE")
p.gr.S.L5O      <- add.emulation(grad.after, grad.SIMPLE.L5O     , "SIMPLE")

p.gr.Sa.complete <- add.emulation(grad.after, grad.SIMPLE_a.complete, "SIMPLE_a")
p.gr.Sa.LOO      <- add.emulation(grad.after, grad.SIMPLE_a.LOO     , "SIMPLE_a")
p.gr.Sa.L4O      <- add.emulation(grad.after, grad.SIMPLE_a.L4O     , "SIMPLE_a")
p.gr.Sa.L5O      <- add.emulation(grad.after, grad.SIMPLE_a.L5O     , "SIMPLE")

p.gr.D.complete <- add.emulation(grad.after, grad.DICE.complete, "GIS-DICE")
p.gr.D.LOO      <- add.emulation(grad.after, grad.DICE.LOO     , "GIS-DICE")
p.gr.D.L4O      <- add.emulation(grad.after, grad.DICE.L4O     , "GIS-DICE")
p.gr.D.L5O      <- add.emulation(grad.after, grad.DICE.L5O     , "GIS-DICE")

p.gr.Da.complete <- add.emulation(grad.after, grad.DICE_a.complete, "DICE_a")
p.gr.Da.LOO      <- add.emulation(grad.after, grad.DICE_a.LOO     , "DICE_a")
p.gr.Da.L4O      <- add.emulation(grad.after, grad.DICE_a.L4O     , "DICE_a")
p.gr.Da.L5O      <- add.emulation(grad.after, grad.DICE_a.L5O     , "DICE_a")

p.gr.Db.complete <- add.emulation(grad.after, grad.DICE_b.complete, "DICE_b")
p.gr.Db.LOO      <- add.emulation(grad.after, grad.DICE_b.LOO     , "DICE_b")
p.gr.Db.L4O      <- add.emulation(grad.after, grad.DICE_b.L4O     , "DICE_b")
p.gr.Db.L5O      <- add.emulation(grad.after, grad.DICE_b.L5O     , "DICE_b")

p.gr.q.complete <- add.emulation(grad.after, grad.quadratic.complete, "Quadratic")
p.gr.q.LOO      <- add.emulation(grad.after, grad.quadratic.LOO     , "Quadratic")
p.gr.q.L4O      <- add.emulation(grad.after, grad.quadratic.L4O     , "Quadratic")
p.gr.q.L5O      <- add.emulation(grad.after, grad.quadratic.L5O     , "Quadratic")


## EMULATION (trained by complete set) ##
pdf("figures/emulations_complete.pdf", paper="special", width=10, height=3.5*5/1.618)
grid.arrange(
  arrangeGrob(p.eq.q.complete,
              p.eq.d.complete,
              p.eq.D.complete,
              p.eq.S.complete,
              ncol=1,
              top=textGrob("Temperature stabilization", rot = 0, vjust = 1,
                            gp = gpar(cex = 1.6))),
  arrangeGrob(p.gr.q.complete,
              p.gr.d.complete,
              p.gr.D.complete,
              p.gr.S.complete,
              ncol=1,
              top=textGrob("Gradual temperature draw-down", rot = 0, vjust = 1,
                            gp = gpar(cex = 1.6))),
  ncol=2, widths=c(2, 2),
  left = textGrob("Ice volume [meter sle]\n", rot = 90, vjust = 1, hjust=0.5,
                  gp = gpar(cex = 1.6)),
  bottom = textGrob("\nYear", rot = 0, hjust = 0.5, vjust = 0, 
                 gp = gpar(cex = 1.6))
)
dev.off()

pdf("figures/GIS-emulations.pdf", paper="special", width=10, height=2.8*5/1.618)
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
  bottom = textGrob("Year", rot = 0, hjust = 2.5, vjust = 0, 
                 gp = gpar(cex = 1))
)
dev.off()




# with DICE variants
pdf("figures/emulations_complete_DICE.pdf", paper="special", width=10, height=3*5/1.618)
grid.arrange(
  arrangeGrob(p.eq.D.complete,
              p.eq.Db.complete,
              p.eq.S.complete,
              ncol=1,
              main=textGrob("Temperature stabilization", rot = 0, vjust = 1,
                            gp = gpar(cex = 1))),
  arrangeGrob(p.gr.D.complete,
              p.gr.Db.complete,
              p.gr.S.complete,
              ncol=1,
              main=textGrob("Gradual temperature draw-down", rot = 0, vjust = 1,
                            gp = gpar(cex = 1))),
  legend, ncol=3, widths=c(2.3, 2.3, 0.8),
  left = textGrob("Ice volume [meter sle]", rot = 90, vjust = 1, hjust=0.5,
                  gp = gpar(cex = 1)),
  bottom = textGrob("Year", rot = 0, hjust = 2.5, vjust = 0, 
                 gp = gpar(cex = 1))
)
dev.off()

# SIMPLE variants
pdf("figures/emulations_complete_SIMPLE.pdf", paper="special", width=10, height=2*5/1.618)
grid.arrange(
  arrangeGrob(p.eq.S.complete,
              p.eq.Sa.complete,
              ncol=1,
              main=textGrob("Temperature stabilization", rot = 0, vjust = 1,
                            gp = gpar(cex = 0.8))),
  arrangeGrob(p.gr.S.complete,
              p.gr.Sa.complete,
              ncol=1,
              main=textGrob("Gradual temperature draw-down", rot = 0, vjust = 1,
                            gp = gpar(cex = 0.8))),
  legend, ncol=3, widths=c(2.3, 2.3, 0.8),
  left = textGrob("Ice volume [meter sle]", rot = 90, vjust = 1, hjust=0.5,
                  gp = gpar(cex = 0.9)),
  bottom = textGrob("Year", rot = 0, hjust = 2.5, vjust = 0, 
                 gp = gpar(cex = 1))
)
dev.off()

