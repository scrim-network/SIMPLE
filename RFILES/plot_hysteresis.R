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
source("plot_functions.R")

# Read simulated and emulated datasets
grad     <- read.table("data/SICOPOLIS_grad.txt",  header=T)       # grad
grad.q   <- read.table("data/quadratic_grad.txt",header=T)
grad.d   <- read.table("data/delayed-linear_grad.txt",header=T)
grad.S   <- read.table("data/SIMPLE_grad.txt",header=T)

grad$yr0   <- as.factor(grad$yr0)
grad.q$yr0 <- as.factor(grad.q$yr0)
grad.d$yr0 <- as.factor(grad.d$yr0)
grad.S$yr0 <- as.factor(grad.S$yr0)

grad     <- grad[which(  grad$t  >= as.numeric(levels(grad$yr0))[grad$yr0]) - 1,] # select part after starting year
grad.q   <- grad.q[which(grad.q$t  >= as.numeric(levels(grad.q$yr0))[grad.q$yr0]) - 1,]
grad.d   <- grad.d[which(grad.d$t  >= as.numeric(levels(grad.d$yr0))[grad.d$yr0]) - 1,]
grad.S   <- grad.S[which(grad.S$t  >= as.numeric(levels(grad.S$yr0))[grad.S$yr0]) - 1,]

start     <- grad[which(grad$t    == as.numeric(levels(grad$yr0))[grad$yr0]) - 1,] # select part after starting year
start.q   <- grad.q[which(grad.q$t  == as.numeric(levels(grad.q$yr0))[grad.q$yr0]) - 1,]
start.d   <- grad.d[which(grad.d$t  == as.numeric(levels(grad.d$yr0))[grad.d$yr0]) - 1,]
start.S   <- grad.S[which(grad.S$t  == as.numeric(levels(grad.S$yr0))[grad.S$yr0]) - 1,]

rcp8.5   <- read.table("data/SICOPOLIS_rcp8.5.txt",header=T)      # rcp8.5 
rcp8.5.q <- read.table("data/quadratic_rcp8.5.txt",header=T)
rcp8.5.d <- read.table("data/delayed-linear_rcp8.5.txt",header=T)
rcp8.5.S <- read.table("data/SIMPLE_rcp8.5.txt",header=T)

nyr0      <- length(levels(grad$yr0))

# create legend
legend <- get_legend(ggplot(grad, aes(x=t, y=Vt, color=yr0)) +
                       geom_line(size=1) +
                       labs(color="Starting year \ngeo-\nengineering") +
                       scale_colour_manual(values = rev(brewer.pal(nyr0,"Spectral"))) +
                       theme(legend.key.height = unit(0.8,"lines"))  )

# temperature scenario
pT <- ggplot(rcp8.5, aes(x=t, y=Tt)) +
  geom_line(size=2) +
  theme(legend.position="none") + labs(x=NULL, y=NULL) +
  scale_y_continuous(limits=c(0,12)) +
  theme(legend.position="none") +
  geom_line(data=grad, aes(colour=yr0), size=1) +
  geom_point(data=start, aes(colour=yr0), size=4) +
  scale_colour_manual(values = rev(brewer.pal(nyr0,"Spectral"))) +
  labs(x="Year", y=expression(paste("Temperature anomaly [",degree,"C]")))
  
# volume response
p <- ggplot(data=rcp8.5, aes(x=t, y=Vt)) +
  theme(legend.position="none") + labs(x=NULL, y=NULL) +
  scale_y_continuous(limits=c(0,7.5)) +
  theme(legend.position="none") +
  scale_colour_manual(values = rev(brewer.pal(nyr0,"Spectral"))) +
  labs(x="Year", y="Ice volume [m sle]")

pV.q <- p +   
  geom_line(data=rcp8.5.q, size=2) +
  geom_line(data=grad.q, aes(colour=yr0), size=1) +
  geom_point(data=start.q, aes(colour=yr0), size=4)

  
pV.sim <- p +   
  geom_line(data=rcp8.5, size=2) +
  geom_line(data=grad, aes(colour=yr0), size=1) +
  geom_point(data=start, aes(colour=yr0), size=4)
  
pV.S <- p +   
  geom_line(data=rcp8.5.S, size=2) +
  geom_line(data=grad.S, aes(colour=yr0), size=1) +
  geom_point(data=start.S, aes(colour=yr0), size=4)

pV.S.sim <- p +
  geom_line( data=rcp8.5,                   size=2) +
  geom_line( data=grad,    aes(colour=yr0), size=1) +
  geom_point(data=start,   aes(colour=yr0), size=4) +   
  geom_line( data=rcp8.5.S,                 size=2, linetype="11") +
  geom_line( data=grad.S,  aes(colour=yr0), size=1, linetype="11") +
  geom_point(data=start.S, aes(colour=yr0), size=4)

# hysteresis plots
plot.hysteresis <- function(rcp8.5, df, start) {
  rcp8.5$S <- rcp8.5$Vt[1] - rcp8.5$Vt
  df$S     <- df$Vt[1]     - df$Vt
  start$S  <- start$Vt[1]  - start$Vt
  p <- ggplot(data=rcp8.5, aes(x=Tt, y=S)) +
    scale_colour_manual(values = rev(brewer.pal(nyr0,"Spectral"))) +
    scale_y_continuous(limits=c(-0.1,7.5)) +
    scale_x_continuous(limits=c(-0.1,12)) +
    labs(x=expression(paste("Temperature anomaly [",degree,"C]")),
         y="Greenland contribution to \nsea level rise [meter]") +
    theme(legend.position="none") +
    geom_path(size=2, arrow=arrow(length = unit(5, "pt"), type="closed")) +
    geom_path(data=df, aes(color=yr0), size=1) +
    geom_path(data=df[which(df$Tt>2.5),], aes(color=yr0), size=1, arrow=arrow(length = unit(5, "pt"), type="closed")) +
    geom_point(data=start, aes(colour=yr0), size=4)
  return(p)
}

# little tweak to set arrow in right direction
rcp8.5.q$Vt[nrow(rcp8.5)] <- -0.0001
  
pH.q   <- plot.hysteresis(rcp8.5.q, grad.q, start.q)
pH.d   <- plot.hysteresis(rcp8.5.d, grad.d, start.d)
pH.S   <- plot.hysteresis(rcp8.5.S, grad.S, start.S)
pH.sim <- plot.hysteresis(rcp8.5,   grad,   start)

# hysteresis plots (including original SICOPOLIS hysteresis)
plot.hysteresis <- function(rcp8.5.sim, grad.sim, start.sim, rcp8.5.em, grad.em, start.em) {
  rcp8.5.sim$S  <- rcp8.5.sim$Vt[1] - rcp8.5.sim$Vt
  grad.sim$S    <- grad.sim$Vt[1]   - grad.sim$Vt
  start.sim$S   <- start.sim$Vt[1]  - start.sim$Vt
  
  rcp8.5.em$S  <- rcp8.5.em$Vt[1] - rcp8.5.em$Vt
  grad.em$S    <- grad.em$Vt[1]   - grad.em$Vt
  start.em$S   <- start.em$Vt[1]  - start.em$Vt
  
  p <- ggplot(data=rcp8.5, aes(x=Tt, y=S)) +
    scale_colour_manual(values = rev(brewer.pal(nyr0,"Spectral"))) +
    scale_y_continuous(limits=c(-0.1,7.5)) +
    scale_x_continuous(limits=c(-0.1,12)) +
    labs(x=expression(paste("Temperature anomaly [",degree,"C]")),
         y="Greenland contribution to \nsea level rise [meter]") +
    theme(legend.position="none") +
    geom_path(data=rcp8.5.sim, size=1.5, arrow=arrow(length = unit(5, "pt"), type="closed")) +
    geom_path(data=grad.sim, aes(color=yr0), size=1) +
    geom_path(data=grad.sim[which(grad.sim$Tt>2.5),], aes(color=yr0), size=1, arrow=arrow(length = unit(5, "pt"), type="closed")) +
    geom_point(data=start.sim, aes(colour=yr0), size=4) +
    geom_path(data=rcp8.5.em, size=1.5, arrow=arrow(length = unit(5, "pt"), type="closed"), linetype=11) +
    geom_path(data=grad.em, aes(color=yr0), size=1, linetype=11) +
    geom_path(data=grad.em[which(grad.em$Tt>2.5),], aes(color=yr0), size=1, arrow=arrow(length = unit(5, "pt"), type="closed"), linetype=11) +
    geom_point(data=start.em, aes(colour=yr0), size=4)
  
  return(p)
}

pH.q.sim <- plot.hysteresis(rcp8.5, grad, start, rcp8.5.q, grad.q, start.q)
pH.d.sim <- plot.hysteresis(rcp8.5, grad, start, rcp8.5.d, grad.d, start.d)
pH.S.sim <- plot.hysteresis(rcp8.5, grad, start, rcp8.5.S, grad.S, start.S)

pdf("figures/hysteresis_SIMPLE.pdf", paper="special", width=5, height=5/1.618)
pH.S.sim
dev.off()

# ORGANISE FIGURES AND WRITE TO FILES
# only hysteresis SIMPLE
#pdf("figures/hysteresis_SIMPLE.pdf", paper="special", width=5, height=5/1.618)
grid.arrange(
  pH.S,
  legend,
  ncol=2, widths=c(4.0, 1.0) )
#dev.off()

# SIMPLE and SICOPOLIS
#pdf("figures/hysteresis_SIMPLE_SICOPOLIS.pdf", paper="special", width=5, height=2*5/1.618)
grid.arrange(
  arrangeGrob(textGrob("SICOPOLIS", rot = 0, vjust = 1,
                       gp = gpar(cex = 0.8)),
              pH.sim,
              textGrob("SIMPLE", rot = 0, vjust = 1,
                       gp = gpar(cex = 0.8)),
              pH.S,
              ncol=1, heights=c(0.3, 4.0, 0.3, 4.0)),
  legend,
  ncol=2, widths=c(4.0, 1.0) )
#dev.off()

# all models
#pdf("figures/hysteresis_all_models.pdf", paper="special", width=2*5, height=2*5/1.618)
grid.arrange(
  arrangeGrob(textGrob("Quadratic", rot = 0, vjust = 1,
                       gp = gpar(cex = 0.8)),
              pH.q,
              textGrob("SIMPLE", rot = 0, vjust = 1,
                       gp = gpar(cex = 0.8)),
              pH.S,
              ncol=1, heights=c(0.3, 4.0, 0.3, 4.0)),
  arrangeGrob(textGrob("Delayed-linear", rot = 0, vjust = 1,
                       gp = gpar(cex = 0.8)),
              pH.d,
              textGrob("SICOPOLIS", rot = 0, vjust = 1,
                       gp = gpar(cex = 0.8)),
              pH.sim,
              ncol=1, heights=c(0.3, 4.0, 0.3, 4.0)),
  legend,
  ncol=3, widths=c(2.3, 2.3, 0.8) )
#dev.off()

# emulators vs SICOPOLIS
#pdf("figures/hysteresis_emulators_vs_SICOPOLIS.pdf", paper="special", width=5, height=5*1.4)
grid.arrange(
  arrangeGrob(textGrob("Quadratic", rot = 0, vjust = 1,
                       gp = gpar(cex = 0.8)),
              pH.q.sim,
              textGrob("Delayed-linear", rot = 0, vjust = 1,
                       gp = gpar(cex = 0.8)),
              pH.d.sim,
              textGrob("SIMPLE", rot = 0, vjust = 1,
                       gp = gpar(cex = 0.8)),
              pH.S.sim,
              ncol=1, heights=c(0.4, 4.0, 0.4, 4.0, 0.4, 4.0)),
  legend,
  ncol=2, widths=c(4.0, 1.0) )
#dev.off()

# SIMPLE complete
#pdf("figures/SIMPLE_complete.pdf", paper="special", width=5, height=5*1.4)
grid.arrange(
  arrangeGrob(pT,
              pV.S,
              pH.S,
              ncol=1),
  legend,
  ncol=2, widths=c(4.0, 1.0) )
#dev.off()

#pdf("figures/SIMPLE_vs_SICOPOLIS_complete.pdf", paper="special", width=5, height=5*1.4)
grid.arrange(
  arrangeGrob(pT,
              pV.S.sim,
              pH.S.sim,
              ncol=1),
  legend,
  ncol=2, widths=c(4.0, 1.0) )
#dev.off()

#pdf("figures/SIMPLE_SICOPOLIS_complete.pdf", paper="special", width=10, height=5*1.4)
grid.arrange(
  arrangeGrob(pT,
              pV.S,
              pH.S,
              ncol=1,
              main=textGrob("SIMPLE", rot = 0, vjust = 1,
                            gp = gpar(cex = 0.8))),
  arrangeGrob(pT,
              pV.sim,
              pH.sim,
              ncol=1,
              main=textGrob("SICOPOLIS", rot = 0, vjust = 1,
                            gp = gpar(cex = 0.8))),
  legend,
  ncol=3, widths=c(2.3, 2.3, 0.8) )
#dev.off()



  
#pdf("figures/emulations_complete.pdf", paper="special", width=10, height=3*5/1.618)
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
#dev.off()




