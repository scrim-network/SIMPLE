
# clean environment and set directory  
rm(list = ls())
graphics.off()

# packages and sourcefiles
require(ggplot2)
require("reshape2")
require("grid")
require("gridExtra")
require("RColorBrewer")

# set plot_themes
source("set_theme_AB.R")
theme_set(theme_ali(base_size=16))

source("plot_functions.R")

# read SICOPOLIS data
equil     <- read.table("data/SICOPOLIS_equil.txt", header=T) # 'temperature stabilization' geo-engineering scenario
equil$yr0 <- as.factor(equil$yr0)                                  # set starting year (yr0) as factor
grad      <- read.table("data/SICOPOLIS_grad.txt",  header=T) # 'gradual draw-down' geo-engineering scenario
grad$yr0  <- as.factor(grad$yr0)                                   # set starting year (yr0) as factor
hind      <- read.table("data/SICOPOLIS_hind.txt",  header=T) # hindcast
rcp8.5    <- read.table("data/SICOPOLIS_rcp8.5.txt",header=T) # rcp8.5

# number of starting years for geo-engineering
nyr0      <- length(levels(equil$yr0))
colors <- rev(brewer.pal(nyr0, "Spectral"))

# create legend
legend <- get_legend(ggplot(equil, aes(x=t, y=Vt, color=yr0)) +
                       geom_line(size=1) +
                       labs(color="Starting year \ngeo-\nengineering ") +
                       scale_colour_manual(values = rev(brewer.pal(nyr0,"Spectral"))) +
                       theme(legend.key = element_rect(fill = "white"))
)

legend.bottom <- get_legend(ggplot(equil, aes(x=t, y=Vt, color=yr0)) +
                       geom_line(size=2) +
                       labs(color="Starting year geo-engineering") +
                       scale_colour_manual(values = rev(brewer.pal(nyr0,"Spectral"))) +
                       theme(legend.position="bottom") +
                       theme(legend.key = element_rect(fill = "white")) +
                         guides(col = guide_legend(nrow = 2))
)


# subset data for plotting
equil.start <- equil[ which(equil$t == as.numeric(levels(equil$yr0))[equil$yr0]) - 1,]
equil.after <- equil[ which(equil$t >  as.numeric(levels(equil$yr0))[equil$yr0]) - 1,]
grad.start  <- grad[  which(grad$t  == as.numeric(levels(grad$yr0))[grad$yr0])   - 1,]
grad.after  <- grad[  which(grad$t  >  as.numeric(levels(grad$yr0))[grad$yr0])   - 1,]
hind        <- rbind(hind, rcp8.5[1,])

# create basic plot with hindcast and rcp8.5
p <- ggplot(rcp8.5, aes(x=t, y=Tt)) +
  geom_line(size=2) +
  geom_line(data=hind, size=1, colour="gray") +
  theme(legend.position="none") + labs(x=NULL, y=NULL) +
  scale_y_continuous(limits=c(-2,12)) +
  theme(legend.position="none")

# add layer with 'temperature stabilization' scenarios
p.equil <- p +
  geom_line(data=equil.after, aes(colour=yr0), size=1) +
  geom_point(data=equil.start, aes(colour=yr0), size=4) +
  scale_colour_manual(values = rev(brewer.pal(nyr0,"Spectral")))

# add layer with 'gradual temperature draw-down' scenarios
p.grad <- p +
  geom_line(data=grad.after, aes(colour=yr0), size=1) +
  geom_point(data=grad.start, aes(colour=yr0), size=4) +
  scale_colour_manual(values = rev(brewer.pal(nyr0,"Spectral")))


# arrange 2 grahps in multipanel
orientation = "horizontal"

if(orientation == "horizontal") {
  pdf("figures/Tscenarios.pdf", paper="special", width=10, height=7/1.618)
  grid.arrange(
    arrangeGrob(p.equil + xlab("Year") + ggtitle("Temperature stabilization\n") +
                  theme(axis.title.x = element_text(vjust=0), plot.title = element_text(vjust=1)),
                p.grad  + xlab("Year") + ggtitle("Gradual temperature draw-down\n") +
                  theme(axis.title.x = element_text(vjust=0), plot.title = element_text(vjust=1)),
                ncol=2),
    left = textGrob(expression(paste("Temperature anomaly [",degree,"C]")),rot = 90, vjust = 1, hjust=0.4,
                    gp = gpar(cex = 1.5)),
    legend.bottom, ncol=1, widths=c(1), nrow=2, heights=c(6,1)
  )
  dev.off()
} # horizontal orientation




if(orientation == "vertical") {
  # add plot title
  p.equil <- p.equil + ggtitle("Temperature stabilization")
  p.grad  <- p.grad  + ggtitle("Gradual temperature draw-down")
  
  pdf("../figures/Tscenarios.pdf", paper="special", width=5, height=10/1.618)
  grid.arrange(
    arrangeGrob(p.equil,
                p.grad,
                ncol=1),
    legend, ncol=2, widths=c(4.0, 0.9),
    left = textGrob(expression(paste("Temperature anomaly [",degree,"C]")), rot = 90, vjust = 1, hjust=0.5,
                    gp = gpar(cex = 1)),
    bottom = textGrob("Year", rot = 0, hjust = 2, vjust = 0, 
                   gp = gpar(cex = 1))
  )
  dev.off()
} # vertical orientation
