# construct CPU plot

# clean environment and set directory  
rm(list = ls())
graphics.off()

# packages and sourcefiles
require("ggplot2")
require("reshape2")
require("grid")
require("gridExtra")
#require('DEoptim')
#require("plyr")

# set plot_themes
source("set_theme_AB.R")
theme_set(theme_ali(base_size=20))

#source("plot_functions.R")
source("ggplot_dual_axis.R")

cpus <- c(2.5*10^-3, 5*10^-1, 1*10^5, 7.5*10^5, 4.5*10^6)

CPU.TIMES <- data.frame(run  =c(rep(1,5),rep(10^9,5)),
                        model=as.factor(rep(c("SIMPLE","GLISTEN (36km)","SICOPOLIS (10km)","PSU-3D (20km)", "PSU-3D (10km)"),2)),
                        cpu  =c(cpus,10^9*cpus) )

#CPU.TIMES$model <- as.factor(rep(c("SIMPLE (Bakker et al., 2015)",
#                                   "GLISTEN (Haqq-Misra et al., 2012)",
#                                   "SICOPOLIS (e.g. Applegate and Keller, 2015)",
#                                   "3D (20km) (Pollard, personal communication)",
#                                   "3D (10km) (Pollard, personal communication)"),2))

CPU.TIMES$model <- as.factor(rep(c("SIMPLE",
                                   "GLISTEN (36km)",
                                   "SICOPOLIS (10km)",
                                   "3D-PSU (20km)",
                                   "3D-PSU (10km)"),2))

time.labels <- c(a=expression("minute"),
                 b=expression("hour"),
                 c=expression("day"),
                 d=expression("month"),
                 e=expression("year"),
                 f=expression("century"),
                 g=expression(paste(10^6, " years")) )
t.times <- data.frame(unit=c("minute","hour","day","month","year","century","4%.%10^+12 years"),
                      second=c(60, 3600, 24*3600, 30*24*3600, 365*24*3600, 36525*24*3600, 10^6*3652.5*24*3600) )
t.times$unit <- levels(t.times$unit)[as.numeric(t.times$unit)]

library(scales)

p.base <- ggplot(data=CPU.TIMES, aes(x=run, y=cpu, color=model)) + geom_line(size = 2) +
  annotate("rect", xmin=10^5, xmax=10^6, ymin=10^-3, ymax=30*24*3600, color="lightgrey", size=0.5, fill="green", alpha=0.1) +
  annotate("rect", xmin=10^6, xmax=10^9, ymin=10^-3, ymax=30*24*3600, color="lightgrey", size=0.5, fill="red", alpha=0.1) +
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)),
                breaks = trans_breaks("log10", function(x) 10^x, n=8),
                expand = c(0.0001,0.0001),
                limits = c(10^0,10^9)) +
  labs(x="Number of ensemble members", y="Wall time (1 CPU) time [seconds]") +
  theme(legend.justification=c(0.02,0.88), legend.position=c(0.01,1), legend.direction="vertical",
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text = element_text(size = 10),
        legend.key.height = unit(0.5,"lines"),
        legend.background = element_rect(fill=alpha('white', 0.4))) + 
  scale_colour_discrete(breaks=levels(CPU.TIMES$model)[c(1,2,4,3,5)]) +
  annotate("text", x=0.9 * 10^6, y= 1*10^-2, label="Emulation",                                cex=3, hjust=1, vjust=0) +
  annotate("text", x=0.8 * 10^9, y= 1*10^-2, label="Multi-Objective\nRobust\nDecision Making", cex=3, hjust=1, vjust=0)

p1 <- p.base +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x)),
                breaks = trans_breaks("log10", function(x) 10^x, n=9),
                expand = c(0.0001,0.0001),
                limits = c(10^-3,10^17))


p2 <- p.base +
  scale_y_log10(labels = time.labels,
                breaks = t.times$second,
                expand = c(0.0001,0.0001),
                limits = c(10^-3,10^17)) +
  ylab(NULL)

#cpu.plot <- ggplot_dual_axis(lhs=p1, rhs=p2, axis.title.y.rhs = "rotate")
#p <- grid.draw(cpu.plot)
cpu.plot <- ggplot_dual_axis(p1, p2, which.axis = "y")

grid.draw(cpu.plot)

  

#pdf("CPU.pdf", paper="special", width=7, height=7/1.618)
grid.draw(cpu.plot)
#dev.off()

