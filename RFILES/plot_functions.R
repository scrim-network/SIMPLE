deelplot <- function(X,Y,title,ylim,ylab="relative change [%]") {
  df_long <- melt(X, id="mnd")
  if(is.na(Y)) {
    p <- ggplot()
  } else {
    p <- ggplot() +
      geom_ribbon(data=Y, aes(x=month, ymin=q05, ymax=q95), fill="grey",alpha=0.5) +
      geom_ribbon(data=Y, aes(x=month, ymin=q25, ymax=q75), fill="grey",alpha=0.8)
  }
  
  p <- p +
  geom_line(data=df_long, 
            aes(x=mnd, y=value, colour=variable)) +
  geom_point(data=df_long, 
             aes(x=mnd, y=value, colour=variable)) +
  scale_x_discrete(labels=maanden) +
  scale_colour_manual(values = c("yellow","magenta","green","purple")) +
  scale_y_continuous(limits = c(-ylim, ylim)) +
  labs(title= title, x="month", y=ylab, colour="scenario")
  
  return(p)
}

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


ggplot_dual_axis <- function(lhs, rhs, axis.title.y.rhs = "rotate") {
  # 1. Fix the right y-axis label justification
  rhs <- rhs + theme(axis.text.y = element_text(hjust = 0))
  # 2. Rotate the right y-axis label by 270 degrees by default
  if (missing(axis.title.y.rhs) | 
        axis.title.y.rhs %in% c("rotate", "rotated")) {
    rhs <- rhs + theme(axis.title.y = element_text(angle = 270)) 
  }
  # 3a. Use only major grid lines for the left axis
  lhs <- lhs + theme(panel.grid.minor = element_blank())
  # 3b. Use only major grid lines for the right axis
  #     force transparency of the backgrounds to allow grid lines to show
  rhs <- rhs + theme(panel.grid.minor = element_blank(), 
                     panel.background = element_rect(fill = "transparent", colour = NA), 
                     plot.background = element_rect(fill = "transparent", colour = NA))
  # Process gtable objects
  # 4. Extract gtable
  library("gtable") # loads the grid package
  g1 <- ggplot_gtable(ggplot_build(lhs))
  g2 <- ggplot_gtable(ggplot_build(rhs))
  # 5. Overlap the panel of the rhs plot on that of the lhs plot
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g <- gtable_add_grob(g1, 
                       g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  # Tweak axis position and labels
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[["axis"]]  # ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  g <- gtable_add_grob(g, g2$grobs[[7]], pp$t, length(g$widths), pp$b)
  # Display plot with arrangeGrob wrapper arrangeGrob(g)
  library("gridExtra")
  grid.newpage()
  return(arrangeGrob(g))
}
