library(ggplot2)
source("src/format.R")


visualise_stage_dist <- function(pathways, maps, 
                                   x.lab="Time since first visit (day)", 
                                   y.lab="Stage distribution",
                                   t_end=365, dt=1) {
  stage.dist <- as_stage_dist(pathways, t_end=t_end, dt=dt)
  
  sts0 <- apply(stage.dist, 1, function(x) {
    res <- cumsum(table(factor(x, levels=maps$State)))
    res <- c(0, res[-length(res)])
    names(res) <- maps$State
    res
  })
  sts0 <- data.frame(as.table(sts0), stringsAsFactors = F)
  names(sts0) <- c("State", "x0", "y0")
  
  sts1 <- apply(stage.dist, 1, function(x) cumsum(table(factor(x, levels=maps$State))))
  sts1 <- data.frame(as.table(sts1), stringsAsFactors = F)
  names(sts1) <- c("State", "x0", "y1")
  
  sts <- merge(sts0, sts1)
  sts$x0 <- as.numeric(as.character(sts$x0))
  sts$x1 <- sts$x0 + dt
  sts <- sts[order(sts$x0, sts$State),]
  
  
  g <- ggplot(data=sts) +
    geom_rect(aes(xmin=x0, xmax=x1, ymin=y0, ymax=y1, fill=State)) +
    scale_fill_manual("Stage/State",
                      breaks=maps$State,
                      values=maps$Colour, 
                      labels=maps$StateShow, drop=FALSE) +
    xlab(x.lab) + ylab(y.lab) + 
    guides(fill=guide_legend(ncol=1)) +
    theme_minimal() + 
    theme(strip.text.x=element_text(face="bold"),
          strip.background=element_rect(fill="#CCCCCC", colour="white"))
  
  g
}





