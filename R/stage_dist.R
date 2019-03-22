#' Draw stage distribution diagram
#'
#' @param pathways json formated patient pathways
#' @param maps colour maps
#' @param x.lab label of x-axis
#' @param y.lab label of y-axis
#' @param t_end read end of the diagram, default = 365 days
#' @param dt bar update frequency of the diagram
#'
#' @return
#' @export
#'
#' @examples
#' st.di <- visualise_stage_dist(pseudo.tb.js)
#' print(st.di)
visualise_stage_dist <- function(pathways, maps=state_space_tb$All, 
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
  
  
  g <- ggplot2::ggplot(data=sts) +
    ggplot2::geom_rect(ggplot2::aes(xmin=x0, xmax=x1, ymin=y0, ymax=y1, fill=State)) +
    ggplot2::scale_fill_manual("Stage/State",
                      breaks=maps$State,
                      values=maps$Colour, 
                      labels=maps$StateShow, drop=FALSE) +
    ggplot2::xlab(x.lab) + ggplot2::ylab(y.lab) + 
    ggplot2::guides(fill=ggplot2::guide_legend(ncol=1)) +
    ggplot2::theme_minimal() + 
    ggplot2::theme(strip.text.x=ggplot2::element_text(face="bold"),
          strip.background=ggplot2::element_rect(fill="#CCCCCC", colour="white"))
  
  g
}
