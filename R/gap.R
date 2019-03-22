#' Draw gap diagram
#'
#' @param pathways dataframe of pathway features
#' @param time_end the end of the diagram
#'
#' @return
#' @export
#'
#' @examples
gap_diagram <- function(pathways, time_end=365) {
  day_s <- pathways$DayStart
  day_a = pathways$DayTreatmentAva - day_s
  day_h <- pathways$DayTreatmentHosp - day_s
  day_t <- pathways$DayTreating - day_s
  
  times <- 0:time_end
  
  prop_a <- sapply(times, function(t) mean(t >= day_a)) * 100
  prop_h <- sapply(times, function(t) mean(t >= day_h)) * 100
  prop_t <- sapply(times, function(t) mean(t >= day_t)) * 100
  
  types <- c("Hospital which can provide TB treatment", "Hospital which initialises TB treatment", "Treatment start")
  
  dat <- data.frame(Time=rep(times, 3), 
                    Prop=c(prop_a, Uti=prop_h, Tre=prop_t),
                    Arrival=factor(rep(types, each=length(times)), levels=types))
  
  g <- ggplot2::ggplot(ggplot2::aes(x=Time, y = Prop, group=Arrival), data=dat) + 
    ggplot2::geom_line(ggplot2::aes(colour=Arrival), size=1.4) + 
    ggplot2::labs(x="Time since first visit (day)", y="Proportion of pathways (%)") + 
    ggplot2::scale_colour_brewer(palette="Set1") +
    
    ggplot2::scale_y_continuous(breaks = c(0, 25, 50, 75, 90, 95, 100), limits=c(0, 100), minor_breaks=NULL) +
    ggplot2::scale_x_continuous(breaks = c(0, round(median(day_t)), 180, 365), minor_breaks = c(100, 200, 300)) +
    ggplot2::theme_minimal() + 
    ggplot2::theme(legend.justification=c(1,0), legend.position=c(0.95, 0.1), 
          legend.background=ggplot2::element_rect(fill="#f2f2f2", colour="white"),
          panel.grid.major.y=ggplot2::element_line(colour="gray50"), 
          panel.grid.major.x=ggplot2::element_line(colour="gray70"))
  
  g
}