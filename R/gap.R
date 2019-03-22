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
  
  g <- ggplot(aes(x=Time, y = Prop, group=Arrival), data=dat) + 
    geom_line(aes(colour=Arrival), size=1.4) + 
    labs(x="Time since first visit (day)", y="Proportion of pathways (%)") + 
    scale_colour_brewer(palette="Set1") +
    
    scale_y_continuous(breaks = c(0, 25, 50, 75, 90, 95, 100), limits=c(0, 100), minor_breaks=NULL) +
    scale_x_continuous(breaks = c(0, round(median(day_t)), 180, 365), minor_breaks = c(100, 200, 300)) +
    theme_minimal() + 
    theme(legend.justification=c(1,0), legend.position=c(0.95, 0.1), 
          legend.background=element_rect(fill="#f2f2f2", colour="white"),
          panel.grid.major.y=element_line(colour="gray50"), 
          panel.grid.major.x=element_line(colour="gray70"))
  
  g
}