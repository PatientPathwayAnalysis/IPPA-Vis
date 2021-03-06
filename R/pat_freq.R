#' Draw pattern frequency diagram
#'
#' @param pathways json formated patient pathways
#' @param maps colour maps
#' @param x.lab label of x-axis
#' @param y.lab label of y-axis
#'
#' @return
#' @export
#'
#' @examples
#' pat.freq <- visualise_pattern_freq(pseudo.tb.js)
#' print(pat.freq)
visualise_pattern_freq <- function(pathways, maps=state_space_tb$GP, 
                                   x.lab="Proportion of time in stages (%)", 
                                   y.lab="Accumulated pathways") {
  
  pat.freq <- as_pat_freq(pathways)
  
  pf <- rbind(
    cbind(pat.freq$PreTre, Group="Before Treatment"),
    cbind(pat.freq$PostTre, Group="During Treatment")
  )
  
  pf$State <- factor(pf$State, levels=maps$State)
  
  g <- ggplot2::ggplot(data=pf) +
    ggplot2::geom_rect(ggplot2::aes(xmin=x0, xmax=x1, ymin=y0, ymax=y1, fill=State)) +
    ggplot2::facet_wrap(~Group, nrow=2) +
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


pre_visits <- function(p.json) {
  pathways <- lapply(p.json$Episode, function(x) x$Pathway)
  cut <- c("TREATING_F", "TREATING_S")
  
  pre_pattern <- sapply(pathways, function(x) {
    stages <- sapply(x, function(v) v$Stage)
    stages <- stages[1:which.max(stages %in% cut)]
    paste(stages, collapse = ":")
  })
  
  y1 <- cumsum(rev(sort(table(pre_pattern))))
  y0 <- c(0, y1[-length(y1)])
  ys <- cbind(y0=as.numeric(y0), y1=as.numeric(y1))/length(pre_pattern)
  ys <- data.frame(Pattern=names(y1), ys, stringsAsFactors=F)
  colnames(ys) <- c("Pattern", "y0", "y1")
  rownames(ys) <- NULL
  
  visit <- sapply(p.json$Episode, function(x) {
    atr <- x$Attributes
    c(visits=atr$WaitingVisits + atr$EvaluatingVisits + atr$DetectingVisits,
      cost=atr$WaitingSystem_Cost + atr$EvaluatingSystem_Cost + atr$DetectingSystem_Cost,
      paid=atr$WaitingPatient_Cost + atr$EvaluatingPatient_Cost + atr$DetectingPatient_Cost,
      delay=atr$EvaluationDelay + atr$DetectionDelay + atr$DiagnosisDelay,
      ie=atr$InterruptedEvaluation)
  })
  visit <- data.frame(Pattern=pre_pattern, t(visit))
  
  visit <- merge(visit, ys)
  visit[, c("visits", "cost", "paid", "delay", "ie", "y0", "y1")]
}

