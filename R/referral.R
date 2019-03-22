draw_sankey <- function(hier, flows, stages, y.lab="Level", tag=T, bar.width=10, padding=10, height=200, interval=70, n.step=50) {
  stocks <- data.frame(Stage=rep(names(hier), sapply(hier, length)), Level=unlist(hier),
                       value=0, x0=-1, y0=-1, y1=-1, stringsAsFactors = F)
  rownames(stocks) <- 1:nrow(stocks)
  
  stocks$value <- 0
  stocks[sort(as.numeric(levels(flows$Src))), "value"] <- tapply(flows$Val, flows$Src, sum)
  stocks[sort(as.numeric(levels(flows$Tar))), "value"] <- tapply(flows$Val, flows$Tar, sum)
  
  
  sts <- names(hier)
  sts.n <- length(sts)
  sts.size <- sapply(hier, length)
  sts.size.max <- max(sts.size)
  
  bar.height <- height - (sts.size.max-1) * padding
  width <- sts.n * bar.width + (sts.n-1) * interval
  
  for (i in 1:sts.n) {
    lv <- sts[i]
    nodes.n <- sts.size[i]
    cnts <- stocks[stocks$Stage==lv,]$value
    hei <- bar.height*cnts/sum(cnts)
    pad <- (height-bar.height) / (nodes.n-1)
    x0 <- (i-1) * (bar.width+interval)
    y0 <- cumsum(c(0, (hei+pad)[-nodes.n]))
    
    stocks[stocks$Stage==lv, "x0"] <- x0
    stocks[stocks$Stage==lv, "y0"] <- y0
    stocks[stocks$Stage==lv, "y1"] <- y0 + hei
  }
  
  stocks$x1 <- stocks$x0+bar.width
  
  dis <- function(y0, y1, ns) {
    int <- (y1-y0)/sum(ns) * ns
    cint <- cumsum(int)
    cbind(y0+cint-int, y0+cint)
  }
  
  flows <- cbind(flows, x0=stocks$x1[as.numeric(as.character(flows$Src))],
                 x1=stocks$x0[as.numeric(as.character(flows$Tar))],
                 y0s=-1, y1s=-1, y0t=-1, y1t=-1
  )
  
  for (j in 1:nrow(stocks)) {
    sel <- flows[flows$Src == j,]
    if (nrow(sel)) {
      ns <- sel$Val
      y0 <- stocks[j, "y0"]
      y1 <- stocks[j, "y1"]
      flows[flows$Src == j, c("y0s", "y1s")] <- dis(y0, y1, ns)
    }
    
    sel <- flows[flows$Tar == j,]
    if (nrow(sel)) {
      ns <- sel$Val
      y0 <- stocks[j, "y0"]
      y1 <- stocks[j, "y1"]
      flows[flows$Tar == j, c("y0t", "y1t")] <- dis(y0, y1, ns)
    }
  }
  
  plt <- ggplot2::ggplot(data=stocks) +
    ggplot2::geom_rect(ggplot2::aes(xmin=x0, xmax=x1, ymin=y0, ymax=y1, fill=Level))
  
  if (tag) {
    plt <- plt +
      ggplot2::geom_label(ggplot2::aes(x=(x0+x1)/2, y=(y0+y1)/2, label=Level), alpha=0.7)
  }

  dat <- unique(stocks[c("Stage", "x0", "x1")])
  plt <- plt + ggplot2::scale_x_continuous(breaks=(dat$x0+dat$x1)/2, label=stages)
  
  for (i in 1:nrow(flows)) {
    plt <- plt + with(flows[i, ], {
      xx <- seq(-pi/2, pi/2, length.out = n.step)
      ys.upper <- y0s + (y0t-y0s)/2 * (sin(xx) + 1 )
      ys.lower <- y1s + (y1t-y1s)/2 * (sin(xx) + 1 )
      xs <- seq(x0, x1, length.out = n.step)
      dat <- data.frame(xs, ys.lower, ys.upper)
      ggplot2::geom_ribbon(ggplot2::aes(x=xs, ymin=ys.lower, ymax=ys.upper), fill="darkgreen", alpha=0.3, data=dat)
    })
  }
  
  plt <- plt + ggplot2::labs(x="Stage", y="Hospital level") +
    ggplot2::guides(color=ggplot2::guide_legend(title="F")) +
    ggplot2::theme(legend.position="bottom")
  plt
}


#' Draw referral diagram
#'
#' @param ps dataframe of pathway features
#' @param bar.width width of bars
#' @param padding padding of bars
#' @param height total height of bars
#' @param interval interval between stages
#'
#' @return
#' @export
#'
#' @examples
#' ref <- visualise_referrals(pseudo.tb.p, bar.width=40)
#' print(ref)
visualise_referrals <- function(ps, bar.width=10, padding=10, height=200, interval=70) {
  # ps$WaitingLevel <- factor(as.character(ps$WaitingLevel), levels=LETTERS[1:4])
  # ps$EvaluatingLevel <- factor(as.character(ps$EvaluatingLevel), levels=LETTERS[1:4])
  # ps$DetectingLevel <- factor(as.character(ps$DetectingLevel), levels=LETTERS[1:4])
  # ps$TreatingLevel <- factor(as.character(ps$TreatingLevel), levels=LETTERS[1:4])
    
  
  res <- list()
  ref <- as_referrals(ps, c("Waiting", "Evaluating", "Detecting", "Treating"), "Level")
  res$Level <- draw_sankey(ref$Hierarchy, ref$Flows, ref$Stages, y.lab="Hospital Level", 
                           bar.width=bar.width, padding=padding, height=height, interval=interval)
  
  res$Level <- res$Level +
    ggplot2::scale_fill_manual(values = c('#FFC083', '#EDA864', '#AA7139', '#674019'))
  
  
  res$Level
}
