#' Title
#'
#' @param lor 
#'
#' @return
#' @export
#'
#' @examples
draw.lorenz.join <- function(lor) {
  lor.join <- c()
  gini.join <- c()
  for (x in list(c('Cost', 'Healthcare System Cost'), c('Paid', 'Out-Of-Pocket Cost'), c('Visits', 'Time Cost (number of visits)'))) {
    index <- x[1]
    index.show <- x[2]
    y <- lor[index]
    n <- nrow(lor)
    
    eq <- seq(0, 100, length.out = n)
    gini <- sum(eq - y) / sum(eq) * 100
    gini.join[index.show] <- round(gini, 2)
    
    dat <- data.frame(p=eq, eq=eq, y=y, 
                      dep=lor[paste0(index, 'Dep')], dep=lor[paste0(index, 'DepU')], dep=lor[paste0(index, 'DepL')], 
                      index=index.show)
    
    names(dat) <- c('p', 'eq', 'y', 'dep', 'depu', 'depl', 'index')
    
    lor.join <- rbind(lor.join, dat)
    
  }
  
  gini.join <- data.frame(Gini=gini.join, index=names(gini.join))
  
  ylab1 <- 'Cumulative share of cost (%)'
  ylab2 <- 'Smoothed measure of deprivation (%)'
  xlab <- 'Cumulative share of patients, ranked by cost'
  
  depband <- ggplot(lor.join) + 
    geom_smooth(aes(x = p, y = depl*100), se = FALSE, n=91) +
    geom_smooth(aes(x = p, y = depu*100), se = FALSE, n=91) +
    facet_grid(. ~ index)
  
  depband <- ggplot_build(depband)
  
  # extract data for the loess lines from the 'data' slot
  lor.join$depmax <- c(rep(NA, 5), depband$data[[2]]$y[1:91], rep(NA, 10),
                       depband$data[[2]]$y[1:91+91], rep(NA, 10),
                       depband$data[[2]]$y[1:91+182], rep(NA, 5))
  lor.join$depmin <- c(rep(NA, 5), depband$data[[1]]$y[1:91], rep(NA, 10),
                       depband$data[[1]]$y[1:91+91], rep(NA, 10),
                       depband$data[[1]]$y[1:91+182], rep(NA, 5))
  
  
  ggplot(data=lor.join, aes(x=p, y=y)) +
    geom_line(size=1.4, aes(linetype='L')) + 
    geom_line(aes(y=eq, linetype='E'), size=1.1) +
    geom_ribbon(aes(x=p, ymin=depmin, ymax=depmax, fill='Deprived'), alpha=0.5) +
    geom_smooth(aes(y=dep*100, colour='Deprived'), size=1.5, se=FALSE, n=101) +
    
    facet_grid(. ~ index) +
    geom_text(data = gini.join, aes(x = 0, y = 100, label = paste0('italic(Gini) == ', Gini)), 
              colour='black', parse = TRUE, vjust = 1, hjust=0) +
    labs(x=xlab) +
    scale_linetype_manual(values=c('dotted', 'solid'), labels=c('Data', 'Perfect equality'), breaks=c('L', 'E'), name='') +
    scale_colour_discrete(labels=c('Deprivation'), breaks=c('Deprived'), name=' ') +
    scale_fill_discrete(labels=c('Deprivation'), breaks=c('Deprived'), guide=FALSE) +
    guides(linetype = guide_legend(override.aes = list(size = 1), order=1), color = guide_legend(order=2)) + #, color=guide_legend(title='')) +
    scale_y_continuous(ylab1, sec.axis = sec_axis(~., name = ylab2)) + 
    theme_minimal() +
    theme(legend.position="bottom", axis.title.y.right = element_text(angle = 90, hjust = 0.5))
}
