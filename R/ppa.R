library(gridExtra)
library(ggplot2)
library(tools)
library(RGraphics)


plt.prop <- function (dat, y) {
  d <- data.frame(Level=dat$Level, Y=dat[, y], colours=dat$colours, stringsAsFactors = F)
  ggplot(data=d, aes(Level, Y, fill=Level)) + 
    geom_bar(stat="identity") + 
    geom_text(aes(Level, Y, label = scales::percent(Y/100)), hjust=-.3) +
    coord_flip() +
    scale_fill_manual(values=d$colours) + 
    ylim(0, 120) + 
    theme_minimal() +
    theme(legend.position="none", 
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(),
          axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}


plt.prop.point <- function (dat, y) {
  d <- data.frame(Level=dat$Level, Y=dat[, y], colours=dat$colours, stringsAsFactors = F)
  ggplot(data=d, aes(Level, Y, fill=Level)) + 
    geom_point(aes(colour=Level), size=7) +
    geom_bar(aes(fill=Level), width=0.08, stat="identity") +
    geom_text(aes(Level, Y, label = scales::percent(Y/100)), 
              position=position_dodge(width = 1), colour="black", vjust=-1.8) +
    geom_segment(aes(x = Level, xend = Level, colour = Level), y = 0, yend = 100, size=1) +
    coord_flip() +
    scale_colour_manual(values=d$colours) + 
    scale_fill_manual(values=d$colours) + 
    ylim(0, 115) + 
    theme_minimal() +
    theme(legend.position="none", 
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(),
          axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}


plt.singlebar <- function (dat, y) {
  pr_none <- 100 - sum(dat[, y])
  d <- data.frame(Level=factor(c(as.character(dat$Level), " "), levels=c(" ", LETTERS[4:1])), 
                  Y=c(dat[, y], pr_none), 
                  colours=c(dat$colours[1:4], "white"), stringsAsFactors = F)
  
  g <- ggplot(data=d, aes(1, Y, fill=Level)) +
    geom_rect(xmin=1-.3, xmax=1+.3, ymin=0, ymax=100, fill="white", colour="black") + 
    geom_bar(stat="identity", width=.6) + 
    geom_label(aes(label = scales::percent(Y/100)),
               position = position_stack(vjust = 0.5),
               fill="white", alpha=0.7) +
    scale_fill_manual(breaks=d$Level, values=rev(d$colours)) + 
    ylim(0, 100) +
    theme_minimal() +
    theme(legend.position="none", 
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(),
          axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  g
} 


plt.text <- function(txt, fontsize=20) {
  textGrob(txt, gp=gpar(fontsize = fontsize, fontface = "bold"))
}


visualise_accessibility <- function(hs, ps, gref) {
  hs$Level[!(hs$Level %in% LETTERS[1:4])] <- "A"
  # hs <- hs[hs$Level %in% LETTERS[1:4],]
  hs$Level <- factor(as.character(hs$Level), levels=LETTERS[1:4])
  hos.start <- data.frame(table(ps$HospitalStart))
  rownames(hos.start) <- NULL
  names(hos.start) <- c("HID", "Start")
  
  hosp <- merge(hs, hos.start, all.x=T)
  hosp$Start[is.na(hosp$Start)] <- 0
  
  hosp.lv <- table(hosp$Level)  
  
  n.ini <- tapply(hosp$Start, hosp$Level, sum)
  
  hosp$EL <- ifelse(hosp$EL.TB <= 0, 0, hosp$Start) 
  n.el <- tapply(hosp$EL, hosp$Level, sum)
  
  hosp$EH <- ifelse(hosp$EH.TB <= 0, 0, hosp$Start) 
  n.eh <- tapply(hosp$EH, hosp$Level, sum)
  
  hosp$Anti <- ifelse(hosp$Anti.TB <= 0, 0, hosp$Start) 
  n.anti <- tapply(hosp$Anti, hosp$Level, sum)
  
  
  dat <- data.frame(
    Level = names(hosp.lv),
    n.ini = n.ini,
    prop.ini = n.ini/sum(n.ini) * 100,
    n.el = n.el,
    prop.el.all = n.el/sum(n.ini) * 100,
    prop.el.lv = n.el/n.ini * 100,
    n.eh = n.eh,
    prop.eh.all = n.eh/sum(n.ini) * 100,
    prop.eh.lv = n.eh/n.ini * 100,
    n.anti = n.anti,
    prop.anti.all = n.anti/sum(n.ini) * 100,
    prop.anti.lv = n.anti/n.ini * 100,
    colours = c("#FFC083", "#EDA864", "#AA7139", "#674019"), stringsAsFactors = F 
  )
  
  
  g0 = ggplot(data=dat, aes(x=1, y=Level)) + 
    geom_text(aes(label = paste0("Level ", Level)), cex=6) +
    xlim(0.75, 1.25) +
    theme_minimal() +
    theme(legend.position="none", 
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(),
          axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  g1 = plt.prop(dat, "prop.ini")
  g2.1 = plt.prop.point(dat, "prop.el.lv")
  g2.2 = plt.singlebar(dat, "prop.el.all")
  g3.1 = plt.prop.point(dat, "prop.eh.lv")
  g3.2 = plt.singlebar(dat, "prop.eh.all")
  g4.1 = plt.prop.point(dat, "prop.anti.lv")
  g4.2 = plt.singlebar(dat, "prop.anti.all")
  
  gs <- gref + 
    #scale_fill_manual(values = c("#FFC083", "#EDA864", "#AA7139", "#674019")) + 
    labs(subtitle="Referral flow") +
    #xlim(-20, 270) + 
    theme_minimal() + 
    theme(legend.position="none",
          plot.title = element_text(size = 20, face = "bold"), 
          plot.subtitle = element_text(size=18),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(),
          axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  
  grid.arrange(
    plt.text("A", 20), plt.text("1"), plt.text("2.1"), plt.text("2.2"), 
    plt.text("3.1"), plt.text("3.2"), plt.text("4.1"), plt.text("4.2"), 
    # 9
    plt.text("Level", 16), 
    plt.text("Level of initial care-seeking", 14), plt.text("Evaluations possibly for TB", 14), 
    plt.text("Evaluations probably for TB", 14), plt.text("TB treatment", 14),
    # 14
    textGrob("Coverage", gp = gpar(fontsize = 14)), textGrob("Access", gp = gpar(fontsize = 14)),
    textGrob("Coverage", gp = gpar(fontsize = 14)), textGrob("Access", gp = gpar(fontsize = 14)),
    textGrob("Coverage", gp = gpar(fontsize = 14)), textGrob("Access", gp = gpar(fontsize = 14)),
    # 20
    g0, g1, g2.1, g2.2, g3.1, g3.2, g4.1, g4.2,
    # 28
    plt.text("B", 20), 
    # 29
    plt.text("Waiting Stage"), plt.text("Evaluating Stage"), 
    plt.text("TB Detecting Stage"), plt.text("Notification, \nTreating Stage"),
    gs,
    widths = c(0.75, 1.25, 1.1, 0.9, 1.1, 0.9, 1.1, 0.9),
    heights = c(0.1, 0.05, 0.05, 1, 0.05, 0.9, 0.13),
    layout_matrix = rbind(1:8, c(9, 10, rep(11:13, each=2)), c(NA, NA, 14:19), 20:27, c(28, rep(NA, 7)), rep(33, 8), rep(29:32, each=2)),
    bottom = textGrob(
      paste(),
      gp = gpar(fontface = 3, f6ontsize = 13),
      hjust = 0.7
    )
  )
}
