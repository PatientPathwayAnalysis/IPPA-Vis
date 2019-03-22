as_pat_freq <- function(p.json) {
  pathways <- lapply(p.json$Episode, function(x) x$Pathway)
  cut <- c("TREATING_F", "TREATING_S")
  
  pre_pattern <- sapply(pathways, function(x) {
    stages <- sapply(x, function(v) v$Stage)
    stages <- stages[1:which.max(stages %in% cut)]
    paste(stages, collapse = ":")
  })
  
  
  pre_interval <- lapply(pathways, function(x) {
    times <- sapply(x, function(v) v$Time)
    stages <- sapply(x, function(v) v$Stage)
    sel <- which.max(stages %in% cut)
    
    times <- times[1:sel]
    if (sel > 1) {
      dt <- c(diff(times), 0)
    } else {
      dt <- c(0)
    }
    names(dt) <- stages[1:sel]
    dt
  })
  
  
  y1 <- cumsum(rev(sort(table(pre_pattern))))
  y0 <- c(0, y1[-length(y1)])
  ys <- cbind(y0=as.numeric(y0), y1=as.numeric(y1))
  ys <- data.frame(Pattern=names(y1), ys, stringsAsFactors=F)
  colnames(ys) <- c("Pattern", "y0", "y1")
  rownames(ys) <- NULL
  
  
  xs <- c()
  
  for (pat in ys$Pattern) {
    pat <- unlist(pat)
    vs <- data.frame(pre_interval[pre_pattern == pat])
    n <- ncol(vs)
    vs <- rowSums(vs)
    vs <- vs/sum(vs)
    vs <- cumsum(vs)
    vs[length(vs)] <- 1.02 
    vs <- data.frame(pat, strsplit(pat, ":"), c(ifelse(length(vs)>1, 0, 1), vs[-length(vs)]), vs, stringsAsFactors=F)
    colnames(vs) <- c("Pattern", "State", "x0", "x1")
    rownames(vs) <- NULL
    xs <- rbind(xs, vs)
  }
  
  pre_patfreq <- merge(xs, ys)
  
  post_pattern <- sapply(pathways, function(x) {
    stages <- sapply(x, function(v) v$Stage)
    stages <- stages[which.max(stages %in% cut):length(stages)]
    paste(stages, collapse = ":")
  })
  
  
  post_interval <- lapply(pathways, function(x) {
    times <- sapply(x, function(v) v$Time)
    stages <- sapply(x, function(v) v$Stage)
    sel <- which.max(stages %in% cut)
    
    times <- times[sel:length(times)]
    dt <- c(diff(times), 0)
    
    names(dt) <- stages[sel:length(stages)]
    dt
  })
  
  
  y1 <- cumsum(rev(sort(table(post_pattern))))
  y0 <- c(0, y1[-length(y1)])
  ys <- cbind(y0=as.numeric(y0), y1=as.numeric(y1))
  ys <- data.frame(Pattern=names(y1), ys, stringsAsFactors=F)
  colnames(ys) <- c("Pattern", "y0", "y1")
  rownames(ys) <- NULL
  
  
  xs <- c()
  
  for (pat in ys$Pattern) {
    pat <- unlist(pat)
    vs <- data.frame(post_interval[post_pattern == pat])
    n <- ncol(vs)
    vs <- rowSums(vs)
    vs <- vs/sum(vs)
    vs <- cumsum(vs)
    vs[length(vs)] <- 1.02 
    vs <- data.frame(pat, strsplit(pat, ":"), c(0, vs[-length(vs)]), vs, stringsAsFactors=F)
    colnames(vs) <- c("Pattern", "State", "x0", "x1")
    rownames(vs) <- NULL
    xs <- rbind(xs, vs)
  }
  
  post_patfreq <- merge(xs, ys)
  
  list(PreTre=pre_patfreq, PostTre=post_patfreq)
}


as_stage_dist <- function(p.json, t_end=365, dt=5) {
  t0 <- seq(0, t_end, dt)
  if (t0[length(t0)] != t_end) {
    t0 <- c(t0, t_end)
  }
  
  n_max <- length(t0)
  
  pathways <- lapply(p.json$Episode, function(x) x$Pathway)
  
  trajectory <- sapply(pathways, function(x) {
    stages <- sapply(x, function(v) v$Stage)
    times <- sapply(x, function(v) v$Time)
    names(times) <- stages
    times <- times - times[1]
    
    tf <- floor(times / dt)
    tf <- c(tf[-length(tf)][diff(tf) > 0], tf[length(tf)]) 
    tf <- tf[tf < n_max]
    tf <- c(tf, n_max)
    rep(names(tf)[-length(tf)], diff(tf))
  })
  
  rownames(trajectory) <- t0
  trajectory
}


as_referrals <- function(ps, stages, suffix) {
  referrals <- ps[, paste0(stages, suffix)]
  
  flows <- data.frame()
  for (i in 1:(length(stages)-1)) {
    fr <- referrals[, i]
    to <- referrals[, i + 1]
    tab <- table(fr, to)
    
    nl_fr <- length(levels(fr))
    nl_to <- length(levels(to))
    
    rownames(tab) = 1:nl_fr + (i-1)*nl_fr
    colnames(tab) = 1:nl_to + i*nl_to
    flows <- rbind(flows, data.frame(tab))
  }
  
  names(flows) <- c("Src", "Tar", "Val")
  
  
  hier <- lapply(referrals, levels)
  
  return (list(Flows=flows, Hierarchy=hier, Stages=stages))
}
