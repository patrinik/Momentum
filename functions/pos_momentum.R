
pos.momentum <- function(prices, n.top = 5, n.mom = 100, threshold = 0){
  
  roc <- xts(apply(prices, 2, ROC, n = n.mom), order.by = index(prices))
  
  pos.roc <- roc > threshold
  
  signal <- roc * pos.roc
  
  if( ncol(prices) != 1 ){
    
    rnk <- t(apply(-signal, 1, function(x){ rank(x, na.last = "keep") }))
    
    idx <- signal == 0
    rnk[idx] <- n.top + 1
    
    bool <- rnk <= n.top
    
    rs <- rowSums(bool, na.rm = T)
    rs[rs == 0] <-  1
    weights <- (bool)/rs
  }else{
    weights <- ifelse(pos.roc == TRUE, 1, 0)
  }
  
  weights[is.na(weights)] <- 0
  
  return(weights)
}