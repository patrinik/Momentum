
momentum.models <- function(prices, parameter = NULL, selected = c("EW", "MOM", "PMOM")){
  
  #init <<- prices
  nTop <- parameter$nTop 
  nMom <- parameter$nMom 
  nVol <- parameter$nVol 
  trgtVol <- parameter$trgtVol 
  # nTop <- 4; nMom <- 120; nVol <- 60; trgtVol <- 0.1
  
  # find period ends
  period.ends <- endpoints(prices, 'months')
  period.ends <- period.ends[period.ends > 0]
  
  data <- new.env()
  
  data$prices <- prices
  
  # initialize
  dummy <- prices
  dummy[] <- NA
  data$weight <- dummy
  data$execution.price <- dummy
  
  data$symbolnames <- names(prices)
  data$dates <- index.xts(prices[,1])
  
  # Momentum 
  momentum <- prices / mlag(prices, nMom)
  
  # Positive Momentum
  pmom.weights <- pos.momentum(prices, nTop, nMom)
  
  
  models <- list()
  
  ### SPY benchmark ###
  weight <- prices * 0
  weight$SPY <- 1
  data$weight[] = NA
  data$weight[period.ends,] = weight[period.ends,]
  data$weight[1:200,] = NA
  models$SPY = bt.run.share(data, clean.signal=F, silent = TRUE)
  
  ### EQUAL WEIGHT ###
  data$weight[] = NA
  data$weight[period.ends,] = ntop(prices[period.ends,], ncol(prices))
  data$weight[1:200,] = NA
  models$EW = bt.run.share(data, clean.signal=F, silent = TRUE)
  
  ### Simple Momentum Portfolio ###
  # allocate to top n assets, irrespective of pos or neg momentum 
  data$weight[] = NA
  data$weight[period.ends,] = ntop(momentum[period.ends,], nTop)  
  
  data$weight[1:200,] = NA
  models$MOM = bt.run.share(data, clean.signal=F, silent = TRUE)
  
  
  ### Target Volatliliy MOM ###
  ret.log = bt.apply.matrix(models$MOM$equity, ROC, type='continuous') 
  hist.vol = bt.apply.matrix(ret.log, runSD, n = nVol)
  
  hist.vol = sqrt(252) * as.vector(hist.vol)
  
  data$weight[] = NA
  data$weight[period.ends,] = (trgtVol / hist.vol[period.ends]) * ntop(momentum[period.ends,], nTop)
  rs = rowSums(data$weight, na.rm = T)
  data$weight[] = data$weight / iif(rs > 1, rs, 1)
  
  data$weight[1:200,] = NA
  models$MOMTV = bt.run.share(data, clean.signal=F, silent = TRUE) 
  
  
  ### Volatliliy Position Sizing ###
  # Simple Momentum #
  ret.log = bt.apply.matrix(prices, ROC, type='continuous')
  hist.vol = sqrt(252) * bt.apply.matrix(ret.log, runSD, n = nVol)
  hist.vol <- hist.vol[period.ends,]
  
  adj.vol = (1 / hist.vol) * (ntop(momentum[period.ends,], nTop) > 0)  # (pmom.weights[period.ends,] > 0) < OR > (ntop(momentum[period.ends,], nTop) > 0)
  data$weight[1:200,] = NA 
  
  data$weight[] = NA
  data$weight[period.ends,] = adj.vol / rowSums(adj.vol, na.rm=T)
  
  data$weight[1:200,] = NA
  models$RPMOM = bt.run.share(data, clean.signal=F, silent = TRUE)
  
  
  
  ### Positiv Momentum ###
  # allocate to top n assets, only pos momentum 
  data$weight[] = NA
  data$weight[period.ends,] =  pmom.weights[period.ends,]
  data$weight[1:200,] = NA
  models$PMOM = bt.run.share(data, clean.signal=F, silent = TRUE)
  
  
  ### Target Volatliliy POS MOM ###
  ret.log = bt.apply.matrix(models$PMOM$equity, ROC, type='continuous') 
  hist.vol = bt.apply.matrix(ret.log, runSD, n = nVol)
  
  hist.vol = sqrt(252) * as.vector(hist.vol)
  
  data$weight[] = NA
  data$weight[period.ends,] = (trgtVol / hist.vol[period.ends]) * pmom.weights[period.ends,] 
  rs = rowSums(data$weight, na.rm = T)
  data$weight[] = data$weight / iif(rs > 1, rs, 1)
  
  data$weight[1:200,] = NA
  models$PMOMTV = bt.run.share(data, clean.signal=F, silent = TRUE)
  
  ### Volatliliy Position Sizing ###
  # Positive Momentum #
  ret.log = bt.apply.matrix(prices, ROC, type='continuous')
  hist.vol = sqrt(252) * bt.apply.matrix(ret.log, runSD, n = nVol)
  hist.vol <- hist.vol[period.ends,]
  
  adj.vol = (1 / hist.vol) * (pmom.weights[period.ends,] > 0)  # (pmom.weights[period.ends,] > 0) < OR > (ntop(momentum[period.ends,], nTop) > 0)
  data$weight[1:200,] = NA 
  
  data$weight[] = NA
  data$weight[period.ends,] = adj.vol / rowSums(adj.vol, na.rm=T)
  
  data$weight[1:200,] = NA
  models$RPPMOM = bt.run.share(data, clean.signal=F, silent = TRUE)
  
  
  ### Timing by M. Faber ###
  # https://github.com/systematicinvestor/SIT/blob/master/R/bt.test.r {~3477}
  
  sma = bt.apply.matrix(prices, SMA, 200)
  
  weight = ntop(prices, nTop) * (prices > sma)
  data$weight[] = NA
  data$weight[period.ends,] = weight[period.ends,]
  
  data$weight[1:200,] = NA
  models$TMNG = bt.run.share(data, clean.signal=F, silent = TRUE)
  
  
  ### Timing with target 10% Volatility ###
  ret.log = bt.apply.matrix(models$TMNG$equity, ROC, type='continuous')
  
  hist.vol = bt.apply.matrix(ret.log, runSD, n = nVol) 
  hist.vol = sqrt(252) * as.vector(hist.vol)
  
  data$weight[] = NA
  data$weight[period.ends,] = (trgtVol / hist.vol[period.ends]) * weight[period.ends,]
  rs = rowSums(data$weight, na.rm = T)
  data$weight[] = data$weight / iif(rs > 1, rs, 1)

  data$weight[1:200,] = NA
  models$TMNGTV = bt.run.share(data, clean.signal=F, silent = TRUE) 
  
  
  
  ### Adaptive Asset Allocation (AAA) ###
  # Minimum Variance Algorithm #
  # Simple Momentum #
  ret.log = bt.apply.matrix(prices, ROC, type='continuous')
  pmom.weights <- pos.momentum(prices, nTop, nMom)

  weight = NA * prices
  weight[period.ends,] = ntop(momentum[period.ends,], nTop)
  #weight[period.ends,] = mom.weights[period.ends,]

  for( i in period.ends[period.ends >= nMom] ) {
    hist = ret.log[ (i - nVol + 1):i, ]

    # require all assets to have full price history
    include.index = count(hist)== nVol

    # also only consider assets in the Momentum Portfolio
    index = ( weight[i,] > 0 ) & include.index
    n = sum(index)

    if( n == 1 ){
      weight[i,] = 0
      weight[i,index] = 1
    }

    if(n > 1) {
      hist = hist[ , index]

      # create historical input assumptions
      ia = create.ia(hist)
      s0 = apply(coredata(hist),2,sd)
      ia$cov = cor(coredata(hist), use='complete.obs',method='pearson') * (s0 %*% t(s0))

      # create constraints: 0<=x<=1, sum(x) = 1
      constraints = new.constraints(n, lb = 0.05, ub = 1)
      constraints = add.constraints(rep(1, n), 1, type = '=', constraints)

      # compute minimum variance weights
      weight[i,] = 0
      weight[i,index] = min.risk.portfolio(ia, constraints)
    }
  }

  # Adaptive Asset Allocation (AAA)
  data$weight[] = NA
  data$weight[period.ends,] = weight[period.ends,]

  data$weight[1:200,] = NA
  models$MV = bt.run.share(data, clean.signal=F, silent = TRUE)
  
  
  out <- lapply(selected, function(x){ models[[x]] })
  names(out) <- selected
  
  return(out)
}