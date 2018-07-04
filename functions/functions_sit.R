
ntop <- function( data, topn = 1, dirMaxMin = TRUE ){
  temp = coredata(data)
  if(is.logical(temp)) temp[] = iif(!temp,NA,temp)
  if(topn == ncol(data)) {
    index = is.na(temp)
    temp[index] = 0
    temp[!index] = 1
    out = data
    out[] = ifna(temp / rowSums(temp),0)
    return( out )
  }
  index.n = rowSums(!is.na(temp))
  for( i in 1:nrow(data) ) {
    if( index.n[i] > 0 ) {
      o = sort.list(temp[i,], na.last = TRUE, decreasing = dirMaxMin)
      temp[i,] = 0
      n = min(topn, index.n[i])
      temp[i,o[1:n]] = 1/n
    } else temp[i,] = 0
  }
  out = data
  out[] = temp
  out
}

bt.run.share <- function(
  b,
  prices = b$prices,
  clean.signal = T,
  trade.summary = F,
  do.lag = 1,
  do.CarryLastObservationForwardIfNA = TRUE,
  silent = F,
  capital = 100000,
  commission = 0,
  weight = b$weight,
  dates = 1:nrow(b$prices)
){
  prices[] = bt.apply.matrix(coredata(prices), ifna.prev)
  weight = mlag(weight, do.lag - 1)
  do.lag = 1
  if(clean.signal)
    weight[] = bt.exrem(weight)
  weight = (capital / prices) * weight
  bt.run(b,
         trade.summary = trade.summary,
         do.lag = do.lag,
         do.CarryLastObservationForwardIfNA = do.CarryLastObservationForwardIfNA,
         type='share',
         silent = silent,
         capital = capital,
         commission = commission,
         weight = weight,
         dates = dates)
}

bt.apply.matrix <- function(b, xfun = Cl, ...){
  out = b
  out[] = NA
  nsymbols = ncol(b)
  xfun = match.fun(xfun)
  for( i in 1:nsymbols ) {
    msg = try( xfun( coredata(b[,i]),... ) , silent=TRUE)
    if (class(msg)[1] == 'try-error')
      warning(i, msg, '\n')
    else
      out[,i] = msg
  }
  return(out)
}

bt.run <- function(
  b,
  trade.summary = F,
  do.lag = 1,
  do.CarryLastObservationForwardIfNA = TRUE,
  type = c('weight', 'share'),
  silent = F,
  capital = 100000,
  commission = 0,
  weight = b$weight,
  dates = 1:nrow(b$prices)
){
  dates.index = dates2index(b$prices, dates)
  type = type[1]
  weight[] = ifna(weight, NA)
  if(do.lag > 0)
    weight = mlag(weight, do.lag)
  if(do.CarryLastObservationForwardIfNA)
    weight[] = apply(coredata(weight), 2, ifna.prev)
  weight[is.na(weight)] = 0
  weight1 = mlag(weight, -1)
  tstart = weight != weight1 & weight1 != 0
  tend = weight != 0 & weight != weight1
  trade = ifna(tstart | tend, FALSE)
  prices = b$prices
  if( sum(trade) > 0 ) {
    execution.price = coredata(b$execution.price)
    prices1 = coredata(b$prices)
    prices1[trade] = iif( is.na(execution.price[trade]), prices1[trade], execution.price[trade] )
    prices[] = prices1
  }
  if( type == 'weight') {
    ret = prices / mlag(prices) - 1
    ret[] = ifna(ret, NA)
    ret[is.na(ret)] = 0
  } else {
    ret = prices
  }
  temp = b$weight
  temp[] = weight
  weight = temp
  bt = bt.summary(weight, ret, type, b$prices, capital, commission)
  bt$dates.index = dates.index
  bt = bt.run.trim.helper(bt, dates.index)
  if( trade.summary ) bt$trade.summary = bt.trade.summary(b, bt)
  if( !silent ) {
    cat('Latest weights :\n')
    print(round(100*last(bt$weight),2))
    cat('\n')
    cat('Performance summary :\n')
    cat('', spl('CAGR,Best,Worst'), '\n', sep = '\t')
    cat('', sapply(cbind(bt$cagr, bt$best, bt$worst), function(x) round(100*x,1)), '\n', sep = '\t')
    cat('\n')
  }
  return(bt)
}

mlag <- function(m, nlag = 1){
  
  if( is.null(dim(m)) ) {
    n = len(m)
    if(nlag > 0) {
      m[(nlag+1):n] = m[1:(n-nlag)]
      m[1:nlag] = NA
    } else if(nlag < 0) {
      m[1:(n+nlag)] = m[(1-nlag):n]
      m[(n+nlag+1):n] = NA
    }
  } else {
    n = nrow(m)
    if(nlag > 0) {
      m[(nlag+1):n,] = m[1:(n-nlag),]
      m[1:nlag,] = NA
    } else if(nlag < 0) {
      m[1:(n+nlag),] = m[(1-nlag):n,]
      m[(n+nlag+1):n,] = NA
    }
  }
  return(m);
}

dates2index <- function( x, dates = 1:nrow(x) ) {
  dates.index = dates
  if(!is.numeric(dates)) {
    temp = x[,1]
    temp[] = 1:nrow(temp)
    dates.index = as.numeric(temp[dates])
  }
  return(dates.index)
}

ifna <- function( x, y ){
  
  return(iif(is.na(x) | is.nan(x) | is.infinite(x), y, x))
}

iif <- function( cond, truepart, falsepart ){
  
  if( len(cond) == 1 ){ 
    
    if(cond) truepart else falsepart 
  }else{
    
    if(length(falsepart) == 1) {
      temp = falsepart
      falsepart = cond
      falsepart[] = temp
    }
    if(length(truepart) == 1)
      falsepart[cond] = truepart
    else {
      cond = ifna(cond,F)
      if(requireNamespace('xts', quietly = T) && xts::is.xts(truepart))
        falsepart[cond] = coredata(truepart)[cond]
      else
        falsepart[cond] = truepart[cond]
    }
    falsepart
  }
}

len <- function(x) length(x)

ifna.prev <- function(y){
  y1 = !is.na(y)
  y1[1]=T
  return( y[cummax( (1:length(y)) * y1 )]	)
}


bt.summary <- function(
  weight,
  ret,
  type = c('weight', 'share'),
  close.prices,
  capital = 100000,
  commission = 0
){
  if( !is.list(commission) ) {
    if( type == 'weight')
      commission = list(cps = 0.0, fixed = 0.0, percentage = commission)
    else
      commission = list(cps = commission, fixed = 0.0, percentage = 0.0)
  }
  type = type[1]
  n = nrow(ret)
  bt = list()
  bt$weight = weight
  bt$type = type
  com.weight = mlag(weight,-1)
  if( type == 'weight') {
    temp = ret[,1]
    temp[] = rowSums(ret * weight) -
      rowSums(abs(com.weight - mlag(com.weight)) * commission$percentage, na.rm=T)
    - rowSums(sign(abs(com.weight - mlag(com.weight))) * commission$fixed, na.rm=T)
    bt$ret = temp
  } else {
    bt$share = weight
    bt$capital = capital
    prices = ret
    prices[] = bt.apply.matrix(coredata(prices), ifna.prev)
    close.prices[] = bt.apply.matrix(coredata(close.prices), ifna.prev)
    cash = capital - rowSums(bt$share * mlag(close.prices), na.rm=T)
    share.nextday = mlag(bt$share, -1)
    tstart = bt$share != share.nextday & share.nextday != 0
    tend = bt$share != 0 & bt$share != share.nextday
    trade = ifna(tstart | tend, FALSE)
    tstart = trade
    index = mlag(apply(tstart, 1, any))
    index = ifna(index, FALSE)
    index[1] = T
    totalcash = NA * cash
    totalcash[index] = cash[index]
    totalcash = ifna.prev(totalcash)
    totalcash = ifna(totalcash,0)
    portfolio.ret = (totalcash  + rowSums(bt$share * prices, na.rm=T)
                     - rowSums(abs(com.weight - mlag(com.weight)) * commission$cps, na.rm=T)
                     - rowSums(sign(abs(com.weight - mlag(com.weight))) * commission$fixed, na.rm=T)
                     - rowSums(prices * abs(com.weight - mlag(com.weight)) * commission$percentage, na.rm=T)
    ) / (totalcash + rowSums(bt$share * mlag(prices), na.rm=T) ) - 1
    bt$weight = bt$share * mlag(prices) / (totalcash + rowSums(bt$share * mlag(prices), na.rm=T) )
    bt$weight[is.na(bt$weight)] = 0
    temp = ret[,1]
    temp[] = ifna(portfolio.ret,0)
    temp[1] = 0
    bt$ret = temp
  }
  bt$best = max(bt$ret)
  bt$worst = min(bt$ret)
  bankrupt = which(bt$ret <= -1)
  if(len(bankrupt) > 0) bt$ret[bankrupt[1]:n] = -1
  bt$equity = cumprod(1 + bt$ret)
  bt$cagr = compute.cagr(bt$equity)
  return(bt)
}

bt.run.trim.helper <- function(bt, dates.index) {
  n.dates = len(dates.index)
  for(n in ls(bt)) {
    if( !is.null(dim(bt[[n]])) ) {
      if( nrow(bt[[n]]) > n.dates )
        bt[[n]] = bt[[n]][dates.index,,drop=F]
    } else if( len(bt[[n]]) > n.dates )
      bt[[n]] = bt[[n]][dates.index]
  }
  bt$equity = bt$equity / as.double(bt$equity[1])
  bt$best = max(bt$ret)
  bt$worst = min(bt$ret)
  bt$cagr = compute.cagr(bt$equity)
  bt
}

spl <- function(s, delim = ','){
  unlist(strsplit(s,delim))
} 


compute.cagr <- function(equity, nyears = NA){
  if(is.numeric(nyears))
    as.double( xts::last(equity,1)^(1/nyears) - 1 )
  else
    as.double( xts::last(equity,1)^(1/compute.nyears(equity)) - 1 )
}

nyears <- function (x) {
  length(endpoints(x, on = "years")) - 1
}

compute.nyears <- function(x){
  as.double(diff(as.Date(range(index.xts(x)))))/365
}

index.xts <- function(x) {
  temp = attr(x, 'index')
  class(temp) = c('POSIXct', 'POSIXt')
  type = attr(x, '.indexCLASS')[1]
  if( type == 'Date' || type == 'yearmon' || type == 'yearqtr')
    temp = as.Date(temp)
  return(temp)
}

bt.exrem <- function(weight){
  bt.apply.matrix(weight, exrem)
}

exrem <- function(x) {
  temp = c(0, ifna(ifna.prev(x),0))
  itemp = which(temp != mlag(temp))
  x[] = NA
  x[(itemp-1)] = temp[itemp]
  return(x)
}

## output ##
# out = list()
# out$Period = join( format( range(index.xts(bt$equity)), '%b%Y'), ' - ')
# out$Cagr = compute.cagr(bt$equity)
# out$Sharpe = compute.sharpe(bt$ret) / 100
# out$DVR = compute.DVR(bt) / 100
# out$Volatility = compute.risk(bt$ret)
# out$MaxDD = compute.max.drawdown(bt$equity)
# out$AvgDD = compute.avg.drawdown(bt$equity)
# if( !is.null(trade.summary) ) {
#   out$Profit.Factor = trade.summary$stats['profitfactor', 'All']
# }
# out$VaR = compute.var(bt$ret)
# out$CVaR = compute.cvar(bt$ret)
# out$Exposure = compute.exposure(bt$weight)

if( FALSE ){
  compute.cagr(mdls$PMOM$equity)
  compute.sharpe(mdls$PMOM$ret)
  compute.DVR(mdls$PMOM)
  compute.risk(mdls$PMOM$equity)
  compute.max.drawdown(mdls$PMOM$equity)
  compute.avg.drawdown(mdls$PMOM$equity)
  compute.var(mdls$PMOM$equity)
  compute.cvar(mdls$PMOM$equity)
  compute.exposure(mdls$PMOM$weight)
}

compute.sharpe <- function(x){ # x = ret
  temp = compute.annual.factor(x)
  x = as.vector(coredata(x))
  return(sqrt(temp) * mean(x)/sd(x) )
}

compute.annual.factor <- function(x) {
  possible.values = c(252,52,26,13,12,6,4,3,2,1)
  index = which.min(abs( compute.raw.annual.factor(x) - possible.values ))
  round( possible.values[index] )
}

compute.raw.annual.factor <- function(x) {
  round( nrow(x) / compute.nyears(x) )
}

compute.DVR <- function(bt){
  return( compute.sharpe(bt$ret) * compute.R2(bt$equity) )
}

compute.R2 <- function(equity){
  x = as.double(index.xts(equity))
  y = equity
  return( cor(y,x)^2 )
}

compute.risk <- function(x){
  temp = compute.annual.factor(x)
  x = as.vector(coredata(x))
  return( sqrt(temp)*sd(x) )
}

compute.max.drawdown <- function(x){
  as.double( min(compute.drawdown(x)) )
}

compute.drawdown <- function(x){
  return(x / cummax(c(1,x))[-1] - 1)
}

compute.avg.drawdown <- function(x){
  drawdown = c( 0, compute.drawdown(coredata(x)), 0 )
  dstart = which( drawdown == 0 & mlag(drawdown, -1) != 0 )
  dend = which(drawdown == 0 & mlag(drawdown, 1) != 0 )
  drawdowns = apply( cbind(dstart, dend), 1, function(x) min(drawdown[ x[1]:x[2] ], na.rm=T) )
  mean(drawdowns)
}

compute.var <- function(x, probs=0.05)
{
  quantile( coredata(x), probs=probs)
}

compute.cvar <- function(x, probs=0.05)
{
  x = coredata(x)
  mean( x[ x < quantile(x, probs=probs) ] )
}

compute.exposure <- function(weight){
  sum( apply(weight, 1, function(x) sum(x != 0) ) != 0 ) / nrow(weight)
}

join <- function(v,delim = ''){
  paste(v,collapse=delim)
}

plotbt.strategy.sidebyside <- function
(
  ... ,
  perfromance.metric = spl('System,Trade,Period'),
  perfromance.fn = 'bt.detail.summary',
  return.table = FALSE,
  make.plot = TRUE
)
{
  models = variable.number.arguments( ... )
  out = list()
  for( i in 1:len(models) ) {
    out[[ names(models)[i] ]] = match.fun(perfromance.fn)(models[[ i ]])[[ perfromance.metric[1] ]]
  }
  temp = list2matrix(out, keep.names=F)
  if(make.plot) plot.table( temp, smain = perfromance.metric[1] )
  if(return.table) return(temp)
}

bt.detail.summary <- function(bt, trade.summary = NULL){
  out.all = list()
  out = list()
  out$Period = join( format( range(index.xts(bt$equity)), '%b%Y'), ' - ')
  out$CAGR = compute.cagr(bt$equity)
  out$Volatility = compute.risk(bt$ret)
  out$MaxDD = compute.max.drawdown(bt$equity)
  out$Calmar = out$CAGR / abs(out$MaxDD) 
  out$Sharpe = compute.sharpe(bt$ret) / 100
  out$DVR = compute.DVR(bt) / 100
  out$AvgDD = compute.avg.drawdown(bt$equity)
  if( !is.null(trade.summary) ) {
    out$Profit.Factor = trade.summary$stats['profitfactor', 'All']
  }
  out$VaR = compute.var(bt$ret)
  out$CVaR = compute.cvar(bt$ret)
  out$Exposure = compute.exposure(bt$weight)
  out.all$System = lapply(out, function(x) if(is.double(x)) round(100*x,2) else x)
  if( !is.null(bt$trade.summary) ) trade.summary = bt$trade.summary
  out = list()
  if( !is.null(trade.summary) ) {
    out$Win.Percent = trade.summary$stats['win.prob', 'All']
    out$Avg.Trade = trade.summary$stats['avg.pnl', 'All']
    out$Avg.Win = trade.summary$stats['win.avg.pnl', 'All']
    out$Avg.Loss = trade.summary$stats['loss.avg.pnl', 'All']
    out = lapply(out, function(x) if(is.double(x)) round(100*x,1) else x)
    out$Best.Trade = max(as.double(trade.summary$trades[, 'return']))
    out$Worst.Trade = min(as.double(trade.summary$trades[, 'return']))
    out$WinLoss.Ratio = round( -trade.summary$stats['win.avg.pnl', 'All']/trade.summary$stats['loss.avg.pnl', 'All'] , 2)
    out$Avg.Len = round(trade.summary$stats['len', 'All'],2)
    out$Num.Trades = trade.summary$stats['ntrades', 'All']
  }
  out.all$Trade = out
  out = list()
  out$Win.Percent.Day = sum(bt$ret > 0, na.rm = T) / len(bt$ret)
  out$Best.Day = bt$best
  out$Worst.Day = bt$worst
  month.ends = endpoints(bt$equity, 'months')
  mret = ROC(bt$equity[month.ends,], type = 'discrete')
  out$Win.Percent.Month = sum(mret > 0, na.rm = T) / len(mret)
  out$Best.Month = max(mret, na.rm = T)
  out$Worst.Month = min(mret, na.rm = T)
  year.ends = endpoints(bt$equity, 'years')
  mret = ROC(bt$equity[year.ends,], type = 'discrete')
  out$Win.Percent.Year = sum(mret > 0, na.rm = T) / len(mret)
  out$Best.Year = max(mret, na.rm = T)
  out$Worst.Year = min(mret, na.rm = T)
  out.all$Period = lapply(out, function(x) if(is.double(x)) round(100*x,1) else x)
  return(out.all)
}

variable.number.arguments <- function(...) {
  out = lst(...)
  if( is.list(out[[1]]) && is.list(out[[1]][[1]]) )
    out[[1]]
  else
    out
}

list2matrix <- function(ilist, keep.names = TRUE){
  if ( is.list( ilist[[1]] ) ) {
    inc = 1
    if( keep.names ) inc = 2
    out = matrix('', nr = max(unlist(lapply(ilist, len))), nc = inc * len(ilist) )
    colnames(out) = rep('', inc * len(ilist))
    for( i in 1:len(ilist) ) {
      nr = len(ilist[[i]])
      colnames(out)[inc * i] = names(ilist)[i]
      if(nr > 0){
        if( keep.names ) {
          out[1:nr,(2*i-1)] = names(ilist[[i]])
        } else {
          rownames(out) = names(ilist[[i]])
        }
        out[1:nr,inc*i] = unlist(ilist[[i]])
      }
    }
    return(out)
  } else {
    return( as.matrix(unlist(ilist)) )
  }
}

count <- function(x, side = 2){
  if( is.null(dim(x)) ) {
    sum( !is.na(x) )
  } else {
    apply(!is.na(x), side, sum)
  }
}

create.ia <- function(hist.returns, index=1:ncol(hist.returns), nperiod=nrow(hist.returns)){
  ia = list()
  ia$hist.returns = hist.returns
  ia$nperiod = nperiod
  ia$index = index
  ia$n = ncol(ia$hist.returns)
  ia$symbols = colnames(ia$hist.returns)
  ia$risk = apply(ia$hist.returns, 2, sd, na.rm = T)
  ia$correlation = cor(ia$hist.returns, use='complete.obs',method='pearson')
  ia$cov = ia$correlation * (ia$risk %*% t(ia$risk))
  ia$expected.return = apply(ia$hist.returns, 2, mean, na.rm = T)
  return(ia)
}

new.constraints <- function(n, A = NULL, b = NULL, type = c('=', '>=', '<='), lb = NA, ub = NA ){
  meq = 0
  if ( is.null(A) || is.na(A) || is.null(b) || is.na(b) ) {
    A = matrix(0, n, 0)
    b = c()
  } else {
    if ( is.null(dim(A)) ) dim(A) = c(len(A), 1)
    if ( type[1] == '=' ) meq = len(b)
    if ( type[1] == '<=' ) {
      A = -A
      b = -b
    }
  }
  if ( is.null(lb) || is.na(lb) ) lb = rep(NA, n)
  if ( len(lb) != n ) lb = rep(lb[1], n)
  if ( is.null(ub) || is.na(ub) ) ub = rep(NA, n)
  if ( len(ub) != n ) ub = rep(ub[1], n)
  return( list(n = n, A = A, b = b, meq = meq, lb = lb, ub = ub) )
}

add.constraints <- function(A, b, type = c('=', '>=', '<='), constraints){
  if(is.null(constraints)) constraints = new.constraints(n = nrow(A))
  if(is.null(dim(A))) A = matrix(A)
  if(len(b) == 1) b = rep(b, ncol(A))
  if ( type[1] == '=' ) {
    constraints$A = cbind( A, constraints$A )
    constraints$b = c( b, constraints$b )
    constraints$meq = constraints$meq + len(b)
  }
  if ( type[1] == '>=' ) {
    constraints$A = cbind( constraints$A, A )
    constraints$b = c( constraints$b, b )
  }
  if ( type[1] == '<=' ) {
    constraints$A = cbind( constraints$A, -A )
    constraints$b = c( constraints$b, -b )
  }
  return( constraints )
}

min.risk.portfolio <- function(ia, constraints){
  x = NA
  binary.vec = 0
  if(!is.null(constraints$binary.index)) binary.vec = constraints$binary.index
  if(is.null(ia$cov.temp)) ia$cov.temp = ia$cov
  sol = try(solve.QP.bounds(Dmat = ia$cov.temp, dvec = rep(0, nrow(ia$cov.temp)) ,
                            Amat=constraints$A, bvec=constraints$b, constraints$meq,
                            lb = constraints$lb, ub = constraints$ub, binary.vec = binary.vec),TRUE)
  if(!inherits(sol, 'try-error')) {
    if(binary.vec[1] != 0) cat(sol$counter,'QP calls made to solve problem with', len(constraints$binary.index), 'binary variables using Branch&Bound', '\n')
    x = sol$solution;
  }
  return( x )
}

solve.QP.bounds <- function(Dmat, dvec, Amat, bvec, meq=0, factorized=FALSE, binary.vec = 0, lb = -Inf, ub = +Inf){
  Amat1 = Amat
  bvec1 = bvec
  n = len(dvec)
  if( len(lb) == 1 ) lb = rep(lb, n)
  if( len(ub) == 1 ) ub = rep(ub, n)
  lb = ifna(lb, -Inf)
  ub = ifna(ub, +Inf)
  index = which( ub < +Inf )
  if( len(index) > 0 ) {
    bvec = c(bvec, -ub[index])
    Amat = cbind(Amat, -diag(n)[, index])
  }
  index = which( lb > -Inf )
  if( len(index) > 0 ) {
    bvec = c(bvec, lb[index])
    Amat = cbind(Amat, diag(n)[, index])
  }
  if ( binary.vec[1] == 0 ) {
    qp.data.final = solve.QP.remove.equality.constraints(Dmat, dvec, Amat, bvec, meq)
    Dmat = qp.data.final$Dmat
    dvec = qp.data.final$dvec
    Amat = qp.data.final$Amat
    bvec = qp.data.final$bvec
    meq = qp.data.final$meq
    sol = try(solve.QP(Dmat, dvec, Amat, bvec, meq, factorized),TRUE)
    if(inherits(sol, 'try-error')) {
      ok = F
      sol = list()
    } else {
      tol = 1e-3
      ok = T
      check = sol$solution %*% Amat - bvec
      if(meq > 0) ok = ok & all(abs(check[1:meq]) <= tol)
      ok = ok & all(check[-c(1:meq)] > -tol)
    }
    if(!ok) {
      require(kernlab)
      index.constant.variables = which(!is.na(qp.data.final$solution))
      if( len(index.constant.variables) > 0 ) {
        Amat1 = Amat[,1:ncol(Amat1)]
        bvec1 = bvec[1:ncol(Amat1)]
        lb = lb[-index.constant.variables]
        ub = ub[-index.constant.variables]
      }
      sv = ipop(c = matrix(-dvec), H = Dmat, A = t(Amat1),
                b = bvec1, l = ifna(lb,-100), u = ifna(ub,100),
                r = c(rep(0,meq), rep(100, len(bvec1) - meq))
      )
      sol$solution = primal(sv)
    }
    x = qp.data.final$solution
    x[qp.data.final$var.index] = sol$solution
    sol$solution = x
  } else {
    qp_data = qp_new(binary.vec, Dmat = Dmat, dvec = dvec,
                     Amat=Amat, bvec=bvec, meq=meq, factorized=factorized)
    sol = binary_branch_bound(binary.vec, qp_data, qp_solve,
                              control = bbb_control(silent=T, branchvar='max', searchdir='best' ))
    qp_delete(qp_data)
    sol$value = sol$fmin
    sol$solution = sol$xmin
  }
  return(sol)
}

qp_delete <- function(qp_data){
  rm(list = ls(qp_data,all=TRUE), envir = qp_data)
}

qp_new <- function(index_binvar, Dmat, dvec, Amat, bvec, meq = 0, factorized = FALSE){
  nbinvar = length(index_binvar)
  nx = nrow(Dmat)
  nbvec = length(bvec)
  Amat = cbind( Amat, diag(nx)[,index_binvar], -diag(nx)[,index_binvar] )
  bvec = c(bvec, rep(0,nx)[index_binvar], rep(1,nx)[index_binvar] )
  lb_bin_index = (1:nbinvar) + nbvec
  ub_bin_index = (1:nbinvar) + nbvec + nbinvar
  qp_data = new.env()
  qp_data$Dmat = Dmat
  qp_data$dvec = dvec
  qp_data$Amat = Amat
  qp_data$bvec = bvec
  qp_data$meq = meq
  qp_data$factorized = factorized
  qp_data$x0 = rep(0,nx)
  qp_data$lb_bin_index = lb_bin_index
  qp_data$ub_bin_index = ub_bin_index
  qp_data$lb = bvec[lb_bin_index]
  qp_data$ub = bvec[ub_bin_index]
  return(qp_data)
}

binary_branch_bound <- function(index_binvar, bbb_data, bbb_solve, control = bbb_control()){
  fbest = Inf
  xbest = 0 * bbb_data$x0
  counter = 0
  nbinvar = length(index_binvar)
  flag = 7
  stack = new.env()
  stack$data = list()
  stack$cost = c()
  stack$pointer = c()
  stack$data[[1]] = list(lb = bbb_data$lb,
                         ub = bbb_data$ub,
                         var = 1:nbinvar,
                         path = rep(0,nbinvar),
                         level = 0,
                         fval = Inf)
  stack$cost = 0
  stack$pointer = 1
  control$proborder.selected = control$proborder
  if(F) {
    lb = bbb_data$lb
    ub = bbb_data$ub
    for( i in 0:1 ) {
      lb[] = i
      ub[] = i
      sol = match.fun(bbb_solve)(bbb_data, lb, ub)
      if( sol$ok ) {
        x = sol$x
        fval = sol$fval
        xi = x[index_binvar]
        if ( max(abs( round(xi,0) - xi )) < control$bineps ) {
          fbest = fval
          xbest = x
          flag = 1
          if( !control$silent ) cat('FOUND SOLUTION =', fbest, '\n');
        }
      }
    }
  }
  while ( length(stack$data) > 0 ) {
    subprob = bbb_pop(stack)
    if( !control$silent ) {
      cat('-----------------------------------------------------', '\n')
      if( max(subprob$path) > 0 ) {
        temp.index = order(-subprob$path)[1 : sum(subprob$path > 0)]
        cat('\t',
            paste('b', temp.index, ' = ', subprob$lb[temp.index],sep='')
            , '\n')
      } else {
        cat(counter, '\t', 'FIRST NODE', '\n')
      }
      cat(counter, '\t', subprob$lb, '\t', subprob$var, '\t', subprob$fval, '\t', fbest, '\n')
      cat('\t', subprob$ub, '\n')
      cat('stack size =', len(stack$pointer), '\n')
    }
    if( is.finite( subprob$fval ) & is.finite( fbest ) & fbest <= subprob$fval ) {
      if( !control$silent ) cat('SKIP this problem because a solution with lower FVAL already found\n')
    } else {
      counter = counter + 1
      sol = match.fun(bbb_solve)(bbb_data, subprob$lb, subprob$ub)
      if( !sol$ok ) {
        if( !control$silent ) cat('NO SOLUTION EXISTS\n\n');
      } else {
        x = sol$x
        fval = sol$fval
        if( !control$silent ) {
          cat('SOLUTION OK', '\t', sol$fval, '\n')
          cat('\t', round(x[index_binvar[subprob$var]],3), '\n\n')
        }
        if ( flag !=1 ) flag=5
        if ( fval <= fbest ) {
          if ( length(subprob$var ) == 0 ) {
            fbest = fval
            xbest = x
            flag = 1
            if( !control$silent ) cat('FOUND SOLUTION =', fbest, '\n');
          } else {
            xi = x[index_binvar[subprob$var]]
            if ( max(abs( round(xi,0) - xi )) < control$bineps ) {
              fbest = fval
              xbest = x
              flag = 1
              if( !control$silent ) cat('FOUND SOLUTION =', fbest, '\n');
            } else {
              branchvar = bbb_decision(xi,control)
              probs = bbb_separate(subprob, branchvar, fval)
              p0 = probs$p0
              p1 = probs$p1
              if( !control$silent ) cat('Branch on =', subprob$var[branchvar], '\n');
              if( control$searchdir == 0 ) {
                cost=1/(subprob$level+1)
              } else if( control$searchdir == 1 ) {
                cost=subprob$level+1
              } else if( control$searchdir == 2 ) {
                cost=fval
              } else if( control$searchdir == 3 ) {
                cost=fval/(subprob$level+1)
              }
              if( control$proborder == 2 ) {
                control$proborder.selected = round(xi[branchvar],0)
              }
              if( control$proborder.selected == 0 ) {
                bbb_push(stack, p1, p0, cost)
              } else {
                bbb_push(stack, p0, p1, cost)
              }
            }
          }
        }
      }
      if( F ) {
        cat('counter =', counter, '\n')
        cat('fbest     =', fbest, '\n')
        cat('stack$pointer =', stack$pointer, '\n')
        cat('\n')
      }
    }
  }
  rm(list=ls(stack,all=TRUE), envir=stack)
  return(list(xmin = xbest, fmin = fbest, counter = counter, flag = flag))
}

solve.QP.remove.equality.constraints <- function(Dmat, dvec, Amat, bvec, meq=0){
  qp.data = list()
  qp.data$Amat = Amat
  qp.data$bvec = bvec
  qp.data$Dmat = Dmat
  qp.data$dvec = dvec
  qp.data$meq = meq
  Amat1 = t(qp.data$Amat)
  bvec1 = qp.data$bvec
  Dmat1 = qp.data$Dmat
  dvec1 = qp.data$dvec
  meq1 = qp.data$meq
  qp.data$solution = rep(NA, ncol(Amat1))
  qp.data$var.index = 1:ncol(Amat1)
  while(T) {
    one.non.zero.index = which( rowSums(Amat1!=0) == 1 )
    if( len(one.non.zero.index) == 0 ) break
    temp0 = rowSums(Amat1[one.non.zero.index,])
    temp = abs( temp0 )
    bvec1[one.non.zero.index] = bvec1[one.non.zero.index] / temp
    Amat1[one.non.zero.index,] = Amat1[one.non.zero.index,] / temp
    temp0.index = matrix(1:ncol(Amat1), nr=ncol(Amat1), nc=len(one.non.zero.index))[t(Amat1[one.non.zero.index,]!=0)]
    equality.constraints = rep(NA, ncol(Amat1))
    lb = ub = rep(NA, ncol(Amat1))
    index = temp0 > 0
    temp = order(bvec1[one.non.zero.index[index]], decreasing = FALSE)
    lb[temp0.index[index][temp]] = bvec1[one.non.zero.index[index]][temp]
    index = temp0 < 0
    temp = order(-bvec1[one.non.zero.index[index]], decreasing = TRUE)
    ub[temp0.index[index][temp]] = -bvec1[one.non.zero.index[index]][temp]
    remove.index = which(lb == ub)
    if( len(remove.index) > 0 ) {
      equality.constraints[remove.index] = lb[remove.index]
      Dmat1 = Dmat1[-remove.index, -remove.index,drop=F]
      dvec1 = dvec1[-remove.index]
      bvec1 = bvec1 - Amat1[,remove.index,drop=F] %*% equality.constraints[remove.index]
      Amat1 = Amat1[,-remove.index,drop=F]
      qp.data$solution[ qp.data$var.index[remove.index] ] = lb[remove.index]
      qp.data$var.index = which(is.na(qp.data$solution))
      if( ncol(Amat1) > 0 ) {
        remove.index = which( rowSums(Amat1!=0) == 0 & bvec1 == 0 )
        if(len(remove.index)>0) {
          bvec1 = bvec1[-remove.index]
          Amat1 = Amat1[-remove.index,,drop=F]
          if( meq1 > 0 ) meq1 = meq1 - len(intersect((1:meq1), remove.index))
        }
      } else break
    } else break
  }
  qp.data$Amat = t(Amat1)
  qp.data$bvec = bvec1
  qp.data$Dmat = Dmat1
  qp.data$dvec = dvec1
  qp.data$meq = meq1
  return(qp.data)
}