
mlm <-function(formula, data){return(summary(lm(formula,data)))}

mf<-function(ct){return(transform(ct,freq=n/sum(n)))}
ratio <- function(x, y){
  return((x+.0001) /(y + .0001))
}
notNA<-function(df){
  ret = apply(df, 2, function(x) length(which(!is.na(x)))/length(x))
  return(sort(ret))
}
mySumm <- function(df){
  relRows = c('min','max','median','mean','std.dev')
  return(stat.desc(df)[relRows,])
}
me <- function(df){
  relRows = c('mean')
  return(stat.desc(df)[relRows,])
}
keepNumeric <-function(x){
  nums <- sapply(x, is.numeric)
  return(x[ , nums])
}
histos<-function(m) {
  m = melt(m)
  ggplot(m,aes(x = value)) + 
    facet_wrap(~variable,scales = "free_x") + geom_density()
}
dropBoring<-function(fp){
  out <- lapply(fp, function(x) length(unique(x)))
  keep <- which(out > 1)
  return(fp[keep])
}
naRep<-function(col, val){
  col[is.na(col)] <- val
  return(col)
}
charFac<-function(ep){
  i <- sapply(ep, is.factor)
  ep[i] <- lapply(ep[i], as.character)
  return(ep)
}

ggplot(r2, aes(x=YEAR, y=meanYEAR)) + geom_smooth()
r2 = dropBoring(keepNumeric(rd))
m = melt(r2, id.vars=c('YEAR'))

# Seeing how every variable responds to year
ggplot(m, aes(x=YEAR, y=value)) + 
  geom_smooth() +
  facet_wrap(~variable, scales='free_y')
#1 Is something different about the sample between 1950 and 1965?
#2 What does tempreature look like before 1950?
#3 Is there higher variance between months as time passes? (could be difficult)
#4 Would be useful to have CO2 emissions as an explanatory variable. (USE GDP Growth as proxy)
