# Functions for analysis -----

# get the risk table
RiskSetCount <- function(timeindex, survivaltime) {
  atrisk <- NULL
  for (t in timeindex)
    atrisk <- c(atrisk, sum(survivaltime >= t))
  return(atrisk)
}

# hazard function over time extraction ----
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time,hazard,lower.ci,upper.ci))
}

# measuring time in minutes using tictoc package
toc_min <- function(tic,toc,msg="") {
  mins <- round((((toc-tic)/60)),2)
  outmsg <- paste0(mins, " minutes elapsed")
}

nice.num1<-function(x) {
  base::trimws(format(round(x,1),
                      big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}

nice.num2<-function(x) {
  base::trimws(format(round(x,2),
                      big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}

