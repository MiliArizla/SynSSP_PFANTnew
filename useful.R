#########################
# FUNCTION FIT_CONT
#########################
fit_cont <- function(x, y, nsig_up, nsig_low, degree, span, Nit){
  if(missing(nsig_up)){nsig_up = 20}
  if(missing(nsig_low)){nsig_low = 2}
  if(missing(degree)){degree = 2}
  if(missing(span)){span = 0.1}
  if(missing(Nit)){Nit = 10}
  
  x2_temp = y
  x1_temp = x
  
  for(nn in 1:Nit){
    temp <- loess(x2_temp ~ x1_temp, degree = degree, span = span)
    xx.xx_temp1 = x2_temp - predict(temp) <= nsig_up * sd(x2_temp - predict(temp)) & x2_temp - predict(temp) >= 0
    xx.xx_temp2 = abs(x2_temp - predict(temp)) <= nsig_low * sd(x2_temp - predict(temp)) & x2_temp - predict(temp) < 0
    
    xx.xx_temp = xx.xx_temp1 | xx.xx_temp2
    
    x1_temp = x1_temp[xx.xx_temp]
    x2_temp = x2_temp[xx.xx_temp]
  }
  
  temp <- loess(x2_temp ~ x1_temp, degree = degree, span = span)
  
  return(temp)
}
