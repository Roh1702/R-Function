
outlier <- function(data)
{
    u <- function(vec)
    {
      UB = quantile(vec, probs = 0.75, na.rm = T) + 1.5*IQR(vec, na.rm = T)
      return(sum(vec > UB))
    }
    upper = apply(data, 2, u)
    upper = as.matrix(upper)
    colnames(upper) <- "Upper"
    
    
    l <- function(vec)
    {
      LB = quantile(vec, probs = 0.25, na.rm = T) - 1.5*IQR(vec, na.rm = T)
      return(sum(vec < LB))
    }
    lower = apply(data, 2, l)
    lower = as.matrix(lower)
    colnames(lower) <- "Lower"
    
    
    log_u <- function(vec)
    { 
      vec = log(vec)
      UB = quantile(vec, probs = 0.75, na.rm = T) + 1.5*IQR(vec, na.rm = T)
      return(sum(vec > UB))
    }
    upper_log = apply(data, 2, log_u)
    upper_log = as.matrix(upper_log)
    colnames(upper_log) <- "Upper_Log"
    
    
    log_l <- function(vec)
    {
      vec = log(vec)
      LB = quantile(vec, probs = 0.25, na.rm = T) - 1.5*IQR(vec, na.rm = T)
      return(sum(vec < LB))
    }
    lower_log = apply(data, 2, log_l)
    lower_log = as.matrix(lower_log)
    colnames(lower_log) <- "Lower_Log"
    
    out = cbind(lower, upper, lower_log, upper_log)
    return(out)
  
}