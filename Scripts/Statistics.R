# Generalized Additive Model library
library(ggplot2)

# Real median and 50% quantile value: 26000 for overall
# Margin of Error
# Create Margin of Error Calculation Function for PWGTP
calculate_me <- function(df, start = 1, end = 80, alpha = 0.05){
  margin_errors <- c()
  for(i in seq(1,dim(df)[1],1)){
    x_r <- as.numeric(t(df[i,start:end]))  # All PWGTP replicates
    x <- as.numeric(df[i,start-1][1])       # Original PWGTP
    n <- 80                    # PWGTP replicates will always be 80. Period.
    tscore <- qt(alpha/2, n-1, lower.tail = F)   # Find two-sided tscore 
    
    se <- sqrt(4*sum((x-x_r)^2)/n) # Use US Census Bureau's Formula of Direct SE
    
    margin_errors[i]<-tscore*se    # Calculate Margin Error
  }
  return(data.frame(margin_errors))
}

# Median Standard Deviation
median_sd <- function(x,m){
  # x = Median, x = median replications
  Median = as.numeric(m)
  x_i = as.numeric(x)
  # Apply sd
  return(sqrt(sum((x_i-Median)^2)/(80-1)))
}

# Create Margin of Error Calculation for other than PWGTP
calculate_regular_me <- function(df, start = 1, end = 80, alpha = 0.05){
  margin_errors <- c()
  for(i in seq(1,dim(df)[1],1)){
    x_r <- as.numeric(t(df[i,start:end]))  # All PWGTP replicates
    x <- as.numeric(df[i,start-1][1])       # Original PWGTP
    n <- 80                    # PWGTP replicates will always be 80. Period.
    tscore <- qt(alpha/2, n-1, lower.tail = F)   # Find two-sided tscore 
    
    se<-median_sd(x,x_r)/sqrt(80)
    
    margin_errors[i]<-tscore*se    # Calculate Margin Error
  }
  return(data.frame(margin_errors))
}

# Total margin of error in sampling that is in use for confidence interval
total_me <- function(PWGTP){
  me<-sum(PWGTP)*margin_error(PWGTP) # sampling size * margin of error
  return(round(me,0))                   # round up or down to 0
  # Proof: length(PWGTP)*mean(PWGTP) == sum(PWGTP)
}

# Confidence interval
confidence <- function(PWGTP, lower_limit = T){
  if(lower_limit == T){
    return(sum(PWGTP) - total_me(PWGTP))
  }else{
    return(sum(PWGTP) + total_me(PWGTP))
  }
}

# Obtain this for Relative Standard Error
calculate_se <- function(df, start = 1, end = 80){
  se <- c()
  for(i in seq(1,dim(df)[1],1)){
    x_r <- as.numeric(t(df[i,start:end]))  # All PWGTP replicates
    x <- as.numeric(df[i,start-1][1])       # Original PWGTP
    n <- 80                    # PWGTP replicates will always be 80. Period.
    se[i] <- sqrt(4*sum((x-x_r)^2)/n) # Use US Census Bureau's Formula of Direct SE
  }
  return(data.frame(se))
}

