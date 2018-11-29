#Jason Colgrove
#Stat 223
#Homework #4

set.seed(2020)

anova_sim <- function(x, nreps = 5000){
  #Monte Carlo simulation
  p_values <- numeric()
  for (j in 1:nreps) {
    all_data <- data.frame()
    for (i in 1:length(x)){
      this_data <- cbind(x[[i]](), i)
      all_data <- rbind(all_data, this_data)
    }
    colnames(all_data) <- c("response", "treatment")
    anova_mod <- anova(aov(response ~ as.factor(treatment), data = all_data))
    p_values[j] <- anova_mod$"Pr(>F)"[1]
  }
  
  #Find the mean proportion and the confidence interval
  check_p_values <- (p_values < 0.05)
  
  p_hat <- mean(check_p_values)
  n <- length(p_values)
  alpha <- 0.05
  lower_ci <- p_hat + -1 * qnorm(1 - alpha/2) * sqrt(p_hat * (1 - p_hat) / n)
  upper_ci <- p_hat + 1 * qnorm(1 - alpha/2) * sqrt(p_hat * (1 - p_hat) / n)
  mean_and_ci <- c(lower_ci, p_hat, upper_ci)
  
  return_list <- list(mean_and_ci, p_values)
  
  return(return_list)
}

#Part A ----
data1 <- anova_sim(list(function() rnorm(10), function() rnorm(15), function() runif(20,-sqrt(3),sqrt(3))))
data1


#Part B ----
data2 <- anova_sim(list(function() rnorm(10), function() rnorm(10), function() rnorm(10)))
data2
hist(data2[[2]])


#Part C ----
#Sample size
n <- 2

shape1 <- 0.05
shape2 <- 0.2
mean <- shape1/(shape1+shape2)
sd <- sqrt((shape1*shape2)/((shape1+shape2)^2*(shape1+shape2+1)))
pop <- function() (rbeta(n, shape1 = shape1, shape2 = shape2) - mean) / sd

data3.1 <- anova_sim(list(function() rnorm(n), function() rnorm(n), 
         function() ((rbeta(n, shape1 = shape1, shape2 = shape2) - mean) / sd)))
data3.1

n <- 4
data3.2 <- anova_sim(list(function() rnorm(n), function() rnorm(n), 
                        function() ((rbeta(n, shape1 = shape1, shape2 = shape2) - mean) / sd)))
data3.2

n <- 10
data3.3 <- anova_sim(list(function() rnorm(n), function() rnorm(n), 
                        function() ((rbeta(n, shape1 = shape1, shape2 = shape2) - mean) / sd)))
data3.3
