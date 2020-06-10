Part1 <- function() {
  # Define Constants
  lambda <- 0.2
  numExp <- 40
  simulations <- 1000
  
  # Theoretical Mean
  theoreticalMean <- 1 / lambda
  
  # Theoretical Sample Standard Deviation and Variance
  theoreticalSamStDev <- 1 / lambda
  theoreticalSamVariance <- theoreticalSamStDev^2
  
  # Theoretical Population Standard Deviation and Variance
  theoreticalVariance <- theoreticalSamVariance / numExp
  
  # Simulations
  meanExpSims <- c()
  for (i in 1:simulations) {
    sam <- rexp(numExp, lambda)
    meanExpSims <- c(meanExpSims, mean(sam))
  }
  simDF <- data.frame(1:simulations, meanExpSims)
  colnames(simDF) <- c("Simulation_Number", "Exponential_Distribution_Mean")
  
  # Compare Theoretical and Sample Means
  sampleMean <- mean(simDF$Exponential_Distribution_Mean)
  print(paste("Sample Mean:", sampleMean))
  print(paste("Theoretical Mean:", theoreticalMean))
  
  # Compare Theoretical and Sample Variance
  sampleVariance <- var(simDF$Exponential_Distribution_Mean)
  print(paste("Sample Variance:", sampleVariance))
  print(paste("Theoretical Variance:", theoreticalVariance))
  
  # Show Distribution is Approximately Normal
  hist(simDF$Exponential_Distribution_Mean, breaks = 30, main = "Frequency of Means of Exponential Distribution Simulations", xlab = "Mean of Exponential Distribution Simulations")
}