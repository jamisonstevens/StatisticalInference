Part2 <- function() {
  # Load Tooth Growth Data
  library(datasets)
  data("ToothGrowth")
  
  # Provide A Basic Summary of the Data and  and Perform some basic exploratory data analyses
  print(head(ToothGrowth))
  print(str(ToothGrowth))
  print(summary(ToothGrowth))
  ToothGrowth$dose <- as.factor(ToothGrowth$dose)
  print(head(ToothGrowth))
  print(str(ToothGrowth))
  print(summary(ToothGrowth))
  par(mfrow = c(1,2))
  boxplot(ToothGrowth$len ~ ToothGrowth$supp, main = "len vs. supp", xlab = "supp", ylab = "len")
  boxplot(ToothGrowth$len ~ ToothGrowth$dose, main = "len vs. dose", xlab = "dose", ylab = "len")
  
  # Compare Tooth Growth by supp and dose with confidence intervals and/or hypothesis tests
  print(t.test(len ~ supp, ToothGrowth))
  print(t.test(ToothGrowth[ToothGrowth$dose == 1.0, "len"], ToothGrowth[ToothGrowth$dose == 0.5, "len"]))
  print(t.test(ToothGrowth[ToothGrowth$dose == 2.0, "len"], ToothGrowth[ToothGrowth$dose == 0.5, "len"]))
  print(t.test(ToothGrowth[ToothGrowth$dose == 2.0, "len"], ToothGrowth[ToothGrowth$dose == 1.0, "len"]))
}