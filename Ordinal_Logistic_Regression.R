#Ordinal_Logistic_Regression

#Clearing up the environment

rm(list=ls())

#set up working directory
setwd("C:/")
getwd()

#loading the required packages
library(MASS)
library(pROC)

data <- read.csv("Ordinal_Logistic_Regression.csv", header = T, sep = ",")
View(data) 
dim(data)
str(data)

# Set seed for reproducibility
set.seed(123)

#set outcome as factor
data$outcome <- factor(data$outcome,levels = c("LOW","MEDIUM","HIGH"))

#Ordinal Logistic Regression 

m <- polr(outcome ~ variable_1+variable_2+
            variable_3+variable_4+variable_5+variable_6+variable_7+
            variable_8+variable_9+variable_10+
            variable_11+variable_12+variable_13+variable_14, data = data, Hess=TRUE) 

ctable <- coef(summary(m)) #only the coefficients
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate p-value
ctable <- cbind(ctable, "p value" = p)  # merge coefficients and p-value
ctable
ci <- confint.default(m) # calculate Confidence Interval
exp(coef(m))
cbind(exp(cbind(OR = coef(m), ci)),p)

#check for true positives
prop.table(table(data$outcome== predict(m)))

#ROC calculation

probs <- predict(m, type = "probs")
# Calculate ROC curve for each category
roc_objs <- lapply(1:3, function(i) {
  # Create binary outcome variable for the current category
  binary_outcome <- ifelse(data$outcome == levels(data$outcome)[i], 1, 0)
  
  # Obtain predicted probabilities for the binary outcome
  binary_probs <- probs[, i]
  
  # Calculate ROC curve
  roc_obj <- roc(binary_outcome, binary_probs)
  
  roc_obj
})

# Plot ROC curves for each category
plot(roc_objs[[1]], col = "red", main = "ROC Curves")
lines(roc_objs[[2]], col = "green")
lines(roc_objs[[3]], col = "blue")
legend("bottomright", legend = levels(data$outcome), col = c("red", "green", "blue"), lty = 1)

auc(roc_objs[[1]])
auc(roc_objs[[2]])
auc(roc_objs[[3]])


