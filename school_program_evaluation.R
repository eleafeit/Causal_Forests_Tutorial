# School program evaluation using causal forests
# Based on tutorial at https://grf-labs.github.io/grf/articles/grf_guide.html
# Elea McDonnell Feit
# 17 May 2023

# Data
data <- read.csv("bruhn2016.csv")
summary(data)
nrow(data)
head(data)

# Data munging
Y <- data$outcome
W <- data$treatment
school <- data$school
X <- data[-(1:3)]

# Around 30% have one or more missing covariates, the missingness pattern doesn't seem
# to vary systematically between the treated and controls, so we'll keep them in the analysis
# since GRF supports splitting on X's with missing values.
sum(!complete.cases(X)) / nrow(X)
t.test(W ~ !complete.cases(X))
              
# Run the causal forest
cf <- causal_forest(X, Y, W, W.hat = 0.5, clusters = school)

# Variable importance (how often var was split on)
varimp <- variable_importance(cf)
ranked.vars <- order(varimp, decreasing = TRUE)
colnames(X)[ranked.vars[1:5]]

# Plot relationship between modifiers and predictions
plot(X$financial.autonomy.index, cf$predictions)
plot(X$intention.to.save.index, cf$predictions)
plot(X$is.female, cf$predictions)

# Best linear projection
best_linear_projection(cf, X[ranked.vars[1:5]])

# Average treatment effect
ate <- average_treatment_effect(cf)
ate
