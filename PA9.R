# Name: Lyle O'Dell
# Course: COP2073C - R Programming
# Assignment: Module 9 Programming Assignment
# Date: November 4, 2026

# Explore the relationship between vehicle weight and fuel efficiency.
data(mtcars)
plot(mtcars$wt, mtcars$mpg,
     main = "Fuel Efficiency vs. Vehicle Weight",
     xlab = "Vehicle Weight (1,000 pounds)",
     ylab = "Fuel Efficiency (MPG)",
     pch = 19, col = "steelblue")

# Fit the linear regression model and display its fitted line.
mpg_model <- lm(mpg ~ wt, data = mtcars)
abline(mpg_model, col = "firebrick", lwd = 2)

# Examine the slope, its p-value, and its 95% confidence interval.
model_summary <- summary(mpg_model)
slope <- coef(mpg_model)["wt"]
p_value <- model_summary$coefficients["wt", "Pr(>|t|)"]
slope_ci <- confint(mpg_model, "wt", level = 0.95)

cat(sprintf("Slope Estimate: %.6f\n", slope))
cat(sprintf("p-value: %.6g\n", p_value))
cat(sprintf("95%% Confidence Interval for Slope: %.6f %.6f\n",
            slope_ci[1], slope_ci[2]))
cat(sprintf("Each additional 1,000 pounds is associated with %.2f",
            abs(slope)), "fewer MPG on average.\n")

if (p_value < 0.05) {
  cat("The slope is statistically significant at the 0.05 level.\n")
} else {
  cat("The slope is not statistically significant at the 0.05 level.\n")
}

# Convert 6,000 pounds to the units used by mtcars and predict MPG.
vehicle_weight_lb <- 6000
new_car <- data.frame(wt = vehicle_weight_lb / 1000)
mpg_prediction <- predict(mpg_model, newdata = new_car,
                          interval = "prediction", level = 0.95)

cat("\nPredicted MPG for a 6,000-pound vehicle:\n")
print(mpg_prediction)

# Check whether this prediction extrapolates beyond the observed data.
weight_range <- range(mtcars$wt)
cat(sprintf("\nObserved weight range: %.3f to %.3f",
            weight_range[1], weight_range[2]))
cat(" thousand pounds.\n")

if (new_car$wt < weight_range[1] || new_car$wt > weight_range[2]) {
  cat("A 6,000-pound vehicle is outside the observed weight range.\n")
  cat("This is extrapolation, so the estimate is less reliable.\n")
  cat("The wide prediction interval also shows substantial uncertainty.\n")
  cat("Its negative lower bound is not physically meaningful for MPG.\n")
  cat("Use caution rather than trusting this prediction as precise.\n")
} else {
  cat("The weight is within the observed range, but predictions\n")
  cat("still have uncertainty and depend on model assumptions.\n")
}
