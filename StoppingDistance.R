library(lmtest)
library(MASS)
library(normtest)

# Read in the windmill data
stopping_distance <- read.table("/Users/mymac/Google_Drive/Statistics/Stat330/hw/hw2/StoppingDistance.txt", 
                       header = TRUE)
head(stopping_distance)


# Exploratory data analysis
scatter.smooth(stopping_distance$Speed, stopping_distance$Distance, pch = 20, 
               xlab = "Car Speed (MPH)", ylab = "Required Stopping Distance (Feet)", 
               main = "Stopping Distance Scatter Plot")

cov(stopping_distance$Speed, stopping_distance$Distance)
cor(stopping_distance$Speed, stopping_distance$Distance)


# Linear model (without any transformation)
stop_dist_lm <- lm(Distance ~ Speed, data = stopping_distance)
summary(stop_dist_lm)


# Check linear, independent, and equal variance assumptions
plot(stop_dist_lm$fitted.values, stop_dist_lm$residuals, pch = 20, ylim = c(-30, 30), 
     xlab = "Fitted Values", ylab = "Residuals", main = "Fitted Values vs. Residuals Scatter Plot")
abline(a = 0, b = 0, lwd = 2, col = "red", lty = 2)

lmtest::bptest(stop_dist_lm) # Breush-Pagan test

cd = cooks.distance(stop_dist_lm) # Check to see if there are outliers
plot(cd, type = "h")
outliers = which(cd > (4 / nrow(stopping_distance)))
stopping_distance[outliers,]


# Check normal assumption
std_res <- MASS::stdres(stop_dist_lm)

hist(std_res, xlab = "Standardized Residuals", ylab = "Frequency", 
     main = "Histogram of Standardized Residuals")

qqnorm(std_res, pch = 20)
abline(a = 0, b = 1, col = "red")

ks.test(std_res, "pnorm") # Kolmogorov-Smirnov test
normtest::jb.norm.test(std_res) # Jarque-Bera test


# Plot log data
scatter.smooth(log(stopping_distance$Speed), log(stopping_distance$Distance), pch = 20, 
               xlab = "Car Speed (Log(MPH))", ylab = "Required Stopping Distance (Log(Feet))", 
               main = "Transformed Stopping Distance Scatter Plot")


# Fit lm on log data and check assumptions
trans_lm <- lm(log(Distance) ~ log(Speed), data = stopping_distance)
summary(trans_lm) # R^2 value is now bigger

std_res <- MASS::stdres(trans_lm)

plot(trans_lm$fitted.values, trans_lm$residuals, pch = 20, 
     xlab = "Fitted Values", ylab = "Residuals", 
     main = "Fitted Values vs. Residuals Scatter Plot on Transformed Data")
abline(a = 0, b = 0, lwd = 2, col = "red", lty = 2) # Linear, independent, equal variance

hist(std_res, xlab = "Standardized Residuals", ylab = "Frequency", 
     main = "Histogram of Standardized Residuals on Transformed Data")
qqnorm(std_res, pch = 20) 
abline(a = 0, b = 1, col = "red") 

ks.test(std_res, "pnorm")
normtest::jb.norm.test(std_res) # Normal


# Assess predictive ability of model via cross validation
set.seed(1)
n_test <- 5
n_cv <- 1000
bias_log <- rep(NA, n_cv)
rpmse_log <- rep(NA, n_cv)

for(cv in 1:n_cv){
        test_obs <- sample(1:nrow(stopping_distance), n_test)
        test_stop_dist <- stopping_distance[test_obs,]
        train_stop_dist <- stopping_distance[-test_obs,]
        train_lm <- lm(log(Distance) ~ log(Speed), data = train_stop_dist)
        test_preds <- exp(predict.lm(train_lm, newdata = test_stop_dist))
        bias_log[cv] <- mean((test_preds - test_stop_dist$Distance))
        rpmse_log[cv] <- sqrt(mean((test_preds - test_stop_dist$Distance)^2))
}

mean(bias_log)
mean(rpmse_log)
range(stopping_distance$Distance)
sd(stopping_distance$Distance)


# Plot fitted lm on transformed data
plot(log(stopping_distance$Speed), log(stopping_distance$Distance), pch = 20, 
     xlab = "log(Car Speeds)", ylab = "log(Required Stopping Distances)", 
     main = "Fitted Regression Line on Transformed Stopping Distance Data")
abline(reg = trans_lm, lwd = 3, col = "green")


# Plot fitted lm on raw/untransformed data
my_model = function(x) {
        exp(trans_lm$coef[1] + trans_lm$coef[2] * log(x))
}
plot(my_model, from = 0, to = 42, lwd = 3, col = "green", xlab = "Car Speed (MPH)", 
     ylab = "Required Stopping Distance (Feet)", 
     main = "Fitted Regression Line on Original Stopping Distance Data")
points(stopping_distance$Speed, stopping_distance$Distance, pch = 20)


# Make predictions for 35 mph and 30 mph
exp(trans_lm$coef[1] + trans_lm$coef[2] * log(35))
exp(trans_lm$coef[1] + trans_lm$coef[2] * log(30))


