
# === SETUP ===
if (!require("rstudioapi")) {
  install.packages("rstudioapi")
}
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

annual_temp <- read.csv("AnnualTemp.csv", sep = ";", dec = ",")
colnames(annual_temp)[1] <- "Year"

#COMPARE AVG TEMPERATURE DIFFERNCE SAMPLES
early_period <- annual_temp[annual_temp$Year >= 1907 & annual_temp$Year <= 1950, ]
mid_period   <- annual_temp[annual_temp$Year > 1950 & annual_temp$Year <= 2000, ]
recent_period <- annual_temp[annual_temp$Year > 2000, ]
ww1_period <- annual_temp[annual_temp$Year >= 1914 & annual_temp$Year <= 1918, ]
interbellum_period <- annual_temp[annual_temp$Year >= 1919 & annual_temp$Year <= 1939, ]
ww2_period <- annual_temp[annual_temp$Year >= 1940 & annual_temp$Year <= 1945, ]

entire_period <- annual_temp
post_70s_period <- annual_temp[annual_temp$Year >= 1970, ]

# Function to calculate means for each location
calculate_means <- function(data) {
  means <- c(
    sum(data$De.Bilt) / nrow(data),
    sum(data$Eelde) / nrow(data),
    sum(data$Maastricht) / nrow(data)
  )
  return(means)
}

# Calculate means for each period
mean_early <- calculate_means(early_period)
mean_mid <- calculate_means(mid_period)
mean_recent <- calculate_means(recent_period)
mean_ww1 <- calculate_means(ww1_period)
mean_interbellum <- calculate_means(interbellum_period)
mean_ww2 <- calculate_means(ww2_period)
mean_entire <- calculate_means(entire_period)
mean_post_70s <- calculate_means(post_70s_period)

# Function to compute confidence interval
manual_CI <- function(data) {
  mean_val <- sum(data) / length(data)
  sd_val <- sqrt(sum((data - mean_val)^2) / (length(data) - 1))
  n <- length(data)
  t_critical <- qt(0.975, df = n - 1)
  margin_error <- t_critical * (sd_val / sqrt(n))
  return(c(mean_val - margin_error, mean_val + margin_error))
}


# Calculate confidence intervals for each period and location
CI_early_DeBilt <- manual_CI(early_period$De.Bilt)
CI_mid_DeBilt <- manual_CI(mid_period$De.Bilt)
CI_recent_DeBilt <- manual_CI(recent_period$De.Bilt)
CI_ww1_DeBilt <- manual_CI(ww1_period$De.Bilt)
CI_interbellum_DeBilt <- manual_CI(interbellum_period$De.Bilt)
CI_ww2_DeBilt <- manual_CI(ww2_period$De.Bilt)
CI_entire_DeBilt <- manual_CI(entire_period$De.Bilt)
CI_post_70s_DeBilt <- manual_CI(post_70s_period$De.Bilt)

CI_early_Eelde <- manual_CI(early_period$Eelde)
CI_mid_Eelde <- manual_CI(mid_period$Eelde)
CI_recent_Eelde <- manual_CI(recent_period$Eelde)
CI_ww1_Eelde <- manual_CI(ww1_period$Eelde)
CI_interbellum_Eelde <- manual_CI(interbellum_period$Eelde)
CI_ww2_Eelde <- manual_CI(ww2_period$Eelde)
CI_entire_Eelde <- manual_CI(entire_period$Eelde)
CI_post_70s_Eelde <- manual_CI(post_70s_period$Eelde)

CI_early_Maastricht <- manual_CI(early_period$Maastricht)
CI_mid_Maastricht <- manual_CI(mid_period$Maastricht)
CI_recent_Maastricht <- manual_CI(recent_period$Maastricht)
CI_ww1_Maastricht <- manual_CI(ww1_period$Maastricht)
CI_interbellum_Maastricht <- manual_CI(interbellum_period$Maastricht)
CI_ww2_Maastricht <- manual_CI(ww2_period$Maastricht)
CI_entire_Maastricht <- manual_CI(entire_period$Maastricht)
CI_post_70s_Maastricht <- manual_CI(post_70s_period$Maastricht)


# Print results
cat("Average Temperatures Across Periods:\n")
cat("Period: 1907-1950 | De Bilt:", mean_early[1], "| Eelde:", mean_early[2], "| Maastricht:", mean_early[3], "\n")
cat("Period: 1951-2000 | De Bilt:", mean_mid[1], "| Eelde:", mean_mid[2], "| Maastricht:", mean_mid[3], "\n")
cat("Period: 2001-Present | De Bilt:", mean_recent[1], "| Eelde:", mean_recent[2], "| Maastricht:", mean_recent[3], "\n")
cat("WWI (1914-1918) | De Bilt:", mean_ww1[1], "| Eelde:", mean_ww1[2], "| Maastricht:", mean_ww1[3], "\n")
cat("Interbellum (1919-1939) | De Bilt:", mean_interbellum[1], "| Eelde:", mean_interbellum[2], "| Maastricht:", mean_interbellum[3], "\n")
cat("WWII (1940-1945) | De Bilt:", mean_ww2[1], "| Eelde:", mean_ww2[2], "| Maastricht:", mean_ww2[3], "\n")
cat("Entire Period | De Bilt:", mean_entire[1], "| Eelde:", mean_entire[2], "| Maastricht:", mean_entire[3], "\n")
cat("Post 1970s | De Bilt:", mean_post_70s[1], "| Eelde:", mean_post_70s[2], "| Maastricht:", mean_post_70s[3], "\n")

cat("\n95% Confidence Intervals:\n")
cat("De Bilt (1907-1950):", CI_early_DeBilt, "\n")
cat("De Bilt (1951-2000):", CI_mid_DeBilt, "\n")
cat("De Bilt (2001-Present):", CI_recent_DeBilt, "\n")
cat("De Bilt (WWI):", CI_ww1_DeBilt, "\n")
cat("De Bilt (Interbellum):", CI_interbellum_DeBilt, "\n")
cat("De Bilt (WWII):", CI_ww2_DeBilt, "\n")
cat("De Bilt (Entire):", CI_entire_DeBilt, "\n")
cat("De Bilt (Post 1970s):", CI_post_70s_DeBilt, "\n")

cat("Eelde (1907-1950):", CI_early_Eelde, "\n")
cat("Eelde (1951-2000):", CI_mid_Eelde, "\n")
cat("Eelde (2001-Present):", CI_recent_Eelde, "\n")
cat("Eelde (WWI):", CI_ww1_Eelde, "\n")
cat("Eelde (Interbellum):", CI_interbellum_Eelde, "\n")
cat("Eelde (WWII):", CI_ww2_Eelde, "\n")
cat("Eelde (Entire):", CI_entire_Eelde, "\n")
cat("Eelde (Post 1970s):", CI_post_70s_Eelde, "\n")

cat("Maastricht (1907-1950):", CI_early_Maastricht, "\n")
cat("Maastricht (1951-2000):", CI_mid_Maastricht, "\n")
cat("Maastricht (2001-Present):", CI_recent_Maastricht, "\n")
cat("Maastricht (WWI):", CI_ww1_Maastricht, "\n")
cat("Maastricht (Interbellum):", CI_interbellum_Maastricht, "\n")
cat("Maastricht (WWII):", CI_ww2_Maastricht, "\n")
cat("Maastricht (Entire):", CI_entire_Maastricht, "\n")
cat("Maastricht (Post 1970s):", CI_post_70s_Maastricht, "\n")


# Function to compute t-test for two samples
manual_t_test <- function(data1, data2) {
  n1 <- length(data1)
  n2 <- length(data2)
  
  mean1 <- sum(data1) / n1
  mean2 <- sum(data2) / n2
  
  var1 <- sum((data1 - mean1)^2) / (n1 - 1)
  var2 <- sum((data2 - mean2)^2) / (n2 - 1)
  
  pooled_var <- (((n1 - 1) * var1) + ((n2 - 1) * var2)) / (n1 + n2 - 2)
  se <- sqrt(pooled_var * (1 / n1 + 1 / n2))
  
  t_stat <- (mean1 - mean2) / se
  df <- n1 + n2 - 2
  p_value <- 2 * (1 - pt(abs(t_stat), df))
  
  return(c(t_stat, p_value))
}

# Compute t-tests for early_mid & mid_recent
t_DeBilt_early_mid <- manual_t_test(early_period$De.Bilt, mid_period$De.Bilt)
t_DeBilt_mid_recent <- manual_t_test(mid_period$De.Bilt, recent_period$De.Bilt)

t_Eelde_early_mid <- manual_t_test(early_period$Eelde, mid_period$Eelde)
t_Eelde_mid_recent <- manual_t_test(mid_period$Eelde, recent_period$Eelde)

t_Maastricht_early_mid <- manual_t_test(early_period$Maastricht, mid_period$Maastricht)
t_Maastricht_mid_recent <- manual_t_test(mid_period$Maastricht, recent_period$Maastricht)

# Print results
cat("De Bilt: t-statistic (1907-1950 vs 1951-2000):", t_DeBilt_early_mid[1], "p-value:", t_DeBilt_early_mid[2], "\n")
cat("De Bilt: t-statistic (1951-2000 vs 2001-Present):", t_DeBilt_mid_recent[1], "p-value:", t_DeBilt_mid_recent[2], "\n")

cat("Eelde: t-statistic (1907-1950 vs 1951-2000):", t_Eelde_early_mid[1], "p-value:", t_Eelde_early_mid[2], "\n")
cat("Eelde: t-statistic (1951-2000 vs 2001-Present):", t_Eelde_mid_recent[1], "p-value:", t_Eelde_mid_recent[2], "\n")

cat("Maastricht: t-statistic (1907-1950 vs 1951-2000):", t_Maastricht_early_mid[1], "p-value:", t_Maastricht_early_mid[2], "\n")
cat("Maastricht: t-statistic (1951-2000 vs 2001-Present):", t_Maastricht_mid_recent[1], "p-value:", t_Maastricht_mid_recent[2], "\n")


if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

periods <- c("1907–1950", "1951–2000", "2001–Present")
cities <- c("De Bilt", "Eelde", "Maastricht")

means <- data.frame(
  Period = rep(periods, each = 3),
  City = rep(cities, times = 3),
  Mean = c(
    9.074252, 8.407453, 9.20789,      
    9.546057, 8.790672, 9.660078,     
    10.86229, 10.13592, 11.01953      
  ),
  CI_low = c(
    8.905651, 8.201185, 9.029772,
    9.331068, 8.580148, 9.445075,
    10.58964, 9.852672, 10.72964
  ),
  CI_high = c(
    9.242854, 8.613722, 9.386008,
    9.761045, 9.001196, 9.875081,
    11.13494, 10.41916, 11.30942
  )
)

# Compute error bars
means$CI_lower <- means$Mean - means$CI_low
means$CI_upper <- means$CI_high - means$Mean

city_colors <- c("De Bilt" = "#E45756", "Eelde" = "#4C78A8", "Maastricht" = "#72B7B2")

ggplot(means, aes(x = Period, y = Mean, fill = City)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = Mean - CI_lower, ymax = Mean + CI_upper),
    width = 0.2,
    position = position_dodge(width = 0.8),
    color = "black"
  ) +
  scale_fill_manual(values = city_colors) +
  labs(
    title = "Mean Annual Temperatures by Period with 95% Confidence Intervals",
    subtitle = "For De Bilt, Eelde, and Maastricht (1907–2024)",
    x = "Time Period",
    y = "Temperature (°C)",
    fill = "City"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )


#Historical Focus (WW1, Interbellum, WW2)
# De Bilt: 
t_DeBilt_ww1_interbellum <- manual_t_test(ww1_period$De.Bilt, interbellum_period$De.Bilt)
cat("De Bilt: t-statistic (WWI vs Interbellum):", t_DeBilt_ww1_interbellum[1], "p-value:", t_DeBilt_ww1_interbellum[2], "\n")

t_DeBilt_ww1_ww2 <- manual_t_test(ww1_period$De.Bilt, ww2_period$De.Bilt)
cat("De Bilt: t-statistic (WWI vs WWII):", t_DeBilt_ww1_ww2[1], "p-value:", t_DeBilt_ww1_ww2[2], "\n")

t_DeBilt_interbellum_ww2 <- manual_t_test(interbellum_period$De.Bilt, ww2_period$De.Bilt)
cat("De Bilt: t-statistic (Interbellum vs WWII):", t_DeBilt_interbellum_ww2[1], "p-value:", t_DeBilt_interbellum_ww2[2], "\n")

# Eelde
t_Eelde_ww1_interbellum <- manual_t_test(ww1_period$Eelde, interbellum_period$Eelde)
cat("Eelde: t-statistic (WWI vs Interbellum):", t_Eelde_ww1_interbellum[1], "p-value:", t_Eelde_ww1_interbellum[2], "\n")

t_Eelde_ww1_ww2 <- manual_t_test(ww1_period$Eelde, ww2_period$Eelde)
cat("Eelde: t-statistic (WWI vs WWII):", t_Eelde_ww1_ww2[1], "p-value:", t_Eelde_ww1_ww2[2], "\n")

t_Eelde_interbellum_ww2 <- manual_t_test(interbellum_period$Eelde, ww2_period$Eelde)
cat("Eelde: t-statistic (Interbellum vs WWII):", t_Eelde_interbellum_ww2[1], "p-value:", t_Eelde_interbellum_ww2[2], "\n")

# Maastricht
t_Maastricht_ww1_interbellum <- manual_t_test(ww1_period$Maastricht, interbellum_period$Maastricht)
cat("Maastricht: t-statistic (WWI vs Interbellum):", t_Maastricht_ww1_interbellum[1], "p-value:", t_Maastricht_ww1_interbellum[2], "\n")

t_Maastricht_ww1_ww2 <- manual_t_test(ww1_period$Maastricht, ww2_period$Maastricht)
cat("Maastricht: t-statistic (WWI vs WWII):", t_Maastricht_ww1_ww2[1], "p-value:", t_Maastricht_ww1_ww2[2], "\n")

t_Maastricht_interbellum_ww2 <- manual_t_test(interbellum_period$Maastricht, ww2_period$Maastricht)
cat("Maastricht: t-statistic (Interbellum vs WWII):", t_Maastricht_interbellum_ww2[1], "p-value:", t_Maastricht_interbellum_ww2[2], "\n")


#------------------
#LINEAR UPWARD TREND

annual_temp <- read.csv("AnnualTemp.csv", sep = ";", dec = ",")
colnames(annual_temp)[1] <- "Year"

annual_temp$Year <- as.numeric(annual_temp$Year)
annual_temp$De.Bilt <- as.numeric(annual_temp$De.Bilt)
annual_temp$Eelde <- as.numeric(annual_temp$Eelde)
annual_temp$Maastricht <- as.numeric(annual_temp$Maastricht)

str(annual_temp)

# Linear Regression Function
manual_linear_regression <- function(X, Y) {
  
  # Compute means
  X_bar <- sum(X) / length(X)
  Y_bar <- sum(Y) / length(Y)
  
  # Compute beta (slope)
  numerator <- sum((X - X_bar) * (Y - Y_bar))
  denominator <- sum((X - X_bar)^2)
  beta_hat <- numerator / denominator
  
  # Compute alpha (intercept)
  alpha_hat <- Y_bar - beta_hat * X_bar
  
  # Compute Predicted Y Values
  Y_hat <- alpha_hat + beta_hat * X
  
  # Compute Residuals
  residuals <- Y - Y_hat
  
  # Compute Standard Error of Beta
  n <- length(X)
  SE_beta <- sqrt(sum(residuals^2) / (n - 2)) / sqrt(denominator)
  
  # Compute t-statistic and p-value
  t_stat <- beta_hat / SE_beta
  df <- n - 2
  p_value <- 2 * (1 - pt(abs(t_stat), df))
  
  # Compute 95% Confidence Interval
  t_critical <- qt(0.975, df)
  CI_lower <- beta_hat - t_critical * SE_beta
  CI_upper <- beta_hat + t_critical * SE_beta
  
  # Print Results
  cat("\nEstimated Slope (Beta):", beta_hat, "\n")
  cat("Estimated Intercept (Alpha):", alpha_hat, "\n")
  cat("Standard Error of Beta:", SE_beta, "\n")
  cat("t-statistic:", t_stat, "\n")
  cat("p-value:", p_value, "\n")
  
  if (p_value < 0.05) {
    cat("Reject H0: Significant temperature trend detected.\n")
  } else {
    cat("Fail to reject H0: No significant temperature trend.\n")
  }
  
  cat("95% Confidence Interval for Beta: [", CI_lower, ",", CI_upper, "]\n")
  
  return(list(beta_hat = beta_hat, alpha_hat = alpha_hat, p_value = p_value))
}

# Apply to Each Location
cat("\n--- De Bilt ---\n")
results_debilt <- manual_linear_regression(annual_temp$Year, annual_temp$De.Bilt)

cat("\n--- Eelde ---\n")
results_eelde <- manual_linear_regression(annual_temp$Year, annual_temp$Eelde)

cat("\n--- Maastricht ---\n")
results_maastricht <- manual_linear_regression(annual_temp$Year, annual_temp$Maastricht)


# Apply to Post-1970s Data

cat("\n--- De Bilt (Post-1970s) ---\n")
results_debilt_post70s <- manual_linear_regression(post_70s_period$Year, post_70s_period$De.Bilt)

cat("\n--- Eelde (Post-1970s) ---\n")
results_eelde_post70s <- manual_linear_regression(post_70s_period$Year, post_70s_period$Eelde)

cat("\n--- Maastricht (Post-1970s) ---\n")
results_maastricht_post70s <- manual_linear_regression(post_70s_period$Year, post_70s_period$Maastricht)

# PLOTS MAASTRICHT

plot_data_maastricht_full <- data.frame(Year = annual_temp$Year)
plot_data_maastricht_full$Predicted <- results_maastricht$alpha_hat + results_maastricht$beta_hat * plot_data_maastricht_full$Year

plot_data_maastricht_post70s <- data.frame(Year = post_70s_period$Year)
plot_data_maastricht_post70s$Predicted <- results_maastricht_post70s$alpha_hat + results_maastricht_post70s$beta_hat * plot_data_maastricht_post70s$Year

if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

plot_maastricht <- ggplot() +
  geom_point(data = annual_temp, aes(x = Year, y = Maastricht), color = "blue", alpha = 0.6) +
  geom_line(data = plot_data_maastricht_full, aes(x = Year, y = Predicted), color = "red", linewidth = 1) +
  geom_point(data = post_70s_period, aes(x = Year, y = Maastricht), color = "green", shape = 1) + 
  geom_line(data = plot_data_maastricht_post70s, aes(x = Year, y = Predicted), color = "green", linewidth = 1.2) +
  
  labs(
    title = "Linear Regression Lines for Maastricht",
    x = "Year",
    y = "Temperature (°C)",
    caption = "Red line: 1907-Present, Green line: 1970-Present"
  ) +
  theme_bw()

print(plot_maastricht)


# ----------------------------------------
# Manual Bootstrap for Confidence Interval

set.seed(42)
B <- 1000 

manual_bootstrap_CI <- function(X, Y) {
  beta_boot <- numeric(B)
  n <- length(X)
  
  for (b in 1:B) {
    sample_indices <- sample(1:n, size = n, replace = TRUE)
    X_sample <- X[sample_indices]
    Y_sample <- Y[sample_indices]
    
    # Compute means
    X_bar_boot <- sum(X_sample) / length(X_sample)
    Y_bar_boot <- sum(Y_sample) / length(Y_sample)
    
    # Compute beta 
    numerator <- sum((X_sample - X_bar_boot) * (Y_sample - Y_bar_boot))
    denominator <- sum((X_sample - X_bar_boot)^2)
    beta_boot[b] <- numerator / denominator
  }
  
  # Compute bootstrap confidence interval + Mean of beta boot
  CI_boot <- quantile(beta_boot, c(0.025, 0.975))
  mean_beta <- mean(beta_boot)
  cat("Bootstrap estimate (mean of beta_boot):", mean_beta, "\n")
  cat("Bootstrap 95% Confidence Interval for Beta: [", CI_boot[1], ",", CI_boot[2], "]\n")
}

# Apply Bootstrap to Each Location
cat("\n--- Bootstrap for De Bilt ---\n")
manual_bootstrap_CI(annual_temp$Year, annual_temp$De.Bilt)

cat("\n--- Bootstrap for Eelde ---\n")
manual_bootstrap_CI(annual_temp$Year, annual_temp$Eelde)

cat("\n--- Bootstrap for Maastricht ---\n")
manual_bootstrap_CI(annual_temp$Year, annual_temp$Maastricht)

#----------
#TREND_1970
annual_temp <- read.csv("AnnualTemp.csv", sep = ";", dec = ",")
colnames(annual_temp)[1] <- "Year"

#FULL DATASET REGRESSION: De Bilt
X <- annual_temp$Year
Y_debilt <- annual_temp$De.Bilt
X_bar <- mean(X)
Y_bar <- mean(Y_debilt)

numerator <- sum((X - X_bar) * (Y_debilt - Y_bar))
denominator <- sum((X - X_bar)^2)
beta_hat_full <- numerator / denominator
alpha_hat_full <- Y_bar - beta_hat_full * X_bar

Y_hat_full <- alpha_hat_full + beta_hat_full * X
residuals_full <- Y_debilt - Y_hat_full
n <- length(X)
SE_beta_full <- sqrt(sum(residuals_full^2) / (n - 2)) / sqrt(denominator)
t_stat_full <- beta_hat_full / SE_beta_full
p_value_full <- 2 * (1 - pt(abs(t_stat_full), df = n - 2))

# POST-1970 DATA
temp_1970 <- annual_temp[annual_temp$Year >= 1970, ]
X_1970 <- temp_1970$Year
X_bar_1970 <- mean(X_1970)
n_1970 <- length(X_1970)
df_1970 <- n_1970 - 2
B <- 1000 

# ANALYSIS FUNCTION
manual_regression <- function(Y, label) {
  Y_bar <- mean(Y)
  num <- sum((X_1970 - X_bar_1970) * (Y - Y_bar))
  denom <- sum((X_1970 - X_bar_1970)^2)
  beta <- num / denom
  alpha <- Y_bar - beta * X_bar_1970
  
  Y_hat <- alpha + beta * X_1970
  residuals <- Y - Y_hat
  SE_beta <- sqrt(sum(residuals^2) / df_1970) / sqrt(denom)
  t_stat <- beta / SE_beta
  p_value <- 2 * (1 - pt(abs(t_stat), df_1970))
  
  cat("\n---", label, "---\n")
  cat("Beta:", beta, "\nAlpha:", alpha, "\nSE:", SE_beta, "\nt:", t_stat, "\np:", p_value, "\n")
  
  return(list(alpha = alpha, beta = beta, SE = SE_beta))
}

manual_bootstrap <- function(Y, label) {
  beta_boot <- numeric(B)
  for (b in 1:B) {
    idx <- sample(1:n_1970, n_1970, replace = TRUE)
    X_b <- X_1970[idx]
    Y_b <- Y[idx]
    X_bar_b <- mean(X_b)
    Y_bar_b <- mean(Y_b)
    num <- sum((X_b - X_bar_b) * (Y_b - Y_bar_b))
    denom <- sum((X_b - X_bar_b)^2)
    beta_boot[b] <- num / denom
  }
  ci <- quantile(beta_boot, c(0.025, 0.975))
  cat("Bootstrap 95% CI for", label, ":", ci, "\n")
}

manual_CI_band <- function(X, alpha, beta, SE_beta, X_bar) {
  t_crit <- qt(0.975, df = df_1970)
  SE_Y <- SE_beta * (X - X_bar)
  upper <- alpha + beta * X + t_crit * SE_Y
  lower <- alpha + beta * X - t_crit * SE_Y
  return(list(upper = upper, lower = lower))
}

# ANALYSIS: De Bilt
Y_1970_debilt <- temp_1970$De.Bilt
res_debilt <- manual_regression(Y_1970_debilt, "De Bilt")
manual_bootstrap(Y_1970_debilt, "De Bilt")
CI_debilt <- manual_CI_band(temp_1970$Year, res_debilt$alpha, res_debilt$beta, res_debilt$SE, X_bar_1970)

# ANALYSIS: Eelde
Y_1970_eelde <- temp_1970$Eelde
res_eelde <- manual_regression(Y_1970_eelde, "Eelde")
manual_bootstrap(Y_1970_eelde, "Eelde")
CI_eelde <- manual_CI_band(temp_1970$Year, res_eelde$alpha, res_eelde$beta, res_eelde$SE, X_bar_1970)

# ANALYSIS: Maastricht
Y_1970_maastricht <- temp_1970$Maastricht
res_maastricht <- manual_regression(Y_1970_maastricht, "Maastricht")
manual_bootstrap(Y_1970_maastricht, "Maastricht")
CI_maastricht <- manual_CI_band(temp_1970$Year, res_maastricht$alpha, res_maastricht$beta, res_maastricht$SE, X_bar_1970)
