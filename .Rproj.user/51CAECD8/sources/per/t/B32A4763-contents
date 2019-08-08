# Developer: Brady Lange
# Date: 04/09/2019
# Description:

# Set-up workspace
graphics.off()
rm(list = ls())
setwd("C:/Users/brady/Documents/UWRF Courses/ECON 426/Research Project")

# Load libraries
library(tidyverse)
library(readxl)
library(class)
library(plm)
library(olsrr)
library(MASS)

# Load data
d <- read_excel("./Data/Fortune 100 Data.xlsx", col_names = T, skip = 1)
# Extract NAICS Industry Code from NAICS 
naics_indust <- str_extract(d$NAICS, pattern = "^[0-9][0-9]")
# Convert NAICS Industry Code's to a factor
naics_indust <- as.factor(naics_indust)
# Add NAICS Industry Code to data
d <- d %>%
    mutate(NAICS_Industry_Code = naics_indust)
# Add females proportion to males
d <- d %>%
    mutate(Female_Proportion = (Female / (Male + Female)))

# =============================================================================
# Linear Regression Models
# =============================================================================
contrasts <- C(d$NAICS_Industry_Code, helmert)
library(knitr)
library(broom)
perc_mod <- lm(Percent_Change ~ Female_Proportion + Female_CEO 
               + NAICS_Industry_Code, d)

kable(tidy(perc_mod), digits = 3, caption = "Percent Change Model", 
      format = "rst")

beta_mod <- lm(Beta ~ Female_Proportion + Female_CEO 
               + NAICS_Industry_Code, d)
eps_mod <- lm(EPS ~ Female_Proportion + Female_CEO 
              + NAICS_Industry_Code, d)

# Analyze models 
summary(perc_mod)
summary(beta_mod)
summary(eps_mod)

# Stepwise regressions
library(MASS)
stepAIC(perc_mod, direction = "both")
stepAIC(beta_mod, direction = "both")
stepAIC(eps_mod, direction = "both")

ols_step_both_p(perc_mod)
ols_step_both_p(beta_mod)
ols_step_both_p(eps_mod)

stepAIC(lm(Percent_Change ~ NAICS_Industry_Code, data = d), direction = "both")
stepAIC(lm(Beta ~ NAICS_Industry_Code, data = d), direction = "both")
stepAIC(lm(EPS ~ NAICS_Industry_Code, data = d), direction = "both")

# -----------------------------------------------------------------------------
step_per_mod <- ols_step_both_p(lm(Percent_Change ~ NAICS_Industry_Code, 
                                   data = d), details = T)
subset(step_per_mod$pvalues, step_per_mod$pvalues < 0.11)
step_beta_mod <- ols_step_both_p(lm(Beta ~ NAICS_Industry_Code, data = d), 
                                 details = T, pent = 0.11)
subset(step_beta_mod$pvalues, step_beta_mod$pvalues < 0.11)
step_eps_mod <- ols_step_both_p(lm(EPS ~ NAICS_Industry_Code, data = d), 
                                details = T, pent = 0.11)
subset(step_eps_mod$pvalues, step_eps_mod$pvalues < 0.11)
# -----------------------------------------------------------------------------

library(leaps)
m <- regsubsets(Percent_Change ~ Female_Proportion + NAICS_Industry_Code, 
                data = d, method = "exhaustive") 
summary(m)$which

# F Test - joint significance test or Wald test
# Sequentially eliminate predictor variables based upon highest P value

# =============================================================================
# Visualize
# =============================================================================
# Model residual scatter plots:
# -----------------------------------------------------------------------------
# Percent Change Model
ggplot(perc_mod, aes(x = .fitted, y = .resid)) + 
    geom_point()
# Beta Model
ggplot(beta_mod, aes(x = .fitted, y = .resid)) + 
    geom_point()
# Earnings Per Share Model
ggplot(eps_mod, aes(x = .fitted, y = .resid)) + 
    geom_point()

# Visualizing scatter plots by individual variables
# -----------------------------------------------------------------------------

corrplot::corrplot(cor(d[c(7, 10:11, 15)]), type = "lower", diag = F)

GGally::ggpairs(d[c(7:12, 15)])
ggsave("./plots/all_correlation.png", plot = last_plot(), 
       device = "png")
GGally::ggpairs(d[c(7, 10:11, 15)])
ggsave("./plots/percent_change_correlation.png", plot = last_plot(), 
       device = "png")
GGally::ggpairs(d[c(8, 10:11, 15)])
ggsave("./plots/beta_correlation.png", plot = last_plot(), 
       device = "png")
GGally::ggpairs(d[c(9, 10:11, 15)])
ggsave("./plots/eps_correlation.png", plot = last_plot(), 
       device = "png")
# Female proportion effect upon percent change in company stock
ggplot(d, aes(x = Female_Proportion, y = Percent_Change)) + 
    geom_point()
# Female proportion effect upon company Beta
ggplot(d, aes(x = Female_Proportion, y = Beta)) + 
    geom_point()
# Female proportion effect upon company EPS
ggplot(d, aes(x = Female_Proportion, y = EPS)) + 
    geom_point()
# Female as CEO effect upon percent change in company stock
ggplot(d, aes(x = Female_CEO, y = Percent_Change)) + 
    geom_point()
# Female as CEO effect upon company Beta
ggplot(d, aes(x = Female_CEO, y = Beta)) + 
    geom_point()
# Female as CEO effect upon company EPS
ggplot(d, aes(x = Female_CEO, y = EPS)) + 
    geom_point()

# Visualizing scatter plots by specific industry
# -----------------------------------------------------------------------------
# Female proportion effect upon percent change in company stock by industries
ggplot(d, aes(x = Female_Proportion, y = Percent_Change)) + 
    geom_point() + 
    facet_wrap(~NAICS_Industry_Code)
# Female proportion effect upon company Beta by industries
ggplot(d, aes(x = Female_Proportion, y = Beta)) + 
    geom_point() + 
    facet_wrap(~NAICS_Industry_Code)
# Female proportion effect upon company EPS by industries
ggplot(d, aes(x = Female_Proportion, y = EPS)) + 
    geom_point() + 
    facet_wrap(~NAICS_Industry_Code)

# =============================================================================
# Classification Analysis 
# =============================================================================
# k-Nearest Neighbor Machine Learning
# -----------------------------------------------------------------------------
# Keep the results the same each time 
set.seed(1)

# Set-up training/testing specifications
sampling_rate <- 0.80
data_size <- nrow(d)
# Train 80% of the data
num_train <- sampling_rate * data_size
# Test 20% of the data
num_test <- (1 - sampling_rate) * data_size

# Set-up training data
train_idx <- sample(1:data_size, num_train, replace = F)
train_data <- d[train_idx, c(7, 15)]
train_codes <- d$NAICS_Industry_Code[train_idx]

# Set-up testing data
test_idx <- setdiff(1:data_size, train_idx)
test_data <- d[test_idx, c(7, 15)]
test_codes <- d$NAICS_Industry_Code[test_idx]
pred_codes <- knn(train_data, test_data, train_codes, k = 3)

# Check accuracy
sum(pred_codes == test_codes) / num_test
table(test_codes)
table(pred_codes)
table(pred_codes, test_codes)

# Check correct classification rate for different values of k
accuracy_rate = numeric(20)
k_tbl <- tibble()
for (k in 1:20) 
{
    # Train out model on a new value of k (num of neighbors used)
    pred_codes <- knn(train_data, test_data, train_codes, k)
    # Number of codes were classified correctly
    correct_codes <- sum(pred_codes == test_codes)
    # Convert number of correct classifications into a %
    accuracy_rate[k] <- correct_codes / num_test
    print(str_c(k, " = ", accuracy_rate[k]))
    temp_tbl <- cbind(k = k, accuracy_rate = accuracy_rate[k])
    k_tbl <- rbind(k_tbl, temp_tbl)
}

# Retrieve best k value
max(k_tbl$accuracy_rate)
best_k <- k_tbl$accuracy_rate == max(k_tbl$accuracy_rate)
k <- k_tbl[best_k, 1]

# Revise kNN model
pred_codes <- knn(train_data, test_data, train_codes, k = k)

# Visualize kNN model
ggplot(d[test_idx, c(7, 15)], aes(x = Female_Proportion, y = Percent_Change,
       color = pred_codes)) + 
    geom_point() + 
    ggtitle("kNN Model: Company Industry Prediction") + 
    scale_x_continuous("Proportion of Female's") + 
    scale_y_continuous("Company's Stock Percent Change (3/13/18 - 3/13/19)") + 
    scale_color_discrete("NAICS")
# -- Conclusion: kNN model's poorly predict this data --