# This script simulates a 10-period cost-plus pricing game between two firms under
# correlated cost shocks. Each firm's price is derived from its cost shock,
# and demand is governed by a multinomial logit model with an outside option
# and a fixed market size. When regressing Firm 1's quantity on its own price
# alone, the demand parameter estimates are biased due to price endogeneity.
# Specifically, an omitted variable bias due to omission of price2
# Controlling for the competitor's price reveals the correctly downward-sloping
# relationship. Graphics illustrate this.

library(tidyverse)

# 1. Simulation parameters and correlated cost shocks
set.seed(14)                   # for reproducibility
n_periods    <- 100            # number of periods
market_size  <- 100            # total market size (e.g. number of customers)
rho          <- 0.7            # influence of costshock1 on costshock2

# Demand model parameters
alpha       <- 0.2             # price sensitivity (common across products)
intercept1  <- 9               # baseline utility for product 1
intercept2  <- 9               # baseline utility for product 2
# (Outside option utility is normalized to 0)

# Simulate cost shocks for the two firms (correlated)
shock1 <- rnorm(n_periods)                          
shock2 <- rho * shock1 + (1 - rho) * rnorm(n_periods)


# Derive prices from costs (higher cost shock -> higher price)
base_cost <- 3
price1 <- base_cost + shock1  
price2 <- base_cost + shock2
cor(price1, price2)

# 2. Compute market shares and quantities using multinomial logit demand
data <- tibble(
  period = 1:n_periods,
  shock1 = shock1,
  shock2 = shock2,
  price1 = price1,
  price2 = price2
) %>% 
  mutate(
    # Indirect utilities for each product and outside option:
    U1 = intercept1 - alpha * price1,
    U2 = intercept2 - alpha * price2,
    U0 = 0,  # outside option utility (baseline 0)
    # Convert utilities to choice probabilities (logit formula):
    expU1 = exp(U1),
    expU2 = exp(U2),
    expU0 = exp(U0),
    share1 = expU1 / (expU1 + expU2 + expU0),
    share2 = expU2 / (expU1 + expU2 + expU0),
    Q1 = market_size * share1,
    Q2 = market_size * share2,
    Q0 = market_size * (1 - share1 - share2)
  )

# Create decile variable for Firm 2's price
data <- data %>%
  mutate(p2_decile = ntile(price2, 10))

# 3. OLS regressions for Firm 1's demand
model_naive <- lm(Q1 ~ price1, data = data)
summary(model_naive)

model_full  <- lm(Q1 ~ price1 + price2, data = data)
summary(model_full)

# 4. Prepare data for plotting regression lines
newdata <- tibble(
  price1 = seq(min(data$price1), max(data$price1), length.out = 100),
  price2 = mean(data$price2)   # used for predicting Q1 with model_full 
)
predictions <- newdata %>%
  mutate(
    Q1_pred_naive = predict(model_naive, newdata = newdata),
    Q1_pred_full  = predict(model_full,  newdata = newdata)
  ) %>%
  pivot_longer(
    cols = starts_with("Q1_pred"),
    names_to = "Model",
    values_to = "Q1_pred"
  ) %>%
  mutate(Model = recode(
    Model,
    "Q1_pred_naive" = "Naive OLS: Q1 ~ P1",
    "Q1_pred_full"  = "Full OLS: Q1 ~ P1 + P2"
  ))

# 5. Plot data points and both regression lines
ggplot(data, aes(x = Q1, y = price1)) +
  geom_point(shape = 21, size = 2, stroke = 1.1) +  
  geom_line(data = predictions, aes(x = Q1_pred, y = price1, color = Model, linetype = Model), size = 1) +
#  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dotted") +
  labs(title = "Firm 1 Demand: Naive (Q1 ~ P1) vs Full (Q1 ~ P1 + P2) Regressions",
       y = "Price of Firm 1 (P1)",
       x = "Quantity of Firm 1 (Q1)") +
  scale_color_manual(values = c("Naive OLS: Q1 ~ P1" = "red", 
                                "Full OLS: Q1 ~ P1 + P2" = "blue")) +
  scale_linetype_manual(values = c("Naive OLS: Q1 ~ P1" = "dashed", 
                                   "Full OLS: Q1 ~ P1 + P2" = "solid")) +
  scale_x_continuous(limits = c(.998*min(data$Q1), max(data$Q1) * 1.002)) +
  scale_y_continuous(limits = c(0, max(data$price1) * 1.02)) +
  theme_minimal() +
  theme(legend.position = "bottom")


# 6. Now plot the same data again, but this time color the points by p2 decile
ggplot(data, aes(x = Q1, y = price1)) +
  # color and fill by p2_decile; shape=21 supports both
  geom_point(aes(color = factor(p2_decile), fill = factor(p2_decile)),
             shape = 21, size = 2, stroke = 1.1) +
  # Draw lines in black, with linetype by model
  geom_line(data = predictions,
            aes(x = Q1_pred, y = price1, linetype = Model),
            size = 1, inherit.aes = FALSE) +
  labs(title = "Firm 1 Demand: Naive (Q1 ~ P1) vs Full (Q1 ~ P1 + P2) Regressions",
       y = "Price of Firm 1 (P1)",
       x = "Quantity of Firm 1 (Q1)",
       fill = "Decile of P2",
       color = "Decile of P2",
       linetype = "") +
  scale_x_continuous(limits = c(.998 * min(data$Q1), 1.002 * max(data$Q1))) +
  scale_y_continuous(limits = c(0, 1.02 * max(data$price1))) +
  theme_minimal() +
  theme(legend.position = "bottom")
