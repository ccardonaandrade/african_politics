
# Creating the 10 lags and forwards

african_politics <- african_politics %>%
  arrange(cown, year) %>%  # Ensure data is sorted by country and year
  mutate(
    coup1_lag1 = lag(coup1, 1),
    coup1_lag2 = lag(coup1, 2),
    coup1_lag3 = lag(coup1, 3),
    coup1_lag4 = lag(coup1, 4),
    coup1_lag5 = lag(coup1, 5),
    coup1_lag6 = lag(coup1, 6),
    coup1_lag7 = lag(coup1, 7),
    coup1_lag8 = lag(coup1, 8),
    coup1_lag9 = lag(coup1, 9),
    coup1_lag10 = lag(coup1, 10),
    coup1_lead1 = lead(coup1, 1),
    coup1_lead2 = lead(coup1, 2),
    coup1_lead3 = lead(coup1, 3),
    coup1_lead4 = lead(coup1, 4),
    coup1_lead5 = lead(coup1, 5),
    coup1_lead6 = lead(coup1, 6),
    coup1_lead7 = lead(coup1, 7),
    coup1_lead8 = lead(coup1, 8),
    coup1_lead9 = lead(coup1, 9),
    coup1_lead10 = lead(coup1, 10)
  )



# Run regression with polity as the dependent variable and lags/leads of coup1 as independent variables
regression <- feols(polity ~ coup1_lag1 + coup1_lag2 + coup1_lag3 + coup1_lag4 + coup1_lag5 +
                      coup1_lag6 + coup1_lag7 + coup1_lag8 + coup1_lag9 + coup1_lag10 +
                      coup1_lead1 + coup1_lead2 + coup1_lead3 + coup1_lead4 + coup1_lead5 +
                      coup1_lead6 + coup1_lead7 + coup1_lead8 + coup1_lead9 + coup1_lead10 |
                      year + cown, data = african_politics)

# Get summary of the regression
tidy(regression, conf.int=TRUE)



# Extract coefficients and confidence intervals
coef_af <- as.data.frame(coef(regression))
confint_af <- as.data.frame(confint(regression))

# Combine coefficients and confidence intervals into a single data frame
coef_data <- data.frame(
  Term = rownames(coef_af),
  Coefficient = coef_af[, 1],
  CI_Lower = confint_af[, 1],
  CI_Upper = confint_af[, 2]
)



# Select only the relevant terms: lags, leads, and the original coup1
coef_data <- coef_data %>%
  filter(Term %in% c("coup1_lag1", "coup1_lag2", "coup1_lag3", "coup1_lag4", "coup1_lag5",
                     "coup1_lag6", "coup1_lag7", "coup1_lag8", "coup1_lag9", "coup1_lag10",
                     "coup1",        # Time 0
                     "coup1_lead1", "coup1_lead2", "coup1_lead3", "coup1_lead4", "coup1_lead5",
                     "coup1_lead6", "coup1_lead7", "coup1_lead8", "coup1_lead9", "coup1_lead10"))

# Rename the terms for better plotting
coef_data$Term <- factor(coef_data$Term, levels = c("coup1_lag10", "coup1_lag9", "coup1_lag8", 
                                                    "coup1_lag7", "coup1_lag6", "coup1_lag5", 
                                                    "coup1_lag4", "coup1_lag3", "coup1_lag2", 
                                                    "coup1_lag1", "coup1",  # Time 0
                                                    "coup1_lead1", "coup1_lead2", "coup1_lead3", 
                                                    "coup1_lead4", "coup1_lead5", "coup1_lead6", 
                                                    "coup1_lead7", "coup1_lead8", "coup1_lead9", 
                                                    "coup1_lead10"))



coef_data <- coef_data %>%
  mutate(Numeric_Term = case_when(
    Term == "coup1_lag10" ~ -10,
    Term == "coup1_lag9" ~ -9,
    Term == "coup1_lag8" ~ -8,
    Term == "coup1_lag7" ~ -7,
    Term == "coup1_lag6" ~ -6,
    Term == "coup1_lag5" ~ -5,
    Term == "coup1_lag4" ~ -4,
    Term == "coup1_lag3" ~ -3,
    Term == "coup1_lag2" ~ -2,
    Term == "coup1_lag1" ~ -1,
    Term == "coup1" ~ 0,         # Time 0
    Term == "coup1_lead1" ~ 1,
    Term == "coup1_lead2" ~ 2,
    Term == "coup1_lead3" ~ 3,
    Term == "coup1_lead4" ~ 4,
    Term == "coup1_lead5" ~ 5,
    Term == "coup1_lead6" ~ 6,
    Term == "coup1_lead7" ~ 7,
    Term == "coup1_lead8" ~ 8,
    Term == "coup1_lead9" ~ 9,
    Term == "coup1_lead10" ~ 10
  ))


# Plot the coefficients with updated x-axis labels
ggplot(coef_data, aes(x = Numeric_Term, y = Coefficient)) +
  geom_point() +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +  # Confidence intervals
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +     # Horizontal line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +    # Dashed line at time 0
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +                  # Custom x-axis labels for 10 lags and leads
  labs(title = "Coefficients of Lags and Leads of coup1 with Confidence Intervals", 
       x = "Time (Lags and Leads)", 
       y = "Coefficient") +
  theme_minimal()

