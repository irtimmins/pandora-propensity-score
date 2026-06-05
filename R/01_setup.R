# =============================================
# 01 - Setup
# Packages, helper functions
# =============================================


library(haven)
library(tidyverse)
library(MatchIt)
library(cobalt)
library(sandwich)
library(lmtest)
library(survival)
library(mice)
library(tableone)
library(balancer)
library(lme4)
library(forcats)
library(openxlsx)

# Helper functions used in multiple scripts:

# -----Helper function: OR FROM CLUSTERED SE-----
# Manual CI from clustered SE avoids non-integer
# warnings from confint() with matching weights
get_or <- function(fit, cluster_var) {
  ct <- lmtest::coeftest(
    fit,
    vcov = sandwich::vcovCL(fit,
                            cluster = cluster_var,
                            type    = "HC3")
  )
  est <- ct[, 1]
  se  <- ct[, 2]
  data.frame(
    OR    = round(exp(est), 3),
    lower = round(exp(est - 1.96 * se), 3),
    upper = round(exp(est + 1.96 * se), 3),
    p     = round(ct[, 4], 3)
  )
}

# Format OR and CI to 2 decimal places
fmt_or <- function(or, lower, upper) {
  paste0(
    formatC(as.numeric(or),    format = "f", digits = 2),
    " (",
    formatC(as.numeric(lower), format = "f", digits = 2),
    " to ",
    formatC(as.numeric(upper), format = "f", digits = 2),
    ")"
  )
}


# -----Helper: RUBIN'S RULES POOLING-----
# For quasibinomial + clustered SEs which
# mice::pool does not handle
pool_rubin <- function(ests, vars, outcome_var, label) {
  m     <- length(ests)
  qbar  <- mean(ests)
  ubar  <- mean(vars)
  b     <- var(ests)
  t_var <- ubar + (1 + 1/m) * b
  se    <- sqrt(t_var)
  data.frame(
    outcome = outcome_var,
    method  = label,
    OR      = round(exp(qbar), 3),
    lower   = round(exp(qbar - 1.96 * se), 3),
    upper   = round(exp(qbar + 1.96 * se), 3),
    p       = round(2 * (1 - pnorm(abs(qbar/se))), 3)
  )
}

cat("Setup complete\n")