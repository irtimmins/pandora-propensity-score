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
library(MatchThem)
library(mice)
library(broom.mixed)

# Helper functions used in multiple scripts:

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

cat("Setup complete\n")