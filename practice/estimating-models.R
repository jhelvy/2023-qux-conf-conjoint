# Estimate logit models with the logitr package

# Install packages
# install.packages("cbcTools")
# install.packages("logitr")

# Load libraries
library(cbcTools)
library(logitr)

# Data ----

# Get familiar with the data format required to estimate a model with logitr

# The package comes with several datasets:

# cars_china    Stated car choice observations by Chinese car buyers
# cars_us       Stated car choice observations by US car buyers
# electricity   Stated preference data for the choice of
#               electricity suppliers (from mlogit package)
# yogurt        Choice observations of yogurt purchases by 100 households

# You can also simulate a dataset using cbcTools!

data <- cbc_profiles(
    price     = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
    type      = c("Fuji", "Gala", "Honeycrisp"),
    freshness = c('Poor', 'Average', 'Excellent')
  ) |> cbc_design(
    n_resp   = 1000, # Number of respondents
    n_alts   = 3,   # Number of alternatives per question
    n_q      = 6    # Number of questions per respondent
  ) |>
  cbc_choices(
    obsID = "obsID",
    priors = list(
      price     = -0.1,
      type      = c(0.1, 0.2),
      freshness = c(0.1, 0.2)
    )
  )



# Models ----

data <- yogurt

# Multinomial logit, preference space

mnl_pref <- logitr(
  data    = yogurt,
  outcome = 'choice',
  obsID   = 'obsID',
  pars    = c('price', 'feat', 'brand')
)

summary(mnl_pref)

# Multinomial logit, WTP space

mnl_wtp <- logitr(
  data    = yogurt,
  outcome = 'choice',
  obsID   = 'obsID',
  pars    = c('feat', 'brand'),
  scalePar = 'price',
  # Since WTP space models are non-convex, run a multistart
  numMultiStarts = 10
)

# Compare WTP from both models

wtpCompare(mnl_pref, mnl_wtp, scalePar = 'price')



# Mixed logit, preference space

mxl_pref <- logitr(
  data     = yogurt,
  outcome  = 'choice',
  obsID    = 'obsID',
  panelID  = 'id',
  pars     = c('price', 'feat', 'brand'),
  randPars = c(feat = 'n', brand = 'n'),
  numMultiStarts = 10
)

summary(mxl_pref)

# Mixed logit, WTP space

mxl_wtp <- logitr(
  data       = yogurt,
  outcome    = 'choice',
  obsID      = 'obsID',
  panelID    = 'id',
  pars       = c('feat', 'brand'),
  scalePar   = 'price',
  randPars   = c(feat = 'n', brand = 'n'),
  numMultiStarts = 10
)

summary(mxl_wtp)

# Compare WTP from both models

wtpCompare(mxl_pref, mxl_wtp, scalePar = 'price')

