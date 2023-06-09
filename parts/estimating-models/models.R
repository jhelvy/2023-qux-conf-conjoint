# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library(logitr)
library(qs)

yogurt$brand <- factor(yogurt$brand, levels = c(
  "weight", "hiland", "yoplait", "dannon"))

# ============================================================================
# Estimate homogeneous MNL models

# Run a MNL model in the Preference Space:
mnl_pref <- logitr(
  data    = yogurt,
  outcome = 'choice',
  obsID   = 'obsID',
  pars    = c('price', 'brand')
)

summary(mnl_pref)

# Run a MNL model in the WTP Space using a multistart:
wtp_mnl_pref <- wtp(mnl_pref, scalePar = 'price')

mnl_wtp <- logitr(
  data    = yogurt,
  outcome = 'choice',
  obsID   = 'obsID',
  pars    = 'brand',
  scalePar = 'price'
)

mnl_wtp_multi <- logitr(
  data    = yogurt,
  outcome = 'choice',
  obsID   = 'obsID',
  pars    = 'brand',
  scalePar = 'price',
  numMultiStarts = 10
)


# Save results
qsave(mnl_pref, here::here('models', 'mnl_pref.qs'))
qsave(mnl_wtp, here::here('models', 'mnl_wtp.qs'))
qsave(mnl_wtp_multi, here::here('models', 'mnl_wtp_multi.qs'))

# ============================================================================
# Estimate heterogeneous MXL models

yogurt_neg_price <- yogurt
yogurt_neg_price$price <- -1*yogurt$price

mxl_pref1 <- logitr(
  data     = yogurt,
  outcome  = 'choice',
  obsID    = 'obsID',
  pars     = c('price', 'brand'),
  randPars = c(brand = 'n'),
  numMultiStarts = 10
)

mxl_pref2 <- logitr(
  data     = yogurt,
  outcome  = 'choice',
  obsID    = 'obsID',
  pars     = c('price', 'brand'),
  randPars = c(price = 'n', brand = 'n'),
  numMultiStarts = 10
)

mxl_pref3 <- logitr(
  data     = yogurt_neg_price,
  outcome  = 'choice',
  obsID    = 'obsID',
  pars     = c('price', 'brand'),
  randPars = c(price = 'ln', brand = 'n'),
  numMultiStarts = 10
)

mxl_wtp <- logitr(
  data     = yogurt,
  outcome  = 'choice',
  obsID    = 'obsID',
  pars     = 'brand',
  scalePar = 'price',
  randPars = c(brand = 'n'),
  numMultiStarts = 10
)

# Save results
qsave(mxl_pref1, here::here('models', 'mxl_pref1.qs'))
qsave(mxl_pref2, here::here('models', 'mxl_pref2.qs'))
qsave(mxl_pref3, here::here('models', 'mxl_pref3.qs'))
qsave(mxl_wtp,   here::here('models', 'mxl_wtp.qs'))
