# Make conjoint survey designs using the cbcTools package

# Install packages
# install.packages("cbcTools")

# Load libraries
library(cbcTools)

# A simple conjoint experiment about apples

# Generate all possible profiles  ----

profiles <- cbc_profiles(
    price     = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
    type      = c("Fuji", "Gala", "Honeycrisp"),
    freshness = c('Poor', 'Average', 'Excellent')
)

# Make a design ----

# Make a randomized survey design

design_rand <- cbc_design(
    profiles = profiles,
    n_resp   = 1000, # Number of respondents
    n_alts   = 3,   # Number of alternatives per question
    n_q      = 6    # Number of questions per respondent
)

# Make a randomized survey design with a "no choice" option

design_rand_nochoice <- cbc_design(
    profiles  = profiles,
    n_resp    = 1000, # Number of respondents
    n_alts    = 3,   # Number of alternatives per question
    n_q       = 6,   # Number of questions per respondent
    no_choice = TRUE
)

# Make a randomized labeled survey design with each "type" appearing in
# each choice question

design_rand_labeled <- cbc_design(
    profiles  = profiles,
    n_resp    = 1000, # Number of respondents
    n_alts    = 3,   # Number of alternatives per question
    n_q       = 6,   # Number of questions per respondent
    label     = "type"
)

# Make a Bayesian D-efficient design with a prior model specified

design_deff <- cbc_design(
    profiles  = profiles,
    n_resp    = 1000, # Number of respondents
    n_alts    = 3,  # Number of alternatives per question
    n_q       = 6,  # Number of questions per respondent
    n_start   = 1,
    priors = list(
        price     = -0.1,
        type      = c(0.1, 0.2),
        freshness = c(0.1, 0.2)
    )
)

# Inspect designs ----

# Inspect balance 

cbc_balance(design_rand)
cbc_balance(design_rand_nochoice)
cbc_balance(design_rand_labeled)
cbc_balance(design_deff)

# Inspect overlap

cbc_overlap(design_rand)
cbc_overlap(design_rand_nochoice)
cbc_overlap(design_rand_labeled)
cbc_overlap(design_deff)

# Simulate choices ----

design <- design_deff

# Random choices 

data <- cbc_choices(
    design = design,
    obsID  = "obsID"
)

# Choices according to a prior

data <- cbc_choices(
    design = design,
    obsID = "obsID",
    priors = list(
        price     = -0.1,
        type      = c(0.1, 0.2),
        freshness = c(0.1, 0.2)
    )
)

# Choices according to a prior with an interaction term

data <- cbc_choices(
    design = design,
    obsID = "obsID",
    priors = list(
        price = 0.1,
        type = c(0.1, 0.2),
        freshness = c(0.1, 0.2),
        `price*type` = c(0.1, 0.5)
    )
)

# Choices according to a prior with a random normal term

data <- cbc_choices(
    design = design,
    obsID = "obsID",
    priors = list(
        price = -0.1,
        type = randN(mean = c(0.1, 0.2), sd = c(1, 2)),
        freshness = c(0.1, 0.2)
    )
)

# Power analysis ----

power <- cbc_power(
    data    = data,
    pars    = c("price", "type", "freshness"),
    outcome = "choice",
    obsID   = "obsID",
    nbreaks = 10,
    n_q     = 6
)

head(power)
tail(power)

# Visualize results

plot(power)
