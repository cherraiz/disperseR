# Fit Hidden Markov Models for Movement Data Classification

Wrapper around `momentuHMM` to fit 2-state or 3-state Hidden Markov
Models (HMM) to animal movement trajectories. Supports global,
individual-level, or group-level model fitting.

## Usage

``` r
fitHMM.wrap(
  data,
  ids = NULL,
  coordNames = c("x", "y"),
  coordtype = "UTM",
  mean.step.1 = NULL,
  sd.step.1 = NULL,
  mean.step.2 = NULL,
  sd.step.2 = NULL,
  ang.con.1 = NULL,
  ang.con.2 = NULL,
  stationary = FALSE,
  mode = "global",
  group.col = NULL,
  ...
)
```

## Arguments

- data:

  A data.frame containing spatial tracking trajectories.

- ids:

  Character. Column name containing individual identifiers. Default is
  NULL.

- coordNames:

  Character vector of length 2 specifying the X and Y coordinate column
  names. Default is `c("x", "y")`.

- coordtype:

  Character. Coordinate type passed to
  [`momentuHMM::prepData`](https://rdrr.io/pkg/momentuHMM/man/prepData.html)
  (e.g., "UTM" or "LL"). Default is "UTM".

- mean.step.1:

  Numeric. Initial mean step length for state 1 (Transit).

- sd.step.1:

  Numeric. Initial standard deviation of step length for state 1
  (Transit).

- mean.step.2:

  Numeric. Initial mean step length for state 2 (Encamped).

- sd.step.2:

  Numeric. Initial standard deviation of step length for state 2
  (Encamped).

- ang.con.1:

  Numeric. Initial concentration parameter for turning angle in state 1
  (Transit).

- ang.con.2:

  Numeric. Initial concentration parameter for turning angle in state 2
  (Encamped).

- stationary:

  Logical. If `TRUE`, fits a 3-state model (including a stationary
  state) and re-maps state 3 to state 2 post-estimation. Default is
  `FALSE`.

- mode:

  Character. Fitting mode: `"individual"` (one model per ID), `"groups"`
  (one model per defined group), or `"global"` (single model for all
  data). Default is `"global"`.

- group.col:

  Character. Column name identifying groups when `mode = "groups"`.
  Default is NULL.

- ...:

  Additional arguments passed to fitHMM.

## Value

An object of class `disperseR` containing:

- data:

  A data.frame with movement parameters and Viterbi-decoded state
  assignments.

- models:

  A list of fitted `momentuHMM` model objects.

## Details

Step lengths are modeled using a gamma distribution and turning angles
using a wrapped Cauchy distribution. In 2-state models, state 1
corresponds to Transit and state 2 to Encamped. Although the 2-state
model generally performs well, in datasets lacking clear transit
movements and containing abundant stationary periods, movement
classification may be biased towards identifying stationary movements as
Encamped and all other movements as Transit. In such cases, a 3-state
model including a stationary state can improve state discrimination. The
stationary state is initialized with a step-length prior equal to one
tenth of the Encamped state (`mean.step.2` ) prior from the 2-state
model.

## Examples

``` r
# \donttest{
trk <- sim_tracks[sim_tracks$ID == "F02", c("ID", "x", "y")]
hmm <- fitHMM.wrap(trk, ids = "ID",
                   mean.step.1 = 600, sd.step.1 = 400,
                   mean.step.2 = 80, sd.step.2 = 50,
                   ang.con.1 = 0.7, ang.con.2 = 0.1)
#> Fitting one model for all individuals.
#> =======================================================================
#> Fitting HMM with 2 states and 2 data streams
#> -----------------------------------------------------------------------
#>  step ~ gamma(mean=~1, sd=~1)
#>  angle ~ wrpcauchy(concentration=~1)
#> 
#>  Transition probability matrix formula: ~1
#> 
#>  Initial distribution formula: ~1
#> =======================================================================
#> Attempting to improve fit using random perturbation. Press 'esc' to force exit from 'fitHMM'
#> 
    Attempt 1 of 5 -- current log-likelihood value: -5537.003         
    Attempt 2 of 5 -- current log-likelihood value: -5537.003         
    Attempt 3 of 5 -- current log-likelihood value: -5537.003         
    Attempt 4 of 5 -- current log-likelihood value: -5333.761         
    Attempt 5 of 5 -- current log-likelihood value: -5333.761         
#> 
#> DONE
table(hmm$data$state)
#> 
#>   1   2 
#> 371 367 
# }
```
