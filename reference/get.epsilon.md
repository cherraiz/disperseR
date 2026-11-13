# Calculate \\\epsilon\\ threshold for spatial clustering

Computes the threshold \\\epsilon\\ (epsilon) based on step lengths. The
threshold can be calculated using density curve intersections (requires
a behavioral state column with exactly two states), or simply by
calculating the overall mean or median of the step lengths. The
calculation can be applied globally, per individual, or per group.

## Usage

``` r
get.epsilon(
  data,
  state.col = "state",
  step.col = "step",
  mode = "global",
  method = "density",
  ids = NULL,
  group.col = NULL
)
```

## Arguments

- data:

  A data.frame or an object of class `disperseR` containing movement
  data.

- state.col:

  Character. The name of the column containing the behavioral states.
  Only used and required if `method = "density"`. Must contain exactly
  two unique states for the evaluated subset. Default is `"state"`.

- step.col:

  Character. The name of the column containing the step lengths. Default
  is `"step"`.

- mode:

  Character. The level at which to compute the threshold: `"global"`,
  `"individual"`, or `"groups"`. Default is `"global"`.

- method:

  Character. The method used to calculate the threshold: `"density"`
  (intersection of density curves for two states), `"mean"`, or
  `"median"`. Default is `"density"`.

- ids:

  Character. The name of the column containing individual identifiers.
  Required if `mode = "individual"`.

- group.col:

  Character. The name of the column containing group identifiers.
  Required if `mode = "groups"`.

## Value

A numeric value representing the threshold if `mode = "global"`, or a
named list of numeric thresholds if `mode = "individual"` or
`mode = "groups"`.

## Examples

``` r
# Intersection of the step-length densities of both states
get.epsilon(sim_states)
#> Computing a global threshold using all data combined.
#> [1] 238.4065

# One threshold per individual, using the median step length
get.epsilon(sim_states, mode = "individual", ids = "ID", method = "median")
#> Computing thresholds separately for each individual.
#> $F02
#> [1] 43.78989
#> 
#> $F04
#> [1] 36.19222
#> 
#> $F05
#> [1] 42.71265
#> 
#> $M01
#> [1] 35.1432
#> 
#> $M03
#> [1] 40.90247
#> 
```
