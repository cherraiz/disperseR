# Print a disperseR object

Prints a short summary of a `disperseR` object: number of individuals
and locations, the steps of the workflow already applied, and the number
of locations in each movement phase when
[`class.mov`](https://cherraiz.github.io/disperseR/reference/class.mov.md)
has been run.

## Usage

``` r
# S3 method for class 'disperseR'
print(x, id.col = "ID", ...)
```

## Arguments

- x:

  An object of class `disperseR`.

- id.col:

  Character. Name of the column containing individual identifiers.
  Default is "ID".

- ...:

  Not used.

## Value

`x`, invisibly.

## Examples

``` r
eps <- get.epsilon(sim_states)
#> Computing a global threshold using all data combined.
arp <- get.arp(sim_states, epsilon = eps, delta.days = 2, pct.state1 = 0.2)
arp
#> <disperseR object>
#>   Individuals: 5 | Locations: 3788 
#>   Steps: ARP clusters (get.arp) 
#>   ARP clusters per individual:
#> F02 F04 F05 M01 M03 
#>   4   2   1   4   2 
class.mov(link.arp(arp, thres = 24 * 3600))
#> <disperseR object>
#>   Individuals: 5 | Locations: 3788 
#>   Steps: ARP clusters (get.arp), Linked ARPs (link.arp), Movement phases (class.mov) 
#>   ARP clusters per individual:
#> F02 F04 F05 M01 M03 
#>   4   2   1   4   2 
#>   Locations per phase:
#>     ARP Type 0 Type 1 Type 2 Type 2 or Type 1 Type 3
#> F02 629     55      0    142                0      0
#> F04 649     54    130      0                0      0
#> F05 159    161      0      0              129      0
#> M01 854     63     72    103                0     95
#> M03 364     55      0     74                0      0
```
