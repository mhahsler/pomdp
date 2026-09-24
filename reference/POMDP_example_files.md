# POMDP Example Files

Some POMDP example files are shipped with the package.

## Details

Currently, the following POMDP example files are available:

- `"light_maze.POMDP"`: a simple maze introduced in Littman (2009).

- `"shuttle_95.POMDP"`: Transport goods between two space stations
  (Chrisman, 1992).

- `"tiger_aaai.POMDP"`: Tiger Problem introduced in Cassandra et al
  (1994).

More files can be found at https://www.pomdp.org/examples/

## References

Anthony R. Cassandra, Leslie P Kaelbling, and Michael L. Littman (1994).
Acting Optimally in Partially Observable Stochastic Domains. *In
Proceedings of the Twelfth National Conference on Artificial
Intelligence,* pp. 1023-1028.

Lonnie Chrisman (1992), Reinforcement Learning with Perceptual Aliasing:
The *Proceedings of the AAAI Conference on Artificial Intelligence,* 10,
AAAI-92.

Michael L. Littman (2009), A tutorial on partially observable Markov
decision processes, *Journal of Mathematical Psychology,* Volume 53,
Issue 3, June 2009, Pages 119-125.
[doi:10.1016/j.jmp.2009.01.005](https://doi.org/10.1016/j.jmp.2009.01.005)

## See also

Other POMDP_examples:
[`POMDP()`](http://michael.hahsler.net/pomdp/reference/POMDP.md),
[`RussianTiger`](http://michael.hahsler.net/pomdp/reference/RussianTiger.md),
[`Tiger`](http://michael.hahsler.net/pomdp/reference/Tiger.md)

## Examples

``` r
dir(system.file("examples/", package = "pomdp"))
#> [1] "light_maze.POMDP" "shuttle_95.POMDP" "tiger_aaai.POMDP"

model <- read_POMDP(system.file("examples/light_maze.POMDP", 
  package = "pomdp"))
model
#> POMDP, list - /home/runner/work/_temp/Library/pomdp/examples/light_maze.POMDP
#>   Discount factor: 0.95
#>   Horizon: Inf epochs
#>   Size: 9 states / 4 actions / 6 obs.
#>   Start: start-rewardright, start-rewardleft
#>   Solved: FALSE
#> 
#>   List components: ‘name’, ‘states’, ‘observations’, ‘actions’,
#>     ‘start’, ‘discount’, ‘transition_prob’, ‘observation_prob’,
#>     ‘reward’, ‘problem’, ‘horizon’
```
