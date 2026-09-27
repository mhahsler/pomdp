# Package index

## POMDPs

Define, inspect, transform, simulate, and analyze partially observable
Markov decision process models.

- [`POMDP()`](http://michael.hahsler.net/pomdp/reference/POMDP.md)
  [`is_solved_POMDP()`](http://michael.hahsler.net/pomdp/reference/POMDP.md)
  [`is_timedependent_POMDP()`](http://michael.hahsler.net/pomdp/reference/POMDP.md)
  [`epoch_to_episode()`](http://michael.hahsler.net/pomdp/reference/POMDP.md)
  [`is_converged_POMDP()`](http://michael.hahsler.net/pomdp/reference/POMDP.md)
  [`O_()`](http://michael.hahsler.net/pomdp/reference/POMDP.md)
  [`T_()`](http://michael.hahsler.net/pomdp/reference/POMDP.md)
  [`R_()`](http://michael.hahsler.net/pomdp/reference/POMDP.md) : Define
  a POMDP Problem
- [`update_belief()`](http://michael.hahsler.net/pomdp/reference/update_belief.md)
  : Belief Update
- [`simulate_POMDP()`](http://michael.hahsler.net/pomdp/reference/simulate_POMDP.md)
  : Simulate Trajectories Through a POMDP
- [`sample_belief_space()`](http://michael.hahsler.net/pomdp/reference/sample_belief_space.md)
  : Sample from the Belief Space
- [`make_partially_observable()`](http://michael.hahsler.net/pomdp/reference/MDP2POMDP.md)
  [`make_fully_observable()`](http://michael.hahsler.net/pomdp/reference/MDP2POMDP.md)
  : Convert between MDPs and POMDPs
- [`start_vector()`](http://michael.hahsler.net/pomdp/reference/accessors.md)
  [`normalize_POMDP()`](http://michael.hahsler.net/pomdp/reference/accessors.md)
  [`normalize_MDP()`](http://michael.hahsler.net/pomdp/reference/accessors.md)
  [`reward_matrix()`](http://michael.hahsler.net/pomdp/reference/accessors.md)
  [`reward_val()`](http://michael.hahsler.net/pomdp/reference/accessors.md)
  [`transition_matrix()`](http://michael.hahsler.net/pomdp/reference/accessors.md)
  [`transition_val()`](http://michael.hahsler.net/pomdp/reference/accessors.md)
  [`observation_matrix()`](http://michael.hahsler.net/pomdp/reference/accessors.md)
  [`observation_val()`](http://michael.hahsler.net/pomdp/reference/accessors.md)
  : Access to Parts of the Model Description
- [`actions()`](http://michael.hahsler.net/pomdp/reference/actions.md) :
  Available Actions
- [`add_policy()`](http://michael.hahsler.net/pomdp/reference/add_policy.md)
  : Add a Policy to a POMDP Problem Description
- [`plot_belief_space()`](http://michael.hahsler.net/pomdp/reference/plot_belief_space.md)
  : Plot a 2-State or 3-State Projection of the Belief Space
- [`projection()`](http://michael.hahsler.net/pomdp/reference/projection.md)
  : Defining a Belief Space Projection
- [`reachable_states()`](http://michael.hahsler.net/pomdp/reference/reachable_and_absorbing.md)
  [`absorbing_states()`](http://michael.hahsler.net/pomdp/reference/reachable_and_absorbing.md)
  [`remove_unreachable_states()`](http://michael.hahsler.net/pomdp/reference/reachable_and_absorbing.md)
  : Reachable and Absorbing States
- [`regret()`](http://michael.hahsler.net/pomdp/reference/regret.md) :
  Calculate the Regret of a Policy
- [`solve_POMDP()`](http://michael.hahsler.net/pomdp/reference/solve_POMDP.md)
  [`solve_POMDP_parameter()`](http://michael.hahsler.net/pomdp/reference/solve_POMDP.md)
  : Solve a POMDP Problem using pomdp-solver
- [`solve_SARSOP()`](http://michael.hahsler.net/pomdp/reference/solve_SARSOP.md)
  : Solve a POMDP Problem using SARSOP
- [`transition_graph()`](http://michael.hahsler.net/pomdp/reference/transition_graph.md)
  [`plot_transition_graph()`](http://michael.hahsler.net/pomdp/reference/transition_graph.md)
  : Transition Graph
- [`value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md)
  [`plot_value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md)
  : Value Function
- [`write_POMDP()`](http://michael.hahsler.net/pomdp/reference/write_POMDP.md)
  [`read_POMDP()`](http://michael.hahsler.net/pomdp/reference/write_POMDP.md)
  : Read and write a POMDP Model to a File in POMDP Format

## MDPs

Define, inspect, transform, simulate, and analyze finite state-space
Markov decision process models.

- [`MDP()`](http://michael.hahsler.net/pomdp/reference/MDP.md)
  [`is_solved_MDP()`](http://michael.hahsler.net/pomdp/reference/MDP.md)
  : Define an MDP Problem
- [`solve_MDP()`](http://michael.hahsler.net/pomdp/reference/solve_MDP.md)
  [`solve_MDP_DP()`](http://michael.hahsler.net/pomdp/reference/solve_MDP.md)
  [`solve_MDP_TD()`](http://michael.hahsler.net/pomdp/reference/solve_MDP.md)
  : Solve an MDP Problem
- [`simulate_MDP()`](http://michael.hahsler.net/pomdp/reference/simulate_MDP.md)
  : Simulate Trajectories in an MDP
- [`q_values_MDP()`](http://michael.hahsler.net/pomdp/reference/MDP_policy_functions.md)
  [`MDP_policy_evaluation()`](http://michael.hahsler.net/pomdp/reference/MDP_policy_functions.md)
  [`greedy_MDP_action()`](http://michael.hahsler.net/pomdp/reference/MDP_policy_functions.md)
  [`random_MDP_policy()`](http://michael.hahsler.net/pomdp/reference/MDP_policy_functions.md)
  [`manual_MDP_policy()`](http://michael.hahsler.net/pomdp/reference/MDP_policy_functions.md)
  [`greedy_MDP_policy()`](http://michael.hahsler.net/pomdp/reference/MDP_policy_functions.md)
  : Functions for MDP Policies
- [`make_partially_observable()`](http://michael.hahsler.net/pomdp/reference/MDP2POMDP.md)
  [`make_fully_observable()`](http://michael.hahsler.net/pomdp/reference/MDP2POMDP.md)
  : Convert between MDPs and POMDPs
- [`start_vector()`](http://michael.hahsler.net/pomdp/reference/accessors.md)
  [`normalize_POMDP()`](http://michael.hahsler.net/pomdp/reference/accessors.md)
  [`normalize_MDP()`](http://michael.hahsler.net/pomdp/reference/accessors.md)
  [`reward_matrix()`](http://michael.hahsler.net/pomdp/reference/accessors.md)
  [`reward_val()`](http://michael.hahsler.net/pomdp/reference/accessors.md)
  [`transition_matrix()`](http://michael.hahsler.net/pomdp/reference/accessors.md)
  [`transition_val()`](http://michael.hahsler.net/pomdp/reference/accessors.md)
  [`observation_matrix()`](http://michael.hahsler.net/pomdp/reference/accessors.md)
  [`observation_val()`](http://michael.hahsler.net/pomdp/reference/accessors.md)
  : Access to Parts of the Model Description
- [`actions()`](http://michael.hahsler.net/pomdp/reference/actions.md) :
  Available Actions
- [`add_policy()`](http://michael.hahsler.net/pomdp/reference/add_policy.md)
  : Add a Policy to a POMDP Problem Description
- [`gridworld_init()`](http://michael.hahsler.net/pomdp/reference/gridworld.md)
  [`gridworld_maze_MDP()`](http://michael.hahsler.net/pomdp/reference/gridworld.md)
  [`gridworld_s2rc()`](http://michael.hahsler.net/pomdp/reference/gridworld.md)
  [`gridworld_rc2s()`](http://michael.hahsler.net/pomdp/reference/gridworld.md)
  [`gridworld_matrix()`](http://michael.hahsler.net/pomdp/reference/gridworld.md)
  [`gridworld_plot_policy()`](http://michael.hahsler.net/pomdp/reference/gridworld.md)
  [`gridworld_plot_transition_graph()`](http://michael.hahsler.net/pomdp/reference/gridworld.md)
  [`gridworld_animate()`](http://michael.hahsler.net/pomdp/reference/gridworld.md)
  : Helper Functions for Gridworld MDPs
- [`reachable_states()`](http://michael.hahsler.net/pomdp/reference/reachable_and_absorbing.md)
  [`absorbing_states()`](http://michael.hahsler.net/pomdp/reference/reachable_and_absorbing.md)
  [`remove_unreachable_states()`](http://michael.hahsler.net/pomdp/reference/reachable_and_absorbing.md)
  : Reachable and Absorbing States
- [`regret()`](http://michael.hahsler.net/pomdp/reference/regret.md) :
  Calculate the Regret of a Policy
- [`transition_graph()`](http://michael.hahsler.net/pomdp/reference/transition_graph.md)
  [`plot_transition_graph()`](http://michael.hahsler.net/pomdp/reference/transition_graph.md)
  : Transition Graph
- [`value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md)
  [`plot_value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md)
  : Value Function

## Solvers

Solve finite state-space MDP and POMDP models using exact, approximate,
and reinforcement learning methods.

- [`solve_POMDP()`](http://michael.hahsler.net/pomdp/reference/solve_POMDP.md)
  [`solve_POMDP_parameter()`](http://michael.hahsler.net/pomdp/reference/solve_POMDP.md)
  : Solve a POMDP Problem using pomdp-solver
- [`solve_MDP()`](http://michael.hahsler.net/pomdp/reference/solve_MDP.md)
  [`solve_MDP_DP()`](http://michael.hahsler.net/pomdp/reference/solve_MDP.md)
  [`solve_MDP_TD()`](http://michael.hahsler.net/pomdp/reference/solve_MDP.md)
  : Solve an MDP Problem
- [`solve_SARSOP()`](http://michael.hahsler.net/pomdp/reference/solve_SARSOP.md)
  : Solve a POMDP Problem using SARSOP

## Policies and Value Functions

Extract, evaluate, inspect, and visualize policies and value functions
for solved models.

- [`policy()`](http://michael.hahsler.net/pomdp/reference/policy.md) :
  Extract the Policy from a POMDP/MDP
- [`value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md)
  [`plot_value_function()`](http://michael.hahsler.net/pomdp/reference/value_function.md)
  : Value Function
- [`optimal_action()`](http://michael.hahsler.net/pomdp/reference/optimal_action.md)
  : Optimal action for a belief
- [`reward()`](http://michael.hahsler.net/pomdp/reference/reward.md)
  [`reward_node_action()`](http://michael.hahsler.net/pomdp/reference/reward.md)
  : Calculate the Reward for a POMDP Solution
- [`plot_policy_graph()`](http://michael.hahsler.net/pomdp/reference/plot_policy_graph.md)
  [`curve_multiple_directed()`](http://michael.hahsler.net/pomdp/reference/plot_policy_graph.md)
  : POMDP Plot Policy Graphs
- [`estimate_belief_for_nodes()`](http://michael.hahsler.net/pomdp/reference/estimate_belief_for_nodes.md)
  : Estimate the Belief for Policy Graph Nodes
- [`plot_belief_space()`](http://michael.hahsler.net/pomdp/reference/plot_belief_space.md)
  : Plot a 2-State or 3-State Projection of the Belief Space
- [`policy_graph()`](http://michael.hahsler.net/pomdp/reference/policy_graph.md)
  : POMDP Policy Graphs
- [`projection()`](http://michael.hahsler.net/pomdp/reference/projection.md)
  : Defining a Belief Space Projection
- [`solve_POMDP()`](http://michael.hahsler.net/pomdp/reference/solve_POMDP.md)
  [`solve_POMDP_parameter()`](http://michael.hahsler.net/pomdp/reference/solve_POMDP.md)
  : Solve a POMDP Problem using pomdp-solver
- [`solve_SARSOP()`](http://michael.hahsler.net/pomdp/reference/solve_SARSOP.md)
  : Solve a POMDP Problem using SARSOP

## Gridworlds

Create gridworld MDPs, convert between states and grid positions, and
visualize their policies and transitions.

- [`gridworld_init()`](http://michael.hahsler.net/pomdp/reference/gridworld.md)
  [`gridworld_maze_MDP()`](http://michael.hahsler.net/pomdp/reference/gridworld.md)
  [`gridworld_s2rc()`](http://michael.hahsler.net/pomdp/reference/gridworld.md)
  [`gridworld_rc2s()`](http://michael.hahsler.net/pomdp/reference/gridworld.md)
  [`gridworld_matrix()`](http://michael.hahsler.net/pomdp/reference/gridworld.md)
  [`gridworld_plot_policy()`](http://michael.hahsler.net/pomdp/reference/gridworld.md)
  [`gridworld_plot_transition_graph()`](http://michael.hahsler.net/pomdp/reference/gridworld.md)
  [`gridworld_animate()`](http://michael.hahsler.net/pomdp/reference/gridworld.md)
  : Helper Functions for Gridworld MDPs
- [`Cliff_walking`](http://michael.hahsler.net/pomdp/reference/Cliff_walking.md)
  [`cliff_walking`](http://michael.hahsler.net/pomdp/reference/Cliff_walking.md)
  : Cliff Walking Gridworld MDP
- [`DynaMaze`](http://michael.hahsler.net/pomdp/reference/DynaMaze.md)
  [`dynamaze`](http://michael.hahsler.net/pomdp/reference/DynaMaze.md) :
  The Dyna Maze
- [`Maze`](http://michael.hahsler.net/pomdp/reference/Maze.md)
  [`maze`](http://michael.hahsler.net/pomdp/reference/Maze.md) : Steward
  Russell's 4x3 Maze Gridworld MDP
- [`Windy_gridworld`](http://michael.hahsler.net/pomdp/reference/Windy_gridworld.md)
  [`windy_gridworld`](http://michael.hahsler.net/pomdp/reference/Windy_gridworld.md)
  : Windy Gridworld MDP

## POMDP Examples

Example POMDP specifications and example files for learning, testing,
and demonstrating package functionality.

- [`Tiger`](http://michael.hahsler.net/pomdp/reference/Tiger.md)
  [`Three_doors`](http://michael.hahsler.net/pomdp/reference/Tiger.md) :
  Tiger Problem POMDP Specification
- [`RussianTiger`](http://michael.hahsler.net/pomdp/reference/RussianTiger.md)
  : Russian Tiger Problem POMDP Specification
- [`POMDP_example_files`](http://michael.hahsler.net/pomdp/reference/POMDP_example_files.md)
  : POMDP Example Files
- [`POMDP()`](http://michael.hahsler.net/pomdp/reference/POMDP.md)
  [`is_solved_POMDP()`](http://michael.hahsler.net/pomdp/reference/POMDP.md)
  [`is_timedependent_POMDP()`](http://michael.hahsler.net/pomdp/reference/POMDP.md)
  [`epoch_to_episode()`](http://michael.hahsler.net/pomdp/reference/POMDP.md)
  [`is_converged_POMDP()`](http://michael.hahsler.net/pomdp/reference/POMDP.md)
  [`O_()`](http://michael.hahsler.net/pomdp/reference/POMDP.md)
  [`T_()`](http://michael.hahsler.net/pomdp/reference/POMDP.md)
  [`R_()`](http://michael.hahsler.net/pomdp/reference/POMDP.md) : Define
  a POMDP Problem

## MDP Examples

Example MDP specifications, including classic maze and gridworld
problems from reinforcement learning.

- [`Maze`](http://michael.hahsler.net/pomdp/reference/Maze.md)
  [`maze`](http://michael.hahsler.net/pomdp/reference/Maze.md) : Steward
  Russell's 4x3 Maze Gridworld MDP
- [`Cliff_walking`](http://michael.hahsler.net/pomdp/reference/Cliff_walking.md)
  [`cliff_walking`](http://michael.hahsler.net/pomdp/reference/Cliff_walking.md)
  : Cliff Walking Gridworld MDP
- [`Windy_gridworld`](http://michael.hahsler.net/pomdp/reference/Windy_gridworld.md)
  [`windy_gridworld`](http://michael.hahsler.net/pomdp/reference/Windy_gridworld.md)
  : Windy Gridworld MDP
- [`DynaMaze`](http://michael.hahsler.net/pomdp/reference/DynaMaze.md)
  [`dynamaze`](http://michael.hahsler.net/pomdp/reference/DynaMaze.md) :
  The Dyna Maze
- [`MDP()`](http://michael.hahsler.net/pomdp/reference/MDP.md)
  [`is_solved_MDP()`](http://michael.hahsler.net/pomdp/reference/MDP.md)
  : Define an MDP Problem

## Utilities

Color palettes for visualizations and rounding helpers for stochastic
vectors and matrices.

- [`colors_discrete()`](http://michael.hahsler.net/pomdp/reference/colors.md)
  [`colors_continuous()`](http://michael.hahsler.net/pomdp/reference/colors.md)
  : Default Colors for Visualization in Package pomdp
- [`round_stochastic()`](http://michael.hahsler.net/pomdp/reference/round_stochastic.md)
  : Round a stochastic vector or a row-stochastic matrix
