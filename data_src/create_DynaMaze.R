library(pomdp)

DynaMaze <- gridworld_maze_MDP(
                dim = c(6,9),
                start = "s(3,1)",
                goal = "s(1,9)",
                walls = c("s(2,3)", "s(3,3)", "s(4,3)",
                          "s(5,6)",
                          "s(1,8)", "s(2,8)", "s(3,8)"),
                restart = TRUE,
                discount = 0.95,
                name = "Dyna Maze",
                )
DynaMaze

save(DynaMaze, file = "data/DynaMaze.rda")

