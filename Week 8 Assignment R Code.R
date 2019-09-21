##Week 8 Assignment - Reinforcement Learning

#Loading the MDP toolbox package
install.packages("MDPtoolbox")
library(MDPtoolbox)

#Need to define the elements of RL, and assign a label of the states in the navigation matrix
#Defining the Set of Actions - left, right, up & down for a 2X2 matrix

#Up Action
up = matrix(c(1,0,0,0,
              0.7,0.2,0.1,0,
              0, 0.1, 0.2, 0.7,
              0, 0, 0, 1),
            nrow=4, ncol=4, byrow=TRUE)

#Down Action
down = matrix(c(0.3, 0.7, 0, 0,
                0, 0.9, 0.1, 0,
                0, 0.1, 0.9, 0,
                0, 0, 0.7, 0.3),
              nrow=4, ncol=4, byrow=TRUE)

#Left Action
left = matrix(c(0.9, 0.1, 0, 0,
                0.1, 0.9, 0, 0,
                0, 0.7, 0.2, 0.1,
                0, 0, 0.1, 0.9),
              nrow=4, ncol=4, byrow = TRUE)

#Right Action
right = matrix(c(0.9, 0.1, 0, 0,
                 0.1, 0.2, 0.7, 0,
                 0, 0, 0.9, 0.1,
                 0, 0, 0.1, 0.9),
               nrow=4, ncol=4, byrow=TRUE)

#Combined Actions matrix
Actions = list(up=up, down=down, left=left, right=right)

#Define the rewards and penalties 
Rewards = matrix(c(-1, -1, -1, -1,
                   -1, -1, -1, -1,
                   -1, -1, -1, -1,
                   10, 10, 10, 10),
                 nrow=4, ncol=4, byrow=TRUE)

#Next find out if the defined problem can be solved correctly by the package
#Solving the navigation
solver = mdp_policy_iteration(P=Actions, R=Rewards, discount=0.1)

#The results gives us the policy, the value at each step & additionally, the nymber of iterations and time taken. We know, the policy should dictate the correct path to reach the final state S4.
#The policy function to know the matrices used for defining the policy and then the names from the actions list.

#Getting the policy
solver$policy #2 4 1 1
names(Actions)[solver$policy] #down right up up

#The values are contained in V and show the reward at each step
#Getting the values at each ste. These values can be different in each run
solver$V 

#Iter and time can be used to know the iterations and time to keep track of the complexity
#Additional info: # of iterations
solver$iter

#Additional info: time taken, this time can be different on each run
solver$time

#Bring in packages devtools and reinforcementlearning
install.packages("devtools")
library(devtools)

install_github("nproellochs/ReinforcementLearning")
2
1

library(ReinforcementLearning)

#This package has this toy example pre-built, we just look at the function which should have otherwise defined

#Viewing the pre-built function for each state, action and reward
print(gridworldEnvironment)

function(state, action)
{
  next_state <- state
  if (state == state("s1") && action == "down")
    next_state <- state("s2")
  if (state == state("s2") && action == "up")
    next_state <- state("s1")
  if (state == state("s2") && action == "right")
    next_state <- state("s3")
  if (state == state("s3") && action == "left")
    next_state <- state("s2")
  if (state == state("s3") && action == "up")
    next_state <- state("s4")
  if (next_state == state("s4") && state != state("s4")) {
    reward <- 10
  }
  else {
    reward <- -1
  }
  out <- list(NextState = next_state, Reward = reward)
  return(out)
}

#Now define the names of the states and actions and start solving using the sampleExperience() function

#Define names for state and action
states <- c("s1", "s2", "s3", "s4")
actions <- c("up", "down", "left", "right")

#Generate 1000 iterations
sequences <- sampleExperience(N = 1000, env = gridworldEnvironment, states = states, actions = actions)

#Solve the problem
solver_rl <- ReinforcementLearning(sequences, s = "State", a = "Action", r = "Reward", s_new = "NextState")

#Getting the policy; this may be different for each run
solver_rl$Policy

#Getting the Reward; this may be different for each run
solver_rl$Reward

#Here we see that the first 3 stesp are always the same and correct for s4, the 4th action is random and can be different for each run

#Adapting to the changing environment

#Use the tic-tac-toe game data generated in it's pre-built library. We can directly load the data and perform reinforcement learning on the data
#Conclusion: Adapting to the changing environment
#load data set
data("tictactoe")

#Perform reinforcement learning on tictactoe data
model_tic_tac <- ReinforcementLearning(tictactoe, s = "State", a = "Action", r = "Reward", s_new = "New State", iter = 1)

#Optimal policy; this may be different for each run
model_tic_tac$Policy #This will print a very large matrix of the possible step in each step

#Reward; this may be different for each run
model_tic_tac$Reward #5449

# Reinforcement Learning Exercise comes from: dataaspirant.com/2018/02/05/reinforcement-learning-r/











