library(contextual)

#Looking into the effectiveness of the 4 ads that ran for about a month on a social media platform and had Click Through Rates of: 0.030, 0.011, 0.013, 0.013.

prob_per_arm <- c(0.030, 0.011, 0.013, 0.013) #The probabilities are stored in the vector
horizon <- 10000 #Number of single iterations of the simulation
simulations <- 50 #Times the simulation is run 
bandit <- BasicBernoulliBandit$new(prob_per_arm) #Defines the bandit problem
#Generating agents with different policies: Epsilon-greedy with different values and Thompson Sampling
agents <- list(Agent$new(EpsilonGreedyPolicy$new(0.01), bandit, "eGreedy 0.01"),
               Agent$new(EpsilonGreedyPolicy$new(0.1), bandit, "eGreedy 0.1"),
               Agent$new(EpsilonGreedyPolicy$new(0.3), bandit, "eGreedy 0.3"),
               Agent$new(ThompsonSamplingPolicy$new(1.0, 1.0), bandit)
)
simulation <- Simulator$new(agents, horizon, simulations)
history <- simulation$run() #result of the simulation
plot(history, type = "cumulative")
plot(history, type = "cumulative", regret=F)


#Integrating contextual features
horizon <- 5000
click_probs <- matrix(c( 0.028, 0.013, 0.017, 0.014, # d1: business day
                         0.030, 0.012, 0.014, 0.015, # d2: feed day
                         0.032, 0.009, 0.014, 0.014, # d3: business night
                         0.043, 0.014, 0.022, 0.020 # d4: feed night
), # features (rows): d = 4, arms (columns): k= 4
nrow =4, ncol = 4, byrow = TRUE)
bandit <- ContextualBinaryBandit$new(weights = click_probs)
agents <- list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit, "eGreedy"),
               Agent$new(ContextualEpsilonGreedyPolicy$new(0.1), bandit, "ceGreedy"),
               Agent$new(ContextualLinTSPolicy$new(0.1), bandit, "LinTS"))
simulation <- Simulator$new(agents, horizon, simulations, do_parallel = TRUE)
history <- simulation$run()
plot(history, type = "cumulative", rate = FALSE, legend_position = "topleft")
plot(history, type = "cumulative", regret=F, rate = T, legend_position = "topleft")

#The non-contextual policy eGreedy out-performs the two contextual policies ceGreedy and LinTS. The reason for this result is because across all four contexts, the first arm is always optimal and therefore the context do not really add much information. 

#Changing the optimal arm for each context:
horizon <- 5000
click_probs <- matrix(c( 0.019, 0.013, 0.017, 0.014, # d1: business day
                         0.028, 0.020, 0.018, 0.016, # d2: feed day
                         0.007, 0.009, 0.014, 0.014, # d3: business night
                         0.022, 0.014, 0.022, 0.020 # d4: feed night
), # features (rows): d = 4, arms (columns): k= 4
nrow =4, ncol = 4, byrow = TRUE)
bandit <- ContextualBinaryBandit$new(weights = click_probs)
agents <- list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit, "eGreedy"),
               Agent$new(ContextualEpsilonGreedyPolicy$new(0.1), bandit, "ceGreedy"),
               Agent$new(ContextualLinTSPolicy$new(0.1), bandit, "LinTS"))
simulation <- Simulator$new(agents, horizon, simulations, do_parallel = TRUE)
history <- simulation$run()
plot(history, type = "cumulative", rate = FALSE, legend_position = "topleft")
plot(history, type = "cumulative", regret=F, rate = T, legend_position = "topleft")



#Offline Data
contextualdata <- read.csv("contextualdata.csv")

#Convert data
contextualdata$time <- as.factor(contextualdata$time)
contextualdata$page <- as.factor(contextualdata$page)
contextualdata <- data.table::data.table(contextualdata)
data <- contextual::one_hot(contextualdata, cols = c("time","page"), sparsifyNAs = TRUE)

#Set simulation parameters.
simulations <- 2 # here, "simulations" represents the number of boostrap samples
horizon <- 5000
log_S <- data
formula <- formula("click ~ ad | time_day + time_night + page_business + page_feed")
bandit <- OfflineBootstrappedReplayBandit$new(formula = formula, data = data)

#Define agents.
agents <-list(Agent$new(RandomPolicy$new(), bandit, "Random"),
              Agent$new(EpsilonGreedyPolicy$new(0.03), bandit, "eGreedy 0.03"),
              Agent$new(ThompsonSamplingPolicy$new(), bandit, "ThompsonSampling"))

#Initialize the simulation.
simulation <- Simulator$new(agents = agents,simulations = simulations,horizon = horizon)
# Run the simulation.
sim <- simulation$run()

#Plot the results
plot(sim, type = "cumulative", regret = F, rate = F,legend_position = "topleft")
