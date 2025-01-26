

exposure_probs <- function(agent_based_model) {

  # Use Agent class method to get probability of adoption for each agent.
  return map_vec(agent_based_model$agents, \(a) { a$exposure_prob() })
}

mean_exposure_prob <- function(exposure_probs) {
  YOUR_CODE_HERE <- 0  # <---- COMMENT OUT OR DELETE THIS LINE AND WRITE YOUR CODE BELOW

  ret <- YOUR_CODE_HERE

  return (ret)
}
