

exposure_probs <- function(agent_based_model) {

  # Use Agent class method to get probability of adoption for each agent.
  return (purrr::map_vec(agent_based_model$agents, \(a) { a$exposure_prob() }))
}

mean_exposure_prob <- function(exposure_probs) {
  # COMMENT OUT OR DELETE NEXT LINE AND WRITE YOUR CODE BELOW
  YOUR_CODE_HERE <- 0  

  ret <- YOUR_CODE_HERE

  return (ret)
}
