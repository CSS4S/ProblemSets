source("../socmod.R")


####### PROBLEMS 2-1 and 2-2: Agent-based Legacy-Adaptive-Legacy model. ########

Legacy <- Behavior$new()
Adaptive <- Behavior$new()


# Learning is the smae in LA and LAL models: a learner adopts A if they do
# L and their chosen teacher does A.
lal_learning <- function(learner, teacher, model) {

  # Learning only happens if the learner does Legacy, teacher does Adaptive.
  if (learner$behavior == Legacy && teacher$behavior == Adaptative) {

    # Learner adopts Adaptive behavior with probability of adoption rate, α.
    if (rand() < model$params$adoption_rate) {
      learner$behavior <- Adaptive
    }
  }

  # Return learner silently so nothing would print to screen on return.
  invisible(learner)
}


# Partner selection is the same in LA and LAL models: randomly select a neighbor.
lal_partner_selection <- function(learner, model) {
  teacher <- sample(learner$neighbors, 1)
  return (teacher)
}


# In the LAL model step, each agent doing the adaptation drops it 
# with probability drop rate, δ.
lal_model_step <- function(model) {

  # First get list of all agents doing A...
  adaptive_agents <- filter(model$agents, \(agent) {agent$behavior == Adaptive})

  # ...then iterate through them and adopt Legacy behavior with prob. drop_rate.
  for (agent in adaptive_agents) {
    if (rand() < model$drop_rate) {
      agent$behavior <- Legacy
    }
  }
  
  invisible(model)
}



################### END OF CODE FOR PROBLEMS 2-1 AND 2-2 #######################




#### PROBLEM 2-4: diffusion of adaptations with social learning strategies. ####

conformity_learning <- function(learner, teacher) {

}


success_biased_learning <- function(learner, teacher) {

}

######################### END OF CODE FOR PROBLEM 2-4 ##########################
