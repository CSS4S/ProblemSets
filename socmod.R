library(assertthat)
library(R6)


Behavior <- R6Class(classname = "Behavior", public = list(
  payoff = NA
))


Agent <- R6Class(classname="Agent", public = list(
  behavior = NA,
  neighborhood = vector(mode = "integer", length = 0),
  name = NA
))


# Set up empty stubs for default model subroutines.
partner_selection_default = function(agent) {}
interaction_default  = function(agent1, agent2, model) {}
model_step_default        = function(model) {}

AgentBasedModel <- R6Class(classname="AgentBasedModel",
  public = list(
    agents = c(), 
    step = 0,
    partner_selection = NULL,
    interaction = NULL,
    model_step = NULL ,
    network = NULL,
    params = list(),
    add_agents = function(agents_to_add) {
      self$agents <- c(self$agents, agents_to_add)
      invisible(self)
    },                    
    initialize_network = function(edges, self) {},
    initialization = 
      function(partner_selection = NULL, interaction = NULL, model_step = NULL, 
               agents = NULL, network = NULL, ...) {

        # Initialize model subroutines with default stubs if not provided.
        if (is.null(partner_selection)) {
          self$partner_selection = partner_selection
        }
        if (is.null(interaction)) {
          self$interaction = interaction_default
        }
        if (is.null(model_step)) {
          self$model_step = model_step
        }

        # Initialize agents and network if the agents were provided. Do nothing
        # if no agents provided. If agents provided but not network, 
        # make a complete, undirected network.
        if (!is.null(agents)) {
          self$agents <- agents
          if (is.null(network)) {
            self$network <- igraph::make_full_graph(length(agents))
          }
          # 
          for (agent in self$agents) {
          }
        }

        # Convert keyword arguments after model_step to parameters in named list.
        self$params = list(...)

        invisible(self)
      }
  ), 
  private = list(
    .full_step = function() {
      for (learner in self$agents) {
        teacher <- self$partner_selection(agent)
        self$interaction(learner, teacher, self)
        self$model_step(self)
      }
    }
  )

)


run <- function(
    model, 
    stop_cond = function(max_t, model) { return (model$step >= max_t) }
  ) {

  # Check that required components are not null
  check_not_null <- 
    c(model$partner_selection, model$interaction, model$model_step, model$network)
  for (component in check_not_null)
    assert_that(!is.null(component))

  # Check that there are some agents.
  assert_that(length(model$agents) > 0)
  
  while (!stop_cond(model)) {
    model$.full_step()
  }

  return (model)
}


# Adapted from 
# https://github.com/USCCANA/netdiffuseR/blob/1efc0be4539d23ab800187c73551624834038e00/src/rgraph.cpp#L90
# Difference here is we'll only use undirected for now, so need to adjust by 
# default (see also NetLogo routine in Smaldino Ch. 9 p. 266).
ring_lattice <- function(N, k, directed = FALSE) {
  assert_that(N - 1 > k, msg = "Lattice degree, k, can be at most N-1.")
  assert_that(!directed, msg = "Directed lattice not yet implemented")

  ret_lattice <- make_empty_graph(N, directed = directed)

  # Iterate over all agents, making links between k/2 neighbors with lesser
  # agent_idx and k/2 neighbors with greater agent_idx.
  k_per_side <- as.integer(k/2)
  for (a_idx in 1:N) {
    for (jj in 1:k_per_side) {

      # The neighbor on the first side.
      neighbor_side_1 <- a_idx + jj 

      if (neighbor_side_1 > N) {
        neighbor_side_1 <- neighbor_side_1 - N
      }

      neighbor_side_2 <- a_idx - jj
      if (neighbor_side_2 <= 0) {
        neighbor_side_2 <- neighbor_side_2 + N
      }

      if (!igraph::are_adjacent(ret_lattice, a_idx, neighbor_side_1)) {
        ret_lattice <- igraph::add_edges(ret_lattice, c(a_idx, neighbor_side_1))
      }
      if (!igraph::are_adjacent(ret_lattice, a_idx, neighbor_side_2)) {
        ret_lattice <- igraph::add_edges(ret_lattice, c(a_idx, neighbor_side_2))
      }

    }

    a_idx <- a_idx + 1
  }

  return (ret_lattice)
}
