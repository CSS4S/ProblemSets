##
# Computational social science modeling tools focused on social behavior.
#
# Author: Matthew A. Turner <maturner01@gmail.com>
# Date: 2025-01-21
#
library(assertthat)
library(R6)


Behavior <- R6Class(classname = "Behavior", public = list(
  payoff = NA,
  initialize = function(payoff = 1.0) {self$payoff = payoff}
))


Agent <- R6Class(classname="Agent", public = list(
  curr_behavior = NA,
  prev_behavior = NA,
  # neighbors = vector(mode = "integer", length = 0),
  neighbors = NA,
  fitness = NA,
  name = NA,
  initialize = function(curr_behavior, fitness = NA, name = NA) {
    self$curr_behavior = curr_behavior
  },
  add_neighbors = function(new_neighbors) {
    self$neighbors <- c(self$neighbors, new_neighbors) 
  }
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
    # initialize_network = function(edges) {
    #   # Create the network based on the edges and set `network` attribute.

    #   # Read and store neighbors for each agent so they 
    #   # don't have to be looked up every time.
    #   for (agent in self$agents) {
    #     agent$neighbors <- c(1)  ## XXX FIX THIS ##
    #   }
    # },
    initialize = 
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

          if (is.null(network)) {
            self$network <- igraph::make_full_graph(length(agents))
          } else {
            self$network <- network    
          }

          for (agent_idx in 1:length(agents)) {
            agents[[agent_idx]]$neighbors <- 
              igraph::neighbors(self$network, agent_idx)
          }

          self$agents <- agents
        }

        # Convert keyword arguments after model_step to parameters in named list.
        self$params = list(...)

        invisible(self)
      },

    .full_step = function() {
      for (learner in self$agents) {
        teacher <- self$partner_selection(agent, self)
        self$interaction(learner, teacher, self)
        self$model_step(self)
        self$step <- self$step + 1
      }
    }
  ), 
  private = list()
)


run <- function(model, partner_selection, interaction, model_step, max_t) {

  # Currently define the stop_cond by the max time step. 
  stop_cond <- function(model, max_t) { return (model$step >= max_t) }

  # Check that required components are not null
  check_not_null <- 
    c(model$partner_selection, model$interaction, model$model_step, model$network)

  for (component in check_not_null)
    assert_that(!is.null(component))

  # Check that there are some agents.
  assert_that(length(model$agents) > 0)

  # Initialize output tibble.
  output <- tibble(t = 0:max_t,
                   A = rep(0.0, max_t + 1))

  total_adoption <- function(agents) {
    sum(purrr::map_vec(agents, \(a) { ifelse(a$curr_behavior == "Adaptive", 1, 0) }))
  }

  output[1, ] <- list(0, total_adoption(model$agents))
  
  while (!stop_cond(model, max_t)) {

    for (learner in sample(model$agents)) {
      teacher <- partner_selection(learner, model)
      interaction(learner, teacher, model)
    }
    model_step(model)
    model$step <- model$step + 1

    output[model$step + 1, ] <- list(model$step, adoption(model$agents))
  }

  return (output)
}


# Adapted from 
# https://github.com/USCCANA/netdiffuseR/blob/1efc0be4539d23ab800187c73551624834038e00/src/rgraph.cpp#L90
# Difference here is we'll only use undirected for now, so need to adjust by 
# default (see also NetLogo routine in Smaldino Ch. 9 p. 266). 
#
# Beacaus igraph is flexible, it will add duplicate edges, so we have to check
# to make sure an edge does not exist between two nodes before adding it, using
# the `igraph::are_adjacent` function ("adjacent" means there is an edge between two
# nodes in an undirected graph–in a directed graph the definition is subjective,
# i.e., v1 and v2 are sometimes defined as adjacent if there's an edge from
# v1 to v2, and others define adjacency as an edge from v2 to v1). 
regular_lattice <- function(N, k, directed = FALSE) {

  # Check that lattice parameters satisfy listed conditions below.
  assert_that(N - 1 >= k, msg = "Lattice degree, k, can be at most N-1.")
  assert_that(k %% 2 == 0, msg = "Lattice degree, k, must be even.")
  assert_that(!directed, msg = "Directed lattice not yet implemented")

  # Initialize an empty graph to which we add edges.
  ret_lattice <- igraph::make_empty_graph(N, directed = directed)

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

      # The neighbor on the second side.
      neighbor_side_2 <- a_idx - jj
      if (neighbor_side_2 <= 0) {
        neighbor_side_2 <- neighbor_side_2 + N
      }

      # Add first edge if not already present.
      ret_lattice <- add_unique_edge(ret_lattice, a_idx, neighbor_side_1)

      # Add second edge if not already present.
      ret_lattice <- add_unique_edge(ret_lattice, a_idx, neighbor_side_2)
    }
  }

  return (ret_lattice)
}


# Add an undirected edge from v1 to v2 to graph g if it does not already exist.
add_unique_edge <- function(g, v1, v2) {
  
  if (!igraph::are_adjacent(g, v1, v2)) {
      g <- igraph::add_edges(g, c(v1, v2))
  }

  return (g)
}

# library(testthat)
# test_that("multiplication works", {
#   expect_equal(2 * 2, 4, label="expect true")
#   expect_equal(2 + 2, 5, label="expect false")
# })
