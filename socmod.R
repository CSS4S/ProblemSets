##
# Computational social science modeling tools focused on social behavior.
#
# Author: Matthew A. Turner <maturner01@gmail.com>
# Date: 2025-01-21
#
library(assertthat)
library(igraph)
library(R6)


Behavior <- R6Class(classname = "Behavior", public = list(
  payoff = NA,
  initialize = function(payoff = 1.0) {self$payoff = payoff}
))

.agent_exposure_prob <- function(agent) {

  neighbors <- agent$neighbors

  n_neighbors_adopted <- 
    sum(map_vec(neighbors, \(n) { n$behavior == "Adaptive" }))

  return (n_neighbors_adopted / length(neighbors))
}


Agent <- R6Class(classname="Agent", public = list(
  curr_behavior = NA,
  prev_behavior = NA,
  # neighbors = vector(mode = "integer", length = 0),
  neighbors = c(),
  fitness = NA,
  name = "",
  # vertex = NA,
  initialize = 
    function(init_behavior, fitness = NA, name = NA, neighbors = NULL) {

      self$curr_behavior <- init_behavior
      self$prev_behavior <- init_behavior
      self$name <- name
      self$add_neighbors(neighbors)  
    },
  add_neighbors = function(new_neighbors) {
    self$neighbors <- c(self$neighbors, new_neighbors) 
  },
  exposure_prob = function() {
    return (.agent_exposure_prob(self))
  }
))


# Set up empty stubs for default model subroutines.
partner_selection_default = function(agent) {}
interaction_default  = function(agent1, agent2, model) {}
model_iter_default        = function(model) {}
# The default stop_cond.
stop_cond_default <- function(max_t) { 
  
  return (function(model, max_t) {
    return (model$step >= max_t) 
  })
}


AgentBasedModel <- R6Class(classname="AgentBasedModel",
  public = list(
    agents = c(), 
    step = 0,
    network = NULL,
    params = list(),
    output = NULL,
    add_agents = function(agents_to_add) {
      self$agents <- c(self$agents, agents_to_add)
      invisible(self)
    },                    
    initialize = 
      function(agents = NULL, network = NULL, ...) {

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
        } else {
          # Now dealing with the case where `agents` is null, but network not.
          if (!is.null(network)) {

            # Create new agents defaulting to Legacy behavior on init.
            self$agents <- 
              purrr::map(
                V(network)$name, \(n) { 
                  Agent$new("Legacy", 
                            name=n, 
                            neighbors=neighbors(network, n))
                }
              )

            # Make agents into named list so graph names can look up agents.
            names(self$agents) <- V(network)$name
          }
        }

        # Keyword arguments in ... become model parameters in named list.
        self$params = list(...)

        invisible(self)
      }
  ), 
  private = list()
)


run <- function(model, partner_selection = partner_selection_default,
                interaction = interaction_default, 
                model_iter = model_iter_default, stop_cond = 1L) {

  # Check that there are some agents.
  assert_that(length(model$agents) > 0)

  # Initialize output tibble.
  if (is.null(model$output)) {

    model$output <- tibble(t = 0:max_t,
                           A = rep(0.0, max_t + 1))

    model$output[1, ] <- list(0, total_adoption(model$agents))

  } else {

    prev_t <- model$output$t
    prev_tmax <- model$output$t[length(prev_t)]
    min_t <- prev_tmax + 1
    max_t <- prev_tmax + stop_cond

    model$output <- 
      rbind(model$output, tibble(t = min_t:max_t, A = rep(0.0, max_t + 1)))
  }

  # If stop_cond is an integer, use as max time step in new stop_cond func.
  if (!is.function(stop_cond)) {
    n_iterations <- stop_cond
    curr_model_step <- model$step

    # Redefine stop condition with dummy variable to run model another 
    # n_iterations = stop_cond steps.
    stop_cond <- function(model, .) { 
      return (model$step <= curr_model_step + n_iterations) 
    }
  }


  total_adoption <- function(agents) {
    sum(purrr::map_vec(agents, 
        \(a) { ifelse(a$curr_behavior == "Adaptive", 1, 0) }))
  }
  
  while (!stop_cond(model, max_t)) {
    
    for (learner in sample(model$agents)) {
      teacher <- partner_selection(learner, model)
      interaction(learner, teacher, model)
    }

    model_iter(model)

    # Increment time step.
    model$step <- model$step + 1

    # Need to add one to current step, i.e., output[1,] was row 1, but tstep 0,
    # and so when model$step <- model$step + 1 runs for the first time, 
    # model$step increments from 0 to 1. If we did not have +1, the last 
    # time step in the output would just be 0 from the initialization of output.
    output[model$step + 1, ] <- list(model$step, total_adoption(model$agents))
  }

  return (output)
}


run_trials <- function(model_factory, n_trials = 2, ...) {
  
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



#******************************** UNIT TESTS *********************************#

# library(testthat)

# test_that("Agents have expected neighbors in initialized Model", {
  # If network is not specified, use complete network.
  # m <- AgentBasedModel$new()
  # expect_equal()
# })


# test_that("multiplication works", {
  # expect_equal(2 * 2, 4, label="expect true")
  # expect_equal(2 + 2, 5, label="expect false")
# })
