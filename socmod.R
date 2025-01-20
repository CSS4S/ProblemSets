library(R6)

Behavior <- R6Class(classname = "Behavior", public = list(
  payoff = NA
))


Agent <- R6Class(classname="Agent", public = list(
  behavior = Legacy,
  neighborhood = vector(mode = "integer", length = 0),
  name = NA
))

Model <- R6Class(
  public = list(
    agents = NULL,
    step = 0,
    partner_selection = function(agent) {},
    interaction = function(agent1, agent2) {},
    model_step = function() {} #,
    # initialization(agents, partner_select, interaction, model_step) {}
    #   self$step = 0
    #   self$agents = agents
    #   self$partner_select
  ), 
  private = list(
    .full_step = function() {}
  )

)