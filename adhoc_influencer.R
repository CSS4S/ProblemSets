library(purrr)
library(tibble)
library(dplyr)

run_influencer_model <- function(N=5, adopt_rate = 0.2, t_max = 10) {
  
  # Initialize influenced individuals' traits, time step, and ids.
  prev_adopted_A <- rep(FALSE, N)
  tbl_ret <- tibble(agent_id = 1:N, 
                    step = rep(0, N),
                    adopted_A = prev_adopted_A
                    )
  # Initialize fixated to be FALSE, since at t=0 no influenced agents have adopted A
  fixated <- FALSE
  t <- 1

  while (!fixated || (t > t_max)) {

    # Probabilistically update adopted_A to be TRUE with probability adopt_rate (α)
    new_adopted_A <- 
      prev_adopted_A %>%
        map_vec(\(adopted) { ifelse(adopted, TRUE, runif(1) < adopt_rate) })

    prev_adopted_A <- new_adopted_A

    # The influenced have fixated if all have adopted A
    fixated <- all(new_adopted_A)

    print(new_adopted_A)
    print(fixated)
    
    new_tstep <- tibble(agent_id = 1:N,
                        step = rep(t, N),
                        adopted_A = new_adopted_A)

    tbl_ret <- rbind(tbl_ret, new_tstep)
    
    t <- t + 1
  }

  return (tbl_ret)
}
