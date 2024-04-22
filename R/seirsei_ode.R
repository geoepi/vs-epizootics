seirsei_ode <- function(Nh, vect_mult, obs_bias, targ_year) {
  
  Nv <- Nh*vect_mult # number of vectors
  
  i_0 = 1  # initial infectious hosts      
  e_0 = 0  # initial exposed hosts    
  ev_0 = 0 # initial exposed vectors   
  iv_0 = 1 # initial infectious vectors 
  
  # initial values
  init <- c(Sh = Nh - i_0 - e_0, # susceptible hosts
            Eh = e_0,            # exposed hosts
            Ih = i_0,            # infectious hosts
            Rh = 0,              # removed hosts
            CIh = 0,             # cumulative infections
            Sv = Nv - iv_0 - ev_0, # susceptible vectors 
            Ev = ev_0,             # exposed vectors
            Iv = iv_0)             # infectious vectors
  
  # parameters 
  parameters <- c(beta_h = 0.259,   # Sh to Eh rate
                  gamma_h = 0.068,  # hist Ih to Rh rate (recovery)
                  sigma_h = 0.158,  # host Eh to Ih rate
                  sigma_v = 0.307,  # vector Ev to Iv rate
                  rho_v = 0.018,    # reduction in virus fitness
                  beta_v = 0.125,   # Sv to Ev rate  
                  mu_v = 0.410,     # vector natural mortality
                  Nh = Nh,          # total hosts
                  Nv = Nv,          # total vectors
                  i_0 = i_0,        # initial values as above
                  e_0 = e_0,        
                  ev_0 = ev_0,       
                  iv_0 = iv_0)        
  
  # simulation time span
  times <- seq(0, 365, by = 1)  # days of year
  
  # solving the ODE
  output <- as.data.frame(
    deSolve::ode(y = init, times = times, func = run_ode, parms = parameters)
  )
  
  output <- estimate_biased_incidence(output, obs_bias, parameters)
  
  output$date <- convert_doy_to_date(targ_year, output$time)
  
  return(output)
}



run_ode <- function(t, state, parameters) {
 
  with(as.list(c(state, parameters)), {
    
    # Force of infection 
    lambda_h = beta_h * Iv / Nh
    lambda_v = beta_v * Ih / Nh
    
    # Differential equations
    dSh = -lambda_h * Sh
    dEh = lambda_h * Sh - sigma_h * Eh
    dIh = sigma_h * Eh - gamma_h * Ih
    dRh = gamma_h * Ih
    dCIh = sigma_h * Eh 
    dSv = mu_v * Nv - lambda_v * Sv - mu_v * Sv
    dEv = lambda_v * Sv - (sigma_v + mu_v) * Ev
    dIv = sigma_v * Ev * rho_v - mu_v * Iv
    
    return(list(c(dSh, dEh, dIh, dRh, dCIh, dSv, dEv, dIv))) 
  })
}


estimate_biased_incidence <- function(ode_output, obs_bias, parameters) {
  if (obs_bias < 0 || obs_bias > 1) {
    stop("obs_bias must be between 0 and 1")
  }
  
  # approximate incidence
  incidence <- params["beta_h"] * ode_output$Sh * (ode_output$Ih/params["Nh"])
  
  ode_output$adj_inc <- incidence * obs_bias
  
  ode_output$adj_inc[is.na(ode_output$adj_inc)] <- 0
  
  ode_output$adj_inc <- round(ode_output$adj_inc, 0)
  
  ode_output <- ode_output %>% filter(time >= 1 & time <= 365)
  
  return(ode_output)
}






