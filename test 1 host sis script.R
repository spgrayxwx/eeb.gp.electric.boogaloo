## One Host SIS Model Using deSolve

## parameters
biting_ratepday = 0.24
transmission_probability = 0.6
hum.inf.period.in.days = 120

#compute beta/transmission rate/disease reproductive number
beta_value = biting_ratepday * transmission_probability
gamma_value = 1 / hum.inf.period.in.days
Ro = beta_value / gamma_value
parameter_list = c(beta = beta_value, gamma = gamma_value)

# set # of infected + susceptible + tot pop + find initial pop
X = 50000
Y = 150
N = X + Y
initial_values = c(S = X/N, I = Y/N)

# set time points from 0-50 done over units of one
timepoints = seq(0, 50, by=1)

sis_model = function (current_timepoint, state_values, parameters) {
  S = state_values [1]
  I = state_values [2]
  
  with (
    as.list (parameter_list), 
    {
      #derivative computations
      dS = (-beta * S * I) + (gamma * I)
      dI = (beta * S * I) - (gamma * I)
      
      # combine results
     results = c(dS, dI)
     list (results)
    }
  )
}

#simulate the model
output = lsoda(initial_values, timepoints, sis_model, parameter_list)

#now with the output, we can plot it
plot (S ~ time, data = output, type='b', col = 'red')
> par(new = TRUE)
> plot (I ~ time, data = output, type='b', col = 'blue')