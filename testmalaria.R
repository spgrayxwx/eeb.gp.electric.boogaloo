# malaria test fr fr (failed but it was a good start)
#parameters
biting.rate.p.day = 0.24
transmission_probability = 0.6
hum.inf.period = 120
##what to do for mosq infectious period? for now, let's do 2 weeks
mosq.inf.period = 14

#initial population amounts
SHum = 900
IHum = 100
Tot.Hum = SHum + IHum
Tot.Mos = Tot.Hum*2
IMos = Tot.Hum*0.041
SMos = Tot.Mos-IMos
initial_values = c(SH = SHum/Tot.Hum, IH = IHum/Tot.Hum, SM = SMos/Tot.Mos, 
                  SI = IMos/Tot.Mos)

##need to figure out infectious period first
beta_valueY = biting.rate.p.day * transmission_probability
gamma_valueMos = 1 / 21
gamma_valueHum = 1 / 120
RoHum = beta_valueY/gamma_valueHum
RoMos = beta_valueY/gamma_valueMos

#set parameter list
parameter_list = c(beta = beta_valueY, gammaM = gamma_valueMos, gammaH = gamma_valueHum)

#occurs over the course of a year, 365 days
timepoints = seq(0, 365, by=1)

#actual function/model
sis_si_model= function(current_timepoint, state_values, parameters) {
  SM = state_values [1]
  IM = state_values [2]
  SH = state_values [3]
  IH = state_values [4]
  
  with (
    as.list (parameters), 
    
    { 
      dSM = (-beta*SM*IH) + (gammaM * IM)
      dSH = (-beta*SH*IM) + (gammaH * IH)
      dIM = (beta*SM*IH) - (gammaM * IM)
      dIH = (beta*SM*IH) - (gammaH * IH)
      
      results = c (dSM, dSH, dIM, dIH)
      list (results)
      
      }
  )
}

output = lsoda (initial_values, timepoints, sis_si_model, parameter_list)
plot (SH ~ time, data=output, type='b', col='blue')
par (new = TRUE)
plot (IH ~ time, data=output, type='b', col='red')
