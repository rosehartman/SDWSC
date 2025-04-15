

bio_rose1_fxn2 <- function(DataFile, Weight.Initial, # DataFile includes temp and turbidity 
                          food, prey.ed, nprey, k.df, v.df, # prey density parameters on consumption
                          a.turb, b.turb, min1, # turbidity parameters on consumption
                          Spawning.Day = 0, Gonad.Loss = 0){
  #Read in the user defined data:
  TheData <- DataFile
  
  #===============================================================================================================
  #-----INPUT PHYSIOLOGICAL PARAMETERS-----#
  #===============================================================================================================
  #Consumption related parameters (user defined):
  CA <- 0.096 #Estimate from Smith and Nobriga 2023 (0.090, 0.110)
  CB <- -0.354 #Estimate from Smith and Nobriga 2023 (-0.53, -0.251) 
  CQ <- 10 # Rose et al. 2013 
  CTO <- 20 # Rose et al. 2013 
  CTM <- 23 #Estimate from Smith and Nobriga 2023 (20.5, 23.6), Rose = 23
  CTL <- 27  # Rose et al. 2013 
  CK1 <- 0.40 # Rose et al. 2013
  CK4 <- 0.01 # Rose et al. 2013
  
  #Respiration related parameters (user defined):
  RA <- 0.0027 # Rose et al. 2013 
  RB <- -0.216 # Rose et al. 2013
  RQ <- 0.064 #Estimate from Smith and Nobriga 2023 (0.036, 0.080) #Rosie's code has this at 0.036
  #RQ <- 0.036
  #I don't know where any of these are in my code
  RTO <- 0.0196 # Hewett and Johnson 1992 via Whitledge and Hayward 1997 
  RTM <- 0 
  RTL <- 25 
  RK1 <- 1 # Hewett and Johnson 1992 via Whitledge and Hayward 1997 
  RK4 <- 0 # Hewett and Johnson 1992 via Whitledge and Hayward 1997 
  ACT <- 1.0198 # Rice 1983 
  BACT <- 0 # Hewett and Johnson 1992 via Whitledge and Hayward 1997 
  SDA <- 0.175 # Rose et al. 2013 
  OxyConv <- 13562 #J/gram of O2 in respiration conversions (Elliot and Davidson 1975).   
  
  #Egestion and excretion parameters (user defined):
  FA <- 0.16 # Rose et al. 2013
  #  FB <- -0.222
  #  FG <- 0.631
  UA <- 0.1 # Rose et al. 2013
  #  UB <- 0.58
  #  UG <- -0.299
  
  #Predator energy densities for EQUATION 1 (user defined):
  Predator.Energy <- 4814 # Rose et al. 2013 J/g. reported energy density of 4.42kJ/g  # 1020.59 cal/g (4270.149 J/g) from Irwin et al. 2003. 4814 J/g from Smith and Nobriga 2023 code 
  
  #Consumer inputs (grams):
  Weight0 <- W0 <- Weight.Initial #Intial consumer weight.
  Gonad.Loss <- Gonad.Loss  #As a proportion of fish body mass.  
  
  Gonad.Loss.Day <- Spawning.Day   #Day of simulation that corresponds to spawning and gamete 
  
  #===============================================================================================================
  #FUNCTION NUMBER 1: {Bioenergetics}          
  
  #Runs bioenergetics model for a single day in a given simulation and spits out
  #vector of bioenergetics quantities --> Used for fitting the p-value and for
  #generating consumption once p-value has been fit --> For looping below.
  #===============================================================================================================
  #"i" is a row in the input data file, "WO" is the consumer weight, and 
  # food is a matrix of available food densities.
  
  loop_fxn <- function(food){
    # Empty Matrix to store results #
    results <- matrix(0, ncol = 9, nrow = length(TheData$SimDay))
    food1 <- food[1, ]
    
    results[1, ] <- Bioenergetics2(1, Weight0, food,  TheData)
    
    for (i in 2:nrow(results)){
      results[i, ] <- Bioenergetics2(i, results[i - 1, 3], food, TheData)
    } #i
    
    results_df <- data.frame(FinalWGT_g = results[length(TheData$SimDay), 3],
                             Growth_g = results[length(TheData$SimDay), 3] - W0,
                             GrowthRate_gd = (results[length(TheData$SimDay), 3] - W0) / 
                               max(TheData$SimDay),
                             SpecificGrowth_ggd = ((results[length(TheData$SimDay), 3] - W0) /
                                                     W0) / max(TheData$SimDay))
    return(results_df)
  }
  
  results_df <- loop_fxn(food)
  return(results_df)
}



Bioenergetics2 <- function(i, W0, food, DataFile){  
  TheData <- DataFile
  
  #===============================================================================================================
  #-----INPUT PHYSIOLOGICAL PARAMETERS-----#
  #===============================================================================================================
  #Consumption related parameters (user defined):
  CA <- 0.096 #Estimate from Smith and Nobriga 2023 (0.090, 0.110)
  CB <- -0.354 #Estimate from Smith and Nobriga 2023 (-0.53, -0.251) 
  CQ <- 10 # Rose et al. 2013 
  CTO <- 20 # Rose et al. 2013 
  CTM <- 23 #Estimate from Smith and Nobriga 2023 (20.5, 23.6), Rose = 23
  CTL <- 27  # Rose et al. 2013 
  CK1 <- 0.40 # Rose et al. 2013
  CK4 <- 0.01 # Rose et al. 2013
  
  #Respiration related parameters (user defined):
  RA <- 0.0027 # Rose et al. 2013 
  RB <- -0.216 # Rose et al. 2013
  RQ <- 0.064 #Estimate from Smith and Nobriga 2023 (0.036, 0.080) #Rosie's code has this at 0.036
  #RQ <- 0.036
  #I don't know where any of these are in my code
  RTO <- 0.0196 # Hewett and Johnson 1992 via Whitledge and Hayward 1997 
  RTM <- 0 
  RTL <- 25 
  RK1 <- 1 # Hewett and Johnson 1992 via Whitledge and Hayward 1997 
  RK4 <- 0 # Hewett and Johnson 1992 via Whitledge and Hayward 1997 
  ACT <- 1.0198 # Rice 1983 
  BACT <- 0 # Hewett and Johnson 1992 via Whitledge and Hayward 1997 
  SDA <- 0.175 # Rose et al. 2013 
  OxyConv <- 13562 #J/gram of O2 in respiration conversions (Elliot and Davidson 1975).   
  
  #Egestion and excretion parameters (user defined):
  FA <- 0.16 # Rose et al. 2013
  #  FB <- -0.222
  #  FG <- 0.631
  UA <- 0.1 # Rose et al. 2013
  #  UB <- 0.58
  #  UG <- -0.299
  
  #Predator energy densities for EQUATION 1 (user defined):
  Predator.Energy <- 4814 # Rose et al. 2013 J/g. reported energy density of 4.42kJ/g  # 1020.59 cal/g (4270.149 J/g) from Irwin et al. 2003. 4814 J/g from Smith and Nobriga 2023 code 
  
  #Consumer inputs (grams):
  Weight0 <- W0 <- Weight.Initial #Intial consumer weight.
  Gonad.Loss <- Gonad.Loss  #As a proportion of fish body mass.  
  
  Gonad.Loss.Day <- Spawning.Day   #Day of simulation that corresponds to spawning and gamete 
  
  
  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  #-----CONSUMPTION: Cool and Cold-water Species Function for CONSUMPTION-----# 
  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  # Cmax Relationship with Mass #
  Cmax <- (CA * W0^CB)
  
  # Temperature Effects #
  G1 <- (1 / (CTO - CQ)) * log((0.98 * (1 - CK1)) / (CK1 * 0.02))
  L1 <- exp(G1 * (TheData$Temp[i] - CQ))
  KA <- (CK1 * L1) / (1 + CK1 * (L1 - 1))
  G2 <- (1 / (CTL - CTM)) * log((0.98 * (1 - CK4)) / (CK4 * 0.02))
  L2 <- exp(G2 * (CTL - TheData$Temp[i]))
  KB <- (CK4 * L2) / (1 + CK4 * (L2 - 1))
  ft.con <- KA * KB 
  
  # Prey Availability/Density Effect #
  ## Multispecies functional response based on Holling (1965) type II model used in Rose et al. (2013) and smith and Nobriga (2023) ## 
  # v_df <- rep(1, nprey) # vulnerability of prey to DS (assume all = 1 for simplicity)
  food_sat <- food[i, ] # * (v_df / k.df)  #food in the environment
  food_num <- food_sat * (v.df / k.df) #food eaten by smelt - by taxon
  food_den <- (1 + sum(food_sat * (v.df / k.df))) #total food eaten by smelt? Why do we add one?
  food_prop <- food_num / food_den
  
  food.fxn <- sum(food_prop)
  
  # Turbidity Effect #
  turb.fxn <- min1 + (1 - min1) / 
    (1 + (exp(-(b.turb * (TheData$Turb[i] - a.turb)))))
  
  # Bring all components into a full consumption model: Realized Consumption #
  Consumption <- Cmax * food.fxn * turb.fxn * ft.con #Units --> Specific (g/g/d).
  
  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  #-----EGESTION (solid) and EXCRETION (nitrogenous): Equation 1 Not accounting for Indigestible Prey -----# 
  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  #Both types of waste loss are simply a constant fraction of consumption: 
  Egestion <- FA*Consumption 
  Excretion <- UA*(Consumption-Egestion)
  #Units --> Specific (g/g/d).
  
  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  #-----RESPIRATION and SDA: Equation 1-----# 
  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  #Exponential with swimming speed--dependent on mass and water temperature below a cuttoff (RTL):
  ifelse(TheData$Temp[i] > RTL, 
         VEL <- RK1 * W0^RK4, 
         VEL <- ACT * (W0^RK4) * exp(BACT * TheData$Temp[i]))
  ft.metabolism <- exp(RQ * TheData$Temp[i])
  ACTIVITY <- exp(RTO*VEL)                           
  
  Respiration <- RA * (W0^RB) * ft.metabolism * ACTIVITY #Units --> Specific (g O2/g/d). 
  SDAction <- SDA * (Consumption - Egestion) #Units --> Specific (g/g/d) 
  
  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  #-----PREDATOR ENERGY (J/g): Equation 1-----# 
  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> 
  Pred.Energy <- Predator.Energy
  
  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  #-----Convert grams to joules and calculate GROWTH for simulation day i-----# 
  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  Growth <- ((((Consumption) * 
                 sum(prey.ed * food_prop)) - 
                ((Egestion + Excretion + SDAction) *
                   sum(prey.ed * food_prop) +
                   (Respiration * OxyConv))) / Pred.Energy) * W0    #In terms of daily weight gain (g/day).
  #Divide this value by W0 to get back to a specific rate (g/g/d).
  
  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  #-----Update GROWTH if spawning occurred and return vector of bioenergetics quantities-----# 
  #<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  Consumption.Specific <- Consumption #Units --> g/g/d
  Consumption.total <- Consumption.Specific * W0 #Units --> g/d
  
  #If spawning occurred: need to update total growth on spawning day:
  if (i == Gonad.Loss.Day & Gonad.Loss > 0) {
    Growth <- Growth - (Gonad.Loss * (Growth + W0))
  } 
  else { 
    Growth <- Growth
  } #Units --> g       
  
  Growth.Specific <- Growth / W0 #Units --> g/g/d
  
  #Weight of fish at end of simulation day:
  Weight.End <- Growth + W0 #Units --> g   
  
  #Egestion, Excretion, Respiration, and SDA are in specific terms.
  
  #Return a vector of all bionergetics quantities from the days simulation: 
  return(c(Growth, Growth.Specific, Weight.End, Consumption.total,
           Consumption.Specific, Egestion, Excretion, Respiration, SDAction))
}

