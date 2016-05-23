# In this script three semi-empirical models are trained on the basis of different subsets 
# of 20 geo-engineering scenarios simulated by SICOPOLIS (Applegate and Keller, 2015)
#
# subsets:
# complete:                     1 subset
# LOO (leave one out subsets): 20 subsets
# L4O (leave four out subsets) [2025-2075, 2125-2175, 2225-2275, 2325-2375, 2425-2475]
# L5O (leave five out subsets) [1:5, 6:10, 11-15, 16-20]
#
# semi-empirical models:
#   quadratic
#   delayed-linear
#   SIMPLE

# clean environment and set directory  
rm(list = ls())
graphics.off()

# packages and sourcefiles
require("reshape2")
require('DEoptim')

# combine temperature stabilisation and gradual drawdwon geo.engineering scenarios
equil <- read.table("data/SICOPOLIS_equil.txt", header=T) # read data
grad  <- read.table("data/SICOPOLIS_grad.txt",  header=T)

equil$sc <- as.factor( paste("equil",equil$yr0,sep=".") )    # combine
grad$sc  <- as.factor( paste("grad",  grad$yr0,sep=".") )
geoEngineeringScenarios    <- rbind(equil,grad)[c(1,7,3:6)]

# and prepare trainingsset
Tt          <- dcast(geoEngineeringScenarios, t ~ sc, value.var="Tt")
Vt          <- as.matrix(
  dcast(geoEngineeringScenarios, t ~ sc, value.var="Vt")[,-1] )
Vdot_model  <- as.matrix(
  dcast(geoEngineeringScenarios, t ~ sc, value.var="Vdt")[,-1] )
t           <- as.numeric(Tt[, 1])
Tt          <- as.matrix( Tt[,-1])

#############
### SIMPLE ##
source("sem_SIMPLE.R")
sem <- "SIMPLE"

train.emulator <- function(training.set) {
  # uses global parameters
  set.seed(1234)
  fit = DEoptim(sem_rmse,
                lower     = c(-10^1 ,  1, 10^-5, 10^-7),
                upper     = c(-10^-4, 20, 10^-1, 10^-4),
                control    = DEoptim.control(itermax = 200, c = 0.1, trace = 20),
                Tt         = Tt[,training.set],
                Vt         = Vt[,training.set],
                Vdot_model = Vdot_model[,training.set] )
  return(fit$optim$bestmem)
}

params <- data.frame(training.set=c("complete",rep("LOO",20),rep("LO4",5),rep("L5O",4)),
                     member=c(1,1:20,1:5,1:4),
                     a     = rep(NA,30),
                     b     = rep(NA,30),
                     alpha = rep(NA,30),
                     beta  = rep(NA,30))

# complete
print(paste(sem,": complete"))
params[1,-1:-2] <- train.emulator(training.set=1:20)
write.table(params, "data/params.SIMPLE.txt")

# LOO
for(i in 1:20) {
  print(paste(sem,": LOO",i))
  params[i+1, -1:-2] <- train.emulator( training.set=c(1:20)[-i] )
}
write.table(params, "data/params.SIMPLE.txt")

# L4O
print(paste(sem,": L4O", 1)); params[22, -1:-2] <- train.emulator( training.set= c(     3:10, 13:20) )
print(paste(sem,": L4O", 2)); params[23, -1:-2] <- train.emulator( training.set= c(1:2, 5:12, 15:20) )
print(paste(sem,": L4O", 3)); params[24, -1:-2] <- train.emulator( training.set= c(1:4, 7:14, 17:20) )
print(paste(sem,": L4O", 4)); params[25, -1:-2] <- train.emulator( training.set= c(1:6, 9:16, 19:20) )
print(paste(sem,": L4O", 5)); params[26, -1:-2] <- train.emulator( training.set= c(1:8,11:18       ) )
write.table(params, "data/params.SIMPLE.txt")

# L5O
print(paste(sem,": L5O", 1)); params[27, -1:-2] <- train.emulator( training.set= c(      6:20) )
print(paste(sem,": L5O", 2)); params[28, -1:-2] <- train.emulator( training.set= c(1:5, 11:20) )
print(paste(sem,": L5O", 3)); params[29, -1:-2] <- train.emulator( training.set= c(1:10,16:20) )
print(paste(sem,": L5O", 4)); params[30, -1:-2] <- train.emulator( training.set= c(1:15      ) )
write.table(params, "data/params.SIMPLE.txt")

#############
### SIMPLE_a ##
source("sem_SIMPLE.R")
sem <- "SIMPLE_a"

train.emulator <- function(training.set) {
  # uses global parameters
  set.seed(1234)
  fit = DEoptim(sem_rmse,
#                lower     = c(-10^1 ,  1, 10^-5, 10^-7),
#                upper     = c(-10^-4, 20, 10^-1, 10^-4),
                # tweak in tested range
                lower     = c( -1.7,  5.50, 10^-5, 10^-7),
                upper     = c( -1.5,  5.60, 10^-1, 10^-4),
                control    = DEoptim.control(itermax = 200, c = 0.1, trace = 20),
                Tt         = Tt[,training.set],
                Vt         = Vt[,training.set],
                Vdot_model = Vdot_model[,training.set] )
  return(fit$optim$bestmem)
}

params <- data.frame(training.set=c("complete",rep("LOO",20),rep("LO4",5),rep("L5O",4)),
                     member=c(1,1:20,1:5,1:4),
                     a     = rep(NA,30),
                     b     = rep(NA,30),
                     alpha = rep(NA,30),
                     beta  = rep(NA,30))

# complete
print(paste(sem,": complete"))
params[1,-1:-2] <- train.emulator(training.set=1:20)
write.table(params, "data/params.SIMPLE_a.txt")

# LOO
for(i in 1:20) {
  print(paste(sem,": LOO",i))
  params[i+1, -1:-2] <- train.emulator( training.set=c(1:20)[-i] )
}
write.table(params, "data/params.SIMPLE_a.txt")

# L4O
print(paste(sem,": L4O", 1)); params[22, -1:-2] <- train.emulator( training.set= c(     3:10, 13:20) )
print(paste(sem,": L4O", 2)); params[23, -1:-2] <- train.emulator( training.set= c(1:2, 5:12, 15:20) )
print(paste(sem,": L4O", 3)); params[24, -1:-2] <- train.emulator( training.set= c(1:4, 7:14, 17:20) )
print(paste(sem,": L4O", 4)); params[25, -1:-2] <- train.emulator( training.set= c(1:6, 9:16, 19:20) )
print(paste(sem,": L4O", 5)); params[26, -1:-2] <- train.emulator( training.set= c(1:8,11:18       ) )
write.table(params, "data/params.SIMPLE_a.txt")

# L5O
print(paste(sem,": L5O", 1)); params[27, -1:-2] <- train.emulator( training.set= c(      6:20) )
print(paste(sem,": L5O", 2)); params[28, -1:-2] <- train.emulator( training.set= c(1:5, 11:20) )
print(paste(sem,": L5O", 3)); params[29, -1:-2] <- train.emulator( training.set= c(1:10,16:20) )
print(paste(sem,": L5O", 4)); params[30, -1:-2] <- train.emulator( training.set= c(1:15      ) )
write.table(params, "data/params.SIMPLE_a.txt")



#############
### DICE ##
source("sem_DICE.R")
sem <- "DICE"

train.emulator <- function(training.set) {
  # uses global parameters
  set.seed(1234)
  fit = DEoptim(sem_rmse,
                lower     = c(-10^1 ,  1, 10^-5, 10^-7),
                upper     = c(-10^-4, 20, 10^-1, 10^-4),
                control    = DEoptim.control(itermax = 200, c = 0.1, trace = 20),
                Tt         = Tt[,training.set],
                Vt         = Vt[,training.set],
                Vdot_model = Vdot_model[,training.set] )
  return(fit$optim$bestmem)
}

params <- data.frame(training.set=c("complete",rep("LOO",20),rep("LO4",5),rep("L5O",4)),
                     member=c(1,1:20,1:5,1:4),
                     a     = rep(NA,30),
                     b     = rep(NA,30),
                     alpha = rep(NA,30),
                     beta  = rep(NA,30))

# complete
print(paste(sem,": complete"))
params[1,-1:-2] <- train.emulator(training.set=1:20)
write.table(params, "data/params.DICE.txt")

# LOO
for(i in 1:20) {
  print(paste(sem,": LOO",i))
  params[i+1, -1:-2] <- train.emulator( training.set=c(1:20)[-i] )
}
write.table(params, "data/params.DICE.txt")

# L4O
print(paste(sem,": L4O", 1)); params[22, -1:-2] <- train.emulator( training.set= c(     3:10, 13:20) )
print(paste(sem,": L4O", 2)); params[23, -1:-2] <- train.emulator( training.set= c(1:2, 5:12, 15:20) )
print(paste(sem,": L4O", 3)); params[24, -1:-2] <- train.emulator( training.set= c(1:4, 7:14, 17:20) )
print(paste(sem,": L4O", 4)); params[25, -1:-2] <- train.emulator( training.set= c(1:6, 9:16, 19:20) )
print(paste(sem,": L4O", 5)); params[26, -1:-2] <- train.emulator( training.set= c(1:8,11:18       ) )
write.table(params, "data/params.DICE.txt")

# L5O
print(paste(sem,": L5O", 1)); params[27, -1:-2] <- train.emulator( training.set= c(      6:20) )
print(paste(sem,": L5O", 2)); params[28, -1:-2] <- train.emulator( training.set= c(1:5, 11:20) )
print(paste(sem,": L5O", 3)); params[29, -1:-2] <- train.emulator( training.set= c(1:10,16:20) )
print(paste(sem,": L5O", 4)); params[30, -1:-2] <- train.emulator( training.set= c(1:15      ) )
write.table(params, "data/params.DICE.txt")

#############
### DICE_b ##
source("sem_DICE.R")
sem <- "DICE_b"

train.emulator <- function(training.set) {
  # uses global parameters
  set.seed(1234)
  fit = DEoptim(sem_rmse,
                lower     = c( -1.7,  5.50, 10^-5, 10^-7),
                upper     = c( -1.5,  5.60, 10^-1, 10^-4),
                control    = DEoptim.control(itermax = 200, c = 0.1, trace = 20),
                Tt         = Tt[,training.set],
                Vt         = Vt[,training.set],
                Vdot_model = Vdot_model[,training.set] )
  return(fit$optim$bestmem)
}

params <- data.frame(training.set=c("complete",rep("LOO",20),rep("LO4",5),rep("L5O",4)),
                     member=c(1,1:20,1:5,1:4),
                     a     = rep(NA,30),
                     b     = rep(NA,30),
                     alpha = rep(NA,30),
                     beta  = rep(NA,30))

# complete
print(paste(sem,": complete"))
params[1,-1:-2] <- train.emulator(training.set=1:20)
write.table(params, "data/params.DICE_b.txt")

# LOO
for(i in 1:20) {
  print(paste(sem,": LOO",i))
  params[i+1, -1:-2] <- train.emulator( training.set=c(1:20)[-i] )
}
write.table(params, "data/params.DICE_b.txt")

# L4O
print(paste(sem,": L4O", 1)); params[22, -1:-2] <- train.emulator( training.set= c(     3:10, 13:20) )
print(paste(sem,": L4O", 2)); params[23, -1:-2] <- train.emulator( training.set= c(1:2, 5:12, 15:20) )
print(paste(sem,": L4O", 3)); params[24, -1:-2] <- train.emulator( training.set= c(1:4, 7:14, 17:20) )
print(paste(sem,": L4O", 4)); params[25, -1:-2] <- train.emulator( training.set= c(1:6, 9:16, 19:20) )
print(paste(sem,": L4O", 5)); params[26, -1:-2] <- train.emulator( training.set= c(1:8,11:18       ) )
write.table(params, "data/params.DICE_b.txt")

# L5O
print(paste(sem,": L5O", 1)); params[27, -1:-2] <- train.emulator( training.set= c(      6:20) )
print(paste(sem,": L5O", 2)); params[28, -1:-2] <- train.emulator( training.set= c(1:5, 11:20) )
print(paste(sem,": L5O", 3)); params[29, -1:-2] <- train.emulator( training.set= c(1:10,16:20) )
print(paste(sem,": L5O", 4)); params[30, -1:-2] <- train.emulator( training.set= c(1:15      ) )
write.table(params, "data/params.DICE_b.txt")



#############
### DICE_a ##
source("sem_DICE_a.R")
sem <- "DICE_a"

train.emulator <- function(training.set) {
  # uses global parameters
  set.seed(1234)
  fit = DEoptim(sem_rmse,
                lower     = c(-10^1 ,  1, 10^-5, 10^-7),
                upper     = c(-10^-4, 20, 10^-1, 10^-4),
                control    = DEoptim.control(itermax = 200, c = 0.1, trace = 20),
                Tt         = Tt[,training.set],
                Vt         = Vt[,training.set],
                Vdot_model = Vdot_model[,training.set] )
  return(fit$optim$bestmem)
}

params <- data.frame(training.set=c("complete",rep("LOO",20),rep("LO4",5),rep("L5O",4)),
                     member=c(1,1:20,1:5,1:4),
                     a     = rep(NA,30),
                     b     = rep(NA,30),
                     alpha = rep(NA,30),
                     beta  = rep(NA,30))

# complete
print(paste(sem,": complete"))
params[1,-1:-2] <- train.emulator(training.set=1:20)
write.table(params, "data/params.DICE_a.txt")

# LOO
for(i in 1:20) {
  print(paste(sem,": LOO",i))
  params[i+1, -1:-2] <- train.emulator( training.set=c(1:20)[-i] )
}
write.table(params, "data/params.DICE_a.txt")

# L4O
print(paste(sem,": L4O", 1)); params[22, -1:-2] <- train.emulator( training.set= c(     3:10, 13:20) )
print(paste(sem,": L4O", 2)); params[23, -1:-2] <- train.emulator( training.set= c(1:2, 5:12, 15:20) )
print(paste(sem,": L4O", 3)); params[24, -1:-2] <- train.emulator( training.set= c(1:4, 7:14, 17:20) )
print(paste(sem,": L4O", 4)); params[25, -1:-2] <- train.emulator( training.set= c(1:6, 9:16, 19:20) )
print(paste(sem,": L4O", 5)); params[26, -1:-2] <- train.emulator( training.set= c(1:8,11:18       ) )
write.table(params, "data/params.DICE_a.txt")

# L5O
print(paste(sem,": L5O", 1)); params[27, -1:-2] <- train.emulator( training.set= c(      6:20) )
print(paste(sem,": L5O", 2)); params[28, -1:-2] <- train.emulator( training.set= c(1:5, 11:20) )
print(paste(sem,": L5O", 3)); params[29, -1:-2] <- train.emulator( training.set= c(1:10,16:20) )
print(paste(sem,": L5O", 4)); params[30, -1:-2] <- train.emulator( training.set= c(1:15      ) )
write.table(params, "data/params.DICE_a.txt")


#####################
### delayed-linear ##
source("sem_delayed-linear.R")
sem <- "delayed-linear"

train.emulator <- function(training.set) {
  # uses global parameters
  set.seed(1234)
  fit = DEoptim(sem_rmse,
                lower     = c(-10^1 ,  1, 10^1),
                upper     = c(-10^-4, 20, 10^6),
                control    = DEoptim.control(itermax = 200, c = 0.1, trace = 20),
                Tt         = Tt[,training.set],
                Vt         = Vt[,training.set],
                Vdot_model = Vdot_model[,training.set] )
  return(fit$optim$bestmem)
}

params <- data.frame(training.set=c("complete",rep("LOO",20),rep("LO4",5),rep("L5O",4)),
                     member=c(1,1:20,1:5,1:4),
                     a   = rep(NA,30),
                     b   = rep(NA,30),
                     tau = rep(NA,30))

# complete
print(paste(sem,": complete"))
params[1,-1:-2] <- train.emulator(training.set=1:20)
write.table(params, "data/params.delayed-linear.txt")

# LOO
for(i in 1:20) {
  print(paste(sem,": LOO",i))
  params[i+1, -1:-2] <- train.emulator( training.set=c(1:20)[-i] )
}
write.table(params, "data/params.delayed-linear.txt")

# L4O
print(paste(sem,": L4O", 1)); params[22, -1:-2] <- train.emulator( training.set= c(     3:10, 13:20) )
print(paste(sem,": L4O", 2)); params[23, -1:-2] <- train.emulator( training.set= c(1:2, 5:12, 15:20) )
print(paste(sem,": L4O", 3)); params[24, -1:-2] <- train.emulator( training.set= c(1:4, 7:14, 17:20) )
print(paste(sem,": L4O", 4)); params[25, -1:-2] <- train.emulator( training.set= c(1:6, 9:16, 19:20) )
print(paste(sem,": L4O", 5)); params[26, -1:-2] <- train.emulator( training.set= c(1:8,11:18       ) )
write.table(params, "data/params.delayed-linear.txt")

# L5O
print(paste(sem,": L5O", 1)); params[27, -1:-2] <- train.emulator( training.set= c(      6:20) )
print(paste(sem,": L5O", 2)); params[28, -1:-2] <- train.emulator( training.set= c(1:5, 11:20) )
print(paste(sem,": L5O", 3)); params[29, -1:-2] <- train.emulator( training.set= c(1:10,16:20) )
print(paste(sem,": L5O", 4)); params[30, -1:-2] <- train.emulator( training.set= c(1:15      ) )
write.table(params, "data/params.delayed-linear.txt")

################
### quadratic ##
source("emulator_quadratic.R")
sem <- "quadratic"

train.emulator <- function(training.set) {
  # uses global parameters
  set.seed(1234)
  fit = DEoptim(sem_rmse,
                lower      = c( -1.0, -1.0, -1.0),
                upper      = c(  0.0,  1.0,  1.0),
                control    = DEoptim.control(itermax = 200, c = 0.1, trace = 20),
                Tt         = Tt[,training.set],
                Vdot_model = Vdot_model[,training.set] )
  return(fit$optim$bestmem)
}

params <- data.frame(training.set=c("complete",rep("LOO",20),rep("LO4",5),rep("L5O",4)),
                     member=c(1,1:20,1:5,1:4),
                     a=rep(NA,30),
                     b=rep(NA,30),
                     c=rep(NA,30))

# complete
print(paste(sem,": complete"))
params[1,-1:-2] <- train.emulator(training.set=1:20)
write.table(params, "data/params.quadratic.txt")

# LOO
for(i in 1:20) {
  print(paste(sem,": LOO",i))
  params[i+1, -1:-2] <- train.emulator( training.set=c(1:20)[-i] )
}
write.table(params, "data/params.quadratic.txt")

# L4O
print(paste(sem,": L4O", 1)); params[22, -1:-2] <- train.emulator( training.set= c(     3:10, 13:20) )
print(paste(sem,": L4O", 2)); params[23, -1:-2] <- train.emulator( training.set= c(1:2, 5:12, 15:20) )
print(paste(sem,": L4O", 3)); params[24, -1:-2] <- train.emulator( training.set= c(1:4, 7:14, 17:20) )
print(paste(sem,": L4O", 4)); params[25, -1:-2] <- train.emulator( training.set= c(1:6, 9:16, 19:20) )
print(paste(sem,": L4O", 5)); params[26, -1:-2] <- train.emulator( training.set= c(1:8,11:18       ) )
write.table(params, "data/params.quadratic.txt")

# L5O
print(paste(sem,": L5O", 1)); params[27, -1:-2] <- train.emulator( training.set= c(      6:20) )
print(paste(sem,": L5O", 2)); params[28, -1:-2] <- train.emulator( training.set= c(1:5, 11:20) )
print(paste(sem,": L5O", 3)); params[29, -1:-2] <- train.emulator( training.set= c(1:10,16:20) )
print(paste(sem,": L5O", 4)); params[30, -1:-2] <- train.emulator( training.set= c(1:15      ) )
write.table(params, "data/params.quadratic.txt")



