# Construct Validation datasets for different models and parameter sets

# clean environment and set directory  
rm(list = ls())
graphics.off()

# read geo-engineering scenarios SICOPOLIS (Applegate and Keller 2015)
equil  <- read.table("data/SICOPOLIS_equil.txt", header=T)
grad   <- read.table("data/SICOPOLIS_grad.txt",  header=T)
rcp8.5 <- read.table("data/SICOPOLIS_rcp8.5.txt",  header=T)

equil$yr0 <- as.factor(equil$yr0)
grad$yr0  <- as.factor(grad$yr0)

# identify which parameter sets should be used for which scenario in case
# of particular CV setting
param.set.ids.complete  <- data.frame(yr0=c(levels(equil$yr0)), paramset =  rep(1,10))
param.set.ids.LOO.equil <- data.frame(yr0=c(levels(equil$yr0)), paramset =      2:11)
param.set.ids.LOO.grad  <- data.frame(yr0=c(levels(equil$yr0)), paramset =     12:21)
param.set.ids.L4O       <- data.frame(yr0=c(levels(equil$yr0)), paramset =   c(22,22,
                                                                               23,23,
                                                                               24,24,
                                                                               25,25,
                                                                               26,26))
param.set.ids.L5O.equil <- data.frame(yr0=c(levels(equil$yr0)), paramset=c(rep(27, 5),
                                                                           rep(28, 5)))
param.set.ids.L5O.grad  <- data.frame(yr0=c(levels(equil$yr0)), paramset=c(rep(29, 5),
                                                                           rep(30, 5)))

# function to emulate geo-engineering scenarios
construct.validation.complete <- function(df, param.set.ids) {
  df.emulator <- df
  for(yr0 in levels(df$yr0)) {
    # select all rows on the basis of start year
    subset.yr0 <- which(df$yr0==yr0)
    # select row of parameter sets (globally assigned)
    paramset   <- param.set.ids$paramset[which(param.set.ids$yr0==yr0)]
    params     <- as.numeric(param.sets[paramset, -1:-2])
    # emulate Vt for selected geo-scenario on the basis of these parameters
    # emulator is globally assigned
    df.emulator$Vt[subset.yr0] <- sem_traject(params,
                                                V0=df$Vt[subset.yr0][1],
                                                Tt=df$Tt[subset.yr0])
    # estimate Vdt for every time step
    df.emulator$Vdt <- c(NA,diff(df.emulator$Vt)/diff(df.emulator$t))
  }
  return(df.emulator)
}

###############
## Quadratic ##
param.sets <- read.table("data/params.quadratic.txt")
source("emulator_quadratic.R")

# equil
write.table(construct.validation.complete(equil, param.set.ids.complete),
            "data/quadratic_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.LOO.equil),
            "data/quadratic.LOO_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.L4O),
            "data/quadratic.L4O_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.L5O.equil),
            "data/quadratic.L5O_equil.txt",col.names=T, row.names=F)
# grad
write.table(construct.validation.complete(grad, param.set.ids.complete),
            "data/quadratic_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.LOO.grad),
            "data/quadratic.LOO_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.L4O),
            "data/quadratic.L4O_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.L5O.grad),
            "data/quadratic.L5O_grad.txt",col.names=T, row.names=F)

# rcp8.5
df <- rcp8.5
df$Vt <- sem_traject(as.numeric(param.sets[1,-1:-2]),
                  V0=df$Vt[1],
                  Tt=df$Tt)
df$Vdt <- c(NA,diff(df$Vt)/diff(df$t))

write.table(df, "data/quadratic_rcp8.5.txt",col.names=T, row.names=F)


## Delayed-linear ##
param.sets <- read.table("data/params.delayed-linear.txt")
source("sem_delayed-linear.R")

# equil
write.table(construct.validation.complete(equil, param.set.ids.complete),
            "data/delayed-linear_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.LOO.equil),
            "data/delayed-linear.LOO_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.L4O),
            "data/delayed-linear.L4O_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.L5O.equil),
            "data/delayed-linear.L5O_equil.txt",col.names=T, row.names=F)
# grad
write.table(construct.validation.complete(grad, param.set.ids.complete),
            "data/delayed-linear_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.LOO.grad),
            "data/delayed-linear.LOO_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.L4O),
            "data/delayed-linear.L4O_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.L5O.grad),
            "data/delayed-linear.L5O_grad.txt",col.names=T, row.names=F)

# rcp8.5
df <- rcp8.5
df$Vt <- sem_traject(as.numeric(param.sets[1,-1:-2]),
                     V0=df$Vt[1],
                     Tt=df$Tt)
df$Vdt <- c(NA,diff(df$Vt)/diff(df$t))

write.table(df, "data/delayed-linear_rcp8.5.txt",col.names=T, row.names=F)


## SIMPLE ##
param.sets <- read.table("data/params.SIMPLE.txt")
source("sem_SIMPLE.R")

# equil
write.table(construct.validation.complete(equil, param.set.ids.complete),
            "data/SIMPLE_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.LOO.equil),
            "data/SIMPLE.LOO_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.L4O),
            "data/SIMPLE.L4O_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.L5O.equil),
            "data/SIMPLE.L5O_equil.txt",col.names=T, row.names=F)
# grad
write.table(construct.validation.complete(grad, param.set.ids.complete),
            "data/SIMPLE_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.LOO.grad),
            "data/SIMPLE.LOO_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.L4O),
            "data/SIMPLE.L4O_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.L5O.grad),
            "data/SIMPLE.L5O_grad.txt",col.names=T, row.names=F)

# rcp8.5
df <- rcp8.5
df$Vt <- sem_traject(as.numeric(param.sets[1,-1:-2]),
                     V0=df$Vt[1],
                     Tt=df$Tt)
df$Vdt <- c(NA,diff(df$Vt)/diff(df$t))

write.table(df, "data/SIMPLE_rcp8.5.txt",col.names=T, row.names=F)


## SIMPLE_a ##
param.sets <- read.table("data/params.SIMPLE_a.txt")
source("sem_SIMPLE.R")

# equil
write.table(construct.validation.complete(equil, param.set.ids.complete),
            "data/SIMPLE_a_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.LOO.equil),
            "data/SIMPLE_a.LOO_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.L4O),
            "data/SIMPLE_a.L4O_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.L5O.equil),
            "data/SIMPLE_a.L5O_equil.txt",col.names=T, row.names=F)
# grad
write.table(construct.validation.complete(grad, param.set.ids.complete),
            "data/SIMPLE_a_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.LOO.grad),
            "data/SIMPLE_a.LOO_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.L4O),
            "data/SIMPLE_a.L4O_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.L5O.grad),
            "data/SIMPLE_a.L5O_grad.txt",col.names=T, row.names=F)

# rcp8.5
df <- rcp8.5
df$Vt <- sem_traject(as.numeric(param.sets[1,-1:-2]),
                     V0=df$Vt[1],
                     Tt=df$Tt)
df$Vdt <- c(NA,diff(df$Vt)/diff(df$t))

write.table(df, "data/SIMPLE_a_rcp8.5.txt",col.names=T, row.names=F)


## DICE ##
param.sets <- read.table("data/params.DICE.txt")
source("sem_DICE.R")

# equil
write.table(construct.validation.complete(equil, param.set.ids.complete),
            "data/DICE_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.LOO.equil),
            "data/DICE.LOO_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.L4O),
            "data/DICE.L4O_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.L5O.equil),
            "data/DICE.L5O_equil.txt",col.names=T, row.names=F)
# grad
write.table(construct.validation.complete(grad, param.set.ids.complete),
            "data/DICE_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.LOO.grad),
            "data/DICE.LOO_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.L4O),
            "data/DICE.L4O_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.L5O.grad),
            "data/DICE.L5O_grad.txt",col.names=T, row.names=F)

# rcp8.5
df <- rcp8.5
df$Vt <- sem_traject(as.numeric(param.sets[1,-1:-2]),
                     V0=df$Vt[1],
                     Tt=df$Tt)
df$Vdt <- c(NA,diff(df$Vt)/diff(df$t))

write.table(df, "data/DICE_rcp8.5.txt",col.names=T, row.names=F)

## DICE_b ##
param.sets <- read.table("data/params.DICE_b.txt")
source("sem_DICE.R")

# equil
write.table(construct.validation.complete(equil, param.set.ids.complete),
            "data/DICE_b_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.LOO.equil),
            "data/DICE_b.LOO_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.L4O),
            "data/DICE_b.L4O_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.L5O.equil),
            "data/DICE_b.L5O_equil.txt",col.names=T, row.names=F)
# grad
write.table(construct.validation.complete(grad, param.set.ids.complete),
            "data/DICE_b_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.LOO.grad),
            "data/DICE_b.LOO_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.L4O),
            "data/DICE_b.L4O_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.L5O.grad),
            "data/DICE_b.L5O_grad.txt",col.names=T, row.names=F)

# rcp8.5
df <- rcp8.5
df$Vt <- sem_traject(as.numeric(param.sets[1,-1:-2]),
                     V0=df$Vt[1],
                     Tt=df$Tt)
df$Vdt <- c(NA,diff(df$Vt)/diff(df$t))

write.table(df, "data/DICE_b_rcp8.5.txt",col.names=T, row.names=F)


## DICE_a ##
param.sets <- read.table("data/params.DICE_a.txt")
source("sem_DICE_a.R")

# equil
write.table(construct.validation.complete(equil, param.set.ids.complete),
            "data/DICE_a_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.LOO.equil),
            "data/DICE_a.LOO_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.L4O),
            "data/DICE_a.L4O_equil.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(equil, param.set.ids.L5O.equil),
            "data/DICE_a.L5O_equil.txt",col.names=T, row.names=F)
# grad
write.table(construct.validation.complete(grad, param.set.ids.complete),
            "data/DICE_a_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.LOO.grad),
            "data/DICE_a.LOO_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.L4O),
            "data/DICE_a.L4O_grad.txt",col.names=T, row.names=F)
write.table(construct.validation.complete(grad, param.set.ids.L5O.grad),
            "data/DICE_a.L5O_grad.txt",col.names=T, row.names=F)

# rcp8.5
df <- rcp8.5
df$Vt <- sem_traject(as.numeric(param.sets[1,-1:-2]),
                     V0=df$Vt[1],
                     Tt=df$Tt)
df$Vdt <- c(NA,diff(df$Vt)/diff(df$t))

write.table(df, "data/DICE_a_rcp8.5.txt",col.names=T, row.names=F)

