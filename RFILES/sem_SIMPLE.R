
# SIMPLE  is a semi-empirical model (sem) to estimate volumetric change
# Vdot [m sle/yr] from volume Vt [m sle] from temperature [degC relative to 
# 1976-2005]
# used by Moore et al. 2010; Grinsted et al. 2010 for sea level projections
#
# variables needed
# params: numeric vector containing 'a', 'b', 'alpha' and 'beta'
# Tt:     numeric vector or matrix containing annual mean temperature
# Vt:     numeric vector or matrix containing current ice volume


# semi-empirical model
sem_core <- function(params, Tt, Vt) {  
  # Extract the parameters from the vector params.  
  a     = params[1]
  b     = params[2]
  alpha = params[3]
  beta  = params[4]
  
  # sem
  Veq  <- a * Tt + b # equilibrium
  tau  <- 1 / (alpha * Tt + beta)
  Vdot <- -(Vt-Veq)/tau
    
  return(Vdot)
}

# estimation of RMSE
sem_rmse <- function(params, Tt, Vt, Vdot_model) {
  # estimate Vdot with sem_core  
  Vdot <- sem_core(params, Tt, Vt)
  
  # Calculate RMSE between the 'modeled' and 'emulated' Vdot
  rmse = sqrt( mean( (Vdot - Vdot_model)^ 2) )
  
  return(rmse)
}

sem_traject = function(params, V0, Tt) {  
  # Extract the parameters from the vector params.  
  a     = params[1]
  b     = params[2]
  alpha = params[3]
  beta  = params[4]
  
  nt  <- length(Tt)  # number of time steps
  Vt  <- rep(V0, nt)
  
  Veq <- a * Tt + b              # equilibrium volume
  tau <- 1 / (alpha * Tt + beta) # tau
  
  # Step through time, calculating the ice volume <Vt> for each time step <i>
  for (i in 2:nt) {
    Vt[i] <- max( 0,  Vt[i-1] + -(Vt[i-1]-Veq[i]) / tau[i] )
  } # end for (i in 2: nt)
  
  return(Vt)
}

library(Rcpp)
cppFunction('NumericVector emulatorC(NumericVector params, NumericVector Tt, double V0) {
  double a     = params[0];
  double b     = params[1];
  double alpha = params[2];
  double beta  = params[3];

  double Veq      = -10.0;
  double tau      = -10.0;
  double deltaVeq = -10.0;
  
  int    nt    = Tt.size();

  NumericVector Vt(nt);

  Vt[0] = V0;

  for(int i = 1; i < nt; ++i) {

    Veq      = a * Tt[i] + b;
    tau      = 1 / (alpha * Tt[i] + beta);
    deltaVeq = Vt[i-1] - Veq;

    Vt[i] = deltaVeq  *  exp(-1 /tau) + Veq;
    Vt[i] = ( Vt[i] < 0.0  ?  0.0 : Vt[i] );

  }
  return Vt;
}')

# (Vt[i-1] - a * Tt[i] + b) * exp(- (alpha * Tt[i] + beta) ) + a * Tt[i] + b;
