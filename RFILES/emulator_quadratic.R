
# 'Quadratic' is a emulator in order to reproduce ice sheet simulations
# Vdot [m sle/yr] from volume Vt [m sle] from temperature [degC relative to 
# 1976-2005]
# used by AR4, MAGICC and Irvine et al., 2012
#
# variables needed
# params: numeric vector containing 'a', 'b' and 'c'
# Tt:     numeric vector or matrix containing annual mean temperature

# emulator
sem_core <- function(params, Tt) {  
  # Extract the parameters from the vector params.  
  a     = params[1]
  b     = params[2]
  c     = params[3]
  
  # emulator
  Vdot <- a + b*Tt + c*(Tt^2)
  
  return(Vdot)
}

# estimation of RMSE
sem_rmse <- function(params, Tt, Vdot_model) {
  # estimate Vdot with sem_core  
  Vdot <- sem_core(params, Tt)
  
  # Calculate RMSE between the 'modeled' and 'emulated' Vdot
  rmse = sqrt( mean( (Vdot - Vdot_model)^ 2) )
  
  return(rmse)
}

sem_traject = function(params, V0, Tt) {  
  # Extract the parameters from the vector params.  
  a     = params[1]
  b     = params[2]
  c     = params[3]
    
  nt  <- length(Tt)  # number of time steps
  Vt  <- rep(V0, nt)
  
  # Step through time, calculating the ice volume <Vt> for each time step <i>
  for (i in 2:nt) {
    Vt[i] <- max( 0, Vt[i-1] + (a + b*Tt[i] + c*(Tt[i]^2)) )
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
