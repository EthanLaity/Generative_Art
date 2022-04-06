######################################################################
# Load in libraries
######################################################################
library(Rcpp) # to iterate fast
library(tidyverse) # to plot and transform data
library(colourlovers) # to color drawings with nice colors
library(reshape2) # to convert matrix into data frames

# Import C++ code
sourceCpp(code='
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]


// This function allows to do convolutions on the edge elements of the 
// matrix since it extracts elements from the opposite rows or columns 
// to avoid losing dimensionality after convolution
int get_index(int M, int i)
{
  if (i < 0)
    return (M + i % M) % M;
  if(i >= M)
    return i % M;
  return i;
}

// Sensor stage
Rcpp::DataFrame sensor (arma::mat envM,
                        Rcpp::DataFrame parF,
                        double FL,
                        double FR,
                        double RA,
                        double SO) {
  
  int m = envM.n_rows;
  int n = envM.n_cols;
  int npart = parF.nrows();
  
  Rcpp::IntegerVector x = parF["x"];
  Rcpp::IntegerVector y = parF["y"];
  Rcpp::NumericVector h = parF["h"];
  
  // create a vector to store heading
  Rcpp::NumericVector hnew(npart);

  for(int i = 0; i < npart; i++) {
    
    // Sensory stage
    int Fx = get_index(m, (int)round(x[i] + SO * cos(h[i])));
    int Fy = get_index(n, (int)round(y[i] + SO * sin(h[i])));
    
    int FLx = get_index(m, (int)round(x[i] + SO * (cos(h[i] + FL))));
    int FLy = get_index(n, (int)round(y[i] + SO * (sin(h[i] + FL))));
    
    int FRx = get_index(m, (int)round(x[i] + SO * (cos(h[i] + FR))));
    int FRy = get_index(n, (int)round(y[i] + SO * (sin(h[i] + FR))));
    
    double F  = envM(Fx, Fy);
    double FL = envM(FLx, FLy);
    double FR = envM(FRx, FRy);
    
    if ((F > FL) && (F > FR))
    {
      hnew[i] = h[i];
    }
    else if ((F < FL) && (F < FR))
    {
      if ( rand() % 2 == 0 )
      {
        hnew[i] = h[i] + RA;
      }
      else
      {
        hnew[i] = h[i] - RA;
      }
      
    }
    else if (FL < FR)
    {
      hnew[i] = h[i] - RA;
    }
    else if (FR < FL)
    {
      hnew[i] = h[i] + RA;
    }
    else
    {
      hnew[i] = h[i];
    }
  }
  
  Rcpp::DataFrame dest = Rcpp::DataFrame::create(Rcpp::Named("x") = x,
                                                 Rcpp::Named("y") = y,
                                                 Rcpp::Named("h") = hnew);
  
  return dest;
  
}

// Motor stage
Rcpp::DataFrame motor (Rcpp::DataFrame parF,
                       int n,
                       int m,
                       double SS){
  
  int npart = parF.nrows();

  Rcpp::IntegerVector x = parF["x"];
  Rcpp::IntegerVector y = parF["y"];
  Rcpp::NumericVector h = parF["h"];
  
  
  // create the new columns
  Rcpp::IntegerVector xnew(npart);
  Rcpp::IntegerVector ynew(npart);
  
  for(int i = 0; i < npart; i++) {
    
    xnew[i] = get_index(n, (int)round(x[i] + SS * cos(h[i])));
    ynew[i] = get_index(m, (int)round(y[i] + SS * sin(h[i])));
    
    
  }
  
  Rcpp::DataFrame dest = Rcpp::DataFrame::create(Rcpp::Named("x") = xnew,
                                                 Rcpp::Named("y") = ynew,
                                                 Rcpp::Named("h") = h);
  
  return dest;
}

// Deposition
arma::mat deposition (Rcpp::DataFrame parF,
                      double depT,
                      arma::mat envM) {
  
  int npart = parF.nrows();
  
  Rcpp::IntegerVector x = parF["x"];
  Rcpp::IntegerVector y = parF["y"];

  for(int i = 0; i < npart; i++) {
    envM(x[i], y[i]) = envM(x[i], y[i]) + depT;    
    
    
  }
    
return envM;
}

// Evaporation
arma::mat evaporate (arma::mat source,
                     double factor) {
  
  int m = source.n_rows;
  int n = source.n_cols;
  
  arma::mat dest(m, n);
  
  for (int y = 0; y < n; ++y) {
    for (int x = 0; x < m; ++x) {
      dest(x, y) = source(x, y) * (1 - factor);
    }
  }
  return dest;
};

// Gather all into a funtion called physarum
// [[Rcpp::export]]
arma::mat physarum(arma::mat envM,
                 Rcpp::DataFrame parF,
                 double decayT,
                 double FL,
                 double FR,
                 double RA,
                 double SO,
                 int SS,
                 double depT,
                 int iters)
{
  int m = envM.n_rows;
  int n = envM.n_cols;
  
  // envM = scale(envM);
  
  for (int k = 0; k < iters; k++){

    // Sensor stage
    parF = sensor (envM, parF, FL, FR, RA, SO);
    
    // Motor stage
    parF = motor (parF, m, n, SS);
    
    // Deposition stage
    envM = deposition (parF, depT, envM);

    // Evaporation
    envM = evaporate(envM, decayT);
    
  }  

  return envM;
};
')

# Default aesthetics of the ggplot
opt <-  theme(panel.border = element_rect(color="black", fill = NA),
              legend.position = "none",
              axis.ticks       = element_blank(),
              panel.grid       = element_blank(),
              axis.title       = element_blank(),
              axis.text        = element_blank())

######################################################################
# Parameters
######################################################################
# Framework environment
imageW <- 800 # image width (pixels)
imageH <- 600 # image heigth (pixels)
decayT <- 0.1 # Trail-map chemoattractant diffusion decay factor

# Agent
FL <-  22.5 * pi / 180 # FL sensor angle from forward position (degrees, 22.5 - 45)
FR <- -22.5 * pi / 180 # FR sensor angle from forward position (degrees, 22.5 - 45)
RA <-  45 * pi / 180 # Agent rotation angle (degrees)
SO <- 6 # Sensor offset distance (pixels)
SS <- 1 # Step size-how far agent moves per step (pixels) 
depT <- 15 # Chemoattractant deposition per step

iters <- 2000 # Number if iterations
agents <- 1000 # Number of agents

######################################################################
# Initialization of environment layer and particle positions
######################################################################
# Environment matrix, initialized with zeros
envM <- matrix(0 , imageH, imageW)

# Create a magnetic disc
for (i in 1:nrow(envM)){
  for (j in 1:ncol(envM)){
    if(sqrt((i-imageH/2)^2+(j-imageH/2)^2)>imageH/8 &
       sqrt((i-imageH/2)^2+(j-imageH/2)^2)<imageH/6) envM[i,j]=5
  }
}

# Place agents in a circle
tibble(h = seq(from = 0, to = 2*pi, length.out = agents)) %>% 
  mutate(x = (imageH/20)*cos(h)+imageH/2,
         y = (imageH/20)*sin(h)+imageH/2,
         h = jitter(h+pi, amount = 0) ) -> parF


# Make agents dance
envM <- physarum(envM, parF, decayT, FL, FR, 
                 RA, SO, SS, depT, iters)

# Transform resulting environment matrix into data frame
df <- melt(envM)
colnames(df) <- c("x","y","v") # to name columns

# Pick a top palette from colourlovers
palette <- sample(clpalettes('top'), 1)[[1]] 
colors <- palette %>% swatch %>% .[[1]]

# Do the plot
ggplot(data = df %>% filter(v>0), aes(x = x, y = y, fill = log(v))) + 
  geom_raster(interpolate = TRUE) +
  coord_equal() +
  scale_fill_gradientn(colours = colors) +
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0)) +
  opt -> plot

plot

# Do you like it? Save it!
# ggsave("choose_a_name.png", plot, width  = 3, height = 3)
