library(generativeart) # devtools::install_github("cutterkom/generativeart")
library(ambient)
library(dplyr)
# set the paths
IMG_DIR <- "img/"
IMG_SUBDIR <- "everything/"
IMG_SUBDIR2 <- "handpicked/"
IMG_PATH <- paste0(IMG_DIR, 
                   IMG_SUBDIR)
LOGFILE_DIR <- "logfile/"
LOGFILE <- "logfile.csv"
LOGFILE_PATH <- paste0(LOGFILE_DIR, 
                       LOGFILE)
# create the directory structure
generativeart::setup_directories(IMG_DIR, 
                                 IMG_SUBDIR, 
                                 IMG_SUBDIR2, 
                                 LOGFILE_DIR)

# list of equations
equations <- list(# quote(runif(1, -1, 10) * x_i),
                  # quote(runif(1, -1, 10) * y_i),
                  # quote(runif(1, -1, 10) * x_i^2),
                  # quote(runif(1, -1, 10) * y_i^2),
                  # quote(runif(1, -1, 10) * x_i^3),
                  # quote(runif(1, -1, 10) * y_i^3),
                  quote(runif(1, -1, 10) * x_i^2 - sin(y_i^2)),
                  quote(runif(1, -1, 10) * y_i^3 - cos(x_i^2) * y_i^4),
                  quote(runif(1, -1, 10) * x_i^2 - sin(y_i^3)),
                  quote(runif(1, -1, 10) * y_i^3 - cos(x_i^3) * y_i^4),
                  quote(runif(1, -1, 10) * x_i^5 - sin(log(y_i^2))),
                  quote(runif(1, -1, 10) * y_i^2 - exp(cos(x_i^2)) * y_i^5),
                  quote(runif(1, -1, 10) * exp(x_i^2) - cos(y_i^2)),
                  quote(runif(1, -1, 10) * y_i^4 - cos(x_i^3) * log(abs(y_i^4))),
                  quote(runif(1, -1, 10) * x_i^5 - sin(y_i^2)),
                  quote(runif(1, -1, 10) * y_i^3 - sqrt(abs(cos(x_i^2))) * y_i^7),
                  quote(runif(1, -1, 10) * x_i^4 - sin(y_i^2)*sqrt(abs(x_i)+1)),
                  quote(runif(1, -1, 10) * y_i^3 - cos(x_i^8) * y_i^4),
                  quote(runif(1, -1, 10) * x_i^2 - sqrt(abs(asin(y_i^2)))),
                  quote(runif(1, -1, 10) * y_i^(3/7) - cosh(x_i^2) * exp((y_i^4)-x_i^4)),
                  quote(runif(1, -1, 10) * (-3 * x_i)^2 - sinh(y_i^3) * x_i^5),
                  quote(runif(1, -1, 10) * y_i^3 - cospi(x_i^3) * y_i^4),
                  quote(runif(1, -1, 10) * x_i^(2/5) - atan(log(y_i^2)) + exp(1 + log(x_i))),
                  quote(runif(1, -1, 10) * y_i^2 - exp(tanh(x_i^2)) * log(abs(y_i^5))),
                  quote(runif(1, -1, 10) * exp(x_i^3) - asinh(y_i^2)),
                  quote(runif(1, -1, 10) * y_i^4 - tanpi(x_i^3) * log(abs(y_i^4))),
                  quote(runif(1, -1, 10) * (-2) * x_i^5 - sin(y_i^2)),
                  quote(runif(1, -1, 10) * y_i^3 - sqrt(abs(acos(x_i^2))) * y_i^7),
                  quote(runif(1, -1, 10) * x_i^4 - tan(y_i^2)*sqrt(abs(x_i)+1)),
                  quote(runif(1, -1, 10) * y_i^(1/3) - acos(x_i^8) * y_i^4)
                  )
# chosen_eqns <- sample(equations, 2, replace=FALSE)
# 
# # include a specific formula, for example:
# my_formula <- list(
#   x = chosen_eqns[[1]],
#   y = chosen_eqns[[2]]
# )

# define n

n = 4+3*1

# loop

# number of loops

loops = 10

# colours (removes a lot of grey/gray)

colors <- colors()[-162:-240]
colors <- colors[-191:-276]

for (i in 1:loops) {
  chosen_eqns <- sample(equations, 2, replace=FALSE)
  
  # include a specific formula, for example:
  my_formula <- list(
    x = chosen_eqns[[1]],
    y = chosen_eqns[[2]]
  )
  
  # generate random numbers
  rand <- sample(1:length(colors), 4, replace = FALSE)
  rand_index <- sample(1:n, 2, replace = FALSE)
  gradient <- colorRampPalette(c(colors[rand[1]], colors[rand[2]], colors[rand[3]], colors[rand[4]]))(n)
  
  # call the main function to create five images with a polar coordinate system
  generativeart::generate_img(
    formula = my_formula,
    nr_of_img = sample(1:3, 1), # set the number of iterations
    polar = c(TRUE, FALSE)[sample(1:2, 1)],
    filetype = "png",
    color = gradient[rand_index[1]],
    background_color = gradient[rand_index[2]]
  )
}

