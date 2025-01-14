#####################################
# CODE FOR BAYESIAN GAMETIC MARK-   #
# RECAPTURE ANALYSIS.               #
#                                   #
# Last updated: 2023-06-14          #
# Tim Frasier                       #
#####################################


#----------------------------#
# Load appropriate libraries #
#----------------------------#
library(cmdstanr)
library(posterior)
library(ggplot2)


#-------------------------------#
#   Read In & Organize the Data #
#-------------------------------#
data = read.table("../data/paternity_summary.csv", sep = ",", header = TRUE)

#--- Remove Years Without Sampled Calves ---#
data = data[complete.cases(data), ]
n1 = data$nCandidates
n2 = data$nGenotypedCalves
m = data$nPaternities
N = length(data[, 1])
year = 1:N

#--- Add 1 to n1, n2, and m, as per equation ---#
n1 = n1 + 1
n2 = n2 + 1
m = m + 1
       

#------------------------------#
# Save data as a list for STAN #
#------------------------------#
dataList = list (
  N = N,
  n2 = n2,
  m = m,
  year = year
)

#------------------------------#
#       Define the Model       #
#------------------------------#
stan_model = write_stan_file("
  data {
    int<lower=0> N;         // Number of years of data
    array[N] int n2;        // Number of genotyped calves in each year
    array[N] int m;         // Number of paternities assigned in each year
  }

  parameters {
    array[N] real<lower=0, upper=1> theta;   // Estimate of theta for each year
    real<lower=0, upper=1> thetaMean;  
    real<lower=0> thetaSD;
  }

  model {
  
    // Likelihood
    for (i in 1:N) {
      m[i] ~ binomial(n2[i], theta[i]);
    }
    
    // Priors
    for (i in 1:N) {
      theta[i] ~ normal(thetaMean, thetaSD);
    }
    
    // Hyperpriors
    thetaMean ~ uniform(0, 1);
    thetaSD ~ exponential(0.5);
  }
")

# translate the model to C++ and compile
# The "pedantic = TRUE" setting provides a more thorough check of the code. See manual.
model_stan = cmdstan_model(stan_file = stan_model, pedantic = FALSE)

# Run the model
fit_model = model_stan$sample(data = dataList,
                              chains = 3,
                              parallel_chains = 3,
                              refresh = 1000,
                              iter_warmup = 2000,
                              iter_sampling = 10000
)

#--------------------------#
#   Evaluate Model         #
#--------------------------#
print(fit_model)
#fit_model$cmdstan_diagnose()


#-------------------------#
#  Extract the data       #
#-------------------------#
posteriors = data.frame(as_draws_df(fit_model$draws()))
str(posteriors)
head(posteriors)

# Get just estimates
father.probs = posteriors[, 2:(N+1)]

# Multiply candidate males by probability
males = mapply('*', (1 / father.probs), n1)
males = males - 1
head(males)

#--------------------------#
# Extract Mean and HDIs    #
#--------------------------#
males.mean = rep(0, times = N)
males.low = rep(0, times = N)
males.high = rep(0, times = N)

for (i in 1:N) {
  males.mean[i] = mean(males[, i])
  males.low[i] = quantile(males[, i], probs = 0.025)
  males.high[i] = quantile(males[, i], probs = 0.975)
}


#-------------------------------#
# Organize Data Into Data Frame #
#-------------------------------#
results = data.frame(data$year, males.mean, males.low, males.high)

#------------------------#
#  Write Results to File #
#------------------------#
write.csv(results, "../results/results.csv", row.names = FALSE, quote = FALSE)

#-----------------------#
#  Plot the Data        #
#-----------------------#
ggplot(results) +
  theme_bw() +
  geom_point(aes(x = data.year, y = males.mean)) +
  geom_segment(aes(x = data.year, xend = data.year, y = males.low, yend = males.high)) +
  ylim(0, 350) +
  labs(
    x = "Year",
    y = "# of Males"
  )

#-------------------------------------------------#
# Add Results from Pace Model (years 1990 - 2021) #
#-------------------------------------------------#
pace.mean = c(146, 149, 157, 157, 162, 165, 169, 177, 179, 180, 181, 192, 199, 211, 218, 229, 240, 250, 261, 273, 280, 281, 278, 279, 280, 276, 266, 251, 227, 217, 201, 197)
pace.low = c(142, 144, 153, 153, 157, 162, 165, 172, 175, 176, 177, 188, 194, 206, 213, 225, 236, 246, 257, 269, 275, 276, 273, 272, 274, 268, 261, 245, 223, 213, 195, 190)
pace.high = c(150, 153, 161, 161, 165, 169, 173, 180, 183, 184, 184, 196, 202, 214, 221, 233, 244, 254, 265, 277, 284, 285, 283, 284, 286, 283, 273, 256, 232, 222, 206, 203)

#-- Get results for corresponding years --#
results.pace = results[results$data.year > 1989 & results$data.year < 2022, ]
results.pace$pace.mean = pace.mean
results.pace$pace.low = pace.low
results.pace$pace.high = pace.high

#-- Write results to file --#
write.csv(results.pace, "../results/results_pace.csv", row.names = FALSE, quote = FALSE)

#-- Plot the data --#
ggplot(results.pace) +
  theme_bw() +
  geom_ribbon(aes(x = data.year, ymin = males.low, ymax = males.high), fill = "grey", color = "gray47") +
  geom_point(aes(x = data.year, y = males.mean)) +
  geom_point(aes(x = data.year, y = pace.mean), color = "red") +
  geom_segment(aes(x = data.year, y = pace.low, yend = pace.high), color = "red") +
  ylim(0, 350) +
  labs(
    x = "Year",
    y = "# of Males"
  )
