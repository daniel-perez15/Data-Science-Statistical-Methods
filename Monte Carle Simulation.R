# This simulation will be conducted using R. Built-in functions such as rexp will be used
# to generate random numbers in exponential distribution. Further, the results will be 
# plotted using histogram and curve functions of R.
# Monte Carle Simulation study using R for block execution times of X1, X2, and X3

# Setting seed to reuse the same set of random variables
set.seed(1) 
		#
		MC_simulation = function(n) {
		  # Find block execution time for each of X1,X2 and X3 using exponential function.
			  X=rexp(3,0.2)
		  # Replicate the simulation n number of times on the max time obtained from exponential function of X1,X2 and X3
			  Z_replicate = replicate(n,max(rexp(3,0.2)))
		  # print the Mean value of block execution times for n replications 
			  print(mean(Z_replicate))
		  # Plot the histogram of replicated values (Question 3)
			  hist(Z_replicate,probability = TRUE)
		  # Get the values of  density function
			  density =function(Z_replicate) dexp(Z_replicate,0.2)
		  # Plot the smooth curve
			  curve( density, add = TRUE, col = "Red")
		}

# Experiment for 10,000 draws (Question 2) 
MC_simulation(10000)

# Replication of 10,000 draws experiment (Question 5)
replicate(5,MC_simulation(10000))

# Experiment for 1,000 draws (Question 6)
MC_simulation(1000)

# Experiment for 1,00,000 draws (Question 6)
MC_simulation(100000)




