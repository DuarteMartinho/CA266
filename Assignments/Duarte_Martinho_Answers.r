# Student ID: 20410974
# Duarte Martinho
# Information about the system
reliability_probabilities <- c(.97, .68, .89, .93)
num_of_components_in_parallel <- c(1, 5, 3, 2)

# PART A 
calc_system_reliability <- function(num_of_components_in_parallel, reliability_probabilities)
{
    system_reliability_probability <- 1
    for (i in 1:length(num_of_components_in_parallel))
    {
        system_reliability_probability <- system_reliability_probability * (1 - ((1 - reliability_probabilities[i]) ^ num_of_components_in_parallel[i]))
        # print(1 - ((1 - reliability_probabilities[i]) ^ num_of_components_in_parallel[i]))
    }
    return(system_reliability_probability)
}

sprintf("Probability of system working = %f", calc_system_reliability(num_of_components_in_parallel, reliability_probabilities))

# PART B
num_iterations <- 100
simulate_system_reliability <- function(num_iterations, num_of_components_in_parallel, reliability_probabilities)
{
    total_system_reliability_probability = 0
    for (iterations in 1:num_iterations)
    {
        system_reliability_probability <- 1
        for (i in 1:length(num_of_components_in_parallel))
        {
            system_reliability_probability <- system_reliability_probability * (1 - ((1 - reliability_probabilities[i]) ^ num_of_components_in_parallel[i]))
            
        }
        
        if (runif(1) < system_reliability_probability) {
            total_system_reliability_probability <- total_system_reliability_probability + 1
        }
    }
    return(total_system_reliability_probability / num_iterations)
}
sprintf("Probability of system working for %i iterations is %f", num_iterations, simulate_system_reliability(num_iterations, num_of_components_in_parallel, reliability_probabilities))

# PART C
iteration_intervals <- seq(1,10000,50)

simulate_system_reliability <- function(num_iterations, num_of_components_in_parallel, reliability_probabilities)
{
    total_system_reliability_probability = 0
    for (iterations in 1:num_iterations)
    {
        system_reliability_probability <- 1
        for (i in 1:length(num_of_components_in_parallel))
        {
            system_reliability_probability <- system_reliability_probability * (1 - ((1 - reliability_probabilities[i]) ^ num_of_components_in_parallel[i]))
            
        }
        
        if (runif(1) < system_reliability_probability) {
            total_system_reliability_probability <- total_system_reliability_probability + 1
        }
    }
    return(total_system_reliability_probability / num_iterations)
}

count = 0
for (iteration_n in iteration_intervals )
{
    count = count + 1
    reliabilities[count] <- simulate_system_reliability(iteration_n, num_of_components_in_parallel, reliability_probabilities)
}

plot(iteration_intervals, reliabilities, xlab = 'Number of iterations in simulation', ylab = 'System Reliability')
abline(h=system_reliability_probability, col='red')