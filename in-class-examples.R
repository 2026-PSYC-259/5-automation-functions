library(tidyverse)
library(broom)

# Functions and across ----

ds <- ds <- palmerpenguins::penguins %>% filter_out(species == "Chinstrap")

# Write a coefficient of variation fx (sd / mean * 100) that omits NAs and rounds the output
# Make digits an optional argument that controls how many digits to round the response
# Think about what would make a sensible default value for digits



# Using across, find the coefficient of variation for all numeric variables except year
# rounded to 1 decimals


# Correct output
# bill_length_mm bill_depth_mm flipper_length_mm body_mass_g
# <dbl>         <dbl>             <dbl>       <dbl>
#  12.2          12.0              7.44        19.4

# Iteration ----

# Make a wrapper around the base R t.test function that lets you pass a dv to test
# the effect of species (which will work because we filtered_out the 3rd level)
# Tidy the output before returning using broom::tidy and add a columns for DV
# Hint: Use the t.test formula interface and as.formula 

my_t_test <- function(dv) {

  
}

# If it works, running this...
my_t_test("bill_length_mm")
# Should give this...
# dv              estimate estimate1 estimate2 statistic  p.value parameter conf.low conf.high method             alternative
# <chr>             <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>    <dbl>     <dbl> <chr>              <chr>      
# bill_length_mm    -8.71      38.8      47.5     -24.7 3.09e-68      243.    -9.41     -8.02 Welch Two Sample … two.sided 

# Now, pick a map function to run the t-test on each
# of these DVs: bill_length_mm bill_depth_mm flipper_length_mm body_mass_g

