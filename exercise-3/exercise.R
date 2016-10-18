# Exercise 3: Data Frame Practice with `dplyr`.
# Use a different appraoch to accomplish the same tasks as exercise-1


# Install devtools package: allows installations from GitHub
install.packages('devtools')

# Install "fueleconomy" package from GitHub
devtools::install_github("hadley/fueleconomy")

# Require/library the fueleconomy package


# Which Accura model has the best hwy MPG in 2015? (without method chaining)


# Which Accura model has the best hwy MPG in 2015? (nesting functions)
Acura<- select(filter(vehicles, make == 'Acura' & year == 2015 & max(hwy)), model)

# Which Accura model has the best hwy MPG in 2015? (pipe operator)
best.hwy<- filter(vehicles, make == 'Acura' & year == 2015) %>% 
  filter(vehicles, hwy == max(hwy))
  select(vehicles, model)
  


### Bonus ###

# Write 3 functions, one for each approach.  Then, 
# Test how long it takes to perform each one 1000 times
