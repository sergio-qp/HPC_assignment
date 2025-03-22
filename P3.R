rm(list=ls())
library(ggplot2)
library(gridExtra)

# C.11, problem 6.22

# setting global vars, used in all following functions
set.seed(256)
MC = 1000 #Monte Carlo replicates
n = 50
B = 500 # number of permutations

#Declare our statistics for boot() function permutations later
pearson <- function(data, perm_index) {
  data[, 2] = data[, 2][perm_index]
  abs(cor(x = data[, 1], y = data[, 2], method = "pearson")) #return actual test statistic
}

spearman <- function(data, perm_index) {
  data[, 2] <- data[, 2][perm_index]
  abs(cor(x = data[, 1], y = data[, 2], method = "spearman"))
}

d_cov <- function(data, perm_index) { #working as intended, just very very small
  data[, 2] = data[, 2][perm_index]
  energy::dcov(data[, 1], data[, 2]) #returns actual statistic output
}

MC_permutations = function(x,MC,B){ #data, mc repetitions and permutations
  pvP = vector(length=MC) #pearson P values
  pvS = vector(length=MC) #spearman's rho P values
  pvD = vector(length=MC) #distance covariance P values
  
  for (i in 1:MC){
    P = boot::boot(data = x, statistic = pearson, sim = "permutation", R = B) #pearson
    pvP[i] = mean(P$t > P$t0)
    S = boot::boot(data = x, statistic = spearman, sim = "permutation", R = B) #spearman
    pvS[i] = mean(S$t > S$t0)
    D = boot::boot(data = x, statistic = d_cov, sim = "permutation", R = B) #distance covariance
    pvD[i] = mean(D$t > D$t0)
    print(i/MC) #to make sure it's running properly
  }
  out = data.frame(cbind(Pearson=pvP,Spearman=pvS,D_cov=pvD))
  print("Done!")
  return(out)
}

#function for making plot lists from our data for later
plotrow = function(x,p_vals){
  row = list(
    ggplot(x, aes(x = x,y=y)) +
      geom_point() +
      labs(x = "X",
           y = "Y")
    ,
    ggplot(p_vals, aes(x = Pearson)) +
      geom_histogram(fill = "skyblue", color = "black") +
      labs(x = "P-value",
           y = "Frequency")
    ,
    ggplot(p_vals, aes(x = Spearman)) +
      geom_histogram(fill = "skyblue", color = "black") +
      labs(x = "P-value",
           y = "Frequency")
    ,
    ggplot(p_vals, aes(x = D_cov)) +
      geom_histogram(fill = "skyblue", color = "black") +
      labs(x = "P-value",
           y = "Frequency")
  )
  return(row)
  # you can then just concat all the lists using c()
}

## 1) Independence
x = rnorm(n)
y = rnorm(n)
data1 = cbind(x,y)

test1 = MC_permutations(data1,MC,B)
plot_list = plotrow(data1,test1)
grid.arrange(grobs = plot_list, nrow = 1, ncol = 4)


## 2) Pearson Favoring
x = rnorm(n)
y = rnorm(n) * x + rnorm(n) 
data2 = cbind(x,y)

test2 = MC_permutations(data2,MC,B)
plot_list = plotrow(data2,test2)
grid.arrange(grobs = plot_list, nrow = 1, ncol = 4)

# Pearson is favored here because the relationship is linear  and the n is reasonably low

## 3) Spearman Favoring
x = rnorm(n)
y = rnorm(n-1) * -(x[n-1]^2)
y = c(y,9)
data3 = cbind(x,y)

test3 = MC_permutations(data3,MC,B)
plot_list = plotrow(data3,test3)
grid.arrange(grobs = plot_list, nrow = 1, ncol = 4)

# Spearman is favored here because it handles the extreme outlier much better than the other two,
# even though the correlation is 0

## 4) Equal Powers
x = rnorm(n)
y = (rnorm(n-5)*x[n-5])*runif(n-5)
y = c(y,runif(5,max=1.5))
data4 = cbind(x,y)

test4 = MC_permutations(data4,MC,B)
plot_list = plotrow(data4,test4)
grid.arrange(grobs = plot_list, nrow = 1, ncol = 4)

# All perform about the same because y has both a linear relationship with x, several outliers
# attached on and another runif() component to add extra variability, meaning no one statistic
# is favored over the other, as they all deal with their advantages as well as the others'


## 5) Spearman Undetectable
x <- rnorm(n)
y <- rnorm(n, sd = abs(x))
data5 = cbind(x,y)

test5 = MC_permutations(data5,MC,B)
plot_list = plotrow(data5,test5)
grid.arrange(grobs = plot_list, nrow = 1, ncol = 4)

# Undetectable by Spearman's rho because it cannot detect heteroscedasticity

# Print all
full_list=c(plotrow(data1,test1),
            plotrow(data2,test2),
            plotrow(data3,test3),
            plotrow(data4,test4),
            plotrow(data5,test5))
grid.arrange(grobs = full_list, nrow = 5, ncol = 4)



