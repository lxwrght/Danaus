## Author: A.D. Wright
## Project: Monarch example - Girls Math and Science Day 2018


#########
## Part - Working directory, data, packages
#########

## Set working directory
setwd("C:/Users/Al/Files/PHD/25_AUG_2016/Research Documents/Zipkin-Lab/Girls-Math-Science_Day_2018/Danaus/JAGS")

## Load Monarch data
monarchs <- read.csv('monarchs_shiny.csv')

## Packages
library(jagsUI)


#########
## Part - Data Manipulation 
#########

#Rename columns
colnames(monarchs) <- c('Y','Ha','N')

#Reformat Year
attach(monarchs)
monarchs$Y <- monarchs$Y -  monarchs$Y[1]


#########
## Part - Jags
#########

jagsDat <- list(N=monarchs$N, Y=dim(monarchs)[1])

jagsPar <- c('r')
  


n.iter <- 100000
n.burnin <- 50000
n.adapt <- 50000
n.thin <- 10


jagsFit <- jags(data = jagsDat,
                parameters.to.save = jagsPar,
                model.file = "jags-file.txt",
                n.chains=3,
                n.iter=n.iter,
                n.adapt=n.adapt,
                n.burnin=n.burnin,
                n.thin=n.thin,
                parallel=T
)

jagsFit$mean$r



# input <- list(milk=NA, nectar=NA, log=NA)
# input$milk <- 0.5
# input$nectar <- 0.5
# input$log <- 0.5


# # Simulate linear models
# set.seed(10309393)
# growths <- seq(0.9, 1.1,length.out=100) + rnorm(100, 0, 0.1)
# milk <- seq(0.1,0.9,length.out=100) + rnorm(100,0, 0.05)
# nec <-  seq(0.25,0.75,length.out=100) + rnorm(100,0, 0.1)
# log <-  seq(0.8,0.3,length.out=100) + rnorm(100,0, 0.12)
# 
# x <- lm(growths ~ milk+nec+log)
# summary(x)

