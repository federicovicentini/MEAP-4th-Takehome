# MEAP - 4th TAKEHOME
# Author: Federico Vicentini
# Date: 15/11/2022


# PRELIMINARY OPERATIONS


# Clear the variables
rm(list = ls())


# Set the working directory to source file location with
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ~  (this character will be needed later for copypasting)
phi <- 0.3
matrix <- matrix(0,2,2)
matrix[1,] <- c(phi, (1-phi))
matrix[2,] <- c((1-phi), phi)
matrix

beta <- 0.96
gamma <- 2
h <- 1.05
l <- 0.97


#CASE 1: 
#In this case we find ourselves in h at time t

pi <- matrix(NA, 2,2)
pi[1,]=c(phi, 2*(phi)^2+1-2*phi)
pi[2,]=c(1-phi, (2*phi)-2*(phi)^2)

yt_1=-log(beta*(pi[1,1]*(h)^(-gamma)+pi[2,1]*(l)^(-gamma)))
yt_2=-0.5*log(((beta)^2)*(pi[1,1]*pi[1,2]*(h*h)^(-gamma) + 
                          pi[2,1]*pi[2,2]*(l*l)^(-gamma) + 
                          pi[1,1]*pi[2,2]*(h*l)^(-gamma) + 
                          pi[2,1]*pi[1,2]*(l*h)^(-gamma)))

yieldcurve=c(yt_1,yt_2)
plot(yieldcurve, type="b")

yt_0=-log((h)^(-gamma))
yieldcurve=c(yt_0,yt_1,yt_2)
plot(yieldcurve, type="b")

#CASE 2: 
#In this case we find ourselves in l at time t

pi2 <- matrix(NA, 2,2)
pi2[1,]=c(1-phi, (2*phi)-2*(phi)^2)
pi2[2,]=c(phi, 2*(phi)^2+1-2*phi)

yt_1=-log(beta*(pi2[1,1]*(h)^(-gamma)+pi2[2,1]*(l)^(-gamma)))
yt_2=-0.5*log(((beta)^2)*(pi2[1,1]*pi2[1,2]*(h*h)^(-gamma) + 
                          pi2[2,1]*pi2[2,2]*(l*l)^(-gamma) + 
                          pi2[1,1]*pi2[2,2]*(h*l)^(-gamma) + 
                          pi2[2,1]*pi2[1,2]*(l*h)^(-gamma)))

yieldcurve=c(yt_1,yt_2)
plot(yieldcurve, type="b")


yt_0=-log((l)^(-gamma))
yieldcurve=c(yt_0,yt_1,yt_2)
plot(yieldcurve, type="b")

























