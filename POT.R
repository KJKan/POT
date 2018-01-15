##############################################################################################
#                                                                                            #
# Interpretation of Kovacs & Conway's Verbal and Structural Relations Process Overlap Theory #
#                                                                                            #
#                             Author: Kees-Jan Kan                                           #
#                                                                                            #
##############################################################################################

# Clear working space
# not run
rm(list=ls(all=TRUE))

# load required libraries
library(MASS)
library(lavaan)
library(semPlot)

# According to POT-V: 
#   1. 'cognitive tests tap executive (domain-general) processes' and
#   2. 'cognitive tests tap domain-specific processes' 
#   3. 'executive processes are tapped in an overlapping manner across cognitive testst 
#   4. 'executive processes are required more often than domain-specific ones'
#   Ad. 4 , does this mean:
   # a. across domains:  E.g. more often in executive (fluid) tasks than in domain specific (e.g. verbal) tasks 
   # b. within specific domains:  That is, while completing, say, verbal tasks 
   # c. both? (assumed here)

# Define parameter values
ni <- 250          # number of Individuals
ng <- 500          # number of General executive capacities (per individual)
nv <- 500          # number of domain specific Verbal capacities (per individual)
ns <- 500          # number of domain specific visuoSpatial capacities (per individual)
nc <- ng + nv +ns  # total number of Capacities (per individual)
pgf <- .60         # the probability executive processes are required in executive tasks (fluid)
pde <- .50         # the probability executive processes are required in domain-specific tasks 
pds <- .35         # the probability domain-specific processes are required in domain-specific tasks

# Create variables (individual values of the capacities)
# Capacities are multivariate distributed, e.g. multivariate normal
# explicate assumptions
# assume means are all 0  (for example), as in multivariate normal standard distributions
mu <- rep(0,nc)
# assume variances are all 1 (for example), as in multivariate normal standard distributions 
sig <- rep(1,nc)
# assume capacities are all independent (in POT), so that the psi matrix is diagonal
psi <- diag(sig)
# draw values for the ni individuals 
data<-mvrnorm(ni,mu,psi)

# Introduce (test) sampling (That is, assume, tests/subtest/items sample from the capacities)
# following Figure 8 in Kovacs and Conway
# introduce three fluid subtests, three verbal items/tests, three visuospatial items/tests
# Create observed test/item scores
Gf_scores<-data[,1:ng]%*%replicate(3,rbinom(ng,1,p=pgf))
Gv_scores<-data[,1:ng]%*%replicate(3,rbinom(ng,1,p=pde))+data[,(ng+1):(ng+nv)]%*%replicate(3,rbinom(ng,1,p=pds))
Gs_scores<-data[,1:ng]%*%replicate(3,rbinom(ng,1,p=pde))+data[,(ng+nv+1):nc  ]%*%replicate(3,rbinom(ng,1,p=pds))
scores<-cbind(Gf_scores,Gv_scores,Gs_scores)

# the scores provide (nonperfect) indicators of the following 3 variables (total capacities)
# which can be interprted as the variables that 'underly' the test scores
# That is, when test sampling is present individual differences in the three (statistically independent) 
# total Fluid, Verbal and VisuoSpatial capacities will give rise to individual differences in subtest scores
sumGf<-rowSums(data[,1:ng])            # scores on Fluid (executive functioning) tests
sumGv<-rowSums(data[,(ng+1):(ng+nv)])  # scores on Verbal tests
sumGs<-rowSums(data[,(ng+nv+1):nc])    # Scores on VisuoSpatial tests

# Store data
obsdata<-as.data.frame(scores)
alldata<-as.data.frame(cbind(scores,sumGf,sumGv,sumGs))

# Print observed correlations
# (we observe a positive manifold)
round(cor(obsdata),1)

# Print all correlations
# (since all tests, including 'domain specific' tests,  tap - to some extent - executive capacities)
round(cor(alldata),1)

# Fit structural model(s)
# 1. A hierarchical model
g.model <- ' 
           Verbal =~ V4 + V5 + V6 
           Fluid  =~ V1 + V2 + V3
           Visual =~ V7 + V8 + V9 
        
           g =~ Fluid + Verbal + Visual

           # to prevent Heywood cases           
           Fluid ~~ vf*Fluid
           vf>0
           ' 

fit.g <- sem(g.model, data = alldata, orthogonal=TRUE, fixed.x=FALSE) 
summary(fit.g, standardized=TRUE)

# In principle fluid and general intelligence are one and the same factor 
# They both give an estimate of sumGf 
# Statistically they sometimes differ (this is simply due to sampling error)
gisFluid <- ' 
           Verbal =~ V4 + V5 + V6 
           Fluid  =~ V1 + V2 + V3
           Visual =~ V7 + V8 + V9 
        
           g =~ fl*Fluid + Verbal + Visual

           # fluid and general intelligence should be identical 
           # (they both estimate sumGf)           
           Fluid ~~ vf*Fluid
           vf==0
           ' 

check.gisFluid <- sem(gisFluid, data = alldata, orthogonal=TRUE, fixed.x=FALSE) 
summary(check.gisFluid, standardized=TRUE)

lavTestLRT(fit.g,check.gisFluid)

# 2. A (truncated) bifactor model
b.model <- ' 
           Fluid =~  V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9
           Verbal =~ V4 + V5 + V6 
           Visual =~ V7 + V8 + V9 
           '

fit.b <- sem(b.model, data = obsdata, orthogonal = TRUE, std.lv = TRUE ) 
summary(fit.b, standardized=TRUE)

# test the hierachical model against the bifactor model
lavTestLRT(fit.b,fit.g)

# plot models
layout(1:2)
semPaths(fit.b,'std',style='lisrel',edge.color=1,sizeLat=5,sizeMan = 2,
         nDigits=2,edge.label.cex=.6)

semPaths(fit.g,'std',style='lisrel',edge.color=1,sizeLat=5,sizeMan = 2,
         nDigits=2,edge.label.cex=.6)
