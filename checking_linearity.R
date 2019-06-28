source("api/R_Client.R")
source("api/api_helper.R")
source("utils/data_processing.R")

base="optim.uni-muenster.de:5000/"
token="5d5ff737873440f7989f234f821f125e"

#expand
x = seq(-5,5,length.out = 7)
d = expand.grid(x=x,y=x,z=x)

e_f1 = fetch_test_data(d, 1, 3, token, base)
e_f2 = fetch_test_data(d, 2, 3, token, base)

#runif
x = runif(343, -5, 5)
y = runif(343, -5, 5)
z = runif(343, -5, 5)


d = data.frame(x,y,z)
d_f1 = fetch_test_data(d, 1, 3, token, base)
d_f2 = fetch_test_data(d, 2, 3, token, base)

## 1. Normality of Standard residuals 2. Homosecedasticity of Standard residuals 3. Independency of standard residuals
##check rediual
r<-lm(output~.,e_f1)

par(mfrow=c(2,2),mar=c(1,1,1,1))
plot(r) ## Normal Q-Q plot shows non-normality. the other plots tell us our dataset is not suitable for linear regression

r2<-glm(output~.,d_f1,family = gaussian)

par(mfrow=c(2,2),mar=c(1,1,1,1))
plot(r2) ## Normal Q-Q plot shows non-normality. the other plots tell us our dataset is not suitable for linear regression

# Check normality of standard residuals
n = rstandard(r)
# H0 : standard residuals have normal distribution, H1: No, it is not
shapiro.test(n) #p-value is tooo lower than 0.05. we chose H1. So, Normality of standard residuals is violated.
hist(n)

# Homosecedasticity of Standard residuals
plot(r) #plot Residuals vs fitted and scale locations are relevant to Homosecedasticity.So, Homosecedasticity of standard residuals is violated.
# you can find right plot Residuals vs fitted and scale locations on google :)

#Independency of standard residuals
#install.packages("lmtest")
library(lmtest) 
dwtest(r)
# dw value has a scale out of 0 to 4
# if dw value is close to 2, Independency of standard residual is okay
# if dw values is not close to 2, residuals have auto-correlation.

# Multicollinearity # Independent variables should have a perfect nonlinear relationship.

cor(e_f1) # correlation bewteen vaiables does not looks strong
cor(d_f1) # correlation bewteen vaiables does not looks strong
library(car)
vif(r) #variance_inflation_factor 5<vif<10 suspicious, abvobe than 10 absolutely Multicollinearity
sqrt(vif(r))>2 #(0.7) Multicollinearity 


# our dataset does not obey the precondition for linear regression. so, we can say that we should apply non-linear model.