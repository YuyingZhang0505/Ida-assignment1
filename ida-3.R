require(Mass)
set.seed(2)
n=500
mu1=mu2=mu3=0
sigma=matrix(c(1,0,0,0,1,0,0,0,1),3,3,byrow=T)
Z=mvrnorm(n,mu=c(mu1,mu2,mu3),Sigma=sigma)
Z1=Z[ ,1]
Z2=Z[ ,2]
Z3=Z[ ,3]
Y=data.frame(Y1=1+Z1,Y2=5+2*Z1+Z2)
Y[1:10, ]
Y1=Y[ ,1]
Y2=Y[ ,2]
#Y2[1:10]

# a=2,b=0
Y2_new=ifelse(2*Z1+Z3<0,NA,Y$Y2)
#Y2_new[1:10]
Y_new=data.frame(Y1=Y1,Y2_new=Y2_new)
#Y_new[1:10, ]
Y2_new=Y_new[ ,2]
Y2_obs=na.omit(Y2_new)
#Y2_obs[1:10]

# Display
plot(density(Y2_obs),lwd=2,col="blue",main="Marginal distribution",xlab="Y2",ylim=c(0,0.25))
lines(density(Y2),lwd=2,col="red")
legend(8,0.20,legend=c("Observed data","Completed data"),col=c("blue","red"),lty=c(1,1),lwd=c(2,2),bty="n")

# question b
fit=lm(Y2_new~Y1,data=Y_new)
summary(fit)
fit$coefficients
set.seed(3)
predicted_y2=predict(fit,newdata=Y_new)+rnorm(nrow(Y_new),0,sigma(fit))
Y2_pre=ifelse(is.na(Y_new$Y2_new),predicted_y2,Y_new$Y2_new)
#Y2_pre[1:10]

# check the assumption of linearity and of constant variance
plot(fit$fitted.values,residuals(fit),xlab="Fitted values",ylab="Residuals")
#check the assumption of normality of error terms using a QQ-plot
qqnorm(rstandard(fit),,xlim=c(0,2),ylim=c(-0.5,2.5))
qqline(rstandard(fit),col=2)

# Display
plot(density(Y2_pre),lwd=2,col="red",main="Marginal distribution",xlab="Y2",xlim=c(-4,21))
lines(density(Y2),lwd=2,col="blue")
legend(6.50,0.20,legend=c("Completed imputation data","Completed data"),col=c("blue","red"),lty=c(1,1),lwd=c(2,2),bty="n")

# question c 
# a=0,b=2
Y2_3=ifelse(4*Z1+2*Z2+Z3<0,NA,Y$Y2)
#Y2_new[1:10]
Y_3=data.frame(Y1=Y1,Y2_3=Y2_3)
#Y_new[1:10, ]
Y2_3=Y_3[ ,2]
Y3_obs=na.omit(Y2_3)
#Y3_obs[1:10]

# Display
plot(density(Y3_obs),lwd=2,col="red",main="Marginal distribution",xlab="Y2_3",xlim=c(-3,18),ylim=c(0,0.30))
lines(density(Y2),lwd=2,col="blue")
legend(8,0.25,legend=c("Observed-3 data ","Completed data"),col=c("blue","red"),lty=c(1,1),lwd=c(2,2),bty="n")

# question d
fit2=lm(Y2_3~Y1,data=Y_3)
summary(fit2)
fit2$coefficients
set.seed(3)
predicted_y2_3=predict(fit2,newdata=Y_3)+rnorm(nrow(Y_3),0,sigma(fit2))
Y2_3_pre=ifelse(is.na(Y_3$Y2_3),predicted_y2_3,Y_3$Y2_3)
#Y2_3_pre[1:10]

# check the assumption of linearity and of constant variance
plot(fit2$fitted.values,residuals(fit2),xlab="Fitted values",ylab="Residuals")
#check the assumption of normality of error terms using a QQ-plot
qqnorm(rstandard(fit2),,xlim=c(0,2),ylim=c(-0.5,2.0))
qqline(rstandard(fit2),col=2)

# Display
plot(density(Y2_3_pre),lwd=2,col="red",main="Marginal distribution",xlab="Y2_3",xlim=c(-3,23),ylim=c(0,0.25))
lines(density(Y2),lwd=2,col="blue")
legend(7,0.20,legend=c("Completed imputation-3 data","Completed data"),col=c("blue","red"),lty=c(1,1),lwd=c(2,2),bty="n")