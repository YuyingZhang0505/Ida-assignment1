require(MASS)
load("databp.Rdata")

# complete case analysis 
logdoess=databp[ ,1]
bloodbp=databp[ ,2]

ind=which(is.na(databp$recovtime)==FALSE)
# the estimated mean recovtime is 19.273 with associated standard error of 2.603
mecom_coverall=mean(databp$recovtime,na.rm=TRUE)
secom_coverall=sd(databp$recovtime,na.rm=TRUE)/sqrt(length(ind))
mecom_coverall
secom_coverall

# the Pearson correlation between recovtime and the does is 0.2391
# the Pearson correlation between recovtime and blood pressure is -0.01953
cor1_1=cor(databp$recovtime,databp$logdose,use="complete")
cor1_2=cor(databp$recovtime,databp$bloodp,use="complete")
cor1_1
cor1_2

# mean imputation
recov_me=ifelse(is.na(databp$recovtime)==TRUE,mean(databp$recovtime,na.rm=TRUE),databp$recovtime)
# the estimated mean recovtime is 19.273 with associated standard error of 2.284
mean_meimpu=mean(recov_me)
sem_meimpu=sd(recov_me)/sqrt(length(recov_me))
mean_meimpu
sem_meimpu

# the Pearson correlation between recovtime and the does is 0.2151
# the Pearson correlation between recovtime and blood pressure is -0.01934
cor2_1=cor(recov_me,logdoess)
cor2_2=cor(recov_me,bloodbp)
cor2_1
cor2_2

# mean regression imputation
ffit=lm(recovtime~logdoess+bloodbp,data=databp)
summary(ffit)
ffit$coefficients

# check the assumption of linearity and of constant variance
plot(ffit$fitted.values,residuals(ffit),xlab="Fitted values",ylab="Residuals")
#check the assumption of normality of error terms using a QQ-plot
qqnorm(rstandard(ffit),,xlim=c(-0.2,2),ylim=c(0,1.5))
qqline(rstandard(ffit),col=2)

# the estimated mean recovtime is 19.444 with associated standard error of 2.313
predicted_recme=predict(ffit,newdata=databp)
rec_mereg=ifelse(is.na(databp$recovtime),predicted_recme,databp$recovtime)
mean_mereg=mean(rec_mereg)
sem_mereg=sd(rec_mereg)/sqrt(length(rec_mereg))
mean_mereg
sem_mereg

# the Pearson correlation between recovtime and the does is 0.2802
# the Pearson correlation between recovtime and blood pressure is -0.0111
cor3_1=cor(rec_mereg,logdoess)
cor3_2=cor(rec_mereg,bloodbp)
cor3_1
cor3_2

# stochastic regression imputation
set.seed(5)
predicted_recsto=predict(ffit,newdata=databp)+rnorm(nrow(databp),0,sigma(ffit))
rec_storeg=ifelse(is.na(databp$recovtime),predicted_recsto,databp$recovtime)
# the estimated mean recovtime is 20.00797 with associated standard error of 2.414
mean_storeg=mean(rec_storeg)
sem_storeg=sd(rec_storeg)/sqrt(length(rec_storeg))
mean_storeg
sem_storeg

# the Pearson correlation between recovtime and the does is 0.3327
# the Pearson correlation between recovtime and blood pressure is -0.0035
cor4_1=cor(rec_storeg,logdoess)
cor4_2=cor(rec_storeg,bloodbp)
cor4_1
cor4_2

# predictive mean matching
predicted_recme[ind]

mis=which(is.na(databp$recovtime)==TRUE)
length(mis)
mis_pre=predicted_recme[mis]
comp_pre=predicted_recme[ind]

# select the minimal squared difference of each missing subject
mis_pre[1]
sq_diff1=abs(comp_pre*2-mis_pre[1]*2)
donor4=names(which.min(sq_diff1))

mis_pre[2]
sq_diff2=abs(comp_pre*2-mis_pre[2]*2)
donor10=names(which.min(sq_diff2))


mis_pre[3]
sq_diff3=abs(comp_pre*2-mis_pre[3]*2)
donor22=names(which.min(sq_diff3))

# calculate the corresponding donor and the predictive value
donor=as.numeric(c(donor4,donor10,donor22))
pre_mean4=databp$recovtime[donor[1]]
pre_mean10=databp$recovtime[donor[2]]
pre_mean22=databp$recovtime[donor[3]]

# construct the completed recovtime variable
recv_premematch=c(databp$recovtime[1:3],pre_mean4,databp$recovtime[5:9],pre_mean10,
                  databp$recovtime[11:21],pre_mean22,databp$recovtime[23:25])
# the estimated mean recovtime is 19.44 with associated standard error of 2.4645
mean_premematch=mean(recv_premematch)
sem_premematch=sd(recv_premematch)/sqrt(length(recv_premematch))
mean_premematch
sem_premematch

# the Pearson correlation between recovtime and the does is 0.3038
# the Pearson correlation between recovtime and blood pressure is -0.0321
cor5_1=cor(recv_premematch,logdoess)
cor5_2=cor(recv_premematch,bloodbp)
cor5_1
cor5_2