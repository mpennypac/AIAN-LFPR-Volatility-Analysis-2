aian_lfpr = read.csv('AIAN Labor Force Participation Rate.csv')
nat_lfpr = read.csv('National Labor Force Participation Rate.csv')
black_lfpr = read.csv('Black Labor Force Participation Rate.csv')
white_lfpr = read.csv('White Labor Force Participation Rate.csv')
hisp_lfpr = read.csv('Hispanic Labor Force Participation Rate.csv')
asian_lfpr = read.csv('Asian Labor Force Participation Rate.csv')


cat('Cor of AIAN & National', cor(aian_lfpr$labforceparticipation, nat_lfpr$LNU01300000), '\nP-Val for diff in variances', pf((var(aian_lfpr$labforceparticipation)/var(nat_lfpr$LNU01300000)), 275, 275, lower.tail=FALSE))
cat('\nCor of AIAN & Black', cor(aian_lfpr$labforceparticipation, black_lfpr$LNU01300006),'\nP-Val for diff in variances', pf((var(aian_lfpr$labforceparticipation)/var(black_lfpr$LNU01300006)), 275, 275, lower.tail=FALSE))
cat('\nCor of AIAN & White', cor(aian_lfpr$labforceparticipation, white_lfpr$LNU01300003),'\nP-Val for diff in variances', pf((var(aian_lfpr$labforceparticipation)/var(white_lfpr$LNU01300003)), 275, 275, lower.tail=FALSE))
cat('\nCor of AIAN & Hispanic', cor(aian_lfpr$labforceparticipation, hisp_lfpr$LNU01300009),'\nP-Val for diff in variances', pf((var(aian_lfpr$labforceparticipation)/var(hisp_lfpr$LNU01300009)), 275, 275, lower.tail=FALSE))
cat('\nCor of AIAN & Asian', cor(aian_lfpr$labforceparticipation, asian_lfpr$LNU01332183),'\nP-Val for diff in variances', pf((var(aian_lfpr$labforceparticipation)/var(asian_lfpr$LNU01332183)), 275, 275, lower.tail=FALSE))
cat('\nCor of Black & National', cor(black_lfpr$LNU01300006, nat_lfpr$LNU01300000), '\nP-Val for diff in variances', pf((var(black_lfpr$LNU01300006)/var(nat_lfpr$LNU01300000)), 275, 275, lower.tail=FALSE))
cat('\nCor of Black & White', cor(black_lfpr$LNU01300006, white_lfpr$LNU01300003), '\nP-Val for diff in variances', pf((var(black_lfpr$LNU01300006)/var(white_lfpr$LNU01300003)), 275, 275, lower.tail=FALSE))
cat('\nCor of Black & Hispanic', cor(black_lfpr$LNU01300006, hisp_lfpr$LNU01300009), '\nP-Val for diff in variances', pf((var(black_lfpr$LNU01300006)/var(hisp_lfpr$LNU01300009)), 275, 275, lower.tail=FALSE))
cat('\nCor of Black & Asian', cor(black_lfpr$LNU01300006, asian_lfpr$LNU01332183), '\nP-Val for diff in variances', pf((var(black_lfpr$LNU01300006)/var(asian_lfpr$LNU01332183)), 275, 275, lower.tail=FALSE))
cat('\nCor of White & National', cor(white_lfpr$LNU01300003, nat_lfpr$LNU01300000), '\nP-Val for diff in variances', pf((var(white_lfpr$LNU01300003)/var(nat_lfpr$LNU01300000)), 275, 275, lower.tail=FALSE))
cat('\nCor of White & Hispanic', cor(white_lfpr$LNU01300003, hisp_lfpr$LNU01300009), '\nP-Val for diff in variances', pf((var(white_lfpr$LNU01300003)/var(hisp_lfpr$LNU01300009)), 275, 275, lower.tail=FALSE))
cat('\nCor of White & Asian', cor(white_lfpr$LNU01300003, asian_lfpr$LNU01332183), '\nP-Val for diff in variances', pf((var(white_lfpr$LNU01300003)/var(asian_lfpr$LNU01332183)), 275, 275, lower.tail=FALSE))
cat('\nCor of Hispanic & National', cor(hisp_lfpr$LNU01300009, nat_lfpr$LNU01300000), '\nP-Val for diff in variances', pf((var(hisp_lfpr$LNU01300009)/var(nat_lfpr$LNU01300000)), 275, 275, lower.tail=FALSE))
cat('\nCor of Hispanic & Asian', cor(hisp_lfpr$LNU01300009, asian_lfpr$LNU01332183), '\nP-Val for diff in variances', pf((var(hisp_lfpr$LNU01300009)/var(asian_lfpr$LNU01332183)), 275, 275, lower.tail=FALSE))
cat('\nCor of Asian & National', cor(asian_lfpr$LNU01332183, nat_lfpr$LNU01300000), '\nP-Val for diff in variances', pf((var(asian_lfpr$LNU01332183)/var(nat_lfpr$LNU01300000)), 275, 275, lower.tail=FALSE))
plot(c(1:nrow(aian_lfpr)), aian_lfpr$labforceparticipation, type='l',col='blue',ylim=c(55,70))
lines(c(1:nrow(aian_lfpr)), nat_lfpr$LNU01300000, col='red')
lines(c(1:nrow(aian_lfpr)), black_lfpr$LNU01300006, col='black')
lines(c(1:nrow(aian_lfpr)), white_lfpr$LNU01300003, col='orange')
lines(c(1:nrow(aian_lfpr)), hisp_lfpr$LNU01300009, col='green')
lines(c(1:nrow(aian_lfpr)), asian_lfpr$LNU01332183, col='brown')
legend(0,61.5,legend=c('AIAN','National','Black','White','Hispanic','Asian'),col=c('blue','red','black','orange','green','brown'),lty=1:1)
## well that was uneventful, seems like AIAN LFPR is just seriously unique as far as racial/ethnic groups go
## let's compare things by state then instead


state_lfpr = read.csv('state-level-LFPR.csv')
rep_states = c()

for (i in 1:ncol(state_lfpr)) {
  p_val1 = pf((var(state_lfpr[,i])/var(nat_lfpr$LNU01300000)), 275, 275, lower.tail=FALSE)
  if (p_val1 < 0.01) {## this tells us if the state LFPR has a similar difference in variance from the national LFPR
    #cat('Sig Dif Btwn Variance of Nat LFPR and LFPR of: ', colnames(state_lfpr)[i], '\nP-Value: ', p_val, '\n')#Correlation with Nat LFPR: ', cor(state_lfpr[,i], nat_lfpr$LNU01300000), '\n')
    p_val2 = pf((var(state_lfpr[,i])/var(aian_lfpr$labforceparticipation)), 275, 275, lower.tail=FALSE)
    if (p_val2 > 0.1) {## this tells us if the state LFPR has a similar level of volatility as AIAN LFPR, which narrows our search for representative populations
      #cat('Insig Dif with Variance of AIAN LFPR\nP-Value: ', p_val, '\n\n')
      #cat('P-Value Between AIAN and State: ', p_val2, '\nSTATE ABBREVIATION: ', colnames(state_lfpr)[i], '\n')
      rep_states = c(rep_states, colnames(state_lfpr)[i])
    }
    #cat('P-Value Between National and State: ', p_val1, '\n------\n')
    
  }
  #cat('Correlation between ', colnames(state_lfpr)[i], ' LFPR and AIAN LFPR: ', cor(state_lfpr[,i], aian_lfpr$labforceparticipation), '\n\n')
}
#print('-----------------------------------------------------------------------------------------')

## here is our (potentially) representative list of states
print(rep_states)


cps_dec22 = read.csv('dec22pub.csv')
print(table(cps_dec22$ptdtrace))
aian_cps_dec22 = cps_dec22[cps_dec22$ptdtrace == 3,]
avg_age = mean(aian_cps_dec22$prtage[!is.na(aian_cps_dec22$prtage)])
age_survey_count = length(aian_cps_dec22$prtage[!is.na(aian_cps_dec22$prtage)])
age_missing_count = length(aian_cps_dec22$prtage[is.na(aian_cps_dec22$prtage)])


## we've got AIAN average age from Jan 2000 - Dec 2022 ! Nice!
avg_age = read.csv('compiled-data.csv')
avg_age = avg_age$Average.Age
aian_lfpr = read.csv('AIAN Labor Force Participation Rate.csv')
aian_lfpr = aian_lfpr$labforceparticipation

plot(c(1:length(avg_age)), log(avg_age), type='l', col='red', ylim=c(0, 5))
lines(c(1:length(avg_age)), aian_lfpr/100, col='blue')
## not gonna lie, not the greatest plot of all time here... let's just look
## at an F-test of a difference in variances here

cat('P-value for variance difference between average age and LFPR time series\n', pf((var(aian_lfpr)/var(avg_age)), 275, 275, lower.tail=FALSE))
## very significantly different... hmmm.... well I have low hopes then of age being a
## good explanation of the difference in cyclicalities but we've got the series so 
## let's test it anyway with an ARDL model first, then maybe we'll venture into 
## the world of Granger causality finally.

## first we have to determine our best guesses for the optimal lag values of each series,
## then we'll find an ARDL package this time so I don't have to construct the 
## lagged dataframes myself

## before we do that, let's obtain the volDiff series again (same method
## as before, just use auto.arima on AIAN and nationally, then take the difference of 
## the residual series)
library(forecast)
aian_model = auto.arima(aian_lfpr)
aian_vol = aian_model$residuals

nat_lfpr = read.csv('National Labor Force Participation Rate.csv')
nat_lfpr = nat_lfpr$LNU01300000
nat_model = auto.arima(nat_lfpr)
nat_vol = nat_model$residuals

volDiff = abs(aian_vol - nat_vol)

## just because I find it interesting, and technically these series aren't exactly
## the same as last time, plot the three time series
plot(c(1:length(aian_vol)), aian_vol, type='l', col='red', ylim=c(-5, 7),main='LFPR Volatility Series by Population',xlab='Months after January 2000',ylab='Difference between Predicted and Actual Values')
lines(c(1:length(aian_vol)), nat_vol, col='blue')
lines(c(1:length(aian_vol)), volDiff, col='black')
legend(0, 7, legend=c('AIANvol','NATvol','volDiff'),col=c('red','blue','black'),lty=1:1)
## once again we can see the obvious drastic difference in volatilities between the
## two populations

## (I'd just like to note, typically I would check here if avg_age was stationary,
## but the auto.arima function actually checks this for me and accommodates
## accordingly, so I omit this step)

max_lags = 1:20
age_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  age_test_model = arima(avg_age, order=c(lag,0,0))
  age_aic_vec = c(age_aic_vec,age_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR AVG_AGE: ', which(age_aic_vec==min(age_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(age_aic_vec),age_aic_vec,type='l')
## tested up to 20th, still seems to be 13 as the optimal lag level for age
## store residuals from the optimal age AR estimation
age_model = arima(avg_age, order=c(13,0,0))
age_res = age_model[['residuals']]
## "filter" volDiff with age_model coefficients
age_coef = age_model[['coef']]
filt_volDiff = age_coef[1] + age_coef[2]*lag(ts(volDiff),k=1) + age_coef[3]*lag(ts(volDiff),k=2) +
  age_coef[4]*lag(ts(volDiff),k=3) + age_coef[5]*lag(ts(volDiff),k=4) + age_coef[6]*lag(ts(volDiff),k=5) +
  age_coef[7]*lag(ts(volDiff),k=6) + age_coef[8]*lag(ts(volDiff),k=7) + age_coef[9]*lag(ts(volDiff),k=8) +
  age_coef[10]*lag(ts(volDiff),k=9) + age_coef[11]*lag(ts(volDiff),k=10) + age_coef[12]*lag(ts(volDiff),k=11) +
  age_coef[13]*lag(ts(volDiff),k=12) + age_coef[14]*lag(ts(volDiff),k=13)
## look at cross-correlogram between age_res and filt_volDiff to determine best candidates
## for avg_age lags in final model
ccf(filt_volDiff,age_res)
## looks like lags 1, 3, and 4 (for now)
## now we find optimal lags for volDiff for this model...
age_lags = cbind(lag(ts(avg_age),k=1), lag(ts(avg_age),k=3), lag(ts(avg_age),k=4))[4:length(avg_age),1:3]
volDiff_lags_model = lm(volDiff[1:nrow(age_lags)] ~ age_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looks like no MA components (from ACF), and lags 6, 9, and 42 (from PACF)
## a bit annoying that I have to get it to go all the way to lag 42 for this series
## but the PACF was too significant at that lag level for me to ignore it

## now we just have to find a good ARDL package and figure out how to use it...
#install.packages('ARDL')
library(ARDL)
## woah woah woah, this package has functionality similar to auto.arima!
## However, it requires I input a max order, and considering I found strong
## evidence that the 42nd lag of volDiff will play a strong role in producing the
## best model, I'll go with my lag findings first before messing with their "auto_ardl" function

#age_volDiff_ardl = ardl(volDiff ~ avg_age, order = c())
## well, turns out I can't exactly do what I want with this package.
## Guess I'll just reuse my old code, which is fine! Just lame I can't get it out of the
## way with one line of code

volDiff_lags = cbind(lag(ts(volDiff),k=6), lag(ts(volDiff),k=9), lag(ts(volDiff),k=42))[42:length(volDiff),1:3]
age_model_exog = cbind(volDiff_lags, age_lags[1:nrow(volDiff_lags),1:3])
age_model = arima(volDiff[1:nrow(age_model_exog)], order=c(0,0,0), xreg=age_model_exog, optim.control = list(maxit=1000))
print(age_model[['coef']])
print(age_model[['aic']])
print(age_model[['loglik']])
ARDL_age_loglik_1 = age_model[['loglik']]
summary(age_model)


## I am now realizing I may have gotten ahead of myself with that analysis, and
## it may have been more useful to look at the /difference/ in average age
## nationally and for AIAN. Unfortunately, this means collecting more data
## since the average age of the CPS isn't publicly available. But that's alright
## really since I have a system I can use to aggregate that data, and I wanted
## to get the weekly (or hourly if that's what's available) earnings
## of AIAN to see if that has a notable effect, too. BUT let's note here,
## I also have the gender distribution of AIAN from the CPS, but for both that series
## as well as the earnings series, we want to look at the /DIFFERENCE/ between
## AIAN and national levels, not just the AIAN levels. So in the data collection that
## will shortly commence, I'm collecting those datasets, too (the national gender
## distribution and weekly/hourly earnings, I mean).

## ok we've got the new file! No earnings unfortunately because the data was too funky,
## but we have proportion of the respondents in each of a list of 14 industries, first
## for AIAN only and then nationally, which I think may be a decent substitute even
## though some papers say it doesn't tend to explain much of level-LFPR (i.e. just LFPR,
## not the cyclicality of it). So now we can just take the AIAN and National difference
## for each series and run it through the above ARDL analysis and I'll call it a day there
## and write up my results.


library(forecast)
aian_lfpr = read.csv('AIAN Labor Force Participation Rate.csv')
aian_lfpr = aian_lfpr$labforceparticipation
aian_model = auto.arima(aian_lfpr)
aian_vol = aian_model$residuals

nat_lfpr = read.csv('National Labor Force Participation Rate.csv')
nat_lfpr = nat_lfpr$LNU01300000
nat_model = auto.arima(nat_lfpr)
nat_vol = nat_model$residuals

volDiff = abs(aian_vol - nat_vol)

plot(c(1:length(aian_vol)), aian_vol, type='l', col='red', ylim=c(-5, 7),main='LFPR Volatility Series by Population',xlab='Months after January 2000',ylab='Difference between Predicted and Actual Values')
lines(c(1:length(aian_vol)), nat_vol, col='blue')
legend(0, 7, legend=c('AIANvol','NATvol','volDiff'),col=c('red','blue','black'),lty=1:1)


data = read.csv('aian-and-nat-compiled-data.csv')

## get average age series and their difference
aian_age = data$AIAN.Average.Age
nat_age = data$National.Average.Age
age_diff = abs(aian_age - nat_age)
plot(c(1:length(aian_age)), aian_age, type='l', col='red',main='Average Age',ylim=c(25,45),ylab='Age in Years',xlab='Months after January 2000')
lines(c(1:length(nat_age)), nat_age, col='blue')
legend(0,45,legend=c('AIAN Average Age','National Average Age'),col=c('red','blue'),lty=1:1)

## plot volDiff and age_diff
plot(c(1:length(volDiff)), volDiff, type='l',col='red',ylim=c(0, 10),main='Age and Volatility Difference Series',xlab='Months after January 2000',ylab='Difference in AIAN and National Values')
lines(c(1:length(volDiff)), age_diff, col='blue')
legend(0,10,legend=c('Difference in LFPR Volatilities','Difference in Average Age'),col=c('red','blue'),lty=1:1)

## age_diff ARDL analysis
max_lags = 1:20
age_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  age_test_model = arima(age_diff, order=c(lag,0,0))
  age_aic_vec = c(age_aic_vec,age_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR AVG_AGE: ', which(age_aic_vec==min(age_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(age_aic_vec),age_aic_vec,type='l')
## tested up to 20th, still seems to be 10 as the optimal lag level for age
## store residuals from the optimal age AR estimation
age_model = arima(age_diff, order=c(10,0,0))
age_res = age_model[['residuals']]
## "filter" volDiff with age_model coefficients
age_coef = age_model[['coef']]
filt_volDiff = age_coef[1] + age_coef[2]*lag(ts(volDiff),k=1) + age_coef[3]*lag(ts(volDiff),k=2) +
  age_coef[4]*lag(ts(volDiff),k=3) + age_coef[5]*lag(ts(volDiff),k=4) + age_coef[6]*lag(ts(volDiff),k=5) +
  age_coef[7]*lag(ts(volDiff),k=6) + age_coef[8]*lag(ts(volDiff),k=7) + age_coef[9]*lag(ts(volDiff),k=8) +
  age_coef[10]*lag(ts(volDiff),k=9) + age_coef[11]*lag(ts(volDiff),k=10)
## look at cross-correlogram between age_res and filt_volDiff to determine best candidates
## for avg_age lags in final model
ccf(filt_volDiff,age_res)
## looks like lags 8, 12, 16, and 20
## now we find optimal lags for volDiff for this model...
age_lags = cbind(lag(ts(age_diff),k=8), lag(ts(age_diff),k=12), lag(ts(age_diff),k=16), lag(ts(age_diff),k=20))[20:length(age_diff),1:4]
volDiff_lags_model = lm(volDiff[1:nrow(age_lags)] ~ age_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looks like no MA components, but possible seasonal MA component at ~9th level (based on ACF)
## looks like lags 6, 9, and 12 (for now, based on PACF)
volDiff_lags = cbind(lag(ts(volDiff),k=6), lag(ts(volDiff),k=9), lag(ts(volDiff),k=12))[12:length(volDiff),1:3]
age_model_exog = cbind(volDiff_lags[1:min(c(nrow(volDiff_lags),nrow(age_lags))),1:3], age_lags[1:min(c(nrow(volDiff_lags),nrow(age_lags))),1:4])
## first try with no seasonal component
age_model = arima(volDiff[1:nrow(age_model_exog)], order=c(0,0,0), xreg=age_model_exog, optim.control = list(maxit=1000))
print(age_model[['coef']])
print(age_model[['aic']])
print(age_model[['loglik']])
ARDL_age_loglik_1 = age_model[['loglik']]
summary(age_model)
## we get AIC of 674.1059, LL of -328.053, 3/4 age coefficients have the expected sign (positive), only significant
## one is the 20th lag which has the negative sign
## try with seasonal MA component at 9th level
age_model = arima(volDiff[1:nrow(age_model_exog)], order=c(0,0,0), seasonal=c(0,0,9),xreg=age_model_exog, optim.control = list(maxit=1000))
print(age_model[['coef']])
print(age_model[['aic']])
print(age_model[['loglik']])
ARDL_age_loglik_2 = age_model[['loglik']]
summary(age_model)
## AIC of 679.1196, LL of -321.5598, same here for coefficients (literally the exact same description)
## we conclude the difference in average age doesn't appear to contribute much to our analysis, and promptly move on!


####################################################################
## we'll do gender distribution now

aian_lfpr = read.csv('AIAN Labor Force Participation Rate.csv')
aian_lfpr = aian_lfpr$labforceparticipation
aian_model = auto.arima(aian_lfpr)
aian_vol = aian_model$residuals

nat_lfpr = read.csv('National Labor Force Participation Rate.csv')
nat_lfpr = nat_lfpr$LNU01300000
nat_model = auto.arima(nat_lfpr)
nat_vol = nat_model$residuals

volDiff = abs(aian_vol - nat_vol)

plot(c(1:length(aian_vol)), aian_vol, type='l', col='red', ylim=c(-5, 7),main='LFPR Volatility Series by Population',xlab='Months after January 2000',ylab='Difference between Predicted and Actual Values')
lines(c(1:length(aian_vol)), nat_vol, col='blue')
legend(0, 7, legend=c('AIANvol','NATvol','volDiff'),col=c('red','blue','black'),lty=1:1)


data = read.csv('aian-and-nat-compiled-data.csv')

aian_male = data$AIAN.Male.Proportion*100
nat_male = data$National.Male.Proportion*100
male_diff = abs(aian_male - nat_male)
plot(c(1:length(aian_male)), aian_male, type='l', col='red',main='Male Proportion',ylim=c(44,52),ylab='Male Proportion',xlab='Months after January 2000')
lines(c(1:length(nat_male)), nat_male, col='blue')
legend(0,52,legend=c('AIAN Male Proportion','National Male Proportion'),col=c('red','blue'),lty=1:1)

## plot volDiff and male_diff
plot(c(1:length(volDiff)), volDiff, type='l',col='red',ylim=c(0, 10),main='Male Proportion and Volatility Difference Series',xlab='Months after January 2000',ylab='Difference in Series Values')
lines(c(1:length(volDiff)), male_diff, col='blue')
legend(0,10,legend=c('Difference in LFPR Volatilities','Difference in Male Proportion'),col=c('red','blue'),lty=1:1)

## male_diff ARDL analysis
max_lags = 1:20
male_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  male_test_model = arima(male_diff, order=c(lag,0,0))
  male_aic_vec = c(male_aic_vec,male_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR MALE_DIFF: ', which(male_aic_vec==min(male_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(male_aic_vec),male_aic_vec,type='l')
## tested up to 20th, seems to be 1 as optimal lag level for male_diff
## store residuals from the optimal age AR estimation
male_model = arima(male_diff, order=c(1,0,0))
male_res = male_model[['residuals']]
## "filter" volDiff with male_model coefficients
male_coef = male_model[['coef']]
filt_volDiff = male_coef[1] + male_coef[2]*lag(ts(volDiff),k=1)
## look at cross-correlogram between age_res and filt_volDiff to determine best candidates
## for avg_age lags in final model
ccf(filt_volDiff,male_res)
## looks like lags 5, 9, and 18
## now we find optimal lags for volDiff for this model...
male_lags = cbind(lag(ts(male_diff),k=5), lag(ts(male_diff),k=9), lag(ts(male_diff),k=18))[18:length(male_diff),1:3]
volDiff_lags_model = lm(volDiff[1:nrow(male_lags)] ~ male_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looks like no notable MA components, but possible seasonal MA component instead at ~11th level (based on ACF)
## looks like lags 9, 30, and 43 (for now, based on PACF)
volDiff_lags = cbind(lag(ts(volDiff),k=9), lag(ts(volDiff),k=30), lag(ts(volDiff),k=43))[43:length(volDiff),1:3]
male_model_exog = cbind(volDiff_lags[1:min(c(nrow(volDiff_lags),nrow(male_lags))),1:3], male_lags[1:min(c(nrow(volDiff_lags),nrow(male_lags))),1:3])
## first try with no seasonal component, but with 9th lag MA component
male_model = arima(volDiff[1:nrow(male_model_exog)], order=c(0,0,0), xreg=male_model_exog, optim.control = list(maxit=1000))
print(male_model[['coef']])
print(male_model[['aic']])
print(male_model[['loglik']])
ARDL_male_loglik_1 = male_model[['loglik']]
summary(male_model)
## we get AIC of 617.9443, LL of -300.9722, 1/3 coefficients have expected positive sign, none are
## significant, though
## try with seasonal MA component at 11th level
male_model = arima(volDiff[1:nrow(male_model_exog)], order=c(0,0,0), seasonal=c(0,0,11),xreg=male_model_exog, optim.control = list(maxit=1000))
print(male_model[['coef']])
print(male_model[['aic']])
print(male_model[['loglik']])
ARDL_male_loglik_2 = male_model[['loglik']]
summary(male_model)
## AIC of 632.3756, LL of -297.1878, with, again, same exact situation for the coefficients
## we conclude the difference in male proportion /may/ have something to do with explaining
## the volatility difference, but not a shocking amount more than we've already seen, so try just the
## difference in female proportions (even though it's likely the same considering it's just the
## mirror of this variable).


#####################################

aian_lfpr = read.csv('AIAN Labor Force Participation Rate.csv')
aian_lfpr = aian_lfpr$labforceparticipation
aian_model = auto.arima(aian_lfpr)
aian_vol = aian_model$residuals

nat_lfpr = read.csv('National Labor Force Participation Rate.csv')
nat_lfpr = nat_lfpr$LNU01300000
nat_model = auto.arima(nat_lfpr)
nat_vol = nat_model$residuals

volDiff = abs(aian_vol - nat_vol)

plot(c(1:length(aian_vol)), aian_vol, type='l', col='red', ylim=c(-5, 7),main='LFPR Volatility Series by Population',xlab='Months after January 2000',ylab='Difference between Predicted and Actual Values')
lines(c(1:length(aian_vol)), nat_vol, col='blue')
legend(0, 7, legend=c('AIANvol','NATvol','volDiff'),col=c('red','blue','black'),lty=1:1)


data = read.csv('aian-and-nat-compiled-data.csv')

aian_female = data$AIAN.Female.Proportion*100
nat_female = data$National.Female.Proportion*100
female_diff = abs(aian_female - nat_female)
plot(c(1:length(aian_female)), aian_female, type='l', col='red',main='Female Proportion of the Surveyed Population',ylim=c(48,58),ylab='Percentage of the Population',xlab='Months after January 2000')
lines(c(1:length(nat_female)), nat_female, col='blue')
legend(0,58,legend=c('AIAN Female Proportion','National Female Proportion'),col=c('red','blue'),lty=1:1)

## plot volDiff and female_diff
plot(c(1:length(volDiff)), volDiff, type='l',col='red',ylim=c(0, 6),main='Female Proportion and Volatility Difference Series',xlab='Months after January 2000',ylab='Difference in AIAN and National Values')
lines(c(1:length(volDiff)), female_diff, col='blue')
legend(0,6,legend=c('Difference in LFPR Volatilities','Difference in Female Proportion'),col=c('red','blue'),lty=1:1)

## female_diff ARDL analysis
max_lags = 1:20
female_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  female_test_model = arima(female_diff, order=c(lag,0,0))
  female_aic_vec = c(female_aic_vec,female_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR FEMALE_DIFF: ', which(female_aic_vec==min(female_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(female_aic_vec),female_aic_vec,type='l')
## tested up to 20th, seems to be 1 as optimal lag level for female_diff
## store residuals from the optimal age AR estimation
female_model = arima(female_diff, order=c(1,0,0))
female_res = female_model[['residuals']]
## "filter" volDiff with female_model coefficients
female_coef = female_model[['coef']]
filt_volDiff = female_coef[1] + female_coef[2]*lag(ts(volDiff),k=1)
## look at cross-correlogram between age_res and filt_volDiff to determine best candidates
## for avg_age lags in final model
ccf(filt_volDiff,female_res)
## looks like lags 5, 9, and 18
## now we find optimal lags for volDiff for this model...
female_lags = cbind(lag(ts(female_diff),k=5), lag(ts(female_diff),k=9), lag(ts(female_diff),k=18))[18:length(female_diff),1:3]
volDiff_lags_model = lm(volDiff[1:nrow(female_lags)] ~ female_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looks like no notable MA components, but possible seasonal MA component instead at ~11th level (based on ACF)
## looks like lags 9, 30, and 43 (for now, based on PACF)
volDiff_lags = cbind(lag(ts(volDiff),k=9), lag(ts(volDiff),k=30), lag(ts(volDiff),k=43))[43:length(volDiff),1:3]
female_model_exog = cbind(volDiff_lags[1:min(c(nrow(volDiff_lags),nrow(female_lags))),1:3], female_lags[1:min(c(nrow(volDiff_lags),nrow(female_lags))),1:3])
## first try with no seasonal component, but with 9th lag MA component
female_model = arima(volDiff[1:nrow(female_model_exog)], order=c(0,0,0), xreg=female_model_exog, optim.control = list(maxit=1000))
print(female_model[['coef']])
print(female_model[['aic']])
print(female_model[['loglik']])
ARDL_female_loglik_1 = female_model[['loglik']]
summary(female_model)
## we get AIC of 617.9443, LL of -300.9722, 1/3 coefficients have expected positive sign, none are
## significant, though
## try with seasonal MA component at 11th level
female_model = arima(volDiff[1:nrow(female_model_exog)], order=c(0,0,0), seasonal=c(0,0,11),xreg=female_model_exog, optim.control = list(maxit=1000))
print(female_model[['coef']])
print(female_model[['aic']])
print(female_model[['loglik']])
ARDL_male_loglik_2 = female_model[['loglik']]
summary(female_model)
## AIC of 632.3756, LL of -297.1878, with, again, same exact situation for the coefficients
## luckily for  me this was just copying and pasting a bunch of code but... yeah
## obviously this produces the exact same analysis since the /difference/ in male and female
## proportions are going to be exactly the same even if the actual proportions themselves
## aren't. Duh. Anyway...


############################################################################################
## moving onto the difference in industry proportions, and we'll just go down the line

aian_lfpr = read.csv('AIAN Labor Force Participation Rate.csv')
aian_lfpr = aian_lfpr$labforceparticipation
aian_model = auto.arima(aian_lfpr)
aian_vol = aian_model$residuals

nat_lfpr = read.csv('National Labor Force Participation Rate.csv')
nat_lfpr = nat_lfpr$LNU01300000
nat_model = auto.arima(nat_lfpr)
nat_vol = nat_model$residuals

volDiff = abs(aian_vol - nat_vol)

plot(c(1:length(aian_vol)), aian_vol, type='l', col='red', ylim=c(-5, 7),main='LFPR Volatility Series by Population',xlab='Months after January 2000',ylab='Difference between Predicted and Actual Values')
lines(c(1:length(aian_vol)), nat_vol, col='blue')
legend(0, 7, legend=c('AIANvol','NATvol'),col=c('red','blue'),lty=1:1)


data = read.csv('aian-and-nat-compiled-data.csv')
## start with agriculture
aian_ag = data$AIAN.Agricultural.Proportion*100
nat_ag = data$National.Agricultural.Proportion*100
ag_diff = abs(aian_ag - nat_ag)
plot(c(1:length(aian_ag)), aian_ag, type='l', col='red',main='Proportion of the Surveyed Population Working in the Agricultural Industry',ylim=c(0,3),ylab='Percentage of the Population',xlab='Months after January 2000')
lines(c(1:length(nat_ag)), nat_ag, col='blue')
legend(0,3,legend=c('AIAN Agricultural Proportion','National Agricultural Proportion'),col=c('red','blue'),lty=1:1)

## plot volDiff and ag_diff
plot(c(1:length(volDiff)), volDiff, type='l',col='red',ylim=c(0, 10),main='Agricultural Proportion and Volatility Difference Series',xlab='Months after January 2000',ylab='Difference in AIAN and National Values')
lines(c(1:length(volDiff)), ag_diff, col='blue')
legend(0,10,legend=c('Difference in LFPR Volatilities','Difference in Agricultural Proportion'),col=c('red','blue'),lty=1:1)

## ag_diff ARDL analysis
max_lags = 1:20
ag_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  ag_test_model = arima(ag_diff, order=c(lag,0,0))
  ag_aic_vec = c(ag_aic_vec,ag_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR AG_DIFF: ', which(ag_aic_vec==min(ag_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(ag_aic_vec),ag_aic_vec,type='l')
## tested up to 20th, seems to be 1 as optimal lag level for ag_diff
## store residuals from the optimal age AR estimation
ag_model = arima(ag_diff, order=c(1,0,0))
ag_res = ag_model[['residuals']]
## "filter" volDiff with ag_model coefficients
ag_coef = ag_model[['coef']]
filt_volDiff = ag_coef[1] + ag_coef[2]*lag(ts(volDiff),k=1)
## look at cross-correlogram between ag_res and filt_volDiff to determine best candidates
## for ag_diff lags in final model
ccf(filt_volDiff,ag_res)
## looks like lags 5, 13, and 15
## now we find optimal lags for volDiff for this model...
ag_lags = cbind(lag(ts(ag_diff),k=5), lag(ts(ag_diff),k=13), lag(ts(ag_diff),k=15))[15:length(ag_diff),1:3]
volDiff_lags_model = lm(volDiff[1:nrow(ag_lags)] ~ ag_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looks like MA component at lag 9(?), looks like no seasonal components (based on ACF)
## looks like lags 2, 9, and 17 (for now, based on PACF)
volDiff_lags = cbind(lag(ts(volDiff),k=2), lag(ts(volDiff),k=9), lag(ts(volDiff),k=17))[17:length(volDiff),1:3]
ag_model_exog = cbind(volDiff_lags[1:min(c(nrow(volDiff_lags),nrow(ag_lags))),1:3], ag_lags[1:min(c(nrow(volDiff_lags),nrow(ag_lags))),1:3])
ag_model = arima(volDiff[1:nrow(ag_model_exog)], order=c(0,0,9), xreg=ag_model_exog, optim.control = list(maxit=1000))
print(ag_model[['coef']])
print(ag_model[['aic']])
print(ag_model[['loglik']])
ARDL_ag_loglik_1 = ag_model[['loglik']]
summary(ag_model)
## AIC of 679.7133, LL of -322.8567, 1/3 coefficients have expected positive sign, and it's (almost) the most
## significant of the three! Nice. Interpretation is with a 1 percentage point decrease in
## the proportion of the population working in the agricultural industry, we find a ~0.54 unit
## decrease in the LFPR volatility difference 13 months later. Cool!

#############

## now mining
aian_mine = data$AIAN.Mining.Proportion*100
nat_mine = data$National.Mining.Proportion*100
mine_diff = abs(aian_mine - nat_mine)
plot(c(1:length(aian_mine)), aian_mine, type='l', col='red',main='Proportion of the Surveyed Population Working in the Mining Industry',ylim=c(0,1.5),ylab='Percentage of the Population',xlab='Months after January 2000')
lines(c(1:length(nat_mine)), nat_mine, col='blue')
legend(0,1.5,legend=c('AIAN Mining Proportion','National Mining Proportion'),col=c('red','blue'),lty=1:1)

## plot volDiff and mine_diff
plot(c(1:length(volDiff)), volDiff, type='l',col='red',ylim=c(0, 5.75),main='Mining Proportion and Volatility Difference Series',xlab='Months after January 2000',ylab='Difference in AIAN and National Values')
lines(c(1:length(volDiff)), mine_diff, col='blue')
legend(0,5.75,legend=c('Difference in LFPR Volatilities','Difference in Mining Proportion'),col=c('red','blue'),lty=1:1)

## mine_diff ARDL analysis
max_lags = 1:20
mine_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  mine_test_model = arima(mine_diff, order=c(lag,0,0))
  mine_aic_vec = c(mine_aic_vec,mine_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR AG_DIFF: ', which(mine_aic_vec==min(mine_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(mine_aic_vec),mine_aic_vec,type='l')
## tested up to 20th, seems to be 9 as optimal lag level for mine_diff
## store residuals from the optimal age AR estimation
mine_model = arima(mine_diff, order=c(9,0,0))
mine_res = mine_model[['residuals']]
## "filter" volDiff with ag_model coefficients
mine_coef = mine_model[['coef']]
filt_volDiff = mine_coef[1] + mine_coef[2]*lag(ts(volDiff),k=1) + mine_coef[3]*lag(ts(volDiff),k=2) + 
  mine_coef[4]*lag(ts(volDiff),k=3) + mine_coef[5]*lag(ts(volDiff),k=4) + mine_coef[6]*lag(ts(volDiff),k=5) + 
  mine_coef[7]*lag(ts(volDiff),k=6) + mine_coef[8]*lag(ts(volDiff),k=7) + mine_coef[9]*lag(ts(volDiff),k=8) +
  mine_coef[10]*lag(ts(volDiff),k=9)
## look at cross-correlogram between mine_res and filt_volDiff to determine best candidates
## for ag_diff lags in final model
ccf(filt_volDiff,mine_res)
## looks like lags 3, 6, and 16
## now we find optimal lags for volDiff for this model...
mine_lags = cbind(lag(ts(mine_diff),k=5), lag(ts(mine_diff),k=13), lag(ts(mine_diff),k=15))[15:length(mine_diff),1:3]
volDiff_lags_model = lm(volDiff[1:nrow(mine_lags)] ~ mine_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looks like MA component at lag 1, 2, 3, 8, 9, 10, 11, 12, or 13(?), looks like no seasonal components (based on ACF)
## looks like lags 2, 9, and 21 (for now, based on PACF)
volDiff_lags = cbind(lag(ts(volDiff),k=2), lag(ts(volDiff),k=9), lag(ts(volDiff),k=21))[21:length(volDiff),1:3]
mine_model_exog = cbind(volDiff_lags[1:min(c(nrow(volDiff_lags),nrow(mine_lags))),1:3], mine_lags[1:min(c(nrow(volDiff_lags),nrow(mine_lags))),1:3])
## try first with 2nd MA
mine_model = arima(volDiff[1:nrow(mine_model_exog)], order=c(0,0,2), xreg=mine_model_exog, optim.control = list(maxit=1000))
print(mine_model[['coef']])
print(mine_model[['aic']])
print(mine_model[['loglik']])
ARDL_mine_loglik_1 = mine_model[['loglik']]
summary(mine_model)
## AIC 664.4765, LL -322.2382, all coefficients negative, 2/3 significant
## try with 3rd MA
mine_model = arima(volDiff[1:nrow(mine_model_exog)], order=c(0,0,3), xreg=mine_model_exog, optim.control = list(maxit=1000))
print(mine_model[['coef']])
print(mine_model[['aic']])
print(mine_model[['loglik']])
ARDL_mine_loglik_2 = mine_model[['loglik']]
summary(mine_model)
## AIC 666.1726, LL -322.0863, all coefficients negative again, all significant
## try with 4th MA
mine_model = arima(volDiff[1:nrow(mine_model_exog)], order=c(0,0,4), xreg=mine_model_exog, optim.control = list(maxit=1000))
print(mine_model[['coef']])
print(mine_model[['aic']])
print(mine_model[['loglik']])
ARDL_mine_loglik_2 = mine_model[['loglik']]
summary(mine_model)
## AIC 668.0372, LL -322.0186, all coefficients negative, all significant
## but we'll cut it off there for now


###############

## now construction
aian_cons = data$AIAN.Construction.Proportion*100
nat_cons = data$National.Construction.Proportion*100
cons_diff = abs(aian_cons - nat_cons)
plot(c(1:length(aian_cons)), aian_cons, type='l', col='red',main='Proportion of the Surveyed Population Working in the Construction Industry',ylim=c(2,7.5),ylab='Percentage of the Population',xlab='Months after January 2000')
lines(c(1:length(nat_cons)), nat_cons, col='blue')
legend(0,7.5,legend=c('AIAN Construction Proportion','National Construction Proportion'),col=c('red','blue'),lty=1:1)

## plot volDiff and cons_diff
plot(c(1:length(volDiff)), volDiff, type='l',col='red',ylim=c(0, 7),main='Construction Proportion and Volatility Difference Series',xlab='Months after January 2000',ylab='Difference in AIAN and National Values')
lines(c(1:length(volDiff)), cons_diff, col='blue')
legend(0,7,legend=c('Difference in LFPR Volatilities','Difference in Construction Proportion'),col=c('red','blue'),lty=1:1)

## cons_diff ARDL analysis
max_lags = 1:20
cons_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  cons_test_model = arima(cons_diff, order=c(lag,0,0))
  cons_aic_vec = c(cons_aic_vec,cons_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR CONS_DIFF: ', which(cons_aic_vec==min(cons_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(cons_aic_vec),cons_aic_vec,type='l')
## tested up to 20th, seems to be 10 as optimal lag level for cons_diff
## store residuals from the optimal age AR estimation
cons_model = arima(cons_diff, order=c(10,0,0))
cons_res = cons_model[['residuals']]
## "filter" volDiff with cons_model coefficients
cons_coef = cons_model[['coef']]
filt_volDiff = cons_coef[1] + cons_coef[2]*lag(ts(volDiff),k=1) + cons_coef[3]*lag(ts(volDiff),k=2) + 
  cons_coef[4]*lag(ts(volDiff),k=3) + cons_coef[5]*lag(ts(volDiff),k=4) + cons_coef[6]*lag(ts(volDiff),k=5) + 
  cons_coef[7]*lag(ts(volDiff),k=6) + cons_coef[8]*lag(ts(volDiff),k=7) + cons_coef[9]*lag(ts(volDiff),k=8) +
  cons_coef[10]*lag(ts(volDiff),k=9) + cons_coef[11]*lag(ts(volDiff),k=10)
## look at cross-correlogram between cons_res and filt_volDiff to determine best candidates
## for cons_diff lags in final model
ccf(filt_volDiff,cons_res)
## looks like lags 4, 9, 10, 11, 12, and 13
## now we find optimal lags for volDiff for this model...
cons_lags = cbind(lag(ts(cons_diff),k=4), lag(ts(cons_diff),k=9), lag(ts(cons_diff),k=10), lag(ts(cons_diff),k=11), lag(ts(cons_diff),k=12), lag(ts(cons_diff),k=13))[13:length(cons_diff),1:6]
volDiff_lags_model = lm(volDiff[1:nrow(cons_lags)] ~ cons_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looks like MA component at lag 2, 3, 7, or 9, looks like no seasonal components (based on ACF)
## looks like lags 2, 7, or 9 (for now, based on PACF)
volDiff_lags = cbind(lag(ts(volDiff),k=2), lag(ts(volDiff),k=7), lag(ts(volDiff),k=9))[9:length(volDiff),1:3]
cons_model_exog = cbind(volDiff_lags[1:min(c(nrow(volDiff_lags),nrow(cons_lags))),1:3], cons_lags[1:min(c(nrow(volDiff_lags),nrow(cons_lags))),1:6])
## try first with 2nd MA
cons_model = arima(volDiff[1:nrow(cons_model_exog)], order=c(0,0,2), xreg=cons_model_exog, optim.control = list(maxit=1000))
print(cons_model[['coef']])
print(cons_model[['aic']])
print(cons_model[['loglik']])
ARDL_cons_loglik_1 = cons_model[['loglik']]
summary(cons_model)
## AIC 692.2424, LL -333.1212, 2/6 coefficients have expected positive sign, neither significant
## try with 3rd MA
cons_model = arima(volDiff[1:nrow(cons_model_exog)], order=c(0,0,3), xreg=cons_model_exog, optim.control = list(maxit=1000))
print(cons_model[['coef']])
print(cons_model[['aic']])
print(cons_model[['loglik']])
ARDL_cons_loglik_1 = cons_model[['loglik']]
summary(cons_model)
## AIC 694.0168, LL -333.0084, same coefficient description
## try with 7th MA
cons_model = arima(volDiff[1:nrow(cons_model_exog)], order=c(0,0,7), xreg=cons_model_exog, optim.control = list(maxit=1000))
print(cons_model[['coef']])
print(cons_model[['aic']])
print(cons_model[['loglik']])
ARDL_cons_loglik_1 = cons_model[['loglik']]
summary(cons_model)
## AIC 693.2747, LL -328.6374, same coefficient description again, so we'll stop there for construction


###############

## now manufacturing
aian_manu = data$AIAN.Manufacturing.Proportion*100
nat_manu = data$National.Manufacturing.Proportion*100
manu_diff = abs(aian_manu - nat_manu)
plot(c(1:length(aian_manu)), aian_manu, type='l', col='red',main='Proportion of the Surveyed Population Working in the Manufacturing Industry',ylim=c(1.5,8),ylab='Percentage of the Population',xlab='Months after January 2000')
lines(c(1:length(nat_manu)), nat_manu, col='blue')
legend(0,8,legend=c('AIAN Manufacturing Proportion','National Manufacturing Proportion'),col=c('red','blue'),lty=1:1)

## plot volDiff and manu_diff
plot(c(1:length(volDiff)), volDiff, type='l',col='red',ylim=c(0, 6),main='Manufacturing Proportion and Volatility Difference Series',xlab='Months after January 2000',ylab='Difference in AIAN and National Values')
lines(c(1:length(volDiff)), manu_diff, col='blue')
legend(0,6,legend=c('Difference in LFPR Volatilities','Difference in Manufacturing Proportion'),col=c('red','blue'),lty=1:1)

## manu_diff ARDL analysis
max_lags = 1:20
manu_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  manu_test_model = arima(manu_diff, order=c(lag,0,0))
  manu_aic_vec = c(manu_aic_vec,manu_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR MANU_DIFF: ', which(manu_aic_vec==min(manu_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(manu_aic_vec),manu_aic_vec,type='l')
## tested up to 20th, seems to be 5 as optimal lag level for manu_diff
## store residuals from the optimal age AR estimation
manu_model = arima(manu_diff, order=c(5,0,0))
manu_res = manu_model[['residuals']]
## "filter" volDiff with manu_model coefficients
manu_coef = manu_model[['coef']]
filt_volDiff = manu_coef[1] + manu_coef[2]*lag(ts(volDiff),k=1) + manu_coef[3]*lag(ts(volDiff),k=2) + 
  manu_coef[4]*lag(ts(volDiff),k=3) + manu_coef[5]*lag(ts(volDiff),k=4) + manu_coef[6]*lag(ts(volDiff),k=5)
## look at cross-correlogram between cons_res and filt_volDiff to determine best candidates
## for manu_diff lags in final model
ccf(filt_volDiff,manu_res)
## looks like just lag 14 for now
## now we find optimal lags for volDiff for this model...
manu_lags = cbind(lag(ts(manu_diff),k=14))[14:length(manu_diff)]
volDiff_lags_model = lm(volDiff[1:length(manu_lags)] ~ manu_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looks like MA component at lag 9, I think it looks like no seasonal components (based on ACF)
## looks like lags 9 and... 50(?) (for now, based on PACF)
volDiff_lags = cbind(lag(ts(volDiff),k=9), lag(ts(volDiff),k=50))[50:length(volDiff),1:2]
manu_model_exog = cbind(volDiff_lags[1:min(c(nrow(volDiff_lags),length(manu_lags))),1:2], manu_lags[1:min(c(nrow(volDiff_lags),nrow(manu_lags)))])
manu_model = arima(volDiff[1:nrow(manu_model_exog)], order=c(0,0,9), xreg=manu_model_exog, optim.control = list(maxit=1000))
print(manu_model[['coef']])
print(manu_model[['aic']])
print(manu_model[['loglik']])
ARDL_manu_loglik_1 = manu_model[['loglik']]
summary(manu_model)
## AIC 609.6445, LL -290.8222, coefficient on manu 14th lag is positive and just about significant...
## interesting! Perhaps manufacturing is a big enough chunk of the labor force to have an
## effect on the LFPR volatility

###########

## now retail
aian_ret = data$AIAN.Retail.Proportion*100
nat_ret = data$National.Retail.Proportion*100
ret_diff = abs(aian_ret - nat_ret)
plot(c(1:length(aian_ret)), aian_ret, type='l', col='red',main='Proportion of the Surveyed Population Working in the Retail Industry',ylim=c(1.5,10),ylab='Percentage of the Population',xlab='Months after January 2000')
lines(c(1:length(nat_ret)), nat_ret, col='blue')
legend(0,10,legend=c('AIAN Retail Proportion','National Retail Proportion'),col=c('red','blue'),lty=1:1)

## plot volDiff and ret_diff
plot(c(1:length(volDiff)), volDiff, type='l',col='red',ylim=c(0, 6),main='Retail Proportion and Volatility Difference Series',xlab='Months after January 2000',ylab='Difference in AIAN and National Values')
lines(c(1:length(volDiff)), ret_diff, col='blue')
legend(0,6,legend=c('Difference in LFPR Volatilities','Difference in Retail Proportion'),col=c('red','blue'),lty=1:1)

## ret_diff ARDL analysis
max_lags = 1:20
ret_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  ret_test_model = arima(ret_diff, order=c(lag,0,0))
  ret_aic_vec = c(ret_aic_vec,ret_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR RET_DIFF: ', which(ret_aic_vec==min(ret_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(ret_aic_vec),ret_aic_vec,type='l')
## tested up to 20th, seems to be 13 as optimal lag level for ret_diff
## store residuals from the optimal age AR estimation
ret_model = arima(ret_diff, order=c(13,0,0))
ret_res = ret_model[['residuals']]
## "filter" volDiff with ret_model coefficients
ret_coef = ret_model[['coef']]
filt_volDiff = ret_coef[1] + ret_coef[2]*lag(ts(volDiff),k=1) + ret_coef[3]*lag(ts(volDiff),k=2) + 
  ret_coef[4]*lag(ts(volDiff),k=3) + ret_coef[5]*lag(ts(volDiff),k=4) + ret_coef[6]*lag(ts(volDiff),k=5) + 
  ret_coef[7]*lag(ts(volDiff),k=6) + ret_coef[8]*lag(ts(volDiff),k=7) + ret_coef[9]*lag(ts(volDiff),k=8) +
  ret_coef[10]*lag(ts(volDiff),k=9) + ret_coef[11]*lag(ts(volDiff),k=10) + ret_coef[12]*lag(ts(volDiff),k=11) +
  ret_coef[13]*lag(ts(volDiff),k=12) + ret_coef[14]*lag(ts(volDiff),k=13)
## look at cross-correlogram between ret_res and filt_volDiff to determine best candidates
## for cons_diff lags in final model
ccf(filt_volDiff,ret_res)
## looks like lags 8 and 13
## now we find optimal lags for volDiff for this model...
ret_lags = cbind(lag(ts(ret_diff),k=8), lag(ts(ret_diff),k=13))[13:length(ret_diff),1:2]
volDiff_lags_model = lm(volDiff[1:nrow(ret_lags)] ~ ret_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looks like MA component at lag 7, 9, 12, looks like no seasonal components (based on ACF)
## looks like lags 10 and 50 (for now, based on PACF)
volDiff_lags = cbind(lag(ts(volDiff),k=10), lag(ts(volDiff),k=50))[50:length(volDiff),1:2]
ret_model_exog = cbind(volDiff_lags[1:min(c(nrow(volDiff_lags),nrow(ret_lags))),1:2], ret_lags[1:min(c(nrow(volDiff_lags),nrow(ret_lags))),1:2])
## try first with 7th MA
ret_model = arima(volDiff[1:nrow(ret_model_exog)], order=c(0,0,7), xreg=ret_model_exog, optim.control = list(maxit=1000))
print(ret_model[['coef']])
print(ret_model[['aic']])
print(ret_model[['loglik']])
ARDL_ret_loglik_1 = ret_model[['loglik']]
summary(ret_model)
## AIC 609.0796, LL -291.5398, both coefficients are positive but insignificant
## try with 9th MA
ret_model = arima(volDiff[1:nrow(ret_model_exog)], order=c(0,0,9), xreg=ret_model_exog, optim.control = list(maxit=1000))
print(ret_model[['coef']])
print(ret_model[['aic']])
print(ret_model[['loglik']])
ARDL_ret_loglik_1 = ret_model[['loglik']]
summary(ret_model)
## AIC 606.1543, LL -288.0771, 1/2 coefficients are positive and both are pretty significant (positive is
## more significant)
## try with 12th MA
ret_model = arima(volDiff[1:nrow(ret_model_exog)], order=c(0,0,12), xreg=ret_model_exog, optim.control = list(maxit=1000))
print(ret_model[['coef']])
print(ret_model[['aic']])
print(ret_model[['loglik']])
ARDL_ret_loglik_1 = ret_model[['loglik']]
summary(ret_model)
## AIC 607.4725, LL -285.7363, 1/2 coefficients are positive and only positive coefficient
## is significant


###########

## now transportation
aian_trans = data$AIAN.Transportation.Proportion*100
nat_trans = data$National.Transportation.Proportion*100
trans_diff = abs(aian_trans - nat_trans)
plot(c(1:length(aian_trans)), aian_trans, type='l', col='red',main='Proportion of the Surveyed Population Working in the Transportation Industry',ylim=c(1,3.5),ylab='Percentage of Population',xlab='Months after January 2000')
lines(c(1:length(nat_trans)), nat_trans, col='blue')
legend(0,3.5,legend=c('AIAN Transportation Proportion','National Transportation Proportion'),col=c('red','blue'),lty=1:1)

## plot volDiff and trans_diff
plot(c(1:length(volDiff)), volDiff, type='l',col='red',ylim=c(0, 6),main='Transportation Proportion and Volatility Difference Series',xlab='Months after January 2000',ylab='Difference in AIAN and National Values')
lines(c(1:length(volDiff)), trans_diff, col='blue')
legend(0,6,legend=c('Difference in LFPR Volatilities','Difference in Transportation Proportion'),col=c('red','blue'),lty=1:1)

## trans_diff ARDL analysis
max_lags = 1:20
trans_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  trans_test_model = arima(trans_diff, order=c(lag,0,0))
  trans_aic_vec = c(trans_aic_vec,trans_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR TRANS_DIFF: ', which(trans_aic_vec==min(trans_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(trans_aic_vec),trans_aic_vec,type='l')
## tested up to 20th, seems to be 10 as optimal lag level for trans_diff
## store residuals from the optimal age AR estimation
trans_model = arima(trans_diff, order=c(10,0,0))
trans_res = trans_model[['residuals']]
## "filter" volDiff with trans_model coefficients
trans_coef = trans_model[['coef']]
filt_volDiff = trans_coef[1] + trans_coef[2]*lag(ts(volDiff),k=1) + trans_coef[3]*lag(ts(volDiff),k=2) + 
  trans_coef[4]*lag(ts(volDiff),k=3) + trans_coef[5]*lag(ts(volDiff),k=4) + trans_coef[6]*lag(ts(volDiff),k=5) + 
  trans_coef[7]*lag(ts(volDiff),k=6) + trans_coef[8]*lag(ts(volDiff),k=7) + trans_coef[9]*lag(ts(volDiff),k=8) +
  trans_coef[10]*lag(ts(volDiff),k=9) + trans_coef[11]*lag(ts(volDiff),k=10)
## look at cross-correlogram between trans_res and filt_volDiff to determine best candidates
## for cons_diff lags in final model
ccf(filt_volDiff,trans_res)
## looks like lags 13 and 17
## now we find optimal lags for volDiff for this model...
trans_lags = cbind(lag(ts(trans_diff),k=13), lag(ts(trans_diff),k=17))[17:length(trans_diff),1:2]
volDiff_lags_model = lm(volDiff[1:nrow(trans_lags)] ~ trans_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looks like no MA component, and looks like no seasonal components (based on ACF)
## looks like lags 6, 9, 42, and 50 (for now, based on PACF)
volDiff_lags = cbind(lag(ts(volDiff),k=6), lag(ts(volDiff),k=9), lag(ts(volDiff),k=42), lag(ts(volDiff),k=50))[50:length(volDiff),1:4]
trans_model_exog = cbind(volDiff_lags[1:min(c(nrow(volDiff_lags),nrow(trans_lags))),1:4], trans_lags[1:min(c(nrow(volDiff_lags),nrow(trans_lags))),1:2])
trans_model = arima(volDiff[1:nrow(trans_model_exog)], order=c(0,0,0), xreg=trans_model_exog, optim.control = list(maxit=1000))
print(trans_model[['coef']])
print(trans_model[['aic']])
print(trans_model[['loglik']])
ARDL_trans_loglik_1 = trans_model[['loglik']]
summary(trans_model)
## AIC 605.8696, LL -294.9348, 1/2 positive coefficients but it isn't significant

################

## now information
aian_info = data$AIAN.Information.Proportion*100
nat_info = data$National.Information.Proportion*100
info_diff = abs(aian_info - nat_info)
plot(c(1:length(aian_info)), aian_info, type='l', col='red',main='Proportion of the Surveyed Population Working in the Information Industry',ylim=c(0,2),ylab='Percentage of the Population',xlab='Months after January 2000')
lines(c(1:length(nat_info)), nat_info, col='blue')
legend(0,2,legend=c('AIAN Information Proportion','National Information Proportion'),col=c('red','blue'),lty=1:1)

## plot volDiff and info_diff
plot(c(1:length(volDiff)), volDiff, type='l',col='red',ylim=c(0, 6),main='Information Proportion and Volatility Difference Series',xlab='Months after January 2000',ylab='Difference in AIAN and National Values')
lines(c(1:length(volDiff)), info_diff, col='blue')
legend(0,6,legend=c('Difference in LFPR Volatilities','Difference in Information Proportion'),col=c('red','blue'),lty=1:1)

## info_diff ARDL analysis
max_lags = 1:20
info_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  info_test_model = arima(info_diff, order=c(lag,0,0))
  info_aic_vec = c(info_aic_vec,info_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR INFO_DIFF: ', which(info_aic_vec==min(info_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(info_aic_vec),info_aic_vec,type='l')
## only tested up to 14th this time, seems to be 9 as optimal lag level for info_diff
## store residuals from the optimal age AR estimation
info_model = arima(info_diff, order=c(9,0,0))
info_res = info_model[['residuals']]
## "filter" volDiff with info_model coefficients
info_coef = info_model[['coef']]
filt_volDiff = info_coef[1] + info_coef[2]*lag(ts(volDiff),k=1) + info_coef[3]*lag(ts(volDiff),k=2) + 
  info_coef[4]*lag(ts(volDiff),k=3) + info_coef[5]*lag(ts(volDiff),k=4) + info_coef[6]*lag(ts(volDiff),k=5) + 
  info_coef[7]*lag(ts(volDiff),k=6) + info_coef[8]*lag(ts(volDiff),k=7) + info_coef[9]*lag(ts(volDiff),k=8) +
  info_coef[10]*lag(ts(volDiff),k=9)
## look at cross-correlogram between info_res and filt_volDiff to determine best candidates
## for cons_diff lags in final model
ccf(filt_volDiff,info_res)
## looks like lags 2 and 3
## now we find optimal lags for volDiff for this model...
info_lags = cbind(lag(ts(info_diff),k=2), lag(ts(info_diff),k=3))[3:length(info_diff),1:2]
volDiff_lags_model = lm(volDiff[1:nrow(info_lags)] ~ info_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looks like lag 9 or 12 MA component, and looks like no seasonal components (based on ACF)
## looks like lags 9, 12, 43, 50, and 60 (for now, based on PACF)
volDiff_lags = cbind(lag(ts(volDiff),k=9), lag(ts(volDiff),k=12), lag(ts(volDiff),k=43), lag(ts(volDiff),k=50), lag(ts(volDiff),k=60))[60:length(volDiff),1:5]
info_model_exog = cbind(volDiff_lags[1:min(c(nrow(volDiff_lags),nrow(info_lags))),1:5], info_lags[1:min(c(nrow(volDiff_lags),nrow(info_lags))),1:2])
## try with 9th lag MA
info_model = arima(volDiff[1:nrow(info_model_exog)], order=c(0,0,9), xreg=info_model_exog, optim.control = list(maxit=1000))
print(info_model[['coef']])
print(info_model[['aic']])
print(info_model[['loglik']])
ARDL_info_loglik_1 = info_model[['loglik']]
summary(info_model)
## AIC 589.9642, LL -276.9821, 2/2 coefficients positive, neither significant though
## try with 11th lag MA
info_model = arima(volDiff[1:nrow(info_model_exog)], order=c(0,0,11), xreg=info_model_exog, optim.control = list(maxit=1000))
print(info_model[['coef']])
print(info_model[['aic']])
print(info_model[['loglik']])
ARDL_info_loglik_1 = info_model[['loglik']]
summary(info_model)
## AIC 592.2036, LL -276.1018, again 2/2 coefficients positive but neither significant


########

## now financial services
aian_fin = data$AIAN.Financial.Services.Proportion*100
nat_fin = data$National.Financial.Services.Proportion*100
fin_diff = abs(aian_fin - nat_fin)
plot(c(1:length(aian_fin)), aian_fin, type='l', col='red',main='Proportion of the Surveyed Population Working in the Financial Services Industry',ylim=c(0,5),ylab='Percentage of the Population',xlab='Months after January 2000')
lines(c(1:length(nat_fin)), nat_fin, col='blue')
legend(0,5,legend=c('AIAN Financial Services Proportion','National Financial Services Proportion'),col=c('red','blue'),lty=1:1)

## plot volDiff and fin_diff
plot(c(1:length(volDiff)), volDiff, type='l',col='red',ylim=c(0, 6),main='Financial Services Proportion and Volatility Difference Series',xlab='Months after January 2000',ylab='Difference in AIAN and National Values')
lines(c(1:length(volDiff)), fin_diff, col='blue')
legend(0,6,legend=c('Difference in LFPR Volatilities','Difference in Financial Services Proportion'),col=c('red','blue'),lty=1:1)

## fin_diff ARDL analysis
max_lags = 1:20
fin_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  fin_test_model = arima(fin_diff, order=c(lag,0,0))
  fin_aic_vec = c(fin_aic_vec,fin_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR FIN_DIFF: ', which(fin_aic_vec==min(fin_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(fin_aic_vec),fin_aic_vec,type='l')
## only tested up to 14th this time, seems to be 9 as optimal lag level for fin_diff
## store residuals from the optimal age AR estimation
fin_model = arima(fin_diff, order=c(9,0,0))
fin_res = fin_model[['residuals']]
## "filter" volDiff with fin_model coefficients
fin_coef = fin_model[['coef']]
filt_volDiff = fin_coef[1] + fin_coef[2]*lag(ts(volDiff),k=1) + fin_coef[3]*lag(ts(volDiff),k=2) + 
  fin_coef[4]*lag(ts(volDiff),k=3) + fin_coef[5]*lag(ts(volDiff),k=4) + fin_coef[6]*lag(ts(volDiff),k=5) + 
  fin_coef[7]*lag(ts(volDiff),k=6) + fin_coef[8]*lag(ts(volDiff),k=7) + fin_coef[9]*lag(ts(volDiff),k=8) +
  fin_coef[10]*lag(ts(volDiff),k=9)
## look at cross-correlogram between fin_res and filt_volDiff to determine best candidates
## for cons_diff lags in final model
ccf(filt_volDiff,fin_res)
## looks like lags 2, 6, and 11
## now we find optimal lags for volDiff for this model...
fin_lags = cbind(lag(ts(fin_diff),k=2), lag(ts(fin_diff),k=6), lag(ts(fin_diff),k=11))[11:length(fin_diff),1:3]
volDiff_lags_model = lm(volDiff[1:nrow(fin_lags)] ~ fin_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looks like lag 2, 3, 5, 7, 9 or 12 MA component, and looks like no seasonal components (based on ACF)
## looks like lags 2, 3, 7, 9, 43, and 50 (for now, based on PACF)
volDiff_lags = cbind(lag(ts(volDiff),k=2), lag(ts(volDiff),k=3), lag(ts(volDiff),k=7), lag(ts(volDiff),k=9), lag(ts(volDiff),k=43), lag(ts(volDiff),k=50))[50:length(volDiff),1:6]
fin_model_exog = cbind(volDiff_lags[1:min(c(nrow(volDiff_lags),nrow(fin_lags))),1:6], fin_lags[1:min(c(nrow(volDiff_lags),nrow(fin_lags))),1:3])
## try with 2nd lag MA
fin_model = arima(volDiff[1:nrow(fin_model_exog)], order=c(0,0,2), xreg=fin_model_exog, optim.control = list(maxit=1000))
print(fin_model[['coef']])
print(fin_model[['aic']])
print(fin_model[['loglik']])
ARDL_fin_loglik_1 = fin_model[['loglik']]
summary(fin_model)
## AIC 604.7218, LL -289.3609, 2/3 coefficients positive, only negative is significant though
## try with 3rd lag MA
fin_model = arima(volDiff[1:nrow(fin_model_exog)], order=c(0,0,3), xreg=fin_model_exog, optim.control = list(maxit=1000))
print(fin_model[['coef']])
print(fin_model[['aic']])
print(fin_model[['loglik']])
ARDL_fin_loglik_1 = fin_model[['loglik']]
summary(fin_model)
## AIC 604.6071, LL -288.3036, 2/3 coefficients positive, 11th (positive) lag is pretty significant
## try with 5th lag MA
fin_model = arima(volDiff[1:nrow(fin_model_exog)], order=c(0,0,5), xreg=fin_model_exog, optim.control = list(maxit=1000))
print(fin_model[['coef']])
print(fin_model[['aic']])
print(fin_model[['loglik']])
ARDL_fin_loglik_1 = fin_model[['loglik']]
summary(fin_model)
## AIC 607.5493, LL -287.7746, 2/3 positive coefficients, 11th lag (positive) significant again
## try with 7th lag MA
fin_model = arima(volDiff[1:nrow(fin_model_exog)], order=c(0,0,7), xreg=fin_model_exog, optim.control = list(maxit=1000))
print(fin_model[['coef']])
print(fin_model[['aic']])
print(fin_model[['loglik']])
ARDL_fin_loglik_1 = fin_model[['loglik']]
summary(fin_model)
## AIC 601.4218, LL -282.7109, 2/3 positive coefficients, only negative one is significant though
## try with 9th lag MA
fin_model = arima(volDiff[1:nrow(fin_model_exog)], order=c(0,0,9), xreg=fin_model_exog, optim.control = list(maxit=1000))
print(fin_model[['coef']])
print(fin_model[['aic']])
print(fin_model[['loglik']])
ARDL_fin_loglik_1 = fin_model[['loglik']]
summary(fin_model)
## AIC 607.9643, LL -283.9822, 2/3 positive, 11th is kind of significant
## try with 12th lag MA
fin_model = arima(volDiff[1:nrow(fin_model_exog)], order=c(0,0,12), xreg=fin_model_exog, optim.control = list(maxit=1000))
print(fin_model[['coef']])
print(fin_model[['aic']])
print(fin_model[['loglik']])
ARDL_fin_loglik_1 = fin_model[['loglik']]
summary(fin_model)
## AIC 612.953, LL -283.4765, 2/3 positive coefficients, again 11th is positive and significant


##########

## now business, somehow different from financial services? idk I'm just going through the motions here
aian_bus = data$AIAN.Business.Proportion*100
nat_bus = data$National.Business.Proportion*100
bus_diff = abs(aian_bus - nat_bus)
plot(c(1:length(aian_bus)), aian_bus, type='l', col='red',main='Proportion of the Surveyed Population Working in the Business Industry',ylim=c(0,7),ylab='Percentage of the Population',xlab='Months after January 2000')
lines(c(1:length(nat_bus)), nat_bus, col='blue')
legend(0,7,legend=c('AIAN Business Proportion','National Business Proportion'),col=c('red','blue'),lty=1:1)

## plot volDiff and bus_diff
plot(c(1:length(volDiff)), volDiff, type='l',col='red',ylim=c(0, 6),main='Business Proportion and Volatility Difference Series',xlab='Months after January 2000',ylab='Difference in AIAN and National Values')
lines(c(1:length(volDiff)), bus_diff, col='blue')
legend(0,6,legend=c('Difference in LFPR Volatilities','Difference in Business Proportion'),col=c('red','blue'),lty=1:1)

## bus_diff ARDL analysis
max_lags = 1:20
bus_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  bus_test_model = arima(bus_diff, order=c(lag,0,0))
  bus_aic_vec = c(bus_aic_vec,bus_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR BUS_DIFF: ', which(bus_aic_vec==min(bus_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(bus_aic_vec),bus_aic_vec,type='l')
## seems to be 12 as optimal lag level for bus_diff
## store residuals from the optimal age AR estimation
bus_model = arima(bus_diff, order=c(12,0,0))
bus_res = bus_model[['residuals']]
## "filter" volDiff with bus_model coefficients
bus_coef = bus_model[['coef']]
filt_volDiff = bus_coef[1] + bus_coef[2]*lag(ts(volDiff),k=1) + bus_coef[3]*lag(ts(volDiff),k=2) + 
  bus_coef[4]*lag(ts(volDiff),k=3) + bus_coef[5]*lag(ts(volDiff),k=4) + bus_coef[6]*lag(ts(volDiff),k=5) + 
  bus_coef[7]*lag(ts(volDiff),k=6) + bus_coef[8]*lag(ts(volDiff),k=7) + bus_coef[9]*lag(ts(volDiff),k=8) +
  bus_coef[10]*lag(ts(volDiff),k=9) + bus_coef[11]*lag(ts(volDiff),k=10) + bus_coef[12]*lag(ts(volDiff),k=11) +
  bus_coef[13]*lag(ts(volDiff),k=12)
## look at cross-correlogram between bus_res and filt_volDiff to determine best candidates
## for cons_diff lags in final model
ccf(filt_volDiff,bus_res)
## looks like lags 2, 5, and 13
## now we find optimal lags for volDiff for this model...
bus_lags = cbind(lag(ts(bus_diff),k=2), lag(ts(bus_diff),k=5), lag(ts(bus_diff),k=13))[13:length(bus_diff),1:3]
volDiff_lags_model = lm(volDiff[1:nrow(bus_lags)] ~ bus_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looks like lag 9, 12, or 21 MA component, and looks like no seasonal components (based on ACF)
## looks like lags 9, 12, and 42 (for now, based on PACF)
volDiff_lags = cbind(lag(ts(volDiff),k=9), lag(ts(volDiff),k=12), lag(ts(volDiff),k=42))[42:length(volDiff),1:3]
bus_model_exog = cbind(volDiff_lags[1:min(c(nrow(volDiff_lags),nrow(bus_lags))),1:3], bus_lags[1:min(c(nrow(volDiff_lags),nrow(bus_lags))),1:3])
## try with 9th lag MA
bus_model = arima(volDiff[1:nrow(bus_model_exog)], order=c(0,0,9), xreg=bus_model_exog, optim.control = list(maxit=1000))
print(bus_model[['coef']])
print(bus_model[['aic']])
print(bus_model[['loglik']])
ARDL_bus_loglik_1 = bus_model[['loglik']]
summary(bus_model)
## AIC 632.1212, LL -299.0606, 2/3 coefficients positive, none significant
## try with 12th lag MA
bus_model = arima(volDiff[1:nrow(bus_model_exog)], order=c(0,0,12), xreg=bus_model_exog, optim.control = list(maxit=1000))
print(bus_model[['coef']])
print(bus_model[['aic']])
print(bus_model[['loglik']])
ARDL_bus_loglik_1 = bus_model[['loglik']]
summary(bus_model)
## AIC 635.9979, LL -297.999, 2/3 coefficients positive, none significant
## try with 21st lag MA
bus_model = arima(volDiff[1:nrow(bus_model_exog)], order=c(0,0,21), xreg=bus_model_exog, optim.control = list(maxit=1000))
print(bus_model[['coef']])
print(bus_model[['aic']])
print(bus_model[['loglik']])
ARDL_bus_loglik_1 = bus_model[['loglik']]
summary(bus_model)
## AIC647.4407, LL -294.7203, 1/3 positive coefficients


############

## now education and health services
aian_educ = data$AIAN.Education.and.Health.Proportion*100
nat_educ = data$National.Education.and.Health.Proportion*100
educ_diff = abs(aian_educ - nat_educ)
plot(c(1:length(aian_educ)), aian_educ, type='l', col='red',main='Proportion of the Surveyed Population Working in the Education and Health Industry',ylim=c(5.5,13),ylab='Percentage of the Population',xlab='Months after January 2000')
lines(c(1:length(nat_educ)), nat_educ, col='blue')
legend(5,13,legend=c('AIAN Education and Health Proportion','National Education and Health Proportion'),col=c('red','blue'),lty=1:1)

## plot volDiff and educ_diff
plot(c(1:length(volDiff)), volDiff, type='l',col='red',ylim=c(0, 6),main='Education and Health Proportion and Volatility Difference Series',xlab='Months after January 2000',ylab='Difference in AIAN and National Values')
lines(c(1:length(volDiff)), educ_diff, col='blue')
legend(0,6,legend=c('Difference in LFPR Volatilities','Difference in Education and Health Proportion'),col=c('red','blue'),lty=1:1)

## educ_diff ARDL analysis
max_lags = 1:20
educ_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  educ_test_model = arima(educ_diff, order=c(lag,0,0))
  educ_aic_vec = c(educ_aic_vec,educ_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR EDUC_DIFF: ', which(educ_aic_vec==min(educ_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(educ_aic_vec),educ_aic_vec,type='l')
## only got to lag 19 on the testing, seems to be 13 as optimal lag level for educ_diff
## store residuals from the optimal age AR estimation
educ_model = arima(educ_diff, order=c(13,0,0))
educ_res = educ_model[['residuals']]
## "filter" volDiff with educ_model coefficients
educ_coef = educ_model[['coef']]
filt_volDiff = educ_coef[1] + educ_coef[2]*lag(ts(volDiff),k=1) + educ_coef[3]*lag(ts(volDiff),k=2) + 
  educ_coef[4]*lag(ts(volDiff),k=3) + educ_coef[5]*lag(ts(volDiff),k=4) + educ_coef[6]*lag(ts(volDiff),k=5) + 
  educ_coef[7]*lag(ts(volDiff),k=6) + educ_coef[8]*lag(ts(volDiff),k=7) + educ_coef[9]*lag(ts(volDiff),k=8) +
  educ_coef[10]*lag(ts(volDiff),k=9) + educ_coef[11]*lag(ts(volDiff),k=10) + educ_coef[12]*lag(ts(volDiff),k=11) +
  educ_coef[13]*lag(ts(volDiff),k=12) + educ_coef[14]*lag(ts(volDiff),k=13)
## look at cross-correlogram between educ_res and filt_volDiff to determine best candidates
## for cons_diff lags in final model
ccf(filt_volDiff,educ_res)
## looks like lags 4 and 14
## now we find optimal lags for volDiff for this model...
educ_lags = cbind(lag(ts(educ_diff),k=4), lag(ts(educ_diff),k=14))[14:length(educ_diff),1:2]
volDiff_lags_model = lm(volDiff[1:nrow(educ_lags)] ~ educ_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looks like lag 6, 9, 11, and 12 MA component, and looks like no seasonal components (based on ACF)
## looks like lags 9, 13, and 42 (for now, based on PACF)
volDiff_lags = cbind(lag(ts(volDiff),k=9), lag(ts(volDiff),k=13), lag(ts(volDiff),k=42))[42:length(volDiff),1:3]
educ_model_exog = cbind(volDiff_lags[1:min(c(nrow(volDiff_lags),nrow(educ_lags))),1:3], educ_lags[1:min(c(nrow(volDiff_lags),nrow(educ_lags))),1:2])
## try with 6th lag MA
educ_model = arima(volDiff[1:nrow(educ_model_exog)], order=c(0,0,6), xreg=educ_model_exog, optim.control = list(maxit=1000))
print(educ_model[['coef']])
print(educ_model[['aic']])
print(educ_model[['loglik']])
ARDL_educ_loglik_1 = educ_model[['loglik']]
summary(educ_model)
## AIC 627.0122, LL -300.5061, 1/2 positive coefficient, insignificant
## try with 9th lag MA
educ_model = arima(volDiff[1:nrow(educ_model_exog)], order=c(0,0,9), xreg=educ_model_exog, optim.control = list(maxit=1000))
print(educ_model[['coef']])
print(educ_model[['aic']])
print(educ_model[['loglik']])
ARDL_educ_loglik_1 = educ_model[['loglik']]
summary(educ_model)
## AIC 628.5423, LL -298.2712, 1/2 positive coefficient, insignificant
## try with 11th lag MA
educ_model = arima(volDiff[1:nrow(educ_model_exog)], order=c(0,0,11), xreg=educ_model_exog, optim.control = list(maxit=1000))
print(educ_model[['coef']])
print(educ_model[['aic']])
print(educ_model[['loglik']])
ARDL_educ_loglik_1 = educ_model[['loglik']]
summary(educ_model)
## AIC 613.5186, LL -297.7593, 1/2 positive coefficient, insignificant
## try with 12th lag MA
educ_model = arima(volDiff[1:nrow(educ_model_exog)], order=c(0,0,12), xreg=educ_model_exog, optim.control = list(maxit=1000))
print(educ_model[['coef']])
print(educ_model[['aic']])
print(educ_model[['loglik']])
ARDL_educ_loglik_1 = educ_model[['loglik']]
summary(educ_model)
## AIC 626.2721, LL -294.136, 2/2 positive coefficients, 13th lag slightly significant


#########

## now hospitality
aian_hosp = data$AIAN.Hospitality.Proportion*100
nat_hosp = data$National.Hospitality.Proportion*100
hosp_diff = abs(aian_hosp - nat_hosp)
plot(c(1:length(aian_hosp)), aian_hosp, type='l', col='red',main='Proportion of the Surveyed Population Working in the Hospitality Industry',ylim=c(1,8.75),ylab='Percentage of the Population',xlab='Months after January 2000')
lines(c(1:length(nat_hosp)), nat_hosp, col='blue')
legend(0,8.75,legend=c('AIAN Hospitality Proportion','National Hospitality Proportion'),col=c('red','blue'),lty=1:1)

## plot volDiff and hosp_diff
plot(c(1:length(volDiff)), volDiff, type='l',col='red',ylim=c(0, 6),main='Hospitality Proportion and Volatility Difference Series',xlab='Months after January 2000',ylab='Difference in AIAN and National Values')
lines(c(1:length(volDiff)), hosp_diff, col='blue')
legend(0,6,legend=c('Difference in LFPR Volatilities','Difference in Hospitality Proportion'),col=c('red','blue'),lty=1:1)

## hosp_diff ARDL analysis
max_lags = 1:20
hosp_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  hosp_test_model = arima(hosp_diff, order=c(lag,0,0))
  hosp_aic_vec = c(hosp_aic_vec,hosp_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR HOSP_DIFF: ', which(hosp_aic_vec==min(hosp_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(hosp_aic_vec),hosp_aic_vec,type='l')
## seems to be 12 as optimal lag level for educ_diff
## store residuals from the optimal age AR estimation
hosp_model = arima(hosp_diff, order=c(12,0,0))
hosp_res = hosp_model[['residuals']]
## "filter" volDiff with hosp_model coefficients
hosp_coef = hosp_model[['coef']]
filt_volDiff = hosp_coef[1] + hosp_coef[2]*lag(ts(volDiff),k=1) + hosp_coef[3]*lag(ts(volDiff),k=2) + 
  hosp_coef[4]*lag(ts(volDiff),k=3) + hosp_coef[5]*lag(ts(volDiff),k=4) + hosp_coef[6]*lag(ts(volDiff),k=5) + 
  hosp_coef[7]*lag(ts(volDiff),k=6) + hosp_coef[8]*lag(ts(volDiff),k=7) + hosp_coef[9]*lag(ts(volDiff),k=8) +
  hosp_coef[10]*lag(ts(volDiff),k=9) + hosp_coef[11]*lag(ts(volDiff),k=10) + hosp_coef[12]*lag(ts(volDiff),k=11) +
  hosp_coef[13]*lag(ts(volDiff),k=12)
## look at cross-correlogram between hosp_res and filt_volDiff to determine best candidates
## for hosp_diff lags in final model
ccf(filt_volDiff,hosp_res)
## looks like lags 8 and 10
## now we find optimal lags for volDiff for this model...
hosp_lags = cbind(lag(ts(hosp_diff),k=8), lag(ts(hosp_diff),k=10))[10:length(hosp_diff),1:2]
volDiff_lags_model = lm(volDiff[1:nrow(hosp_lags)] ~ hosp_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looks like lag 12(?) MA component, and looks like no seasonal components (based on ACF)
## looks like lags 2, 12, 22, 34, and 35 (for now, based on PACF)
volDiff_lags = cbind(lag(ts(volDiff),k=2), lag(ts(volDiff),k=12), lag(ts(volDiff),k=22), lag(ts(volDiff),k=34), lag(ts(volDiff),k=35))[35:length(volDiff),1:5]
hosp_model_exog = cbind(volDiff_lags[1:min(c(nrow(volDiff_lags),nrow(hosp_lags))),1:5], hosp_lags[1:min(c(nrow(volDiff_lags),nrow(hosp_lags))),1:2])
## try with 12th lag MA
hosp_model = arima(volDiff[1:nrow(hosp_model_exog)], order=c(0,0,6), xreg=hosp_model_exog, optim.control = list(maxit=1000))
print(hosp_model[['coef']])
print(hosp_model[['aic']])
print(hosp_model[['loglik']])
ARDL_hosp_loglik_1 = hosp_model[['loglik']]
summary(hosp_model)
## AIC 644.2976, LL -307.1488, 1/2 positive coefficients, insignificant


#########

## now... other jobs
aian_other = data$AIAN.Other.Proportion*100
nat_other = data$National.Other.Proportion*100
other_diff = abs(aian_other - nat_other)
plot(c(1:length(aian_other)), aian_other, type='l', col='red',main='Proportion of Surveyed Population Working in All Other Industries',ylim=c(0,4),ylab='Percentage of the Population',xlab='Months after January 2000')
lines(c(1:length(nat_other)), nat_other, col='blue')
legend(0,4,legend=c('AIAN "Other" Proportion','National "Other" Proportion'),col=c('red','blue'),lty=1:1)

## plot volDiff and other_diff
plot(c(1:length(volDiff)), volDiff, type='l',col='red',ylim=c(0, 6),main='"Other" Proportion and Volatility Difference Series',xlab='Months after January 2000',ylab='Difference in AIAN and National Values')
lines(c(1:length(volDiff)), other_diff, col='blue')
legend(0,6,legend=c('Difference in LFPR Volatilities','Difference in "Other" Proportion'),col=c('red','blue'),lty=1:1)

## other_diff ARDL analysis
max_lags = 1:20
other_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  other_test_model = arima(other_diff, order=c(lag,0,0))
  other_aic_vec = c(other_aic_vec,other_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR OTHER_DIFF: ', which(other_aic_vec==min(other_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(other_aic_vec),other_aic_vec,type='l')
## only got to lag 18 in the testing, seems to be 11 as optimal lag level for educ_diff
## store residuals from the optimal age AR estimation
other_model = arima(other_diff, order=c(11,0,0))
other_res = other_model[['residuals']]
## "filter" volDiff with other_model coefficients
other_coef = other_model[['coef']]
filt_volDiff = other_coef[1] + other_coef[2]*lag(ts(volDiff),k=1) + other_coef[3]*lag(ts(volDiff),k=2) + 
  other_coef[4]*lag(ts(volDiff),k=3) + other_coef[5]*lag(ts(volDiff),k=4) + other_coef[6]*lag(ts(volDiff),k=5) + 
  other_coef[7]*lag(ts(volDiff),k=6) + other_coef[8]*lag(ts(volDiff),k=7) + other_coef[9]*lag(ts(volDiff),k=8) +
  other_coef[10]*lag(ts(volDiff),k=9) + other_coef[11]*lag(ts(volDiff),k=10) + other_coef[12]*lag(ts(volDiff),k=11)
## look at cross-correlogram between other_res and filt_volDiff to determine best candidates
## for other_diff lags in final model
ccf(filt_volDiff,other_res)
## looks like lags 2 and 10
## now we find optimal lags for volDiff for this model...
other_lags = cbind(lag(ts(other_diff),k=2), lag(ts(other_diff),k=10))[10:length(other_diff),1:2]
volDiff_lags_model = lm(volDiff[1:nrow(other_lags)] ~ other_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looks like lag 7, 9, or 12 MA component, and looks like no seasonal components (based on ACF)
## looks like lags 2, 7, 9, 12, and 35 (for now, based on PACF)
volDiff_lags = cbind(lag(ts(volDiff),k=2), lag(ts(volDiff),k=7), lag(ts(volDiff),k=9), lag(ts(volDiff),k=12), lag(ts(volDiff),k=35))[35:length(volDiff),1:5]
other_model_exog = cbind(volDiff_lags[1:min(c(nrow(volDiff_lags),nrow(other_lags))),1:5], other_lags[1:min(c(nrow(volDiff_lags),nrow(other_lags))),1:2])
## try with 7th lag MA
other_model = arima(volDiff[1:nrow(other_model_exog)], order=c(0,0,7), xreg=other_model_exog, optim.control = list(maxit=1000))
print(other_model[['coef']])
print(other_model[['aic']])
print(other_model[['loglik']])
ARDL_other_loglik_1 = other_model[['loglik']]
summary(other_model)
## AIC 635.7172, LL -301.8586, 1/2 positive coefficients, insignificant
## try with 9th lag MA
other_model = arima(volDiff[1:nrow(other_model_exog)], order=c(0,0,9), xreg=other_model_exog, optim.control = list(maxit=1000))
print(other_model[['coef']])
print(other_model[['aic']])
print(other_model[['loglik']])
ARDL_other_loglik_1 = other_model[['loglik']]
summary(other_model)
## AIC 636.6766, LL -300.3383, 1/2 positive coefficients, 2nd lag significant
## try with 12th lag MA
other_model = arima(volDiff[1:nrow(other_model_exog)], order=c(0,0,12), xreg=other_model_exog, optim.control = list(maxit=1000))
print(other_model[['coef']])
print(other_model[['aic']])
print(other_model[['loglik']])
ARDL_other_loglik_1 = other_model[['loglik']]
summary(other_model)
## AIC 643.5832, LL -300.7916, 1/2 positive coefficients, 2nd lag significant


###########

## now public admin
aian_padmin = data$AIAN.Public.Administration.Proportion*100
nat_padmin = data$National.Public.Administration.Proportion*100
padmin_diff = abs(aian_padmin - nat_padmin)
plot(c(1:length(aian_padmin)), aian_padmin, type='l', col='red',main='Proportion of the Surveyed Population Working in the Public Administration Industry',ylim=c(1,7),ylab='Percentage of the Population',xlab='Months after January 2000')
lines(c(1:length(nat_padmin)), nat_padmin, col='blue')
legend(0,7,legend=c('AIAN "Other" Proportion','National "Other" Proportion'),col=c('red','blue'),lty=1:1)

## plot volDiff and padmin_diff
plot(c(1:length(volDiff)), volDiff, type='l',col='red',ylim=c(0, 6),main='Public Admin Proportion and Volatility Difference Series',xlab='Months after January 2000',ylab='Difference in AIAN and National Values')
lines(c(1:length(volDiff)), padmin_diff, col='blue')
legend(0,6,legend=c('Difference in LFPR Volatilities','Difference in Public Admin Proportion'),col=c('red','blue'),lty=1:1)

## padmin_diff ARDL analysis
max_lags = 1:20
padmin_aic_vec = c()
for (lag in max_lags) {
  print(lag)
  padmin_test_model = arima(padmin_diff, order=c(lag,0,0))
  padmin_aic_vec = c(padmin_aic_vec,padmin_test_model[['aic']])
}
cat('OPTIMAL LAG LEVEL FOR PADMIN_DIFF: ', which(padmin_aic_vec==min(padmin_aic_vec)))
## just wanted to take a look at how the AIC changes with the lags;
plot(1:length(padmin_aic_vec),padmin_aic_vec,type='l')
## only got to lag 5 in the testing, seems to be 11 as optimal lag level for padmin_diff
## store residuals from the optimal age AR estimation
padmin_model = arima(padmin_diff, order=c(5,0,0))
padmin_res = padmin_model[['residuals']]
## "filter" volDiff with padmin_model coefficients
padmin_coef = padmin_model[['coef']]
filt_volDiff = padmin_coef[1] + padmin_coef[2]*lag(ts(volDiff),k=1) + padmin_coef[3]*lag(ts(volDiff),k=2) + 
  padmin_coef[4]*lag(ts(volDiff),k=3) + padmin_coef[5]*lag(ts(volDiff),k=4) + padmin_coef[6]*lag(ts(volDiff),k=5)
## look at cross-correlogram between padmin_res and filt_volDiff to determine best candidates
## for padmin_diff lags in final model
ccf(filt_volDiff,padmin_res)
## looks like lags 5, 9, and 11
## now we find optimal lags for volDiff for this model...
padmin_lags = cbind(lag(ts(padmin_diff),k=5), lag(ts(padmin_diff),k=9), lag(ts(padmin_diff),k=11))[11:length(padmin_diff),1:3]
volDiff_lags_model = lm(volDiff[1:nrow(padmin_lags)] ~ padmin_lags - 1)
volDiff_lags_model_res = volDiff_lags_model[['residuals']]
acf(volDiff_lags_model_res,lag.max=60)
pacf(volDiff_lags_model_res,lag.max=60)
## looks like lag no seeming MA component, and looks like no seasonal components (based on ACF)
## looks like lags 9 and 12 (for now, based on PACF)
volDiff_lags = cbind(lag(ts(volDiff),k=9), lag(ts(volDiff),k=12))[12:length(volDiff),1:2]
padmin_model_exog = cbind(volDiff_lags[1:min(c(nrow(volDiff_lags),nrow(padmin_lags))),1:2], padmin_lags[1:min(c(nrow(volDiff_lags),nrow(padmin_lags))),1:3])
## try with 7th lag MA
padmin_model = arima(volDiff[1:nrow(padmin_model_exog)], order=c(0,0,0), xreg=padmin_model_exog, optim.control = list(maxit=1000))
print(padmin_model[['coef']])
print(padmin_model[['aic']])
print(padmin_model[['loglik']])
ARDL_padmin_loglik_1 = padmin_model[['loglik']]
summary(padmin_model)
## AIC 688.1153, LL -337.0577, 2/3 positive coefficients, neither significant


###########

## now military
aian_mil = data$AIAN.Military.Proportion*100
nat_mil = data$National.Military.Proportion*100
mil_diff = abs(aian_mil - nat_mil)
plot(c(1:length(aian_mil)), aian_mil, type='l', col='red',main='Proportion of the Surveyed Population Working in the Military Industry',ylim=c(0,2.25),ylab='Percentage of the Population',xlab='Months after January 2000')
lines(c(1:length(nat_mil)), nat_mil, col='blue')
legend(0,2.25,legend=c('AIAN Military Proportion','National Military Proportion'),col=c('red','blue'),lty=1:1)

## plot volDiff and mil_diff
plot(c(1:length(volDiff)), volDiff, type='l',col='red',ylim=c(0, 10),main='Military Proportion and Volatility Difference Series',xlab='Months after January 2000',ylab='Difference in Series Values')
lines(c(1:length(volDiff)), mil_diff, col='blue')
legend(0,10,legend=c('Difference in LFPR Volatilities','Difference in Military Proportion'),col=c('red','blue'),lty=1:1)
## well, turns out we don't even have all the data we'd need for AIAN military employment, so we can skip this one!
