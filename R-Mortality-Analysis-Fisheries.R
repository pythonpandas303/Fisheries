###Note for equations, ()=subscript, <>=superscript###

###Mortality Rates Snippet for R###

###Total annual mortality (A) is the proportion of individuals in a population closed to IRE, that die in one year. Total annual survival rate (S) is the number of individuals that survive. Thus, A+S=1. (A) is computed by the following equation:

A= C(t)-C(t+1)      C(t+1)
--------------  = ---------- 
      C(t)          C(t)

C(t)= caught number of individuals at (t)ime.    

Instantaneous total mortality rate = Z
Z= log(C(t))-log(C(t+1))

Z is converted to A by A= 1-e<-z>
###

###Catch-Curve data is preferrable. Typical assumptions for data; mortality rate is constant across time and ages, recruitment is the same across sample cohorts, probability of capture is constant.###

###Required Libraries(FSA, car*(before dplyr to reduce conflicts with MASS),dplyr,magrittr,Rcapture)###

###Set WD###

###Load Libraries###

###Data Set Input###

###Direct data.frame input###

name <- data.frame(age=0:6, catch=c(1,2,3,4,5,6,7,8,9,10))
plot(log(catch)~age,data=name,
xlab="Age in Years", ylab="Log Catch", pch=19)

###Rcapture has Chapman-Robson built in as chapmanrobson(). 3 arguments are required.###

nameCR <- champmanrobson(catch~age,data=name,ages2use=2:6)

###Column bind (cbind()) confidence intervals and summary###

cbind(summary(nameCR),confint(nameCR))

###Plot results, confint shown in S & Z###
plot(nameCR)

###REGRESSION METHODS###

###Single Estimate###

name1 <-filter(name,age>=2) %>% mutate(lnct=log(catch))

lm <-lm(lnct~age, data=catch)

coef(lm)

###Maceina and Bettoli (1998) suggest that a weighted regression is more useful to elimante outliers particularly older individuals.#####

###Weighted Regression###

name1 %<>% mutate(wts=predict(lm))

lm2 <- lm(lnct~age,data=name1,weights=wts)

coef(lm2)

confint(lm2)

###CatchCurve is incuded in FSA as catchCurve() using the same 3 arguments as chapmanrobson()###

nameCC <- catchCurve(ct~age,data=catch,ages2use=2:6, weighted=TRUE)

cbind(summary(nameCC),confint(nameCC))
 plot(nameCC, pos.est="bottomleft")

###Total mortality from Cap-Recap###

name <- read.csv("filepath.csv")

headtail(name)

name.ch <- capHistSum(name, cols2use=-1)

name.ch$methodB.top

name.ch$methodB.bot


############EXAMPLE##########

    i=1 i=2 i=3 i=4 i=5 i=6 i=7 i=8 i=9
j=1  NA  22   4   0   0   0   0   0   0
j=2  NA  NA  90   4   2   0   0   0   0
j=3  NA  NA  NA  37  13   1   0   0   0
j=4  NA  NA  NA  NA  43   2   1   0   0
j=5  NA  NA  NA  NA  NA  96   2   2   0
j=6  NA  NA  NA  NA  NA  NA  88  10   1
j=7  NA  NA  NA  NA  NA  NA  NA  40   4
j=8  NA  NA  NA  NA  NA  NA  NA  NA  13
j=9  NA  NA  NA  NA  NA  NA  NA  NA  NA
#######################################

###Jolly-Seber Method###

###In R, performed by submitting capHistSum() to mrOpen()###

name.mr <- mrOpen(name.ch)

cbind(summary(name.mr,parm="phi"),confint(name.mr,parm="phi"))

##########EXAMPLE##########
      phi phi.se phi.lci phi.uci
i=1 0.411  0.088   0.237   0.584
i=2 0.349  0.045   0.261   0.436
i=3 0.370  0.071   0.231   0.509
i=4 0.218  0.031   0.159   0.278
i=5 0.437  0.041   0.356   0.517
i=6 0.451  0.069   0.316   0.585
i=7 0.268  0.072   0.127   0.409
i=8    NA     NA      NA      NA
i=9    NA     NA      NA      NA
################################

###Log-Linear Model, Cormack-Jolly-Seber###

name.op <- openp(name1[,-1])

name.op$model.fit

############EXAMPLE##########
             deviance     df       AIC
fitted model 86.33589    487  315.8965
######################################

name.op$trap.fit

##########EXAMPLE###########
                                  deviance     df
model with homogenous trap effect 85.19608    486
model with trap effect            81.12745    481
                                        AIC
model with homogenous trap effect  316.7567
model with trap effect             322.6881
#################################################

###Setting Confidence Interval###

conf.level <- 0.95

z <-qnorm(.5+conf.level/2)

###Running LL Model###

SurvivalRate <-data.frame(name.op$survivals) %>%
		mutate(phi.LCI=estimate-z*stderr,
		phi.UCI=estimate+z*stderr)

##########EXAMPLE#############
   estimate     stderr   phi.LCI   phi.UCI
1 0.4119850 0.08868670 0.2381623 0.5858078
2 0.3500535 0.04480100 0.2622451 0.4378618
3 0.3739616 0.07158820 0.2336513 0.5142719
4 0.2176769 0.03048458 0.1579282 0.2774255
5 0.4369129 0.04121960 0.3561240 0.5177019
6 0.4545608 0.06941919 0.3185017 0.5906199
7 0.2756015 0.07464255 0.1293048 0.4218982
8        NA         NA        NA        NA
##########################################
