library(data.table)
library(ggplot2)
library(lme4)
library(merTools)

# data load and cleaning
srrs2 <- read.table ("srrs2.dat", header=T, sep=",")
setDT(srrs2)
dat = srrs2[state == 'MN']
dat[, radon := activity]
dat[, log.radon := log (ifelse (radon==0, .1, radon))]

# fit a no pooling estimate
fitNoPool   <- lm(dat, formula = 'log.radon ~ county -1')
NoPoolRes <- data.table(summary(fitNoPool)$'coefficients',keep.rownames = T)
NoPoolRes[,county := substr(rn,start = 7,stop = nchar(rn))]
NoPoolRes <- NoPoolRes[,.(county, estimate = Estimate, sderr = `Std. Error` )]

# compare no pool estimates and sderr vs simple group means and stanard deviation
ggplot(dat[,.(simple_avg = mean(log.radon),simple_avg_sd = sd(log.radon)),by = county][NoPoolRes,on = 'county']
) + geom_point(aes(x=simple_avg,y = estimate))

# fit a multilevel model
fitMultilvl <- lmer(dat, formula = 'log.radon ~ 1 + (1|county) ')
multiLevelstdErr <- data.table(se.ranef(fitMultilvl)$county, keep.rownames = T)
setnames(multiLevelstdErr,names(multiLevelstdErr),c('county','mult_sderr'))
multiLevelest <-  data.table(data.table(coef(fitMultilvl)$county,keep.rownames = T))
setnames(multiLevelest,names(multiLevelest),c('county','mult_est'))
MultiLevelRes <-  multiLevelstdErr[multiLevelest,on = 'county']

res <- NoPoolRes[MultiLevelRes, on = 'county']

ggplot(res) + geom_errorbar(aes(x= county, y= estimate, 
                                ymin = estimate-2*sderr, ymax = estimate + 2*sderr),
                            position = "dodge") +
  geom_errorbar(aes(x= county, y= mult_est, 
                    ymin = mult_est-2*mult_sderr, ymax = mult_est + 2*mult_sderr), col = 'red',
                position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# compare R2 and correlation
dat[,predNoPool := predict(fitNoPool,dat)]
dat[,predMulti := predict(fitMultilvl,dat)]

cor(dat$log.radon,dat$predNoPool)
cor(dat$log.radon,dat$predMulti)

dat[, 1-sum((log.radon - predNoPool)^2)/sum((log.radon-mean(dat$log.radon))^2)]
dat[, 1-sum((log.radon - predMulti)^2)/sum((log.radon-mean(dat$log.radon))^2)]




