#Packages----
library(car)
library(mgcv)
library(lme4)
library(effects)
library(lattice)
library(MuMIn)

#Load data----
dat <- read.csv("data/combined_data.csv", header=T)
head(dat)
str(dat)
names(dat)

#EDA----
#MCP
summary(dat$mcp)
hist(dat$mcp, breaks=1000)
boxplot(dat$mcp)
dat2 <- dat[dat$mcp<10, ] #exclude outliers (TMP)
dim(dat); dim(dat2)
par(mfrow=c(2,2))
for (i in c(7:26,28:34,40)){
  plot(dat2$mcp ~ dat2[,i], main=names(dat2)[i])
}
for (i in c(2,27,36,37,41)){
  boxplot(dat2$mcp ~ dat2[,i], main=names(dat2)[i])
}

#KDE
summary(dat$kdearea)
hist(dat$kdearea, breaks=1000)
boxplot(dat$kdearea)
dat3 <- dat[dat$kdearea<10, ] #exclude outliers (TMP)
dim(dat); dim(dat3)
par(mfrow=c(2,2))
for (i in c(7:26,28:34,40)){
  plot(dat3$kdearea ~ dat3[,i], main=names(dat3)[i])
}
for (i in c(2,27,36,37,41)){
  boxplot(dat3$kdearea ~ dat3[,i], main=names(dat3)[i])
}

#KDE-MCP
plot(dat2$mcp, dat2$kdearea)
cor(dat2$mcp, dat2$kdearea) #strong correlation between the two measures of HR; probably OK to just focus on one

#Collinearity
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
png("results/Correlation.png", width=10000, height=10000, units="px")
pairs(dat2[,c(2,7:37,40,41)], lower.panel = panel.smooth, upper.panel = panel.cor,
      gap=0, row1attop=FALSE)
dev.off()

#>0.6 (~ >0.5)
#study area-sensor type
#study area-year
#~study area-slope
#sensor type-min act
#sensor type-year
#sd act-mean act
#corine mode-tree median (and ~mode)
#stats of slope, aspect, altitude with each other (inc. sd)
#slope-altitude
#~slope-altitude-aspect_sd
#~ndvi median/mean-ndvi sd
#non linear: tree mean-tree sd
#non linear: month-ndvi

#will have to check VIF of final model as well

dat4 <- dat3
#treat factors as such
dat4$sex <- as.factor(dat4$sex)
dat4$sensor_type <- as.factor(dat4$sensor_type)
dat4$study_areas_id <- as.factor(dat4$study_areas_id)
dat4$animals_id <- as.factor(dat4$animals_id)

#standardise covariates
names(dat4)
for (i in c(30, 40, 7, 9, 11)){ #select columns used below
  dat4[,i] <- (dat4[,i] - mean(dat4[,i]))/sd(dat4[,i])
}
#transform response
hist(dat4$kdearea,breaks=100)
hist(log(dat4$kdearea),breaks=100)
dat4$log_kde <- log(dat4$kdearea)


#GLMM----

fit0 <- lmer(kdearea ~ mean_act + sensor_type + sex + age + ndvi_Mean + slope_Mean + tree_Mean  + (1|study_areas_id/animals_id), data=dat4, REML=F, na.action=na.fail)
plot(fit0) #problem with natural response
fit1 <- lmer(log_kde ~ mean_act + sensor_type + sex + age + ndvi_Mean + slope_Mean + tree_Mean  + (1|study_areas_id/animals_id), data=dat4, REML=F, na.action=na.fail)
plot(fit1)
#with predator: singular convergence
#fit1_1 <- lmer(kdearea ~ mean_act + sex + age + ndvi_Mean + slope_Mean + tree_Mean + sensor_type + predator + (1|animals_id), data=dat4, REML=F)
#ok when removing study area
#plot(effect("predator",fit1_1))#seems relevant
#boxplot(dat4$predator~ as.factor(dat4$study_areas_id))
#also tried interaction act-sensor type
#fit1_2 <- lmer(kdearea ~ mean_act * sensor_type + sex + age + ndvi_Mean + slope_Mean + tree_Mean  + (1|study_areas_id/animals_id), data=dat4, REML=F, na.action=na.fail)
#plot(effect('mean_act:sensor_type',fit1_2))

summary(fit1)
vif(fit1)
effs <- allEffects(fit1)
plot(effs, 'mean_act')
plot(effs, 'sex')
plot(effs, 'age')
plot(effs, 'ndvi_Mean')
plot(effs, 'slope_Mean')
plot(effs, 'tree_Mean')
plot(effs, 'sensor_type')
dotplot(ranef(fit1, condVar=TRUE))
#model averaging
dd <- dredge(fit1)
#dd_s <- subset(dd, delta < 4)
#convergence issues (even after standardising covariates)
avg <- model.avg(dd, subset = delta < 4)
summary(avg)
#The subset (or conditional) average only averages over the models where the parameter appears. An alternative, the full average assumes that a variable is included in every model, but in some models the corresponding coefficient (and its respective variance) is set to zero. Unlike the subset average, it does not have a tendency of biasing the value away from zero. The full average is a type of shrinkage estimator and for variables with a weak relationship to the response they are smaller than subset estimators.


#GAMM----
#need to use uGamm; updateable wrapper for mgcv::gamm and gamm4::gamm4
fit2 <- uGamm(log_kde ~ s(mean_act, bs="ts") + sensor_type + sex + age + s(ndvi_Mean, bs="ts") + s(slope_Mean, bs="ts") + s(tree_Mean, bs="ts") + s(month, by=study_areas_id, bs="cc"), random=list(animals_id=~1), data=dat4, na.action=na.fail)
#can ignore warning: https://stackoverflow.com/questions/57664927/warning-in-gam-with-release-of-r-3-6-1
#but month and ndvi are (inversely) related, so maybe enough to include ndvi..? 
# fit2_1 <- gamm(log_kde ~ s(ndvi_Mean, bs="ts"), random=list(animals_id=~1), data=dat4, na.action=na.fail)
# fit2_2 <- gamm(log_kde ~ s(month, bs="cc"), random=list(animals_id=~1), data=dat4, na.action=na.fail)
# plot(fit2_1$gam)
# plot(fit2_2$gam)
# fit2_1_2 <- gamm(log_kde ~ s(ndvi_Mean, bs="ts", by=study_areas_id), random=list(animals_id=~1), data=dat4, na.action=na.fail)
# fit2_2_2 <- gamm(log_kde ~ s(month, bs="cc", by=study_areas_id), random=list(animals_id=~1), data=dat4, na.action=na.fail)
# plot(fit2_1_2$gam)
# plot(fit2_2_2$gam)
#Also, fit gamm using gam+bs="re" - slower but more stable? https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/random.effects.html 
fit2 <- gam(log_kde ~ s(mean_act, bs="ts") + sensor_type + sex + age + s(ndvi_Mean, bs="ts") + s(slope_Mean, bs="ts") + s(tree_Mean, bs="ts") + s(study_areas_id, animals_id, bs="re"), data=dat4, na.action=na.fail, method="ML")
#with predator: it's OK in GAMM fit2_p <- gam(log_kde ~ s(mean_act, bs="ts") + sensor_type + sex + age + s(ndvi_Mean, bs="ts") + s(slope_Mean, bs="ts") + s(tree_Mean, bs="ts") + s(study_areas_id, animals_id, bs="re") + predator, data=dat4, na.action=na.fail, method="ML")
plot(fit2, all.terms=T, scale=0)
gam.check(fit2)
summary(fit2)
fit2_2 <- gam(log_kde ~ s(mean_act, bs="ts", by=sensor_type) + sensor_type + sex + age + s(ndvi_Mean, bs="ts") + s(slope_Mean, bs="ts") + s(tree_Mean, bs="ts") + s(study_areas_id, animals_id, bs="re"), data=dat4, na.action=na.fail, method="ML")
summary(fit2_2)
plot(fit2_2, scale=0, all.terms=T)
#problem is that mean_act has different effect for different sensor type, which suggests it might be measuring something different (or is maybe confounded with how sensors deployed?)
fit2_3 <- gam(log_kde ~ s(mean_act, bs="ts", by=sensor_type) + sex + age + s(ndvi_Mean, bs="ts") + s(slope_Mean, bs="ts") + s(tree_Mean, bs="ts") + s(study_areas_id, animals_id, bs="re"), data=dat4, na.action=na.fail, method="ML")
summary(fit2_3)
plot(fit2_3, scale=0, all.terms=T)

#model averaging
dd2 <- dredge(fit2)
#convergence issues (even after standardising covariates)
avg2 <- model.avg(dd2, subset = delta < 4)
summary(avg2)
