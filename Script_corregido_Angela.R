
if(!require(rcompanion)){install.packages("rcompanion")} #required for nagelkerke function
if(!require(rcompanion)){install.packages("mvtnorm")}
if(!require(rcompanion)){install.packages("survival")}
if(!require(rcompanion)){install.packages("nlme")}
if(!require(rcompanion)){install.packages("MuMIn")}

library(ggplot2)
library(car)
library(multcomp)
library(lme4)
library(emmeans)
library(lmerTest)
library(nlme)
library(MuMIn)

### Data ###

dataAngela<-read.delim('C:/Users/avale/OneDrive - Universidad Rey Juan Carlos/TFG/EXCEL/TablesAngela.txt', header = TRUE, sep = "\t", dec = ".")
levels(dataAngela$diet)
dataAngela$diet <- factor(dataAngela$diet, levels=c("\"C\"","\"0:1\"","\"1:2\"","\"2:1\"","\"1:0\""));
levels(dataAngela$diet)

### analisis behaviours ##

# glm families:Usually the dispositive consideration is the nature of the dependent variable. For example, if the response variable is binomial in nature, you might use logistic regression with a binomial family. If the response variable is count, you might use Poisson or negative binomial regression. If you have proportion data between 0 and 1, you might use beta regression. If you have data that's left-limited, say at zero, and potentially skewed, you might use Gamma regression.
# A simple empirical rule:
# Gaussian family : for continuous decimal data with normal distribution, like weight, length, et al.
# Poisson or quasipoisson family: for positive integer or small natural number like count, individual number, frequency.
# Binomial or quasibinomial family: binary data like 0 and 1, or proportion like survival number vs death number, positive frequency vs negative frequency, winning times vs the number of failtures, et al...
# Gamma family : usually describe time data like the time or duration of the occurrence of the event.
# If your model is overdispered, you should make a correction by choosing quasi-binomial or quasi-poisson.

# Also, for diagnosis, check https://bookdown.org/egarpor/PM-UC3M/glm-diagnostics.html

###################################################################
###### out: Does the cockroach leave the shelter? yes or not ######
###################################################################

par(mfrow = c(1, 1))
hist(dataAngela$out)

lm_mod.outB <- glmer('out ~ sex+week + (1|cockroach_ID)', family="binomial",data=dataAngela)
summary(lm_mod.outB)
Anova(lm_mod.outB, type=3)
## Solo hay un efecto del sexo y la semana, sin interacciones. Podrías pintar una figura para el sexo y otra para la semana

r.squaredGLMM(lm_mod.outB) 

## Aquí verás 4 valores. Los R2m son la variabilidad explicada por los factores fijos (sexo y semana) y los R2C, son la variablidad explicada por todos los factores. 
## Así que puedes ver que el factor aleatorio, individuo, tiene bastante efecto.
## Creo qu está bien que te quedes con delta R2 (http://jslefche.github.io/piecewiseSEM/reference/rsquared.html)
##           For GLMERs fit to the binomial distribution (glmer, glmmPQL), supported methods include:
  
##          - theoretical Assumes observation variance is pi^2/3

##          - delta Approximates the observation variance as above. The DEFAULT method

emmeans(lm_mod.outB, list(pairwise ~sex)) # con esto harías las comparaciones dos a dos, que en este caso no es muy necesario, porque ya es lo que te aparece en el summary. 
# Con semana no lo haces porque es una variable continua.

# Varias figuras para evaluar los residuos
#hist(residuals(lm_mod.outB))
#boxplot(residuals(lm_mod.outB)) 
qqnorm(residuals(lm_mod.outB), main ="Normal Q-Q plot", xlab="Theoretical Quantiles", ylab= "Sample Quantiles", plot.it=TRUE)
qqline(residuals(lm_mod.outB))
plot(fitted(lm_mod.outB), residuals(lm_mod.outB), xlab="Fitted values", ylab ="Residuals" )
abline(h=0, lty=2)
lines(smooth.spline(fitted(lm_mod.outB), residuals(lm_mod.outB),spar=1))

#######################################################################
###### crossed: Does the cockroach crossed the arena? yes or not ######
#######################################################################

par(mfrow = c(1, 1))
hist(dataAngela$crossed)

lm_mod.crossed <- glmer('crossed ~ week + (1|cockroach_ID)', family="binomial",data=dataAngela)
summary(lm_mod.crossed)
Anova(lm_mod.crossed, type=3)
r.squaredGLMM(lm_mod.crossed) 
## En este caso, nada sale significativo, si acaso parece que la semana podría llegar a tener un efecto con más datos, pero no es lo que más nos interesa, así que simplemente podríamos dejarlo fuera. Y no analizarlo.

# Varias figuras para evaluar los residuos
#hist(residuals(lm_mod.crossed))
#boxplot(residuals(lm_mod.crossed)) 
qqnorm(residuals(lm_mod.crossed), main ="Normal Q-Q plot", xlab="Theoretical Quantiles", ylab= "Sample Quantiles", plot.it=TRUE)
qqline(residuals(lm_mod.crossed))
plot(fitted(lm_mod.crossed), residuals(lm_mod.crossed), xlab="Fitted values", ylab ="Residuals" )
abline(h=0, lty=2)
lines(smooth.spline(fitted(lm_mod.crossed), residuals(lm_mod.crossed),spar=1))

###############################
###### t_antenNAe (time)#######
###############################

par(mfrow = c(1, 1))
hist(dataAngela$t_antenNAe)

dataAngela$t_antenNAe1=dataAngela$t_antenNAe+1;#Log(0)=-inf
dataAngela$logt_antenNAe1=log(dataAngela$t_antenNAe1);
hist(dataAngela$logt_antenNAe1) # Uso datos transformados con log() porque la función Gamma con los efectos aleatorios no estaba funcionando bien.

library(lmerTest) # esto lo pongo para que salgan valores de p en la summary, si no, la función lmer no los da por defecto.
lm_mod.t_antenNAe <- lmer('logt_antenNAe1 ~ week+diet+sex + (1|cockroach_ID)', data=dataAngela) # lmer es la version de glmer con family gaussian (y R protesta si no lo pones así)
summary(lm_mod.t_antenNAe)
Anova(lm_mod.t_antenNAe, type=3)
r.squaredGLMM(lm_mod.t_antenNAe) 
## Nada parece tener un efecto en el tiempo que tardan en sacar las antennas, así que de nuevo podrías no mencionarlo.

# Varias figuras para evaluar los residuos
#hist(residuals(lm_mod.t_antenNAe))
#boxplot(residuals(lm_mod.t_antenNAe)) 
qqnorm(residuals(lm_mod.t_antenNAe), main ="Normal Q-Q plot", xlab="Theoretical Quantiles", ylab= "Sample Quantiles", plot.it=TRUE)
qqline(residuals(lm_mod.t_antenNAe))
plot(fitted(lm_mod.t_antenNAe), residuals(lm_mod.t_antenNAe), xlab="Fitted values", ylab ="Residuals" )
abline(h=0, lty=2)
lines(smooth.spline(fitted(lm_mod.t_antenNAe), residuals(lm_mod.t_antenNAe),spar=1))

###############################
###### t_antena_head###########
###############################

# Calculo del tiempo que pasa entre que sacan las antenas y sacan la cabeza, y he usado de nuevo datos transformados con log()

dataAngela$t_antena_head=dataAngela$t_head-dataAngela$t_antenNAe;
hist(dataAngela$t_antena_head)
dataAngela$t_antena_head1=dataAngela$t_antena_head+1;
hist(dataAngela$t_antena_head1)
dataAngela$logt_antena_head1=log(dataAngela$t_antena_head1)
hist(dataAngela$logt_antena_head1)

library(lmerTest)
lm_mod.t_antena_head <- lmer('logt_antena_head1 ~ diet+week + (1|cockroach_ID)', data=dataAngela)
summary(lm_mod.t_antena_head)
Anova(lm_mod.t_antena_head, type=3)
r.squaredGLMM(lm_mod.t_antena_head) 
## Hay un efecto tanto de la dieta como de la semana, quizá quieras usarlo


# Varias figuras para evaluar los residuos
#hist(residuals(lm_mod.t_antena_head))
#boxplot(residuals(lm_mod.t_antena_head)) 
qqnorm(residuals(lm_mod.t_antena_head), main ="Normal Q-Q plot", xlab="Theoretical Quantiles", ylab= "Sample Quantiles", plot.it=TRUE)
qqline(residuals(lm_mod.t_antena_head))
plot(fitted(lm_mod.t_antena_head), residuals(lm_mod.t_antena_head), xlab="Fitted values", ylab ="Residuals" )
abline(h=0, lty=2)
lines(smooth.spline(fitted(lm_mod.t_antena_head), residuals(lm_mod.t_antena_head),spar=1))

###############################
###### t_head (time)###########
###############################

par(mfrow = c(1, 1))
hist(dataAngela$t_head)
dataAngela$t_head1=dataAngela$t_head+1;#Log(0)=-inf
dataAngela$logt_head1=log(dataAngela$t_head1); # también he he usado de nuevo datos transformados con log()
hist(dataAngela$logt_head1)

library(lmerTest)
lm_mod.t_head <- lmer('logt_head1 ~ diet+sex+week  + (1|cockroach_ID)',data=dataAngela)
summary(lm_mod.t_head)
Anova(lm_mod.t_head, type=3)
r.squaredGLMM(lm_mod.t_head) 
## No sale nada significativo, así que podrías no mencionarlo

# Varias figuras para evaluar los residuos
#hist(residuals(lm_mod.t_head))
#boxplot(residuals(lm_mod.t_head)) 
qqnorm(residuals(lm_mod.t_head), main ="Normal Q-Q plot", xlab="Theoretical Quantiles", ylab= "Sample Quantiles", plot.it=TRUE)
qqline(residuals(lm_mod.t_head))
plot(fitted(lm_mod.t_head), residuals(lm_mod.t_head), xlab="Fitted values", ylab ="Residuals" )
abline(h=0, lty=2)
lines(smooth.spline(fitted(lm_mod.t_head), residuals(lm_mod.t_head),spar=1))

###############################
###### t_head_body (time)######
###############################

dataAngela$t_head_body=dataAngela$t_body-dataAngela$t_head;
hist(dataAngela$t_head_body)
dataAngela$t_head_body1=dataAngela$t_head_body+1;
hist(dataAngela$t_head_body1)
dataAngela$logt_head_body1=log(dataAngela$t_head_body1)
hist(dataAngela$logt_head_body1)

library(lmerTest)
lm_mod.t_head_body <- lmer('logt_head_body1 ~ diet+sex + (1|cockroach_ID)', data=dataAngela)
summary(lm_mod.t_head_body)
Anova(lm_mod.t_head_body, type=3)
r.squaredGLMM(lm_mod.t_head_body) 


## La dieta sale significativa aquí también, por lo que podría ser interesante mencionarla. He dejado el sexo porque sale casi significativo, por si quieres hacer
## algún comentario sobre que hacer más réplicas podría aclarar si el sexo juega algún papel o no

# Varias figuras para evaluar los residuos
#hist(residuals(lm_mod.t_antena_head))
#boxplot(residuals(lm_mod.t_antena_head)) 
qqnorm(residuals(lm_mod.t_head_body), main ="Normal Q-Q plot", xlab="Theoretical Quantiles", ylab= "Sample Quantiles", plot.it=TRUE)
qqline(residuals(lm_mod.t_head_body))
plot(fitted(lm_mod.t_head_body), residuals(lm_mod.t_head_body), xlab="Fitted values", ylab ="Residuals" )
abline(h=0, lty=2)
lines(smooth.spline(fitted(lm_mod.t_head_body), residuals(lm_mod.t_head_body),spar=1))


###############################
###### t_body (time)###########
###############################

par(mfrow = c(1, 1))
hist(dataAngela$t_body)
dataAngela$t_body1=dataAngela$t_body+1;#log(0)=-inf
hist(dataAngela$t_body1)
dataAngela$logt_body1=log(dataAngela$t_body1);
hist(dataAngela$logt_body1)

lm_mod.t_body <- lmer('logt_body1~ diet+sex + (1|cockroach_ID)',data=dataAngela)
summary(lm_mod.t_body)
Anova(lm_mod.t_body, type=3)
r.squaredGLMM(lm_mod.t_body) 

## Dieta y sexo salen casi significativos, pero no. Quizá quieras comentarlo.

# Varias figuras para evaluar los residuos
#hist(residuals(lm_mod.t_body))
#boxplot(residuals(lm_mod.t_body)) 
qqnorm(residuals(lm_mod.t_body), main ="Normal Q-Q plot", xlab="Theoretical Quantiles", ylab= "Sample Quantiles", plot.it=TRUE)
qqline(residuals(lm_mod.t_body))
plot(fitted(lm_mod.t_body), residuals(lm_mod.t_body), xlab="Fitted values", ylab ="Residuals" )
abline(h=0, lty=2)
lines(smooth.spline(fitted(lm_mod.t_body), residuals(lm_mod.t_body),spar=1))

##################################
###### t_body_crossing (time)#####
##################################

## tiempo desde que sale hasta que cruza (como hay muchas que no cruzan, puede que esto no tenga mucho interés)
dataAngela$t_body_crossing=dataAngela$t_crossing-dataAngela$t_body;
hist(dataAngela$t_body_crossing)
dataAngela$t_body_crossing1=dataAngela$t_body_crossing+1;
hist(dataAngela$t_body_crossing1)
dataAngela$logt_body_crossing1=log(dataAngela$t_body_crossing1)
hist(dataAngela$logt_body_crossing1)

library(lmerTest)
lm_mod.t_body_crossing <- lmer('logt_body_crossing1 ~ diet+sex+week + (1|cockroach_ID)', data=dataAngela)
summary(lm_mod.t_body_crossing)
Anova(lm_mod.t_body_crossing, type=3)
r.squaredGLMM(lm_mod.t_body_crossing) 

## Nada es sale significativo, no hay efecto, creo que es mejor no mencionarlo.

# Varias figuras para evaluar los residuos
#hist(residuals(lm_mod.t_body_crossing))
#boxplot(residuals(lm_mod.t_body_crossing)) 
qqnorm(residuals(lm_mod.t_body_crossing), main ="Normal Q-Q plot", xlab="Theoretical Quantiles", ylab= "Sample Quantiles", plot.it=TRUE)
qqline(residuals(lm_mod.t_body_crossing))
plot(fitted(lm_mod.t_body_crossing), residuals(lm_mod.t_body_crossing), xlab="Fitted values", ylab ="Residuals" )
abline(h=0, lty=2)
lines(smooth.spline(fitted(lm_mod.t_body_crossing), residuals(lm_mod.t_body_crossing),spar=1))

##################################
#### t_crossing (time)############
##################################

par(mfrow) = c(1, 1)
hist(dataAngela$t_crossing)
dataAngela$logt_crossing=log(dataAngela$t_crossing)
hist(dataAngela$logt_crossing)

library(lmerTest)
lm_mod.t_crossing <- lmer('logt_crossing ~ diet + (1|cockroach_ID)', data=dataAngela)
summary(lm_mod.t_crossing)
Anova(lm_mod.t_crossing, type=3)
r.squaredGLMM(lm_mod.t_crossing) 

## La dieta tiene un efecto

# Varias figuras para evaluar los residuos
#hist(residuals(lm_mod.t_crossing))
#boxplot(residuals(lm_mod.t_crossing)) 
qqnorm(residuals(lm_mod.t_crossing), main ="Normal Q-Q plot", xlab="Theoretical Quantiles", ylab= "Sample Quantiles", plot.it=TRUE)
qqline(residuals(lm_mod.t_crossing))
plot(fitted(lm_mod.t_crossing), residuals(lm_mod.t_crossing), xlab="Fitted values", ylab ="Residuals" )
abline(h=0, lty=2)
lines(smooth.spline(fitted(lm_mod.t_crossing), residuals(lm_mod.t_crossing),spar=1))

##################################
#### num crossings (counts)#######
##################################

par(mfrow = c(1, 1))
hist(dataAngela$num_crossings)
lm_mod.num_crossings <- glmer('num_crossings ~ diet + week  + (1|cockroach_ID)', family="poisson",data=dataAngela)
summary(lm_mod.num_crossings)
Anova(lm_mod.num_crossings, type=3)
r.squaredGLMM(lm_mod.num_crossings) 

## La semana tiene un efecto y la dieta casi, podrías discutirlo

# Varias figuras para evaluar los residuos
#hist(residuals(lm_mod.num_crossings))
#boxplot(residuals(lm_mod.num_crossings)) 
qqnorm(residuals(lm_mod.num_crossings), main ="Normal Q-Q plot", xlab="Theoretical Quantiles", ylab= "Sample Quantiles", plot.it=TRUE)
qqline(residuals(lm_mod.num_crossings))
plot(fitted(lm_mod.num_crossings), residuals(lm_mod.num_crossings), xlab="Fitted values", ylab ="Residuals" )
abline(h=0, lty=2)
lines(smooth.spline(fitted(lm_mod.num_crossings), residuals(lm_mod.num_crossings),spar=1))

##################################
##### num expShelter (counts)#####
##################################

par(mfrow = c(1, 1))
hist(dataAngela$num_expShelter)

lm_mod.num_expShelter <- glmer('num_expShelter ~ diet  + (1|cockroach_ID)', family="poisson",data=dataAngela)
summary(lm_mod.num_expShelter)
Anova(lm_mod.num_expShelter, type=3)
r.squaredGLMM(lm_mod.num_expShelter) 

## La dieta tiene un efecto, así que podría estar bien mencionarlo, aunque los residuos no superbuenos

# Varias figuras para evaluar los residuos
#hist(residuals(lm_mod.num_expShelter))
#boxplot(residuals(lm_mod.num_expShelter)) 
qqnorm(residuals(lm_mod.num_expShelter), main ="Normal Q-Q plot", xlab="Theoretical Quantiles", ylab= "Sample Quantiles", plot.it=TRUE)
qqline(residuals(lm_mod.num_expShelter))
plot(fitted(lm_mod.num_expShelter), residuals(lm_mod.num_expShelter), xlab="Fitted values", ylab ="Residuals" )
abline(h=0, lty=2)
lines(smooth.spline(fitted(lm_mod.num_expShelter), residuals(lm_mod.num_expShelter),spar=1))

##################################
##### num climbShelter (counts)###
##################################

par(mfrow = c(1, 1))
hist(dataAngela$num_climbShelter)

lm_mod.num_climbShelter <- glmer('num_climbShelter ~ week + (1|cockroach_ID)', family="poisson",data=dataAngela)
summary(lm_mod.num_climbShelter)
Anova(lm_mod.num_climbShelter, type=3)
r.squaredGLMM(lm_mod.num_climbShelter) 

## Aquí podría salir significativa la semana, pero de nuevo, la evaluación de los residuos no es muy buena y el modelo también explica muy poco. 
## Yo creo que sería mejor dejar estas dos variables únicamente para el índice

# Varias figuras para evaluar los residuos
#hist(residuals(lm_mod.num_climbShelter))
#boxplot(residuals(lm_mod.num_climbShelter)) 
qqnorm(residuals(lm_mod.num_climbShelter), main ="Normal Q-Q plot", xlab="Theoretical Quantiles", ylab= "Sample Quantiles", plot.it=TRUE)
qqline(residuals(lm_mod.num_climbShelter))
plot(fitted(lm_mod.num_climbShelter), residuals(lm_mod.num_climbShelter), xlab="Fitted values", ylab ="Residuals" )
abline(h=0, lty=2)
lines(smooth.spline(fitted(lm_mod.num_climbShelter), residuals(lm_mod.num_climbShelter),spar=.1))


##################################
#### Analysis survival############
##################################

## Aquí no he tocado nada

library("survival")
library("survminer")

res.cox <- coxph(Surv(days, FiNAlStatus) ~ sex+diet, data = dataAngela)
res.cox
anova(res.cox, test = 'Chisq')


survdiff(Surv(days, FiNAlStatus)~ diet*sex, data=dataAngela)
pairwise_survdiff(Surv(days, FiNAlStatus)~ diet+sex, data=dataAngela)

survdiff(Surv(days, FiNAlStatus)~ diet, data=dataAngela)
pairwise_survdiff(Surv(days, FiNAlStatus)~ diet, data=dataAngela)


sursex= survfit(Surv(days, FiNAlStatus)~sex, data= dataAngela)
surdiet= survfit(Surv(days, FiNAlStatus)~diet, data= dataAngela)
sursexdiet= survfit(Surv(days, FiNAlStatus)~sex*diet, data= dataAngela)

surv_sex_fig= ggsurvplot(
  sursex,                     # survfit object with calculated statistics.
  data = dataAngela,             # data used to fit survival curves.
  
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = FALSE,         # show confidence intervals for 
  # point estimates of survival curves.
  xlim = c(0,60),         # present narrower X axis, but not affect
  # survival estimates.
  xlab = "Time in days",   # customize X axis label.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  
  # in legend of risk table
)
surv_sex_fig
surv_diet_fig=ggsurvplot(
  surdiet,                     # survfit object with calculated statistics.
  data = dataAngela,             # data used to fit survival curves.
  
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = FALSE,         # show confidence intervals for 
  # point estimates of survival curves.
  xlim = c(0,60),         # present narrower X axis, but not affect
  # survival estimates.
  xlab = "Time in days",   # customize X axis label.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  
  # in legend of risk table
)
surv_diet_fig
surv_sexdiet_fig=ggsurvplot(
  sursexdiet,                     # survfit object with calculated statistics.
  data = dataAngela,             # data used to fit survival curves.
  
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = FALSE,         # show confidence intervals for 
  # point estimates of survival curves.
  xlim = c(0,60),         # present narrower X axis, but not affect
  # survival estimates.
  xlab = "Time in days",   # customize X axis label.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  
  # in legend of risk table
)
surv_sexdiet_fig

######################
### boldness index####
######################
install.packages("sqldf")
library(sqldf)
#install.packages("sqldf")
library(sqldf)
dataAngela <- suppressWarnings(sqldf(c("ALTER TABLE dataAngela ADD courage VARCHAR(20) DEFAULT '0' NOT NULL", "SELECT * FROM dataAngela"))) #add new column 'courage' and set default to 0
dataAngela <- suppressWarnings(sqldf(c("UPDATE dataAngela SET courage = courage + 1 WHERE NOT t_body = 'NA' AND t_body <= 300", "SELECT * FROM dataAngela")))
dataAngela <- suppressWarnings(sqldf(c("UPDATE dataAngela SET courage = courage + num_crossings WHERE NOT num_crossings = 'NA'", "SELECT * FROM dataAngela")))
dataAngela <- suppressWarnings(sqldf(c("UPDATE dataAngela SET courage = courage + (num_expShelter*2) WHERE NOT num_expShelter = 'NA'", "SELECT * FROM dataAngela")))
dataAngela <- suppressWarnings(sqldf(c("UPDATE dataAngela SET courage = courage + (num_climbShelter*3) WHERE NOT num_climbShelter = 'NA'", "SELECT * FROM dataAngela")))

dataAngela$courage <- as.numeric(dataAngela$courage)

par(mfrow = c(1, 1))
hist(dataAngela$courage)

lm_mod.courage <- glmer('courage ~ week  + (1|cockroach_ID)', family="poisson",data=dataAngela)
summary(lm_mod.courage)
Anova(lm_mod.courage, type=3)
r.squaredGLMM(lm_mod.courage) 

## Si te fijas aquí, el factor fijo semana explica muy poquito de los datos (delta R2m), mientras que el modelo entero con los factores aleatorios explica mucho más. 
## Según este análisis, en nuestro índice de valentía, la dieta no afecta, y sólo hay un pequeño efecto de la semana. Aunque sí parece que la dieta afecta a alguno
## de los comportamientos que hemos observado.

# Varias figuras para evaluar los residuos
#hist(residuals(lm_mod.courage))
# boxplot(residuals(lm_mod.courage)) 
qqnorm(residuals(lm_mod.courage), main ="Normal Q-Q plot", xlab="Theoretical Quantiles", ylab= "Sample Quantiles", plot.it=TRUE)
qqline(residuals(lm_mod.courage))
plot(fitted(lm_mod.courage), residuals(lm_mod.courage), xlab="Fitted values", ylab ="Residuals" )
abline(h=0, lty=2)
lines(smooth.spline(fitted(lm_mod.courage), residuals(lm_mod.courage),spar=1))
