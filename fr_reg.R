library(lfe)
library(stargazer)

df <- read.csv('donnees_fr.csv')

###################
#  Data Cleaning  #
###################

# remove movies with length of 0
df <- df[df$Duree != 0, ]

# remove movies wihout any sale or screen
#df <- df[df$entree_paris1 > 0 | df$entree_fr1 > 0, ]
#df <- df[df$seance_paris1 > 0 | df$seance_fr1 > 0, ]
df[is.na(df$entree_fr1), 'entree_fr1'] <- 0
df[is.na(df$seance_fr1), 'seance_fr1'] <- 0
df <- df[df$entree_fr1 > 0, ]
df <- df[df$seance_fr1 > 0, ]

# generate logarithm of sales and screens
df$log_entree_paris1 <- log(df$entree_paris1 + 1)
df$log_entree_paris2 <- log(df$entree_paris2 + 1)
df$log_entree_paris3 <- log(df$entree_paris3 + 1)
df$log_entree_paris4 <- log(df$entree_paris4 + 1)
df$log_entree_paris5 <- log(df$entree_paris5 + 1)
df$log_entree_paris6 <- log(df$entree_paris6 + 1)
df$log_entree_paris7 <- log(df$entree_paris7 + 1)
df$log_entree_paris8 <- log(df$entree_paris8 + 1)
df$log_entree_paris9 <- log(df$entree_paris9 + 1)
df$log_entree_paris10 <- log(df$entree_paris10 + 1)
df$log_entree_paris11 <- log(df$entree_paris11 + 1)
df$log_entree_paris12 <- log(df$entree_paris12 + 1)
df$log_entree_paris13 <- log(df$entree_paris13 + 1)
df$log_seance_paris1 <- log(df$seance_paris1 + 1)
df$log_seance_paris2 <- log(df$seance_paris2 + 1)
df$log_seance_paris3 <- log(df$seance_paris3 + 1)
df$log_seance_paris4 <- log(df$seance_paris4 + 1)
df$log_seance_paris5 <- log(df$seance_paris5 + 1)
df$log_seance_paris6 <- log(df$seance_paris6 + 1)
df$log_seance_paris7 <- log(df$seance_paris7 + 1)
df$log_seance_paris8 <- log(df$seance_paris8 + 1)
df$log_seance_paris9 <- log(df$seance_paris9 + 1)
df$log_seance_paris10 <- log(df$seance_paris10 + 1)
df$log_seance_paris11 <- log(df$seance_paris11 + 1)
df$log_seance_paris12 <- log(df$seance_paris12 + 1)
df$log_seance_paris13 <- log(df$seance_paris13 + 1)
df$log_entree_fr1 <- log(df$entree_fr1 + 1)
df$log_entree_fr2 <- log(df$entree_fr2 + 1)
df$log_entree_fr3 <- log(df$entree_fr3 + 1)
df$log_entree_fr4 <- log(df$entree_fr4 + 1)
df$log_entree_fr5 <- log(df$entree_fr5 + 1)
df$log_entree_fr6 <- log(df$entree_fr6 + 1)
df$log_entree_fr7 <- log(df$entree_fr7 + 1)
df$log_entree_fr8 <- log(df$entree_fr8 + 1)
df$log_entree_fr9 <- log(df$entree_fr9 + 1)
df$log_entree_fr10 <- log(df$entree_fr10 + 1)
df$log_entree_fr11 <- log(df$entree_fr11 + 1)
df$log_entree_fr12 <- log(df$entree_fr12 + 1)
df$log_entree_fr13 <- log(df$entree_fr13 + 1)
df$log_seance_fr1 <- log(df$seance_fr1 + 1)
df$log_seance_fr2 <- log(df$seance_fr2 + 1)
df$log_seance_fr3 <- log(df$seance_fr3 + 1)
df$log_seance_fr4 <- log(df$seance_fr4 + 1)
df$log_seance_fr5 <- log(df$seance_fr5 + 1)
df$log_seance_fr6 <- log(df$seance_fr6 + 1)
df$log_seance_fr7 <- log(df$seance_fr7 + 1)
df$log_seance_fr8 <- log(df$seance_fr8 + 1)
df$log_seance_fr9 <- log(df$seance_fr9 + 1)
df$log_seance_fr10 <- log(df$seance_fr10 + 1)
df$log_seance_fr11 <- log(df$seance_fr11 + 1)
df$log_seance_fr12 <- log(df$seance_fr12 + 1)
df$log_seance_fr13 <- log(df$seance_fr13 + 1)

###############
#  Surprises  #
###############

# regression of first week sales on number of screens
regSurprise1 <- lm(log_entree_fr1 ~ log_seance_fr1, data = df)
# including dummies for genre
regSurprise2 <- lm(log_entree_fr1 ~ log_seance_fr1 + genre, data = df)
# including dummies for ratings 
regSurprise3 <- lm(log_entree_fr1 ~ log_seance_fr1 + genre + interdiction, data = df)
# including dummies for distributor 
regSurprise4 <- lm(log_entree_fr1 ~ log_seance_fr1 + genre + id_distributeur, data = df)
# including dummies for month and week 
regSurprise5 <- lm(log_entree_fr1 ~ log_seance_fr1 + genre + id_distributeur + mois + semaine, data = df)
# including dummies for year 
regSurprise6 <- lm(log_entree_fr1 ~ log_seance_fr1 + genre + id_distributeur + mois + semaine + annee, data = df)
# including other variables
regSurprise7 <- lm(log_entree_fr1 ~ log_seance_fr1 + genre + id_distributeur + mois + semaine + annee + MoyennePresse + MoyenneSpectateur + PoidsCasting + pub, data = df)

# display the coefficients of log screens of all the regressions
lscreensCoef <- c(coefficients(regSurprise1)[2], coefficients(regSurprise2)[2], coefficients(regSurprise3)[2], coefficients(regSurprise4)[2], coefficients(regSurprise5)[2], coefficients(regSurprise6)[2], coefficients(regSurprise7)[2])
lscreensCoef
# display the standard error of log screens for all the regressions
lscreensSe <- c(summary(regSurprise1)$coefficients[2, 2], summary(regSurprise2)$coefficients[2, 2], summary(regSurprise3)$coefficients[2, 2], summary(regSurprise4)$coefficients[2, 2], summary(regSurprise5)$coefficients[2, 2], summary(regSurprise6)$coefficients[2, 2], summary(regSurprise7)$coefficients[2, 2])
lscreensSe
# display the R squared of all the regressions
rsquared <- c(summary(regSurprise1)$r.squared, summary(regSurprise2)$r.squared, summary(regSurprise3)$r.squared, summary(regSurprise4)$r.squared, summary(regSurprise5)$r.squared, summary(regSurprise6)$r.squared, summary(regSurprise7)$r.squared)
rsquared

# surprises are defined as the residuals of the regression
df$surprise <- residuals(regSurprise3)
quantile(df$surprise, probs = c(0, .05, .1, .25, .5, .75, .9, .95, 1))

# build a data frame with an entry for each week
n <- length(df$id_film)
N <- 13*n
df2 <- data.frame(1:N)
df2$id <- rep(df$id_film, each=13)
df2$surprise <- rep(df$surprise, each=13)
df2$t <- rep(1:13, n)
df2[df2$t==1, 'entree'] <- df$entree_fr1
df2[df2$t==2, 'entree'] <- df$entree_fr2
df2[df2$t==3, 'entree'] <- df$entree_fr3
df2[df2$t==4, 'entree'] <- df$entree_fr4
df2[df2$t==5, 'entree'] <- df$entree_fr5
df2[df2$t==6, 'entree'] <- df$entree_fr6
df2[df2$t==7, 'entree'] <- df$entree_fr7
df2[df2$t==8, 'entree'] <- df$entree_fr8
df2[df2$t==9, 'entree'] <- df$entree_fr9
df2[df2$t==10, 'entree'] <- df$entree_fr10
df2[df2$t==11, 'entree'] <- df$entree_fr11
df2[df2$t==12, 'entree'] <- df$entree_fr12
df2[df2$t==13, 'entree'] <- df$entree_fr13
df2[df2$t==1, 'seance'] <- df$seance_fr1
df2[df2$t==2, 'seance'] <- df$seance_fr2
df2[df2$t==3, 'seance'] <- df$seance_fr3
df2[df2$t==4, 'seance'] <- df$seance_fr4
df2[df2$t==5, 'seance'] <- df$seance_fr5
df2[df2$t==6, 'seance'] <- df$seance_fr6
df2[df2$t==7, 'seance'] <- df$seance_fr7
df2[df2$t==8, 'seance'] <- df$seance_fr8
df2[df2$t==9, 'seance'] <- df$seance_fr9
df2[df2$t==10, 'seance'] <- df$seance_fr10
df2[df2$t==11, 'seance'] <- df$seance_fr11
df2[df2$t==12, 'seance'] <- df$seance_fr12
df2[df2$t==13, 'seance'] <- df$seance_fr13
df2$log_entree <- log(df2$entree + 1)
df2$log_seance <- log(df2$seance + 1)

# generate variables for surprises
df2$positive_surprise <- df2$surprise >= 0
q_surprise <- quantile(df2$surprise, probs = c(1/3, 2/3))
df2$bottom_surprise <- df2$surprise < q_surprise[1]
df2$middle_surprise <- df2$surprise >= q_surprise[1] & df2$surprise < q_surprise[2]
df2$top_surprise <- df2$surprise >= q_surprise[2]

# surprises and sale dynamics
df2$id <- as.factor(df2$id)
#df2$id.eff <- rnorm(nlevels(df2$id))
regSaleDynamics1 <- felm(log_entree ~ t | id, data = df2)
regSaleDynamics2 <- felm(log_entree ~ t + t : surprise | id, data = df2)
regSaleDynamics3 <- felm(log_entree ~ t + t : positive_surprise | id, data = df2)
regSaleDynamics4 <- felm(log_entree ~ t : bottom_surprise + t : middle_surprise | id, data = df2)

## test for supply effects
#regSaleDynamics5 <- felm(salesperscreen ~ t | id, data = df)
#regSaleDynamics6 <- felm(salesperscreen ~ t + t : surprise | id, data = df)
#regSaleDynamics7 <- felm(salesperscreen ~ t + t : positive_surprise | id, data = df)
#regSaleDynamics8 <- felm(salesperscreen ~ t : bottom_surprise + t : middle_surprise | id, data = df)

############################
#  Precision of the Prior  #
############################

# a dummy for sequels is used to reflect the precision of the prior
df2$sequel = rep(df$saga, each=13)
# we compute the variance of the surprise by genre to reflect the precision of the prior
variance_surprise <- ave(df$surprise, df$genre, FUN=var)
df2$var_surprise <- rep(variance_surprise, each=13)

regPrior1 <- felm(log_entree ~ t + t:positive_surprise + t:sequel + t:positive_surprise:sequel | id, data = df2)
regPrior2 <- felm(log_entree ~ t + t:positive_surprise + t:var_surprise + t:positive_surprise:var_surprise | id, data = df2)

################################
#  Size of the Social Network  #
################################

# create a dummy for toutpublic movies
df2$toutpublic = rep(df$toutpublic, each=13)

regSocialNetwork1 <- felm(log_entree ~ t + t:positive_surprise + t:toutpublic + t:positive_surprise:toutpublic | id, data = df2)

df2$nb_screens <- rep(df$seance_fr1/1000, each=13)
regSocialNetwork2 <- felm(log_entree ~ t + t:positive_surprise + t:nb_screens + t:positive_surprise:nb_screens | id, data = df2)

#######################
#  Decline over time  #
#######################

regDecline <- felm(log_entree ~ t + I(t**2) + t:positive_surprise + I(t**2):positive_surprise | id, data = df2)

#TODO: one-tailed test

###########
#  Graph  #
###########

# compute the average sales each week for movies with positive surprises and movies with negative surprises
average_pos <- NULL
average_neg <- NULL

for (t in c(1:13)){
	av <- mean(df2[df2$t == t & df2$positive_surprise == TRUE, 'log_entree'])
	average_pos <- append(average_pos, av)
}
for (t in c(1:13)){
	av <- mean(df2[df2$t == t & df2$positive_surprise == FALSE, 'log_entree'])
	average_neg <- append(average_neg, av)
}

t <- c(1:13)

# plot a graph with the average observed sales
plot(t, average_pos
     , type='b'
     , col='red'
     , ylim=c(0, 15)
     , main='Decline in sale for movies with positive and negative surprises'
     , xlab='Week'
     , ylab='Log Sales')
lines(t, average_neg
      , type='b'
      , col='blue')
legend(5.5, 14, legend=c('Positive', 'Negative'), col=c('red', 'blue'), lty=1, cex=0.8)

# plot a graph with the average predicted sales
x <- coefficients(regDecline)
average_neg_pred <- average_neg[1] + x[1] * t + x[2] * t^2
average_pos_pred <- average_pos[1] + (x[1]+x[3]) * t + (x[2]+x[4]) * t^2
plot(t, average_pos_pred
     , type='b'
     , col='red'
     , ylim=c(0, 15)
     , main='Decline in sale for movies with positive and negative surprises'
     , xlab='Week'
     , ylab='Log Sales')
lines(t, average_neg_pred
      , type='b'
      , col='blue')
legend(5.5, 14, legend=c('Positive predicted', 'Negative predicted'), col=c('red', 'blue'), lty=1, cex=0.8)

# plot both
plot(t, average_pos
     , type='b'
     , col='red'
     , ylim=c(0, 15)
     , main='Decline in sale for movies with positive and negative surprises'
     , xlab='Week'
     , ylab='Log Sales')
lines(t, average_neg
      , type='b'
      , col='blue')
lines(t, average_pos_pred
     , type='b'
     , col='green')
lines(t, average_neg_pred
      , type='b'
      , col='black')
legend(5.5, 14, legend=c('Positive observed', 'Negative observed', 'Positive predicted', 'Negative predicted'), col=c('red', 'blue', 'green', 'black'), lty=1, cex=0.8)

