library(lfe)
library(stargazer)

fr_df <- read.csv('donnees_fr.csv')

###################
#  Data Cleaning  #
###################

# In this part, we change the dataset to make it closer to the dataset of Moretti.

# Remove the movies without any screen in Paris during the first week (4 movies).
fr_df <- fr_df[fr_df$seance_paris1 != 0,]
# Remove the movies without any id_distributeur (12 movies).
fr_df <- fr_df[!is.na(fr_df$id_distributeur),]

# Set MoyennePresse and MoyenneSpectateur to the mean if no value is specified.
mean_moy <- mean(fr_df[!is.na(fr_df$MoyennePresse), 'MoyennePresse'])
fr_df[is.na(fr_df$MoyennePresse), 'MoyennePresse'] <- mean_moy
mean_moy <- mean(fr_df[!is.na(fr_df$MoyenneSpectateur), 'MoyenneSpectateur'])
fr_df[is.na(fr_df$MoyenneSpectateur), 'MoyenneSpectateur'] <- mean_moy

# Repeat each columns 13 times.
n <- nrow(fr_df)
df <- fr_df[rep(1:n, each=13),]

# Add a column to indicate the week.
df$t <- rep(0:12, n)

# Replace the variables for each week (e.g. 'entree_paris1') with a global variable (e.g. 'entree_paris')
for (i in 0:12) {
  for (variable in c('entree_paris', 'seance_paris', 'entree_fr', 'seance_fr')) {
    # Concatenate the variable name with and indicator for the week (e.g. 'entree_paris1').
    variable_t <- paste(c(variable, toString(i+1)), collapse='')
    # For each week, the variable in the new df (e.g. 'entree_paris') is taken from the old df (e.g. 'entree_paris1').
    df[df$t==i, variable] <- fr_df[,variable_t]
  }
}

# Keep only the useful variables.
df <- df[,c(1:6, 33:43, 70:85)]

# Replace the NAs in seance_fr with zeros.
df[is.na(df$seance_fr), 'seance_fr'] <- 0

# Generate logarithm of sales and screens.
df$log_entree_paris <- log(df$entree_paris + 1)
df$log_seance_paris <- log(df$seance_paris + 1)
df$log_entree_fr <- log(df$entree_fr + 1)
df$log_seance_fr <- log(df$seance_fr + 1)

# Variable id_distributeur is a factor.
df$id_distributeur <- as.factor(df$id_distributeur)

# Variable id is a factor (this is used for movie dummies with the package lfe).
df$X <- as.factor(df$X)
df$X.eff <- rnorm(nlevels(df$X))

###############
#  Surprises  #
###############

# In this part, we estimate the surprises of the movies.

# Regression of first week sales on number of screens.
regSurprise1 <- lm(log_entree_paris ~ log_seance_paris, data = df, subset = (t==0))
# Including dummies for genre
regSurprise2 <- lm(log_entree_paris ~ log_seance_paris + genre, data = df, subset = (t==0))
# Including dummies for ratings 
regSurprise3 <- lm(log_entree_paris ~ log_seance_paris + genre + interdiction, data = df, subset = (t==0))
# Including dummies for distributor 
regSurprise4 <- lm(log_entree_paris ~ log_seance_paris + genre + interdiction + id_distributeur, data = df, subset = (t==0))
# Including dummies for month and week 
regSurprise5 <- lm(log_entree_paris ~ log_seance_paris + genre + interdiction + id_distributeur + factor(mois) + factor(semaine), data = df, subset = (t==0))
# Including dummies for year 
regSurprise6 <- lm(log_entree_paris ~ log_seance_paris + genre + interdiction + id_distributeur + factor(mois) + factor(semaine) + factor(annee), data = df, subset = (t==0))
# Including other variables
regSurprise7 <- lm(log_entree_paris ~ log_seance_paris + genre + interdiction + id_distributeur + factor(mois) + factor(semaine) + factor(annee) + MoyennePresse + MoyenneSpectateur + PoidsCasting + pub, data = df, subset = (t==0))

# Print a table with the results of the last regressions.
stargazer(regSurprise1, regSurprise2, regSurprise3, regSurprise4, regSurprise5, regSurprise6, regSurprise7, keep=c('log_seance_paris'), omit.stat=c("f", "ser"), title='Regression of first-week entries on number of screens for Paris only')

# Surprises are defined as the residuals of the last regression.
surprise <- residuals(regSurprise7)
df$surprise <- rep(residuals(regSurprise3), each = 13)
quantile(df$surprise, probs = c(0, .05, .1, .25, .5, .75, .9, .95, 1))

# Generate additional variables for surprises.
df$positive_surprise <- df$surprise >= 0
q_surprise <- quantile(df$surprise, probs = c(1/3, 2/3))
df$bottom_surprise <- df$surprise < q_surprise[1]
df$middle_surprise <- df$surprise >= q_surprise[1] & df$surprise < q_surprise[2]
df$top_surprise <- df$surprise >= q_surprise[2]

###############################################
#  Prediction 1: Surprises and Sale Dynamics  #
###############################################

# In this part, we study the difference in rate of decline between movies with a positive surprise and movies with a negative surprise.

# Regression of sales on the interaction between time and surprises.
# We use the command felm of the package lfe to compute linear regressions with thousands of dummies.
regSaleDynamics1 <- felm(log_entree_paris ~ t | X, data = df)
regSaleDynamics2 <- felm(log_entree_paris ~ t + t : surprise | X, data = df)
regSaleDynamics3 <- felm(log_entree_paris ~ t + t : positive_surprise | X, data = df)
regSaleDynamics4 <- felm(log_entree_paris ~ t : bottom_surprise + t : middle_surprise | X, data = df)

# Print a table with the results of the regressions.
stargazer(regSaleDynamics1, regSaleDynamics2, regSaleDynamics3, regSaleDynamics4, omit.stat=c("f", "ser"), title='Decline in box-office sales by opening week surprise')

##########################################
#  Prediction 2: Precision of the Prior  #
##########################################

# In this part, we test if the precision of the prior has an impact on social learning.

# We compute the variance of the surprise by genre to measure the precision of the prior.
variance_surprise <- ave(df$surprise[df$t==0], df$genre[df$t==0], FUN=var)
df$var_surprise <- rep(variance_surprise, each=13)
# Display the variance of the surprises for each genre.
#for(g in unique(df$genre)) {
#print(g)
#print(df[df$genre==g, 'var_surprise'][1])
#}

# Regression of sales on the interaction between time, surprise and an indicator for the precision of the prior (sequel or variance by genre). 
regPrior1 <- felm(log_entree_paris ~ t + t:positive_surprise + t:saga + t:positive_surprise:saga | X, data = df)
regPrior2 <- felm(log_entree_paris ~ t + t:positive_surprise + t:var_surprise + t:positive_surprise:var_surprise | X, data = df)
regPrior3 <- felm(log_entree_paris ~ t + t:positive_surprise + t:art_essai + t:positive_surprise:art_essai | X, data = df)

# Print a table with the results of the two regressions.
stargazer(regPrior1, regPrior2, regPrior3, omit.stat=c("f", "ser"), title='Precision of the prior')

##############################################
#  Prediction 3: Size of the Social Network  #
##############################################

# In this part, we test if the size of the social network has an impact on social learning.

# Regression of sales on the interaction between time, surprise and an indicator for the size of the social network
regSocialNetwork1 <- felm(log_entree_paris ~ t + t:positive_surprise + t:toutpublic + t:positive_surprise:toutpublic | X, data = df)
df$seance_paris_first_week <- rep(df[df$t==0, 'seance_paris'], each=13)/1000
regSocialNetwork2 <- felm(log_entree_paris ~ t + t:positive_surprise + t:seance_paris_first_week + t:positive_surprise:seance_paris_first_week | X, data = df)

# Print a table with the results of the regressions.
stargazer(regSocialNetwork1, regSocialNetwork2, omit.stat=c("f", "ser"), title="Precision of peers' signal")

#####################################
#  Prediction 4: Decline over time  #
#####################################

# In this part, we study the convexity of the sales profile.

# Regression of sales on t, t^2, the interaction between surprise and t and the interaction between surprise and t^2.
regDecline <- felm(log_entree_paris ~ t + I(t**2) + t:positive_surprise + I(t**2):positive_surprise | X, data = df)

# Print a table with the results of the regression.
stargazer(regDecline, omit.stat=c("f", "ser"), title="Convexity of the sales profile")

# Test the hypothesis 2(t^2 + t^2:positive_surprise) < 0 (decline of positive surprise movies should be concave).
coeff <- coefficients(regDecline)
varcov_matrix <- vcov(regDecline)
# Compute the variance of 2(t^2 + t^2:positive_surprise).
variance <- 4 * ( varcov_matrix[2, 2] + varcov_matrix[4, 4] + 2*varcov_matrix[2, 4] )
# Compute the value of the statistics.
statistics <- 2 * (coeff[2] + coeff[4]) / variance^(1/2)
# Compute the p-value (the statistics follows a standard normal distribution). 
pvalue_pos <- pnorm(statistics)

# Test the hypothesis 2 t^2 > 0 (decline of negative surprise movies should be convex).
# Compute the variance of 2 t^2.
variance <- varcov_matrix[2, 2]
# Compute the value of the statistics.
statistics <- coeff[2] / variance^(1/2)
# Compute the p-value (the statistics follows a standard normal distribution). 
pvalue_neg <- pnorm(-statistics)

###########
#  Graph  #
###########

# In this part, we plot graphs to show the decline of sales over time for movies with negative surprises and movies with positive surprises.

# Compute the average sales each week for movies with positive surprises and movies with negative surprises.
average_pos <- NULL
average_neg <- NULL
for (t in c(0:12)){
  av <- mean(df[df$t == t & df$positive_surprise == TRUE, 'log_entree_paris'])
  average_pos <- append(average_pos, av)
}
for (t in c(0:12)){
  av <- mean(df[df$t == t & df$positive_surprise == FALSE, 'log_entree_paris'])
  average_neg <- append(average_neg, av)
}

t <- c(0:12)

# Plot a graph with the average observed sales
plot(t, average_pos
     , type='b'
     , col='red'
     , ylim=c(0, 15)
     , main='Decline in sales for Paris data only'
     , xlab='Week'
     , ylab='Log Sales')
lines(t, average_neg
      , type='b'
      , col='blue')
legend(5.5, 14, legend=c('Positive', 'Negative'), col=c('red', 'blue'), lty=1, cex=0.8)

dev.copy(png,filename="plot_paris_only.png");
dev.off ();

##################################################