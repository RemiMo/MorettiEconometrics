library(lfe)
library(stargazer)

df <- read.csv('moretti_data.csv')

###################
#  Data Cleaning  #
###################

# In this part, we clean the data the same way Moretti does.

# Create variable cpi to convert variables in 2005 dollars.
df$cpi <- 96/195
df[df$year == 1983, 'cpi'] <- 99/195
df[df$year == 1984, 'cpi'] <- 103/195
df[df$year == 1985, 'cpi'] <- 107/195
df[df$year == 1986, 'cpi'] <- 109/195
df[df$year == 1987, 'cpi'] <- 113/195
df[df$year == 1988, 'cpi'] <- 118/195
df[df$year == 1989, 'cpi'] <- 124/195
df[df$year == 1990, 'cpi'] <- 130/195
df[df$year == 1991, 'cpi'] <- 136/195
df[df$year == 1992, 'cpi'] <- 140/195
df[df$year == 1993, 'cpi'] <- 144/195
df[df$year == 1994, 'cpi'] <- 148/195
df[df$year == 1995, 'cpi'] <- 152/195
df[df$year == 1996, 'cpi'] <- 156/195
df[df$year == 1997, 'cpi'] <- 160/195
df[df$year == 1998, 'cpi'] <- 163/195
df[df$year == 1999, 'cpi'] <- 166/195
df[df$year == 2000, 'cpi'] <- 172/195

# Convert variables in 2005 dollars.
df$sales <- df$sales/df$cpi
df$sales_we <- df$sales2/df$cpi
df$sales_first_week <- df$sales_first_week/df$cpi
df$cost <- df$cost/df$cpi

# Set cost to mean cost if no cost is specified.
mean_cost <- mean(df[!is.na(df$cost), 'cost'])
df$cost_imputed <- is.na(df$cost)
df[is.na(df$cost), 'cost'] <- mean_cost

# Remove the movies with a high screens growth rate between first and second week (movies opened only in NY and LA).
df[df$t == 1, 'dd'] <- (df[df$t == 1, 'screens'] - df[df$t == 0, 'screens'])/df[df$t == 0, 'screens']
df$dd <- rep(df[df$t == 1, 'dd'], each = 8)
df[is.na(df$dd), 'dd'] <- 0
df$open_ny_la <- df$dd > 5
df <- df[!df$open_ny_la, ]

# Remove potential typos in sales (when weekend sales are higher than total sales).
df$ratio <- df$sales_we / df$sales
df[is.na(df$ratio), 'ratio'] <- 0
df$max <- ave(df$ratio, df$id, FUN=max)
df$typo <- df$max > 3 & !df$max == Inf
df <- df[df$typo == FALSE, ]

# Remove movies wihout any sale or screen.
df <- df[df$sales_first_week > 0 & df$sales_first_weekend > 0 & df$screens_first_week > 0, ]

# Generate logarithm of sales, screens and cost.
df$log_sales <- log(df$sales + 1)
df$log_sales_we <- log(df$sales_we + 1)
#df$log_sales_first_we <- log(df$sales_first_weekend + 1)
df$log_sales_first_we <- log(rep(df[df$t==0, 'sales_we'], each=8) + 1)
#df$log_screens_first_week <- log(df$screens_first_week)
df$log_screens_first_week <- log(rep(df[df$t==0, 'screens'], each=8))
df$cost <- log(df$cost + 1)

# Variable id is a factor (this is used for movie dummies with the package lfe).
df$id <- as.factor(df$id)
df$id.eff <- rnorm(nlevels(df$id))

# Add a dummy variable for sequel.
df$sequel <- df$genre1 == 'Sequel'
# Teen movies are used to measure the size of the social network.
df$network_size <- df$genre1 %in% c('Action', 'Aventure', 'Comedy', 'Fantasy', 'Horror', 'Sci-Fi', 'Suspense')
df$network_size2 <- df$genre3 %in% c('Children', 'Youth')


########################
#  Summary Statistics  #
########################

# In this part, we display some summary statistics of the variables of interest.

# Weekend sales.
mean(df$sales_we)
# Weekend sales in opening weekend.
mean(df[df$t == 0, 'sales_we'])
# Production costs.
mean(df$cost)
# Number of screens.
mean(df$screens)
# Number of screens in opening weekend.
mean(df$screens_first_week)
# Frequency for genres.
N <- length(df$id)
for(g in unique(df$genre1)) {
	sum(df$genre1 == g)/N
}
# Total number of movies.
length(unique(df$id))

###############
#  Surprises  #
###############

# In this part, we estimate the surprises of the movies.

# Regression of first-weekend sales on first week number of screens.
regSurprise1 <- lm(log_sales_first_we ~ log_screens_first_week, data = df, subset = (t==0))
# Including dummies for genre.
regSurprise2 <- lm(log_sales_first_we ~ log_screens_first_week + genre1, data = df, subset = (t==0))
# Including dummies for ratings.
regSurprise3 <- lm(log_sales_first_we ~ log_screens_first_week + genre1 + rating, data = df, subset = (t==0))
# Including a variable for production cost.
regSurprise4 <- lm(log_sales_first_we ~ log_screens_first_week + genre1 + rating + cost, data = df, subset = (t==0))
# Including dummies for distributor.
regSurprise5 <- lm(log_sales_first_we ~ log_screens_first_week + genre1 + rating + cost + distributor, data = df, subset = (t==0))
# Including dummies for weekday, month and week.
regSurprise6 <- lm(log_sales_first_we ~ log_screens_first_week + genre1 + rating + cost + distributor + weekday + month + week, data = df, subset = (t==0))
# Including dummies for year.
regSurprise7 <- lm(log_sales_first_we ~ log_screens_first_week + genre1 + rating + cost + distributor + weekday + month + week + year, data = df, subset = (t==0))

# Print a table with the results of the last regression.
stargazer(regSurprise1, regSurprise2, regSurprise3, regSurprise4, regSurprise5, regSurprise6, regSurprise7, type='text', keep=c('log_screens_first_week'), omit.stat=c("f", "ser"), title='Regression of first-weekend sales on number of screens')

# Surprises are defined as the residuals of the last regression.
surprise <- residuals(regSurprise7)
df$surprise <- rep(surprise, each = 8)

# Quantiles of the surprises.
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
#TODO: problem with the standard errors
regSaleDynamics1 <- felm(log_sales ~ t | id, data = df)
regSaleDynamics2 <- felm(log_sales ~ t + t : surprise | id, data = df)
regSaleDynamics3 <- felm(log_sales ~ t + t : positive_surprise | id, data = df)
regSaleDynamics4 <- felm(log_sales ~ 0 + I(t*bottom_surprise) + I(t*middle_surprise) + I(t*top_surprise) | id, data = df)

# Print a table with the results of the regressions.
stargazer(regSaleDynamics1, regSaleDynamics2, regSaleDynamics3, regSaleDynamics4, type='text', omit.stat=c("f", "ser"), title='Decline in box-office sales by opening week surprise')

##########################################
#  Prediction 2: Precision of the Prior  #
##########################################

# In this part, we test if the precision of the prior has an impact on social learning.

# We compute the variance of the surprises by genre to measure the precision of the prior.
variance_surprise <- ave(df$surprise[df$t == 0], df$genre1[df$t == 0], FUN=var)
df$var_surprise <- rep(variance_surprise, each=8)
# Display the variance of the surprises for each genre.
#for(g in unique(df$genre1)) {
	#print(g)
	#print(df[df$genre1==g, 'var_surprise'][1])
#}

# Regression of sales on the interaction between time, surprise and an indicator for the precision of the prior (sequel or variance by genre). 
regPrior1 <- felm(log_sales ~ t + t:positive_surprise + t:sequel + t:positive_surprise:sequel | id, data = df)
regPrior2 <- felm(log_sales ~ t + t:positive_surprise + t:var_surprise + t:positive_surprise:var_surprise | id, data = df)

# Print a table with the results of the two regressions.
stargazer(regPrior1, regPrior2, type='text', omit.stat=c("f", "ser"), title='Precision of the prior')

##############################################
#  Prediction 3: Size of the Social Network  #
##############################################

# In this part, we test if the size of the social network has an impact on social learning.

# Regression of sales on the interaction between time, surprise and an indicator for the size of the social network
regSocialNetwork1 <- felm(log_sales ~ t + t:positive_surprise + t:network_size + t:positive_surprise:network_size | id, data = df)
regSocialNetwork2 <- felm(log_sales ~ t + t:positive_surprise + t:network_size2 + t:positive_surprise:network_size2 | id, data = df)
df$nb_screens <- df$screens_first_week/1000
regSocialNetwork3 <- felm(log_sales ~ t + t:positive_surprise + t:nb_screens + t:positive_surprise:nb_screens | id, data = df)

# Print a table with the results of the regressions.
stargazer(regSocialNetwork1, regSocialNetwork2, regSocialNetwork3, type='text', omit.stat=c("f", "ser"), title="Precision of peers' signal")

#####################################
#  Prediction 4: Decline over time  #
#####################################

# In this part, we study the convexity of the sales profile.

# Regression of sales on t, t^2, the interaction between surprise and t and the interaction between surprise and t^2.
regDecline <- felm(log_sales ~ t + I(t**2) + t:positive_surprise + I(t**2):positive_surprise | id, data = df)

# Print a table with the results of the regression.
stargazer(regDecline, type='text', omit.stat=c("f", "ser"), title="Convexity of the sales profile")

# Test the hypothesis 2(t^2 + t^2:positive_surprise) < 0 (decline of positive surprise movies should be concave).
coeff <- coefficients(regDecline)
varcov_matrix <- vcov(regDecline)
# Compute the variance of 2(t^2 + t^2:positive_surprise).
variance <- 4 * ( varcov_matrix[2, 2] + varcov_matrix[4, 4] + 2*varcov_matrix[3, 4] )
# Compute the value of the statistics.
statistics <- 2 * (coeff[2] + coeff[4]) / variance

#TODO: one-tailed test

###########
#  Graph  #
###########

# In this part, we plot graphs to show the decline of sales over time for movies with negative surprises and movies with positive surprises.

# Compute the average sales each week for movies with positive surprises and movies with negative surprises.
average_pos <- NULL
average_neg <- NULL
for (t in c(0:7)){
	av <- mean(df[df$t == t & df$positive_surprise == TRUE, 'log_sales'])
	average_pos <- append(average_pos, av)
}
for (t in c(0:7)){
	av <- mean(df[df$t == t & df$positive_surprise == FALSE, 'log_sales'])
	average_neg <- append(average_neg, av)
}

t <- c(0:7)

# Plot a graph with the average observed sales
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
