library(lfe)
library(stargazer)

df <- read.csv('moretti_data.csv')

###################
#  Data Cleaning  #
###################

# create variable cpi to convert variables in 2005 dollars
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

# convert variables in 2005 dollars
df$sales <- df$sales/df$cpi
df$sales_we <- df$sales2/df$cpi
df$sales_first_week <- df$sales_first_week/df$cpi
df$cost <- df$cost/df$cpi

# set length of movie to 100 if movie has not specified length
df$length <- df$lenght
df <- subset(df, select = -lenght)
df[is.na(df$length), 'lenght'] <- 100

# set cost to mean cost if no cost is specified
mean_cost <- mean(df[!is.na(df$cost), 'cost'])
df$cost_imputed <- is.na(df$cost)
df[is.na(df$cost), 'cost'] <- mean_cost

# compute for each movie the number of weeks with positive sales
df$k <- df$sales > 0
df$k_we <- df$sales_we > 0
df$non_zero <- ave(df$k, df$id, FUN=sum)
df$non_zero_we <- ave(df$k_we, df$id, FUN=sum)

# add dummies for each period
df$t0 <- df$t == 0
df$t1 <- df$t == 1
df$t2 <- df$t == 2
df$t3 <- df$t == 3
df$t4 <- df$t == 4
df$t5 <- df$t == 5
df$t6 <- df$t == 6
df$t7 <- df$t == 7

# remove the movies with a high screens growth rate between first and second week (movies opened only in NY and LA)
df[df$t == 1, 'dd'] <- (df[df$t == 1, 'screens'] - df[df$t == 0, 'screens'])/df[df$t == 0, 'screens']
df$dd <- rep(df[df$t == 1, 'dd'], each = 8)
df[is.na(df$dd), 'dd'] <- 0
df$open_ny_la <- df$dd > 5
df <- df[!df$open_ny_la, ]

# remove potential typos in sales
df$ratio <- df$sales_we / df$sales
df[is.na(df$ratio), 'ratio'] <- 0
df$max <- ave(df$ratio, df$id, FUN=max)
df$typo <- df$max > 3 & !df$max == Inf
df <- df[df$typo == FALSE, ]

# remove movies wihout any sale or screen
df <- df[df$sales_first_week > 0 & df$sales_first_weekend > 0 & df$screens_first_week > 0, ]

# generate logarithm of sales, screens and cost
df$log_sales <- log(df$sales + 1)
df$log_sales_we <- log(df$sales_we + 1)
df$log_sales_first_we <- log(df$sales_first_weekend + 1)
df$log_screens_first_week <- log(df$screens_first_week)
df$cost <- log(df$cost + 1)
df$salesperscreen <- df$sales / (df$screens+1)

# consider variable id as factor
df$id <- as.factor(df$id)

########################
#  Summary Statistics  #
########################

# weekend sales
mean(df$sales_we)
# weekend sales in opening weekend
mean(df[df$t == 0, 'sales_we'])
# production cost
mean(df$cost)
# number of screens
mean(df$screens)
# number of screens in opening weekend
mean(df$screens_first_week)
#Genres
N = length(df$id)
for(g in unique(df$genre1)) {
	sum(df$genre1 == g)/N
}
#number of movies
length(unique(df$id))

###############
#  Surprises  #
###############

# regression of first-weekend sales on number of screens
regSurprise1 <- lm(log_sales_first_we ~ log_screens_first_week, data = df, subset = (t==0))
# including dummies for genre (genre must be of type factor, 16 dummies)
regSurprise2 <- lm(log_sales_first_we ~ log_screens_first_week + genre1, data = df, subset = (t==0))
# including dummies for ratings (8 dummies)
regSurprise3 <- lm(log_sales_first_we ~ log_screens_first_week + genre1 + rating, data = df, subset = (t==0))
# including a variable for production cost
regSurprise4 <- lm(log_sales_first_we ~ log_screens_first_week + genre1 + rating + cost, data = df, subset = (t==0))
# including dummies for distributor (273 dummies)
regSurprise5 <- lm(log_sales_first_we ~ log_screens_first_week + genre1 + rating + cost + distributor, data = df, subset = (t==0))
# including dummies for weekday, month and week (6x11x51 dummies)
regSurprise6 <- lm(log_sales_first_we ~ log_screens_first_week + genre1 + rating + cost + distributor + weekday + month + week, data = df, subset = (t==0))
# including dummies for year (18 dummies)
regSurprise7 <- lm(log_sales_first_we ~ log_screens_first_week + genre1 + rating + cost + distributor + weekday + month + week + year, data = df, subset = (t==0))

# print table for the regressions
#print(stargazer(regSurprise1, regSurprise2, regSurprise3, regSurprise4, regSurprise5, regSurprise6, regSurprise7, type='text', keep=c('log_screens_first_week'), out='reg1.txt', omit.stat=c("f", "ser"), title='Regression of first-weekend sales on number of screens'))

# surprises are defined as the residuals of the regression
surprise <- residuals(regSurprise7)
df$surprise <- rep(surprise, each = 8)
quantile(df$surprise, probs = c(0, .05, .1, .25, .5, .75, .9, .95, 1))

# generate variables for surprises
df$positive_surprise <- df$surprise >= 0
q_surprise <- quantile(df$surprise, probs = c(1/3, 2/3))
df$bottom_surprise <- df$surprise < q_surprise[1]
df$middle_surprise <- df$surprise >= q_surprise[1] & df$surprise < q_surprise[2]
df$top_surprise <- df$surprise >= q_surprise[2]

# surprises and sale dynamics
#TODO: problem with the standard errors
df$id.eff <- rnorm(nlevels(df$id))
regSaleDynamics1 <- felm(log_sales ~ t | id, data = df)
regSaleDynamics2 <- felm(log_sales ~ t + t : surprise | id, data = df)
regSaleDynamics3 <- felm(log_sales ~ t + t : positive_surprise | id, data = df)
regSaleDynamics4 <- felm(log_sales ~ t : bottom_surprise + t : middle_surprise | id, data = df)

# print table for the regressions
#print(stargazer(regSaleDynamics1, regSaleDynamics2, regSaleDynamics3, regSaleDynamics4, type='text', out='reg2.txt', omit.stat=c("f", "ser"), title='Decline in box-office sales by opening week surprise', apply.se = function(x) 2*x))

# test for supply effects
regSaleDynamics5 <- felm(salesperscreen ~ t | id, data = df)
regSaleDynamics6 <- felm(salesperscreen ~ t + t : surprise | id, data = df)
regSaleDynamics7 <- felm(salesperscreen ~ t + t : positive_surprise | id, data = df)
regSaleDynamics8 <- felm(salesperscreen ~ t : bottom_surprise + t : middle_surprise | id, data = df)

############################
#  Precision of the Prior  #
############################

# a dummy for sequels is used to reflect the precision of the prior
df$sequel = df$genre1 == 'Sequel'
# we compute the variance of the surprise by genre to reflect the precision of the prior
variance_surprise <- ave(df$surprise[df$t == 0], df$genre1[df$t == 0], FUN=var)
df$var_surprise <- rep(variance_surprise, each=8)

regPrior1 <- felm(log_sales ~ t + t:positive_surprise + t:sequel + t:positive_surprise:sequel | id, data = df)
regPrior2 <- felm(log_sales ~ t + t:positive_surprise + t:var_surprise + t:positive_surprise:var_surprise | id, data = df)

################################
#  Size of the Social Network  #
################################

# create a dummy for teen movies
df$teen = df$genre1 %in% c('Action', 'Aventure', 'Comedy', 'Fantasy', 'Horror', 'Sci-Fi', 'Suspense')

regSocialNetwork1 <- felm(log_sales ~ t + t:positive_surprise + t:teen + t:positive_surprise:teen | id, data = df)

df$nb_screens <- df$screens_first_week/1000
regSocialNetwork2 <- felm(log_sales ~ t + t:positive_surprise + t:nb_screens + t:positive_surprise:nb_screens | id, data = df)

#######################
#  Decline over time  #
#######################

regDecline <- felm(log_sales ~ t + I(t**2) + t:positive_surprise + I(t**2):positive_surprise | id, data = df)

#TODO: one-tailed test

###########
#  Graph  #
###########

# compute the average sales each week for movies with positive surprises and movies with negative surprises
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

