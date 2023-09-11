##### Step 0: Installing packages & dependencies #####

##Downloading libraries
install.packages("car")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("psych")
install.packages("corrplot")
install.packages("mlbench")
install.packages("caret")
install.packages("boot")
install.packages("lmtest")
install.packages("plm")
install.packages("stargazer")

library(car)
library(ggplot2)
library(ggpubr)
library(psych)
library(corrplot)
library(mlbench)
library(caret)
library(boot)
library(lmtest)
library(plm)
library(stargazer)

##### Step 1: Observing the initial dataset #####

##Load the initial dataset
imdb = read.csv("~/Downloads/IMDB_data.csv")
attach(imdb)

View(imdb)

names(imdb)

## Creating duplicate dataset to work with

imdb2 = data.frame(imdb)

## Removing MovieID, imdbLink, releaseYear, actor1, actor2, actor3, colourFilm, genre and plotKeywords

imdb2 = imdb[,-c(2,3,8,17,19,21,23,24,26)]

names(imdb2)

attach(imdb2)


##### Step 2: Exploring the variables individually #####


#Exploring Y-variable
summary(imdbScore)
boxplot(imdbScore, main="IMDB Score", col = "azure",
        border = "blue",
        horizontal = TRUE)

text(x = c(boxplot.stats(imdbScore)$stats, max(imdbScore)), labels = c(boxplot.stats(imdbScore)$stats, max(imdbScore)), y = 1.25)

hist(imdbScore, main = "IMDB Score", col = "azure", border = "dark blue")

##Exploring datatype of predictor variables
describe(imdb2)

str(imdb2)

##Numerical variables
num_cols = unlist(lapply(imdb2, is.numeric))
num_cols
##Looking at just numeric variables first##

#Movie Budget

boxplot(movieBudget/1000000, main="Movie Budget in $(M)", col = "azure", 
        border = "blue",
        horizontal = TRUE)


#Movie duration
boxplot(duration, main="Movie Duration",col = "azure", 
        border = "blue", horizontal = TRUE)  ##Many outliers

##Aspect ratio. Visualizing with barplot since its a discrete numeric variable
ggplot(imdb2, aes(x=reorder(aspectRatio, aspectRatio, function(x)-length(x)))) +
  geom_bar(fill='light blue') +  labs(x='Aspect Ratio') + ggtitle("Aspect Ratio")

#News Articles
boxplot(nbNewsArticles, main = "News Articles", col = "azure", 
        border = "blue", horizontal = TRUE) 

##actor1_starMeter
boxplot(actor1_starMeter, main = "Star Meter of Actor 1", col = "azure", 
border = "blue", horizontal = TRUE, id.method="x") 

##actor2_starMeter
boxplot(actor2_starMeter, main = "Star Meter of Actor 2", col = "azure", 
        border = "blue", horizontal = TRUE) 

##actor3_starMeter
boxplot(actor3_starMeter, main = "Star Meter of Actor 3", col = "azure", 
        border = "blue", horizontal = TRUE) 

##nb Faces
boxplot(nbFaces, main = "Faces on Poster", col = "azure", 
        border = "blue", horizontal = TRUE) 


#Also looking at nbFaces like a discrete variable
ggplot(imdb2, aes(x=reorder(nbFaces, nbFaces, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Faces on Poster')

##movieMeter_IMDBpro
boxplot(movieMeter_IMDBpro, main = "Movie Meter IMDBPro", col = "azure", 
        border = "blue", horizontal = TRUE) 

##Looking at genre-related variables

ggplot(imdb2, aes(x=reorder(action, action, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Action')

ggplot(imdb2, aes(x=reorder(adventure, adventure, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Adventure')

ggplot(imdb2, aes(x=reorder(scifi, scifi, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Science Fiction Movies')

ggplot(imdb2, aes(x=reorder(thriller, thriller, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Thriller Movies')

ggplot(imdb2, aes(x=reorder(musical, musical, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Musical Movies')

ggplot(imdb2, aes(x=reorder(romance, romance, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Romance Movies')

ggplot(imdb2, aes(x=reorder(western, western, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Western Movies')

ggplot(imdb2, aes(x=reorder(sport, sport, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Sport Movies')

ggplot(imdb2, aes(x=reorder(horror, horror, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Horror Movies')

ggplot(imdb2, aes(x=reorder(drama, drama, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Drama Movies')

ggplot(imdb2, aes(x=reorder(war, war, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='War Movies')

ggplot(imdb2, aes(x=reorder(animation, animation, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Animation Movies')

ggplot(imdb2, aes(x=reorder(crime, crime, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Crime Movies')

##Looking at non-numerical variables

#Release Month
ggplot(imdb2, aes(x=reorder(releaseMonth, releaseMonth, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Release Month')  

imdb2$November_Release = ifelse(releaseMonth == "Nov", 1, 0)
attach(imdb2)
table(imdb2$November_Release)

#Language
ggplot(imdb2, aes(x=reorder(language, language, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Language') 

imdb2$Engligh_Language = ifelse(language == "English", 1, 0)
attach(imdb2)

#Country
ggplot(imdb2, aes(x=reorder(country, country, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='country')

sort(table(country))

imdb2$USA_prod = ifelse(country == "USA", 1, 0)

#Maturity Rating
ggplot(imdb2, aes(x=reorder(maturityRating, maturityRating, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Maturity Rating')

imdb2$R_rating = ifelse(maturityRating == "R", 1, 0)
attach(imdb2)

#Distributors
ggplot(imdb2, aes(x=reorder(distributor, distributor, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Distributors')  

sort(table(distributor))

imdb2$distribution_company = ifelse(distributor %in% c("Universal Pictures","Paramount Pictures","Columbia Pictures Corporation","Warner Bros.", "Twentieth Century Fox"), "Big Distribution Company", "Others")

#Directors
ggplot(imdb2, aes(x=reorder(director, director, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Director')  

sort(table(director))

##Let's drop directors

#Cinematographer
ggplot(imdb2, aes(x=reorder(cinematographer, cinematographer, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Cinematographer')  

sort(table(cinematographer))

## Let's drop cinematographer

#Production Company
ggplot(imdb2, aes(x=reorder(productionCompany, productionCompany, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Production Company')  

sort(table(productionCompany))

imdb2$production_company = ifelse(productionCompany %in% c("Universal Pictures","Paramount Pictures","Columbia Pictures Corporation","Warner Bros."), "Big Production Companies", "Others")


##Dropping variables to form a new dataset

View(imdb2)

imdb3 = data.frame(imdb2)

imdb3 = imdb3[,-c(5,7,8,9,11,13,32,33)]
names(imdb3)

attach(imdb3)

##Visualizing the new variables

#November_Release
ggplot(imdb3, aes(x=reorder(November_Release, November_Release, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='November Release')  

#English Language
ggplot(imdb3, aes(x=reorder(Engligh_Language, Engligh_Language, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='English Language')  

#USA Produced
ggplot(imdb3, aes(x=reorder(USA_prod, USA_prod, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='USA Produced')  

#Distribution Company
ggplot(imdb3, aes(x=reorder(distribution_company, distribution_company, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Distribution Company')  

#Production Company
ggplot(imdb3, aes(x=reorder(production_company, production_company, function(x)-length(x)))) +
  geom_bar(fill='blue') +  labs(x='Production Company')  


##### Step 3: Exploring Variable Relationships #####

##Movie Budget with IMDB Score
plot(movieBudget/1000000, imdbScore, main = "Movie Budget with IMDB Score", 
     xlab = "Movie Budget in ($) Million ", col = "dark green")

##Release Day with IMDB Score
plot(releaseDay, imdbScore, main = "Release Day with IMDB Score", 
     xlab = "Release Day", col = "dark green")

##Duration with IMDB Score
plot(duration, imdbScore, main = "Movie Duration with IMDB Score", 
     xlab = "Movie Duration", col = "dark green")

##Aspect ratio with IMDB Score
boxplot(imdbScore~aspectRatio, data=imdb3,
        main="Aspect Ratio with IMDB Score",
        col="orange", border="brown")

plot(aspectRatio, imdbScore, main = "Aspect Ratio with IMDB Score", 
     xlab = "Aspect Ratio", col = "dark green")

##All movies in test set are either 1.85 or 2.35. Let's not use this variable since they don't seem to have a relation to IMDB score.

##News Article with IMDB Score
plot(duration, imdbScore, main = "News Articles with IMDB Score", 
     xlab = "News Articles", col = "dark green")

par(mfrow = c(3,1))

##StarMeter1 with IMDB Score
plot(actor1_starMeter, imdbScore, main = "Star Meter of Actor 1 with IMDB Score", 
     col = "dark green")


##StarMeter2 with IMDB Score
plot(actor2_starMeter, imdbScore, main = "Star Meter of Actor 2 with IMDB Score", 
     col = "dark green")

##StarMeter3 with IMDB Score
plot(actor3_starMeter, imdbScore, main = "Star Meter of Actor 3 with IMDB Score", 
     col = "dark green")

par(mfrow = c(1,1))

##Faces in Movie Poster with IMDB Score
plot(nbFaces, imdbScore, main = "Faces in Movie Poster with IMDB Score", 
     col = "dark green")

boxplot(imdbScore~nbFaces, data=imdb3,
        main="Aspect Ratio with IMDB Score",
        col="orange", border="brown")

##Genres with IMDB Score
par(mfrow = c(1,13))

boxplot(imdbScore~action,
        main = "Action",
        col="orange", border="brown",
        notch = TRUE)

boxplot(imdbScore~adventure,
        main = "Adventure",
        col="orange", border="brown",
        notch = TRUE)

boxplot(imdbScore~scifi,
        main = "Science Fiction",
        col="orange", border="brown",
        notch = TRUE)

boxplot(imdbScore~thriller,
        main = "Thriller",
        col="orange", border="brown",
        notch = TRUE)

boxplot(imdbScore~musical,
        main = "Musical",
        col="orange", border="brown",
        notch = TRUE)

boxplot(imdbScore~romance,
        main = "Romance",
        col="orange", border="brown",
        notch = TRUE)

boxplot(imdbScore~western,
        main = "Western",
        col="orange", border="brown",
        notch = TRUE)

boxplot(imdbScore~sport,
        main = "Sports",
        col="orange", border="brown",
        notch = TRUE)

boxplot(imdbScore~horror,
        main = "Horror",
        col="orange", border="brown",
        notch = TRUE)

boxplot(imdbScore~drama,
        main = "Drama",
        col="orange", border="brown",
        notch = TRUE)

boxplot(imdbScore~war,
        main = "War",
        col="orange", border="brown",
        notch = TRUE)

boxplot(imdbScore~animation,
        main = "Animation",
        col="orange", border="brown",
        notch = TRUE)

boxplot(imdbScore~crime,
        main = "Crime",
        col="orange", border="brown",
        notch = TRUE)

par(mfrow = c(1,1))

##Movie Meter with IMDB Score
plot(movieMeter_IMDBpro, imdbScore, main = "Movie Meter IMDBpro with IMDB Score", 
     col = "dark green")

##November Release with IMDB Score
plot(November_Release, imdbScore, main = "November Release with IMDB Score", 
     col = "dark green")

boxplot(imdbScore~November_Release, data=imdb3,
        main="November Released Movies with IMDB Score",
        col="orange", border="brown")

##English Language with IMDB Score
plot(Engligh_Language, imdbScore, main = "English Language Movies with IMDB Score", 
     col = "dark green")

##Production Country with IMDB Score
boxplot(imdbScore~Prod_country, data=imdb3,
        main="Production Country with IMDB Score",
        col="orange", border="brown")

##R rating with IMDB Score
boxplot(imdbScore~R_rating, data=imdb3,
        main="R Rating with IMDB Score",
        col="orange", border="brown")

##Distribution Company with IMDB Score
boxplot(imdbScore~distribution_company, data=imdb3,
        main="Distribution Company with IMDB Score",
        col="orange", border="brown")

##Production Company with IMDB Score
boxplot(imdbScore~production_company, data=imdb3,
        main="Production Company with IMDB Score",
        col="orange", border="brown")



##### Step 4: Further exploring variable relationships #####

##Correlation Matrix
num_cols1 = unlist(lapply(imdb3, is.numeric))
num_cols1
imdb_nums <- imdb3[, num_cols1]
imdb_nums
res <- cor(imdb_nums)
round(res, 2)
corrplot(res, method="circle")


##Starting with simple linear regression models
reg1=lm(imdbScore~movieBudget) 
summary(reg1)

reg2=lm(imdbScore~releaseDay)
summary(reg2) ## very high p-value, potentially worth exclusion

##Note that aspect ratio is actually not a numeric variable

reg3=lm(imdbScore~nbNewsArticles)
summary(reg3)  

reg4=lm(imdbScore~duration)
summary(reg4)  

reg5=lm(imdbScore~actor1_starMeter)
summary(reg5) ## very high p-value

reg6=lm(imdbScore~actor2_starMeter)
summary(reg6) ## moderately high p-value

reg7=lm(imdbScore~actor3_starMeter)
summary(reg7) ## very high p-value

reg8=lm(imdbScore~nbFaces)
summary(reg8)

reg9=lm(imdbScore~action)
summary(reg9)

reg10=lm(imdbScore~adventure)
summary(reg10)

reg11=lm(imdbScore~scifi)
summary(reg11)

reg12=lm(imdbScore~thriller)
summary(reg11)

reg13=lm(imdbScore~musical)
summary(reg13) ## very high p-value

reg14=lm(imdbScore~romance)
summary(reg14) ## very high p-value

reg15=lm(imdbScore~western)
summary(reg15) 

reg16=lm(imdbScore~sport)
summary(reg16)

reg17=lm(imdbScore~horror)
summary(reg17)

reg18=lm(imdbScore~drama)
summary(reg18)

reg19=lm(imdbScore~war)
summary(reg19)

reg20=lm(imdbScore~crime)
summary(reg20)

reg21=lm(imdbScore~movieMeter_IMDBpro)
summary(reg21)

reg22=lm(imdbScore~November_Release)
summary(reg22)

reg23=lm(imdbScore~Engligh_Language)
summary(reg23)

reg24=lm(imdbScore~R_rating)
summary(reg24)


##### Step 5: Starting to model the data #####

# First shot at regression with only numeric variables with high significance #

mreg1=lm(imdbScore~movieBudget+releaseDay+duration+nbNewsArticles+actor1_starMeter+actor2_starMeter+actor3_starMeter+nbFaces+movieMeter_IMDBpro)
summary(mreg1)

##release day, Actor1_StarMeter, actor2_starMeter and actor3_starMeter have very high p-value as observed in individual regressions. 
##Lets remove them one by one. 

##Removing release day
mreg2=lm(imdbScore~movieBudget+duration+nbNewsArticles+actor1_starMeter+actor2_starMeter+actor3_starMeter+nbFaces+movieMeter_IMDBpro)
summary(mreg2)

##Still high p-values for the other 3. Lets remove Actor3_StarMeter

mreg3=lm(imdbScore~movieBudget+duration+nbNewsArticles+actor1_starMeter+actor2_starMeter+nbFaces+movieMeter_IMDBpro)
summary(mreg3)

##remove Actor2_StarMeter
mreg4=lm(imdbScore~movieBudget+duration+nbNewsArticles+actor1_starMeter+nbFaces+movieMeter_IMDBpro)
summary(mreg4)

##remove Actor1_StarMeter --> this hurts R^2
mreg5=lm(imdbScore~movieBudget+duration+nbNewsArticles+nbFaces+movieMeter_IMDBpro)
summary(mreg5)

##Lets add categorical variables excluding musical and romance
mreg6=lm(imdbScore~movieBudget+duration+nbNewsArticles+nbFaces+movieMeter_IMDBpro+action+adventure+scifi
         +thriller+western+sport+horror+drama+war+animation+crime+Engligh_Language+
           USA_prod+R_rating+distribution_company+production_company)
summary(mreg6)

##Removing adventure
mreg7=lm(imdbScore~movieBudget+duration+nbNewsArticles+nbFaces+movieMeter_IMDBpro+action+scifi
         +thriller+western+sport+horror+drama+war+animation+crime+Engligh_Language+
           USA_prod+R_rating+distribution_company+production_company)
summary(mreg7)

##Removing animation --> this hurts R^2
mreg8=lm(imdbScore~movieBudget+duration+nbNewsArticles+nbFaces+movieMeter_IMDBpro+action+scifi
         +thriller+western+sport+horror+drama+war+crime+Engligh_Language+
           USA_prod+R_rating+distribution_company+production_company)
summary(mreg8)

##Removing scifi
mreg9=lm(imdbScore~movieBudget+duration+nbNewsArticles+nbFaces+movieMeter_IMDBpro+action
         +thriller+western+sport+horror+drama+war+crime+Engligh_Language+
           USA_prod+R_rating+distribution_company+production_company)
summary(mreg9)

##Removing war
mreg10=lm(imdbScore~movieBudget+duration+nbNewsArticles+nbFaces+movieMeter_IMDBpro+action
         +thriller+western+sport+horror+drama+crime+Engligh_Language+
           USA_prod+R_rating+distribution_company+production_company)
summary(mreg10)

##Removing distribution company
mreg11=lm(imdbScore~movieBudget+duration+nbNewsArticles+nbFaces+movieMeter_IMDBpro+action
          +thriller+western+sport+horror+drama+crime+Engligh_Language+
            USA_prod+R_rating+production_company)
summary(mreg11)

##Removing thriller
mreg12=lm(imdbScore~movieBudget+duration+nbNewsArticles+nbFaces+movieMeter_IMDBpro+action
          +western+sport+horror+drama+crime+Engligh_Language+
            USA_prod+R_rating+production_company)
summary(mreg12)

##Removing crime
mreg13=lm(imdbScore~movieBudget+duration+nbNewsArticles+nbFaces+movieMeter_IMDBpro+action
          +western+sport+horror+drama+Engligh_Language+
            USA_prod+R_rating+production_company)
summary(mreg13)

##Now we have no insignificant variables.

##Let's look at the overall linearity of our new model
residualPlot(mreg13, quadratic=FALSE) ## this appears to be quite coney, indicating the potential presence of heteroskedasticity
residualPlots(mreg13, quadratic=FALSE)
ncvTest(mreg13) ## This confirms the presence of heteroskedasticity in the model 

# check for outliers & correcting for heteroskedasticity
qqPlot(mreg13, envelope=list(style="none"))

coeftest(mreg13, vcov=vcovHC(mreg13, type="HC1"))

# remove outliers
imdb4=imdb3[-c(492,1806), ] # removed those more than 4 SD
attach(imdb4)
mreg14=lm(imdbScore~movieBudget+duration+nbNewsArticles+nbFaces+movieMeter_IMDBpro+action
          +western+sport+horror+drama+Engligh_Language+
            USA_prod+R_rating+production_company, data = imdb4)
summary(mreg14) # decent model so far with R2 of .34, still not great.

qqPlot(mreg14, envelope=list(style="none"))

# remove additional outliers
imdb5=imdb4[-c(12, 316), ] 
attach(imdb5)
mreg15=lm(imdbScore~movieBudget+duration+nbNewsArticles+nbFaces+movieMeter_IMDBpro+action
          +western+sport+horror+drama+Engligh_Language+
            USA_prod+R_rating+production_company, data = imdb5)
summary(mreg15)

qqPlot(mreg15, envelope=list(style="none"))

imdb6=imdb5[-c(393, 395, 1578, 1581), ] 
attach(imdb6)
mreg16=lm(imdbScore~movieBudget+duration+nbNewsArticles+nbFaces+movieMeter_IMDBpro+action
          +western+sport+horror+drama+Engligh_Language+
            USA_prod+R_rating+production_company, data = imdb6)
summary(mreg16) 

# We have a decent model so far with an R2 of .35, but it is still not great.

qqPlot(mreg16, envelope=list(style="none"))

##Lets leave the remaining outliers now.

##Checking for collinearity

vif(mreg16) # nothing here even close to VIF greater than 4

residualPlot(mreg16) # looks pretty non-linear, so let's add in some polynomials
residualPlots(mreg16)

# 2nd degree polynomial
mreg17=lm(imdbScore~movieBudget+poly(duration, 2)+poly(nbNewsArticles, 2)+poly(nbFaces, 2)+poly(movieMeter_IMDBpro, 2)
          +action+western+sport+horror+drama+animation+Engligh_Language+USA_prod+R_rating, data = imdb6)
summary(mreg17) ##r-square of 0.39

# Individual degree adjustments
mreg18=lm(imdbScore~movieBudget+poly(duration, 2)+poly(nbNewsArticles, 3)+nbFaces+poly(movieMeter_IMDBpro, 4)
          +action+western+sport+horror+drama+animation+Engligh_Language+USA_prod+R_rating, data = imdb6)

summary(mreg18) ## Produces an r-squared of 0.42

qqPlot(mreg18, envelope=list(style="none"))

anova(mreg17, mreg18) ##Sum of squared residuals goes down with 3rd degree

residualPlots(mreg18) # Plots now look much more linear



##### Step 6: Evaluating predictive ability with K-Fold & LOOCV Cross-Validation #####

## K = 10
fit=glm(imdbScore~movieBudget+poly(duration, 2)+poly(nbNewsArticles, 3)+nbFaces+poly(movieMeter_IMDBpro, 4)
        +action+western+sport+horror+drama+animation+Engligh_Language+USA_prod+R_rating, data = imdb6)
mse=cv.glm(imdb6, fit, K=10)$delta[1]
mse
# provides an MSE of 0.72

## K = 5
fit2=glm(imdbScore~movieBudget+poly(duration, 2)+poly(nbNewsArticles, 3)+nbFaces+poly(movieMeter_IMDBpro, 4)
         +action+western+sport+horror+drama+animation+Engligh_Language+USA_prod+R_rating, data = imdb6)
mse2=cv.glm(imdb6, fit2, K=5)$delta[1]
mse2
# provides an MSE of 0.72

## LOOCV
fit3=glm(imdbScore~movieBudget+poly(duration, 2)+poly(nbNewsArticles, 3)+nbFaces+poly(movieMeter_IMDBpro, 4)
         +action+western+sport+horror+drama+animation+Engligh_Language+USA_prod+R_rating, data = imdb6)
mse3=cv.glm(imdb6, fit3)$delta[1]
mse3
# provides an MSE of 0.72



##### Step 7: Use the model to predict on test data #####

imdb_test <- read.csv("~/Downloads/test_data_IMDB.csv", header=TRUE)

attach(imdb_test)

imdb_test$November_Release = ifelse(releaseMonth == "Nov", 1, 0)

imdb_test$Engligh_Language = ifelse(language == "English", 1, 0)

imdb_test$Prod_country = ifelse(country %in% c("USA","UK","France"), country, "Others")

imdb_test$USA_prod = ifelse(country == "USA", 1, 0)

imdb_test$R_rating = ifelse(maturityRating == "R", 1, 0)

imdb_test$distribution_company = ifelse(distributor %in% c("Universal Pictures","Paramount Pictures","Columbia Pictures Corporation","Warner Bros.", "Twentieth Century Fox"), "Big Distribution Company", "Others")

imdb_test$production_company = ifelse(productionCompany %in% c("Universal Pictures","Paramount Pictures","Columbia Pictures Corporation","Warner Bros."), "Big Production Companies", "Others")

attach(imdb_test)

pred = predict(mreg18, newdata = imdb_test)



##### Step 8: Formatting & Visualizing Results #####

stargazer(pred, type="html", title="IMDB Challenge Predictions", style="apsr", digits = 1, align = TRUE)
stargazer(mreg18, type="html", dep.var.labels=c("IMDB Score"),covariate.labels=c("Movie Budget", "Duration", "Duration<sup>2</sup>", "# of News Articles", "# of News Articles<sup>2</sup>", "# of News Articles<sup>3</sup>", "# of Faces","Movie Meter Score", "Movie Meter Score<sup>2</sup>", "Movie Meter Score<sup>3</sup>", "Movie Meter Score<sup>4</sup>", "Action", "Western", "Sport", "Horror", "Drama","Animation", "English Language", "USA Production", "Rated R"), digits = 2, align=TRUE)
