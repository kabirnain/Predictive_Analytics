library(randomForest)
library(ggplot2)
library(rpart)
library(rpart.plot)

#### Attach 2016 Election dataset ####

attach(election2016)

#### Setting up randomforest with 10,000 trees ####

myforest = randomForest(mostVotes~popChange+under5+over65+female+black+hispanic+undergrad+density
                   ,cp=0.01, ntree=10000, importance = TRUE, na.action = na.omit, do.trace=1000)


#### Adding a column to Election 2016 dataset with predicted values from our randomforest ####

election2016$predictedwinner10K = predict(myforest,election2016)

importance(myforest)

library(usmap)
attach(election2016)


#### Plotting USA map at county level with predicted winners with 10,000 tree randomforest predictions ####


plot_usmap("counties", data = election2016, values = "predictedwinner10K") + 
  scale_fill_manual(values = c("blue", "red"), na.translate = F) + 
  labs(title = "Predicted Winner by County", 
       subtitle = "Predictions from a 10,000 tree random forest.") +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )



#### Plotting USMAP at county level with actual winners ####

#plot_usmap("counties",data = election2016, values = "mostVotes")+ scale_fill_manual(values = c("blue","red"),na.translate = F) 

plot_usmap("counties", data = election2016, values = "mostVotes") + 
  scale_fill_manual(values = c("blue", "red"), na.translate = F) + 
  labs(title = "Predicted Winner by County", 
       subtitle = "Predictions from a 50 tree random forest.") +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )


myforest

varImpPlot(myforest)



#### Setting up randomforest with 50 trees ####

myforest2 = randomForest(mostVotes~popChange+under5+over65+female+black+hispanic+undergrad+density
                        ,cp=0.01, ntree=50, importance = TRUE, na.action = na.omit, do.trace=10)

#### Changing the predictedwinner column values to predicted values from our 50 tree randomforest ####

election2016$predictedwinner50 = predict(myforest2,election2016)


#### Plotting USA map at county level with predicted winners with 50 tree randomforest predictions ####

#plot_usmap("counties",data = election2016, values = "predictedwinner")+ scale_fill_manual(values = c("blue","red"),na.translate = F) 
plot_usmap("counties", data = election2016, values = "predictedwinner50") + 
  scale_fill_manual(values = c("blue", "red"), na.translate = F) + 
  labs(title = "Predicted Winner by County", 
       subtitle = "Predictions from a 50 tree random forest.") +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )


myforest2


#### Setting up randomforest with 5 trees ####

myforest3 = randomForest(mostVotes~popChange+under5+over65+female+black+hispanic+undergrad+density
                         ,cp=0.01, ntree=5, importance = TRUE, na.action = na.omit, do.trace=1)

#### Changing the predictedwinner column values to predicted values from our 5 tree randomforest ####

election2016$predictedwinner5 = predict(myforest3,election2016)

#### Plotting USA map at county level with predicted winners with 5 tree randomforest predictions ####

#plot_usmap("counties",data = election2016, values = "predictedwinner5")+ scale_fill_manual(values = c("blue","red"),na.translate = F) 

plot_usmap("counties", data = election2016, values = "predictedwinner5") + 
  scale_fill_manual(values = c("blue", "red"), na.translate = F) + 
  labs(title = "Predicted Winner by County", 
       subtitle = "Predictions from a 5 tree random forest.") +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )



