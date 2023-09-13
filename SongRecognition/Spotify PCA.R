spotify = read.csv("~/Downloads/spotify.csv")


#####Split data into two

spotify_labels=spotify[,c(1,2)]
spotify_vars=spotify[,c(3:11)]


####Running  principal component analysis  (PCA)######
pca=prcomp(spotify_vars, scale=TRUE)
pca

####Visualizing principal components
install.packages("ggfortify")
library(ggfortify)
autoplot(pca, data = spotify_vars, loadings = TRUE,  loadings.label = TRUE )
autoplot(pca, data = spotify_vars,  loadings = TRUE, col=ifelse(spotify_labels$artist=="Drake","blue","transparent"),  loadings.label = TRUE )
autoplot(pca, data = spotify_vars,  loadings = TRUE, col=ifelse(spotify_labels$artist=="Arcade Fire","blue","transparent"),  loadings.label = TRUE )

######Percentage of variance explained (PVE) plot

pve=(pca$sdev^2)/sum(pca$sdev^2)
par(mfrow=c(1,2))
plot(pve, ylim=c(0,1))
plot(cumsum(pve), ylim=c(0,1))




