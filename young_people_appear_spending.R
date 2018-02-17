###Preparing for Analysis
library(dplyr); library(tidyr); library(ggplot2); library(corrplot)

raw_data <- read.csv("young-people-survey/responses.csv")

movie_data <- raw_data %>% 
  filter(Movies != 1) %>%
  select(Horror:Action, Spending.on.looks) %>%
  na.omit()

glimpse(movie_data)

preferences <- movie_data %>%
  select(-Spending.on.looks) 

#Predictor Distributions

movie_data_long <-  gather(data=movie_data, key=MovieGenre, value=Rating, Horror:Spending.on.looks)

movie_data_long %>% 
  ggplot(aes(x=Rating)) + 
  geom_histogram(binwidth=1, color="red", fill="grey") + 
  scale_fill_hue(l=60) +
  facet_wrap(~MovieGenre) + 
  theme(legend.position="none") + 
  theme_minimal() +
  labs(y="Count", 
       x="Rating", 
       color="Data Type", 
       title="Rating Counts Across Movie Genres", 
       subtitle="N = 969", 
       caption="Source: https://www.kaggle.com/miroslavsabo/young-people-survey")

###Polychoric Correlations 

library(psych)
polycor <- preferences %>%
  polychoric()


col1 <- colorRampPalette(c("black","grey","red"))
polycor$rho %>%
  corrplot(order = "hclust", 
           tl.col='black',
           col = col1(100),
           tl.cex=.75, 
           method = "ellipse") 

###PCA Component Selection

pca_fit <- principal(r = polycor$rho, nfactors = 11, rotate = "Promax")

as.data.frame(pca_fit$values) %>%
  ggplot(aes(x=1:11, y=pca_fit$values)) + 
  geom_line() + geom_vline(xintercept=3, linetype="dashed", color="red") +
  geom_point(color="red", size=3) +
  scale_x_continuous(breaks = unique(1:11)) +
  theme_minimal() +
  theme(axis.text=element_text(size=12), text=element_text(size=12)) +
  labs(x = "Components", 
       y = "Eigenvalues", 
       title = "Eigenvalues for Estimated Components", 
       caption="Source: https://www.kaggle.com/miroslavsabo/young-people-survey") 

###Principal Component Analysis

pca_fit <- principal(r = polycor$rho, nfactors = 3, rotate = "Promax")
pca_fit$scores <- factor.scores(preferences, pca_fit)

###Cluster Size Selection

wss <- (nrow(pca_fit$scores$scores)-1)*sum(apply(pca_fit$scores$scores,2,var))

for(i in 1:7) {
  wss[i] <- sum(kmeans(pca_fit$scores$scores, centers=i)$withinss)
}

as.data.frame(wss) %>%
  ggplot(aes(x=1:7, y=wss)) +
  geom_line() + geom_vline(xintercept=3, linetype="dashed", color="red") +
  geom_point(color="red", size=3) +
  scale_x_continuous(breaks = unique(1:7)) +
  theme_minimal() +
  theme(axis.text=element_text(size=12), text=element_text(size=12)) +
  labs(x = "Clusters", 
       y = "Within Sum of Squares", 
       title = "Within Sum of Squares Using Different Amounts of Clusters", 
       caption="Source: https://www.kaggle.com/miroslavsabo/young-people-survey") 

###K-Means Cluster Analysis

set.seed(88)
k_means_mod <- kmeans(pca_fit$scores$scores, 3) 
k_means_mod$centers

###Cluster Plot

library(plotly)

plot_ly(as.data.frame(pca_fit$scores$scores), x = ~RC2, y = ~RC1, z = ~RC3, 
        color = ~as.factor(k_means_mod$cluster), 
        colors = c("red", "grey", "black")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC 2'),
                      yaxis = list(title = 'PC 1'),
                      zaxis = list(title = 'PC 3')), 
         title="Segments x Principal Components")

###Kruskal-Wallis Rank Sum Test

library(PMCMRplus)
krus_mod <- kruskalTest(Spending.on.looks ~ k_means_mod$cluster, movie_data)
krus_mod

post_krus <- kwAllPairsNemenyiTest(movie_data$Spending.on.looks, k_means_mod$cluster, dist="Chisquare")
post_krus

movie_clust <- cbind(na.omit(movie_data), k_means_mod$cluster)
names(movie_clust)[names(movie_clust) == "k_means_mod$cluster"] <- "Cluster"

movie_clust %>%
  group_by(Cluster, Spending.on.looks) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  ggplot(aes(x=Spending.on.looks,y=freq,fill=as.factor(Cluster))) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_manual("Segments", values = c("red", "grey", "black")) + 
  theme_minimal()  +
  labs(y="Proportion Selected", 
       x="Spending on Looks", 
       title="Proportion of Ratings for 'Spending on Looks' Across Segments", 
       subtitle="N = 969", 
       caption="Source: https://www.kaggle.com/miroslavsabo/young-people-survey")

movie_clust %>% 
  group_by(Cluster) %>%
  summarize(avg_spend_rating=mean(Spending.on.looks)) %>% 
  ggplot(aes(x=Cluster, y=avg_spend_rating, fill=as.factor(Cluster))) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_manual(values=c("red", "grey", "black")) +
  theme_minimal() +
  theme(axis.text=element_text(size=12), text=element_text(size=12)) +
  labs(fill = "Segments", 
       x = "Segments", 
       y = "Mean Appearance Spending (Ordinal Ratings 1-5)", 
       title = "Average Rating of Appearance Spending Across Segments", 
       subtitle="N = 969", 
       caption="Source: https://www.kaggle.com/miroslavsabo/young-people-survey")


