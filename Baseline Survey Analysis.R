baseline <- read.csv(file="/Volumes/GoogleDrive/Team Drives/Shortcut/UX/Research/Montana/02_2018_baseline_survey_t2/02_data/Survey Data/Baseline Follow-up Q1_2018 Numeric Version.csv", header=TRUE)

install.packages("car")
library(car)
install.packages("clustMixType")
library(clustMixType)
install.packages("dbscan")
library(dbscan)
install.packages("ggplot2")
library(ggplot2)


#replace 0's with NA
baseline[baseline == 0] <- NA
baseline$engineer_status

#recode nav strategy to be consistent:
baseline$nav_strategy_3 = (recode(baseline$nav_strategy_3, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1'))
baseline$nav_strategy_4 = (recode(baseline$nav_strategy_4, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1'))
baseline$engineer_status = (recode(baseline$role, '1=0; 2=0; 3=0; 4=0; 5=0; 6=1; 7=0; 8=0; 9=0; 10=0; 11=0; 12=0; 13=0'))
baseline$meetings_hours = (recode(baseline$meetings_hours, '1=0; 2=1; 3=2; 4=3; 5=4; 6=5; 7=6; 8=7; 9=8; 10=9; 11=10; 12=11; 13=12; 14=13; 15=14; 16=15; 17=16; 18=17; 19=18; 20=19; 21=20; 22=21; 23=22; 24=23; 25=24'))

#get average nav strategy var:
baseline$nav_strategy_average = (baseline$nav_strategy_1 + baseline$nav_strategy_2 + baseline$nav_strategy_3 + baseline$nav_strategy_4)/4

#omit any nonresponses for clustering
baseline_subset <- subset(baseline, select=c("SubjectNum", "leesman", "sense_of_direction", "nav_strategy_average", "tenure_building_months", "tenure_Google_months", "travel_frequency", "meetings_hours"))

#omit na's, remove subjectnum, & centralize data
responses = na.omit(baseline_subset)
responses_subset <- subset(responses, select=c("leesman", "sense_of_direction", "nav_strategy_average", "tenure_building_months", "tenure_Google_months", "travel_frequency", "meetings_hours"))
centralized_y = scale(responses_subset, center = TRUE, scale = TRUE)

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15
k.max <- 15 # Maximal number of clusters
# data <- iris.scaled
wss <- sapply(1:k.max, 
              function(k){kmeans(centralized_y, k, nstart=10 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 4, lty =2)

#clustering by x# of clusters using normalized data
kmeans_matrix = kmeans(centralized_y, 4)
#display kmeans clustering outcome:
kmeans_matrix


#add clustering vector to matrix
responses$pesona_cluster = kmeans_matrix$cluster
responses_subset$pesona_cluster = kmeans_matrix$cluster
#personacluster <- as.data.frame()

#add cluster number to original data set & export
persona_analysis <- merge(responses, baseline, by = "SubjectNum")
persona_analysis <- na.omit(persona_analysis)
write.csv(persona_analysis, file="/Volumes/GoogleDrive/Team Drives/Shortcut/UX/Research/Montana/02_2018_baseline_survey_t2/02_data/Survey Data/Baseline Follow-up Q1_2018 With Persona Cluster Labels.csv")

#plot cluster analysis
require(ggplot2)
pca_res <- prcomp(as.matrix(responses_subset, center = TRUE, scale = TRUE))
plot_data <- cbind(as.data.frame(pca_res$x[, 1:2]), labels = responses_subset)

ggplot(plot_data, aes(x = PC1, y = PC2, colour = responses_subset$pesona_cluster)) +
  geom_point()

#analysis based on differences between persona groups
#get num of engineers v. non engineers, sat levels, imp levels
summary(aov(persona_analysis$satisfaction_office_nav~persona_analysis$pesona_cluster))
summary(aov(persona_analysis$satisfaction_overall~persona_analysis$pesona_cluster))
summary(aov(persona_analysis$satisfaction_ease_of_use~persona_analysis$pesona_cluster))
summary(aov(persona_analysis$satisfaction_visual_appeal~persona_analysis$pesona_cluster))
summary(aov(persona_analysis$satisfaction_search~persona_analysis$pesona_cluster))
summary(aov(persona_analysis$satisfaction_map_visualization~persona_analysis$pesona_cluster))

summary(aov(persona_analysis$importance_office_nav~persona_analysis$pesona_cluster))
summary(aov(persona_analysis$importance_ease_of_use~persona_analysis$pesona_cluster))
summary(aov(persona_analysis$importance_visual_appeal~persona_analysis$pesona_cluster))
summary(aov(persona_analysis$importance_search~persona_analysis$pesona_cluster))
summary(aov(persona_analysis$importance_map_visualization~persona_analysis$pesona_cluster))

summary(aov(persona_analysis$difficulty_office_navigation~persona_analysis$pesona_cluster))

t.test(baseline$difficulty_installation~baseline$PrimaryDevice, na.action="na.exclude")

k = kmeans_matrix$centers
k_df = as.data.frame(k)


