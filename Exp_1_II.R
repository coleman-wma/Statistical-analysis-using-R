# Insert comments with hash signs
# What's the working directory?
getwd()

# Use this to read in a dataset
data_demographics <- read.csv('/Volumes/GoogleDrive/My Drive/DIT-PhD/EXP_2/R_Analysis/soundFeatures.csv', 
                              header=TRUE, stringsAsFactors=TRUE)

# Use this to read the dataset into R memory so you can refer directly to variables
attach(data_demographics)

class(gender)
levels(gender)

names(data_demographics)
summary(data_demographics)

head(data_demographics)

?summary

boxplot(data_demographics ~ gender)

summ_data <- summary(data_demographics)
View(summ_data)

sound_ratings <- data_demographics[,4:43] 
View(sound_ratings)

dim(sound_ratings)
head(data_demographics)
summary(data_demographics)
73/112

?apply

# Mean of female scores for Thunderstorm
mean(Thunderstorm[gender=="female"])

# Mean of male scores for Thunderstorm
mean(Thunderstorm[gender=="male"])

# Mean of all scores for Thunderstorm
mean(Thunderstorm)

# Subset Female data - everything in data_demographics where gender = female
FemData <- data_demographics[gender=="female", ]

# Subset Male data
MaleData <- data_demographics[gender=="male", ]

dim(FemData)
dim(MaleData)

# If we want to take this further - subset by two categorical
# This won't work here because the data isn't formatted this way, but just for example
# Subset Male data
MaleOver45 <- data_demographics[gender=="male" & age>45, ]

summFem <- summary(FemData)
summMal <- summary(MaleData)
tsummMal <- t(summMal)
tsummMal
summFem
summ_data

# calculate SDs for all data by columns
allSD <- apply(data_demographics, 2, sd)
# join SDs to other summary stats for all tests
summ_data <- rbind(summ_data, allSD)
View(summ_data)
head(summ_data)

# Repeat for Female respondents
sdFem <- apply(FemData, 2, sd)
View(sdFem)
names(FemData)
summFem <- rbind(summFem, sdFem)
View(summFem)

# Repeat for Male respondents
sdMale <- apply(MaleData, 2, sd)
View(sdMale)
summMal <- rbind(summMal, sdMale)
View(summMal)

# Join all with female and male subsets for comparison
summStats <- rbind(summ_data, summFem, summMal)
?rbind
View(summStats)

# Export this table to .csv
write.csv(summStats, "summStats.csv")

getwd()

# Calculate mean sound rating
sound_rating_Means <- apply(sound_ratings, 2, mean)
head(sound_rating_Means)

# Calculate standard deviation of sound ratings
sound_rating_SDs <- apply(sound_ratings, 2, sd)

# Calculate summary stats
sound_rating_Summary <- apply(sound_ratings, 2, summary)
head(sound_rating_Summary)
View(sound_rating_Summary)

?apply
?rbind
View(sound_rating_SDs)

# Bind the mean and SD values into one table
mean_sd <- rbind(sound_rating_Means, sound_rating_SDs)
View(mean_sd)

# Bind Summary Stats to mean and SD values - compare means to see if they match
summary_mean_sd <- rbind(sound_rating_Summary,mean_sd)
View(summary_mean_sd)

# Bind mean & SD with originl ratings
allratings_meanSD <- rbind(sound_ratings, mean_sd)
View(allratings_meanSD)

# Transpose a data frame
sort_mean_sd <- t(mean_sd)
View(sort_mean_sd)

# Attach sound class data
classFactor <- c("Natural","Natural","Household","Animals","Household","Household","Household",
                 "Animals","Animals","Exterior","Exterior","Natural","Exterior","Animals",
                 "Human","Natural","Human","Animals","Natural","Exterior","Natural","Animals",
                 "Household","Human","Human","Household","Household","Natural","Exterior",
                 "Human","Exterior","Human","Exterior","Household","Animals","Animals",
                 "Natural","Exterior","Human","Human")

View(sound_rating_Means)

meanClass <- cbind(classFactor, sound_rating_Means)
View(meanClass)

sdClass <- cbind(classFactor, sound_rating_SDs)
View(sdClass)

# Plot means and SDs on the same scatter
plot(sound_rating_Means, sound_rating_SDs,
     xlab="Sound Mean Score",
     ylab="Sound Score Standard Deviation",
     main="Summary of Sound Scores and Standard Deviations by Class",
     col=as.factor(classFactor), pch = 19, cex = 1, lty = "solid", lwd = 2)

# add sort_mean_sd row.names as data labels
     text(sound_rating_Means, sound_rating_SDs,
          labels = row.names(sort_mean_sd), cex=0.7, pos = 1)
     
     # legend
     
     legend("bottom", legend=c("Exterior", "Human", "Household", "Animals", "Natural"),
            col = c("red","blue", "green", "black", "cyan"), pch=c(15), bg="white", border="black")

####################################################
     names(data_demographics)
     
     boxplot(Baby_Crying ~ gender, data=data_demographics, 
             main="Gender Comparison", 
             xlab="Score by Gender", ylab="Sound Rating")

     ?tapply
     ?list

     tapply(Thunderstorm, gender, mySummary)     

     mySummary <- function(x) {
       theSD <- sd(x) #Standard deviation
       # fiveNumSumm <- fivenum(x) #Tukey's five number summary, usefull for boxplots
       # IQR(x) #Interquartile range
       # quantile(x) #Compute sample quantiles
       # range(x) # Get minimum and maximum
       # result <- list(fiveNumSumm,theSD)
       data <- cbind(min(x), median(x), max(x), theSD)
       result <- as.data.frame(data, col.names=c("MIN", "MEDIAN", "MAX", "SD"))
       return(result)
     }
     
     tapply(Thunderstorm, gender, mySummary) 

     ?table
     ?fivenum # min, 1stQ, median, 3rdQ, max
     ?as.data.frame
     ?cbind
     ?min
     ?median
     
     ################## MUNGED DATA PLOTTING #############
     
     munged_data <- read.csv('/Volumes/GoogleDrive/My Drive/DIT-PhD/STATS/EXP_1_R_ANALYSIS/Munged_all_male_female.csv', 
                                header=TRUE)
munged_data     

# Plot means and SDs on the same scatter
plot(munged_data$ALL.MEAN, munged_data$ALL.SD,
     xlab="Sound Mean Score",
     ylab="Sound Score Standard Deviation",
     main="Summary of Sound Scores and Standard Deviations",
     col= "red", pch = 19, cex = 1, lty = "solid", lwd = 2)

points(munged_data$FEMALE.MEAN, munged_data$FEMALE.SD,
       col= "blue", pch = 20, cex = 1, lty = "solid", lwd = 2)

points(munged_data$MALE.MEAN, munged_data$MALE.SD,
       col= "orange", pch = 18, cex = 1, lty = "solid", lwd = 2)

# add sort_mean_sd row.names as data labels
text(munged_data$ALL.MEAN, munged_data$ALL.SD,
     labels = munged_data$GROUP, cex=0.7, pos = 1)

# Plot FEMALE mean and SDs on the same scatter
plot(munged_data$FEMALE.MEAN, munged_data$FEMALE.SD,
     xlab="Sound Mean Score",
     ylab="Sound Score Standard Deviation",
     main="Female Sound Scores and Standard Deviations",
     col= "blue", pch = 20, cex = 1, lty = "solid", lwd = 2)

# add data labels
text(munged_data$FEMALE.MEAN, munged_data$FEMALE.SD,
     labels = munged_data$GROUP, cex=0.7, pos = 1)

# Plot MALE mean and SDs on the same scatter
plot(munged_data$MALE.MEAN, munged_data$MALE.SD,
     xlab="Sound Mean Score",
     ylab="Sound Score Standard Deviation",
     main="Male Sound Scores and Standard Deviations",
     col= "brown", pch = 18, cex = 1, lty = "solid", lwd = 2)

# add data labels
text(munged_data$MALE.MEAN, munged_data$MALE.SD,
     labels = munged_data$GROUP, cex=0.7, pos = 1)

?points
?text

labelsVector <- munged_data$GROUP
labelsVector

# What are the graph margins?
par('mar')

# reset graph margins to see sound labels
par(mar=c(9.1,4.1,4.1,2.1))
    
    
# Plot all means on the scatter
plot(munged_data$ALL.MEAN,
     xlab="",
     xaxt="n", # prevents the drawing of tick marks and numbers on the x axis
     ylab="Mean Sound Rating",
     main="Mean Sound Ratings - Comparison by Gender",
     # xlim=c(1,40),
     ylim=c(1,3),
     col= "red", pch = 15, cex = 1, lty = "solid", lwd = 2, las=2)

# Draw gridlines
grid(nx = NULL, ny = NULL, col = "lightgray", lty = 5,
     lwd = par("lwd"), equilogs = TRUE)

# Draw sound names on x axis
axis(1, 
     at=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
          21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40), 
     labels=labelsVector, 
     pos=0.919, # vertical placement of new axis
     lty="solid",
     col="black",
     las=2,
     tck=-0.01, 
     outer=0)

# Draw female mean points to graph
points(munged_data$FEMALE.MEAN,
       col= "blue", pch = 16, cex = 1, lty = "solid", lwd = 2)

# Draw male mean points to graph
points(munged_data$MALE.MEAN,
       col= "green", pch = 17, cex = 1, lty = "solid", lwd = 2)

# Add a legend, 'legend' is for the test int he legend, 'fill' is for colours,
# which has been replaced below with 'col=' and 'pch=' to specify colours and
# shapes for the legend
legend("topleft", legend=c("All", "Female", "Male"),
       col = c("red","blue", "green"), pch=c(15,16,17), bg="white", border="black")
?legend

names(munged_data)
?plot

# A numerical vector of the form c(bottom, left, top, right) which gives the 
# number of lines of margin to be specified on the four sides of the plot. 
# The default is c(5, 4, 4, 2) + 0.1.
par(mar=c(9.1,4.1,4.1,2.1))

?par
    

# Plot all SDs on the scatter
plot(munged_data$ALL.SD,
     xlab="",
     xaxt="n", # prevents the drawing of tick marks and numbers on the x axis
     ylab="Standard Deviation of Sound Ratings",
     main="Standard Deviation of Sound Ratings - Comparison by Gender",
     # xlim=c(1,40), 
     ylim=c(0.1,1),
     col= "red", pch = 15, cex = 1, lty = "solid", lwd = 2, las=2)

# Draw gridlines
grid(nx = NULL, ny = NULL, col = "lightgray", lty = 5,
     lwd = par("lwd"), equilogs = TRUE)

# Draw sound names on x axis
axis(1, 
     at=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
     21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40), 
     labels=labelsVector, 
     pos=0.065, # vertical placement of new axis
     lty="solid",
     col="black",
     las=2,
     tck=-0.01, 
     outer=0)

?axis
?plot

# Draw female SD points to graph
points(munged_data$FEMALE.SD,
       col= "blue", pch = 16, cex = 1, lty = "solid", lwd = 2)

# Draw male SD points to graph
points(munged_data$MALE.SD,
       col= "green", pch = 17, cex = 1, lty = "solid", lwd = 2)

# Add a legend, 'legend' is for the test int he legend, 'fill' is for colours,
# which has been replaced below with 'col=' and 'pch=' to specify colours and
# shapes for the legend
legend("topleft", 
       legend=c("All", "Female", "Male"),
       col = c("red","blue", "green"), pch=c(15,16,17), bg="white", border="black")
