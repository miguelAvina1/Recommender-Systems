# Inteligent Systems: German Credit Homewok.
#
# Authors:  Miguel Avina
#           Arturo Perez
# Date:     06/03/2020
# -----------------------------------------------------------------------------

# ADD GLOSSARY OF VOTE == RATING

# First we will import the following attributes from following files:
#
# geoplaces2.csv: 'placeID', 'name', 'city', 'latitude' and 'longitude'.
# rating_final.csv: 'userID', 'placeID' and 'rating'.
# chefmozcuisine.csv: 'placeID' and 'Rcuisine'.

# Importing all the file 
geoPlaces <- read.csv(file = "RCdata/geoplaces2.csv", header = TRUE, sep = ',')
rating    <- read.csv(file = "RCdata/rating_final.csv", header = TRUE, sep = ',')
cuisine   <- read.csv(file = "RCdata/chefmozcuisine.csv", header = TRUE, sep = ',')

# Keeping only with the previously mentioned variables
library(dplyr)
geoPlaces <- select(.data = geoPlaces, placeID, name, city, latitude, longitude)
rating    <- select(.data = rating, userID, placeID, rating)
cuisine   <- select(.data = cuisine, placeID, Rcuisine)

# Before continuing, we will display the first entries of each imported files.
# We recommended to perform this step to have a general idea of the the data we will
# be handling during the excercise
head(geoPlaces)
head(rating)
head(cuisine)

# Some findings we observe by doing the last step are: 
# 1. Some entries of the 'city' attribute refers to same cities with different notation. 
#    E.g: s.l.p and San Luis Potosi. We might consider removing this attribute or
#    unifying same places with a specific name.
# 2. name attribute listed the natural human-readble name of the restaurants. 
#    Further analysis using this kind of entry could be problematic. We might consider
#    removing this attribute and kept only with placeID.

# -- Homework 1. From the imported files, generate a single data frame with the
# Previously indicated variables

# Before doing this point, we want to know more regarding the data we imported.
# We observe that there are different number of observations in each one of the sets
dim(cuisine)[1]
dim(geoPlaces)[1]
dim(rating)[1]

# In rating we expect to have repeated both UserID and placeID, because each user 
# could have rated more than one place, and more than one place could have had a 
# score from many users. 
# Just for clarity, we will extract the number of unique userID and placeID, which 
# will tell us how many users and restaurants are in the dataset.
rating_UserIDs <- unique(rating$userID)
nlevels(rating_UserIDs)
# So here we are talking about 138 users, the UserID increments from U1001 to U1138
# This can also be noticed looking at the Data window on the right of RStudio

# Know, for clarity also, we will extract the number of unique placeID
# We noticed that placeID is not a factor but a numerical value, this does not make sense
# since we can not have fractional placeID and they must be taken as categorical. 
# Lets transform it to a factor
rating$placeID <- as.factor(rating$placeID)
nlevels(rating$placeID)
# There are 130 different places. The ID does not increase in an ordered way, so there
# are gaps in the PlaceID. This indeed correspond to the number of observation of 
# geoPlaces. (Fill the table for explanation in html)

# We will also transform placeID from geoPlaces to factor. 
geoPlaces$placeID <- as.factor(geoPlaces$placeID) # These are also 130 levels, being the 130 places
# For comprobation we will check if they are the SAME placeIDs in both geoPlaces and rating
setequal(unique(geoPlaces$placeID), unique(rating$placeID))


# However, we observed from the menu on the right that there are 129 levels of name, 
# and we are expecting 130, one for each PlaceID. This tell us that there's a repeted name
# We can know it by doing:
n_occur <- data.frame(table((geoPlaces$name)))
n_occur[n_occur$Freq > 1,]
# We observed that we have a restaurant name repeated
geoPlaces[geoPlaces$name %in% n_occur$Var1[n_occur$Freq > 1],]
# This might not be interesting but it is good to know to avoid any misinterpretation in 
# the future

# FInally, we will analize the data in the cuisine dataframe. What we would expect here is 
# to have the same 130 levels for placeID as in rating and geoPlaces.
# Rcuisine is a 59 level factor, which tell us that there are 59 kinds of restaurants in the dataset.
# However, we have 916 observations for placeID while we are only dealing with 130 
# restaurants.
# Lets convert PlaceID also to a factor as we did for geoPlaces and rating
cuisine$placeID <- as.factor(cuisine$placeID)
nlevels(cuisine$placeID)

# Here we observer that there are 769 different PlaceID, so we have more information
# of restaurants here than in the other set. But, are there ALL the restaurants from
# rating here in cuisine?
commonPlaceIDs <- as.factor(intersect(cuisine$placeID, rating$placeID))
nlevels(commonPlaceIDs) # The intersection between placeID in cuisine and placeID in rating is 95 restaurants


n_occur <- data.frame(table(cuisine$placeID)) # Getting frequencies of each factor (placeID)
n_occur <- n_occur[order(-n_occur$Freq),]
head(n_occur[n_occur$Freq > 1,])  # here we can see the most repeated entries, one single placeID have 9 diff, cuisines!

# Lets look at some of these repeated entries
cuisine[cuisine$placeID %in% n_occur$Var1[n_occur$Freq > 1],][1:10,]
# We observe the pattern we have been describe
# Just for curiosity, we will observe the placeID with more assigned cuisines in this dataset
filter(cuisine, placeID == n_occur$Var1[1])  # Hilarious 

# COnclusion: each restaurant have more than one cuisine related to it.








# P. Ej si mergeamos geoPlaces and ratings, we are just adding the name to each entry, since we have name for ALL placeIDs
places <- merge(rating, geoPlaces, by="placeID") # And we end up with 1161 entries, everything good here!

# This is a quick summary of what we know so far:
# 1. rating dataset and geoPlaces datasets have the same set of restaurants (PlaceIDs), as we would expect
# 2. data from cuisine have a larger set of restaurants than the one from rating and geoPlaces
# 3. Not all restaurants listed in rating and geoPlaces datasets is in cuisine. So we have restaurants from which we don't know their cuisine
# 4. In cuisine, some placeIDs have more than one cuisine linked to it.


# So, finally, to create the daaframe needed (recall that to have a dataframe, we need the same number of observation (rows) in each variable)
# we will:
# Merge info from geoPlaces and rating final using placeID as pivot. This is translated to adding the place's additional information:
# name, city, latitude and logitud.
# Now, we can NOT merge the cuisine info. Altought the merge() will drop all the unused restaurants (the ones that are not listed in neither 
# rating nor geoplaces), extra 'votes' will be added because of the fact that in cuisine there are more than one entry per placeID because 
# they have more than one cuisines. Doing this will give a bad result when doing points regarding popularity of the places (points 4 and 5),
# because we are adding extra non-existing votes. 

# Lets prove what would happen if we merged it:
# Lets first get one placeID to focus on. We will choose one existing in both rating and cuisine dataset, so we will pick from commomPlaceIDs.
# From those common placeIDs, we will get one that have more than one cuisine assigned to it.
z <-  cuisine[cuisine$placeID %in% n_occur$Var1[n_occur$Freq > 1],]   # Getting all placeIDs we have with more than one cuisine
examplePlaceID <- intersect(commonPlaceIDs, z$placeID)[1]  # Extracting the first observation of reference
# Lets look at this placeID in ratings
filter(rating, placeID == examplePlaceID)  # So, we have a total of 10 votes, obviosly from different users

#Furthermore, from lets focus on a specific userID-PlaceID vote
examplePair <- filter(rating, placeID == examplePlaceID)[1,1:2]
examplePair
exampleUserID <- (examplePair[1])

# Now, just merge cuisine with rating and geoPlaces and see what happens

badMerge <- merge(places, cuisine, by="placeID")

# Lets retrieve from this merge the placeID we are examination
z <- filter(badMerge, placeID == examplePlaceID) # Now, suddenly we have 20 votes (or ratings for this restaurant) instead of 10
z

# Now, lets get the userID-PlaceID in this bad merge
filter(z, userID ==  exampleUserID[1,1])
# As expected, this person passed from one original vote to two votes. 
# Conclusion: We can not make this merge

# Lets imagine we have a restaurant with a single vote from a single person. This, will result in having 
# UserID    placeID   rating 
# 100       500       2
# Now, if the data in cuisine is the following:
# placeID   cuisine
# 500       Mexican
# 500       Familiar
# 500       Tacos
# When we merge these two using placeID as pivot, we will get:
# UserID    PlaceID   rating  cuisine
# 100       500       2       Mexican
# 100       500       2       Familiar
# 100       500       2       Tacos
# So, placeID has just increase its popularity by 3. Just because it was saved with 3 different cuisines.


# How to preserve cuisine information without modifyint the ratings and adding these extra votes can be done in 
# many different ways. We will focus on doing what we will need for this homework, however, it might not be the
# best solution if further analysis would be made with this cuisine information. THe homework only asks what is 
# the kind of food of the 10 most popular restaurants. So we are just going to merge placeIDs with more than one 
# level into a single, new level. FOr example, taking the last table, placeID will be combined into:
# placeID   cuisine
# 500       Mexican_Familiar_Tacos.
# We also took this apporach because there are other levels already with this syntax as: Bar_Pub_Brewery.

# we start by keeping just the placeIDs from cuisine we are interested (the 130 listed in geoPlaces):
cuisine <- filter(cuisine, placeID %in% commonPlaceIDs)  # We end up with 112 obs
# Now, we will combine the this info (112 obs) into the real 95 unique placeIDs that we have info.


# Demonstration in one placeID
currentPlaceID <- filter(cuisine, placeID == 135057)
new_level2 <- as.character(currentPlaceID$Rcuisine[1])
currentPlaceID <- currentPlaceID[-c(1),]
for (cuisineKind in currentPlaceID$Rcuisine) {
  new_level2 <- paste(new_level2, as.character(cuisineKind), sep='_')
}
cuisine_merged0 <- cuisine  # We will use as a template (for factors and so) the cuisine dataframe
cuisine_merged0 <- cuisine_merged0[1,]  # And keep first variable to have the structure intact, we will remove this at the end
# ADD A DISCLAIMER THAT THERE IS NOT A LOT OF TRAINING IN R SO BAD PRACTICES AND INEFFICIENT CODE MAY BE PRESENT
cuisine_merged0 <- rbind(cuisine_merged0, c(135053, new_level2))



merged_Places <- list()
merged_Cuisines <- list()
#cuisine_merged_Places <- c(cuisine_merged_Places, 3)

for (place in commonPlaceIDs) { # We will iterate along each placeID we have 
  currentPlaceID <- filter(cuisine, placeID == place)    # and will examine one by one
  new_level2 <- as.character(currentPlaceID$Rcuisine[1])
  currentPlaceID <- currentPlaceID[-c(1),]
  for (cuisineKind in currentPlaceID$Rcuisine) {
    new_level2 <- paste(new_level2, as.character(cuisineKind), sep='_')
  }
  merged_Places <- c(merged_Places, place)
  merged_Cuisines <- c(merged_Cuisines, new_level2)
}
cuisine <- data.frame('placeID' = unlist(merged_Places, use.names = FALSE), 'cuisine' = unlist(merged_Cuisines, use.names = FALSE) )
rm(merged_Places)
rm(merged_Cuisines)
rm(currentPlaceID)
# So we have a useful cuisine data for the places we are analyzing.
# By visualizing the data, we came across a specific case were we merged 'bar' with 'bar_Pub_brewery', leading to 'bar_bar_pub_brewery'
# for this specific case we will replace bar_bar_pub_brewery to just bar_pub_brewery. TODO OR FUTURE WORK BC FUCK IT

# now lets just merge this data with the rest
places_drop <- merge(places, cuisine, by="placeID") # If we do this, we will lost all observations for which we don't have cuisine info
uniquePlaceID <- droplevels(as.factor(unique(places_drop$placeID)))  # demonstration of the previous sentence
nlevels(uniquePlaceID)
# INFO. We used droplevels because you can have levels with 0 occurencies, so to better reflect the number of existing factos, drop those
# We must not do this
places <- merge(places, cuisine, by="placeID", all=TRUE) # Here we dont lose info

levels(places[,8]) <- c(levels(places[,8]),"?")   # we are adding ? to tell that we dont have that info :(
places[is.na(places)] <- '?'

# After all this long process, we finish with point 1.


head(places)
summary(places)


# 2. Realizar una análisis descriptivo de las variables e indica si existen datos perdidos en
# algunas de ellas. De ser así, realiza un análisis para determinar y justificar la decisión
# que tomes sobre dichos datos perdidos.

# placeID
# This variable was numeric and we already changed it to factor because it is actually a qualitative
nlevels(places$placeID)  # We have 130 levels, 130 places
# In previous homewook, we analyzed the proportion of observations per level, however, thi does not make sense 
# since we can not combine them, and the analyzis we will do later does not benefit from merging levels
head(prop.table(table(places$placeID))) # This is not useful

barplot(height = table(places$placeID)) # This plot is useful, it shows a nice represntation of the number of 
# occurences of each placeID, that is, the popularity of the restaurants.
# Graphicall, we can aslo observe that it seems to be a mean value of ~ 5 ratings per place, lets get this number right

# install.packages('plyr')
# library('dplyr')
t <- count(places, 'placeID')
sprintf("Average votes per placeID: %s", mean(t$freq))
# Ok, we know that this is a categorical value, however, the number of votes per restaurant is numerical, lets analyze it a bit
summary(t$freq)  # We are going to advance a bit, but from here we can see that there's a restaurant with 36 vots and the restaurant
# with less votes have just 3

boxplot(t$freq)  # From here we know that there are some atipical values, this atipical values could be seen as 
# outstanding (or popular) restaurants. We are not going to do anything with this info bcause removing it does not make sense.
# I think we can not get anything else from this variable



# userID
# This was already a categorical (factor) variable, so we didn't do any modification
# We'll do the same analysis for this variable as we did for placeID becuase they are fair similar variables
barplot(height = table(places$userID)) 
# GRaphically, we are observing the number of places each user has rated. It doesn't show too uncommon users, 
# altough there are some large bars, they doesn't seem as different as those from places
t <- count(places, 'userID')
sprintf("Average places rated per User: %s", mean(t$freq))
summary(t$freq)  # The info of places and userID is quite similar.
boxplot(t$freq) # And here we observer what we mentioned before, there are not uncommon users: Nobody rated too much
# or too little to become an outlier

# Rating
# Rating was imported as numerical. If we check the dataframe using RStudio, we can manually observe that
# there is not more than 3 values: 0, 1, 2 . So turning it into categorical (factor) would be better
# However, lets assume we have a significantly larger dataset and visually inspecting the data woudln't be feasible
# Then, we would start by plotting the rating as histogram
hist(places$rating)
# From here, it is obvious that we have just three values
plot(density(places$rating))
# Even if we create the density plot, it is also obvious that is a three-modal density.
# So lets change it to a factor
places$rating <- as.factor(places$rating)
# To confirm, lets check the factors
summary(factor(places$rating))
# We observe that there are 3 factors, and we can observe  the number of ratings in all the dataset.
# Know it makes sense to check the proportion of observations per level

prop.table(table(places$rating)) # This is now useful
# We observe that we have a good distribution of observations per level (>10%). This 10% is useful when
# creating models (as we did for titanic and germancredit). So even if we didn't have this distribution, we
# would not merge the ratings


# Name
# There is not much thing we could do with this than observing a few obs
head(places$name) # Each place name is correlated to placeID so this variable just add a better identifier to the numerical placeID




# City
# 
nlevels(places$city)  # 17 cities
levels(places$city) # we can observe the 17 cities by doing this

# After doing this is evident that same places are taken different because of typos or extra spaces
# We know this by observing the data, this would not be feasible for larger sets. 
# In this case we could use latitude and longitude to retrieve the city and just ignore this data.
# But supposing we DON'T have latitud and longitude, in this case we will merge levels manually.
# Maybe correlation between level names could be used to identify same cities with typos or written a bit different,
# however, this is out of the scope of this homework due to time limitation


places$city <- factor(places$city, labels=c("Undefined","Cd. Victoria","Cd. Victoria","Cd. Victoria",
                                            "Cuernavaca","Cuernavaca","Jiutepec", "San Luis Potosi",
                                            "San Luis Potosi", "San Luis Potosi", "San Luis Potosi",
                                            "San Luis Potosi", "San Luis Potosi", "San Luis Potosi",
                                            "Soledad", "Cd. Victoria", "Cd. Victoria"))
# Now we have only 6 levels which are:
levels(places$city)
prop.table(table(places$city))
# Most of the places are in San Luis Potosi and none of the other have more than 10% of the observations

# Now we just do the same analisis as placeID/userID
barplot(height = table(places$city)) 
# GRaphically, we are observing the number of places each user has rated. It doesn't show too uncommon users, 
# altough there are some large bars, they doesn't seem as different as those from places
t <- count(places, 'city')
# It wouldn't be useful to get the average since we can graphically see that it will not give a good info.

summary(t$freq)  # Summary is useful just to know the city with less rankings (834) and with more (17)
boxplot(t$freq) # Also, the boxplot doesn't show anything


# Latitude
# This is an strictly numerical value, so we can do numerical analysis

print(summary(places$latitude))
sprintf("Std dev: %s", round(sd(places$latitude, na.rm = TRUE), digits = 4))
boxplot(places$latitude, ylab = "Lat", xlab = 'Latitude')
hist(places$latitude)  # In this plot we observe as we have divided in three different main locations regarding latitude
plot(density(places$latitude, na.rm = TRUE))  # Here we have 4 'peaks', so there may be locations in 4 different latitudes

# Longitude
print(summary(places$longitude))
sprintf("Std dev: %s", round(sd(places$longitude, na.rm = TRUE), digits = 4))
boxplot(places$longitude, ylab = "Long", xlab = 'Longitude')
hist(places$longitude)  # In this plot we observe as we have divided in four different main locations regarding longitude
plot(density(places$longitude, na.rm = TRUE))  # Here we have 4 'peaks', so there may be locations in 4 different longitude

# To be honest, I didn't feel like I got a lot of info regarding this
# These are coordinates, and they are normally taken as pairs: latitude-longitude.
# Let's plot them

plot(places$longitude, places$latitude)  # Now we can easily observe three main cumulus, but we have 5 different cities (and 1 undefined)
# Now, let's get context. Cuernavaca is a city which adjoints Juitepec, that's why they will be seen as a main cumulous. 
# And Soledad, 9soledad de Graciano Sanchez) is also adjacent to San Luis Potosi, that's why those two merges in one single main cumulus
# I think this kind of analysis is more suitable for this information, rather than mean, quartile, etc.
# Furthermore, if we 'zoom in' we can actually observe the two comulus of juitepec and cuernavaca




# FInally, the last variable
# Cuisine
# This was already a categorical (factor) variable, so we didn't do any modification
barplot(height = table(places$cuisine)) 
# GRaphically, we cant observe much...
t <- count(places, 'cuisine')
sprintf("Average places per cuisine: %s", mean(t$freq))
summary(t$freq)  # So we have just 4 places of a not so popular cuisine and the maximum is regarding the '?'
outvals <- boxplot(t$freq)$out # So we have two outliers, one we know if from '?' The other one should be the most popular
# cuisine of all restaurants which is:
t[which(t$freq %in% outvals),]
# Mexican (no shit) with 230



# realiza un análisis para determinar y justificar la decisión
# que tomes sobre dichos datos perdidos.
# Ok so we have missing data in city (which came with the dataset since we imported it) and 
# missing data in cuisine, that we added when merging cuisine data. NOTE that we could end without missing 
# data in this variable if we haven't added the merge(places, cuisine, by="placeID", all=TRUE) 'all=TRUE' 
# parameter to the merge operation, otherwise we would've ended with no missing data but fewer observations. 
# So, for city we will
# 1. Use revgeo and ggmap to get the city name from lattitude and longitude
# 2. Using the data we have to create a k-means algorithm and get the city for the missing places
# 3. Compare both 1 and 2 outcomes

#install.packages("revgeo")
#install.packages("ggmap")
library(revgeo)
library(ggmap)


undef_cities <- filter(places, city == 'Undefined')
undef_cities$city <- revgeo(undef_cities$longitude, undef_cities$latitude)
# undef_cities_backup <- undef_cities
counter <- 1
for (cit in undef_cities$city) {
  undef_cities$city[counter] <- unlist(strsplit(as.character(cit), ", "))[2]
  counter <- counter + 1
}

#TODO: Add this data to dataframe with city




# Checking for missing data and storing the info.
# Creating the empty table to store the info:
missing_data <- matrix(data = 0, nrow = ncol(places), ncol = 3)
colnames(missing_data) <- c("Variable", "Occurrences", "Percentage")

missing_data[1:7, 1] <- colnames(places)
i <- 1
for (column in places) {
  missing_data[i, 2] <- sum(is.na(column))
  missing_data[i, 3] <- round(mean(is.na(column))*100, digits = 2)  
  i = i + 1
}
row_sub = apply(missing_data, 1, function(row) all(row !=0 )) # Checkig for zeros in rows
missing_data <- missing_data[row_sub,]                         # Subset to remove variables with complete data.

print("Missing data summary:")
print(missing_data)






# EXTRA: Check the best-rated restaurants
# a. Even if they have one obs
# b. Have at least the 8.1223... mean value of ratings per places

