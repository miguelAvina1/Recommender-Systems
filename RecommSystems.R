# Inteligent Systems: German Credit Homewok.
#
# Authors:  Miguel Avina
#           Arturo Perez
# Date:     06/03/2020
# -----------------------------------------------------------------------------


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
sprintf("Figure1. First entries of geoPlaces")
head(rating)
sprintf("Figure2.First entries of rating")
head(cuisine)
sprintf("Figure3. First entries of cuisine")
# Some findings we observe by doing the last step are: 
# 1. Some entries of the 'city' attribute refers to same cities with different notation. 
#    E.g: s.l.p and San Luis Potosi. We might consider removing this attribute or
#    unifying same places with a specific name.
# 2. 'name' attribute listed the natural human-readble name of the restaurants. 
#    Further analysis using this kind of entry could be problematic. We might consider
#    removing this attribute and kept only with placeID.

# -- Homework 1. From the imported files, generate a single data frame with the
# Previously indicated variables

# Before doing this point, we want to know more regarding the data we imported.
# We observe that there are different number of observations in each one of the sets
sprintf("Number of Observations in cuisine: %s",dim(cuisine)[1])
sprintf("Number of Observations in geoPlaces: %s",dim(geoPlaces)[1])
sprintf("Number of Observations in rating: %s",dim(rating)[1])


# In rating we expect to have repeated both UserID and placeID, because each user 
# could have rated more than one place, and more than one place could have had a 
# score from many users. 
# Just for clarity, we will extract the number of unique userID and placeID, which 
# will tell us how many users and restaurants are in the dataset.
rating_UserIDs <- unique(rating$userID)
sprintf("Number of unique userID: %s", nlevels(rating_UserIDs))
# So here we are talking about 138 users, the UserID increments from U1001 to U1138
# This can also be noticed looking at the Data window on the right of RStudio


# Know, for clarity also, we will extract the number of unique placeID
# We noticed that placeID is not a factor but a numerical value, this does not make sense
# since we can not have fractional placeID and they must be taken as categorical. 
# Lets transform it to a factor
rating$placeID <- as.factor(rating$placeID)
sprintf("Number of unique placeID: %s",nlevels(rating$placeID))
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
sprintf("Intersection between placeID and cuisine: %s",nlevels(commonPlaceIDs))


n_occur <- data.frame(table(cuisine$placeID)) # Getting frequencies of each factor (placeID)
n_occur <- n_occur[order(-n_occur$Freq),]
head(n_occur[n_occur$Freq > 1,])  # here we can see the most repeated entries, one single placeID have 9 diff. cuisines!

# Lets look at some of these repeated entries
cuisine[cuisine$placeID %in% n_occur$Var1[n_occur$Freq > 1],][1:10,]
# We observe the pattern we have been describe
# Just for curiosity, we will observe the placeID with more assigned cuisines in this dataset
filter(cuisine, placeID == n_occur$Var1[1])  

# Conclusion: each restaurant have more than one cuisine related to it.








# If we merge geoPlaces and ratings, we will be just adding the name to each entry, since we have name for ALL placeIDs
places <- merge(rating, geoPlaces, by="placeID") # And we end up with 1161 entries, everything good here!

# This is a quick summary of what we know so far:
# 1. rating dataset and geoPlaces datasets have the same set of restaurants (PlaceIDs), as we would expect
# 2. data from cuisine have a larger set of restaurants than the one from rating and geoPlaces
# 3. Not all restaurants listed in rating and geoPlaces datasets is in cuisine. So we have restaurants from which we don't know their cuisine
# 4. In cuisine, some placeIDs have more than one cuisine linked to it.


# So, finally, to create the dataframe needed (recall that to have a dataframe, we need the same number of observation (rows) in each variable)
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
examplePair <- filter(rating, placeID == examplePlaceID)[1,1:3]
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


# How to preserve cuisine information without modifying the ratings and adding these extra votes can be done in 
# many different ways. We will focus on doing what we will need for this homework, however, it might not be the
# best solution if further analysis would be made with this cuisine information. The homework only asks what is 
# the kind of food of the 10 most popular restaurants. So we are just going to merge placeIDs with more than one 
# level into a single, new level. FOr example, taking the last table, placeID will be combined into:
# placeID   cuisine
# 500       Mexican_Familiar_Tacos.
# We also took this approach because there are other levels already with this syntax as: Bar_Pub_Brewery.

# we start by keeping just the placeIDs from cuisine we are interested (the 130 listed in geoPlaces):
cuisine <- filter(cuisine, placeID %in% commonPlaceIDs)  # We end up with 112 obs
# Now, we will combine the this info (112 obs) into the real 95 unique placeIDs that we have info.


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
# for this specific case we will replace bar_bar_pub_brewery to just bar_pub_brewery.

# now lets just merge this data with the rest
places_drop <- merge(places, cuisine, by="placeID") # If we do this, we will lost all observations for which we don't have cuisine info
uniquePlaceID <- droplevels(as.factor(unique(places_drop$placeID)))  # demonstration of the previous sentence
nlevels(uniquePlaceID)
# INFO. We used droplevels because you can have levels with 0 occurencies, so to better reflect the number of existing factos, drop those
# We must not do this
places <- merge(places, cuisine, by="placeID", all=TRUE) # Here we dont lose info

levels(places[,8]) <- c(levels(places[,8]),"?")   # we are adding ? to tell that we dont have that info :(
places[is.na(places)] <- '?'



head(places)
summary(places)


#Realize a descriptive analysis of the variables and indicate if there are missing data in 
#any of them. If so, realize an analysis to determine and justify the decision you take 
#regarding those missing data.



# placeID
# This variable was numeric and we already changed it to factor because it is actually a qualitative
sprintf("Number of places in the dataset: %s",nlevels(places$placeID))  # We have 130 levels, 130 places
# In previous homewook, we analyzed the proportion of observations per level, however, thi does not make sense 
# since we can not combine them, and the analyzis we will do later does not benefit from merging levels
head(prop.table(table(places$placeID))) # This is not useful

barplot(height = table(places$placeID)) # This plot is useful, it shows a nice represntation of the number of 
# occurences of each placeID, that is, the popularity of the restaurants.
# Graphicall, we can aslo observe that it seems to be a mean value of ~ 5 ratings per place, lets get this number right

# install.packages('plyr')
library('plyr')
t <- count(places, 'placeID')
sprintf("Average votes per placeID: %s", mean(t$freq))
# Ok, we know that this is a categorical value, however, the number of votes per restaurant is numerical, lets analyze it a bit
summary(t$freq)  # We are going to advance a bit, but from here we can see that there's a restaurant with 36 vots and the restaurant
# with less votes have just 3

boxplot(t$freq)  # From here we know that there are some atipical values, this atipical values could be seen as 
# outstanding (or popular) restaurants. We are not going to do anything with this info because removing it does not make sense.
# We think we can not get anything else from this variable.



# userID
# This was already a categorical (factor) variable, so we didn't do any modification
# We'll do the same analysis for this variable as we did for placeID becuase they are fair similar variables
barplot(height = table(places$userID)) 
# Graphically, we are observing the number of places each user has rated. It doesn't show too uncommon users, 
# altough there are some large bars, they doesn't seem as different as those from places
t <- count(places, 'userID')
sprintf("Average places rated per User: %s", mean(t$freq))
summary(t$freq)  # The info of places and userID is quite similar.
boxplot(t$freq) # And here we observer what we mentioned before, there are not uncommon users: Nobody rated too much
# or too little to become an outlier.

# Rating
# Rating was imported as numerical. If we check the dataframe using RStudio, we can manually observe that
# there is not more than 3 values: 0, 1, 2 . So turning it into categorical (factor) would be better
# However, lets assume we have a significantly larger dataset and visually inspecting the data woudln't be feasible
# Then, we would start by plotting the rating as histogram.
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
sprintf("Number of cities): %s",nlevels(places$city))  # 17 cities
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
# Graphically, we are observing the number of places each user has rated. It doesn't show too uncommon users, 
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
# And Soledad, (soledad de Graciano Sanchez) is also adjacent to San Luis Potosi, that's why those two merges in one single main cumulus
# I think this kind of analysis is more suitable for this information, rather than mean, quartile, etc.
# Furthermore, if we 'zoom in' we can actually observe the two comulus of juitepec and cuernavaca


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




#Perform an analysis to determine and justify the decision you make about such missing data. 
#For the case of missing data in the ‘city’ variable, you could research about at least the packages “revgeo” or “ggmap” from R to retreive the city name using latitude and longitude information.

# Ok so we have missing data in city (which came with the dataset since we imported it) and 
# missing data in cuisine, that we added when merging cuisine data. NOTE that we could end without missing 
# data in this variable if we haven't added the merge(places, cuisine, by="placeID", all=TRUE) 'all=TRUE' 
# parameter to the merge operation, otherwise we would've ended with no missing data but fewer observations. 
# So, for city we will
# 1. Use revgeo and ggmap to get the city name from latitude and longitude

#install.packages("revgeo")
#install.packages("ggmap")
library(revgeo)
library(ggmap)

# FIRST WITH REVGEO
##
undef_cities <- filter(places, city == 'Undefined')  # Here we store a copy of those places without City, we will use it later
undef_cities <- select(.data = undef_cities, placeID, city, latitude, longitude)



# First using revgeo 
# R doesn't allow to add new entries in a factor if they don't belong to the 
# currently levels. So first, we wil un-factorized city, then add the new cities we get, 
# and then factorize again
places$city <- as.character(places$city)
for (i in 1:length(places$city)) {
  if (places$city[i] == 'Undefined') {
    possibleCity <- revgeo(places$longitude[i], places$latitude[i])
    places$city[i] =  unlist(strsplit(as.character(possibleCity), ", "))[2]
  }
}

# re-factorize
places$city <- as.factor(places$city)
sprintf("Number of places: %s",nlevels(places$city))  # 
levels(places$city)  # We see that as before, we have same cities with different
# labels, we we will correct them as before

places$city <- factor(places$city, labels=c("Cd. Victoria","Cd. Victoria","Cuernavaca","Jiutepec",
                                            "San Luis Potosi", "San Luis Potosi", "Soledad", "Soledad"))


places$city <- droplevels(places$city)
nlevels(places$city)  # 
levels(places$city)  # Finally



# Now we will use google to comprobate if we are ok, we must get to the same city, right?

## To use google ggmap, it is more difficult, it requires an API key so we must register so
# First, we had to go to https://cloud.google.com/maps-platform/ to register for a free trial
# We start with 300 USD so we must use it wisely
# Then we must enable the google APIs for maps which will generate an API key which we used to get
# the required information. 
# Once we have the private key, we tell ggmap about this key using:
register_google(key = "AIzaSyD4mKmiXYdtE9xALBocEVu_Tl15a4iEW4I")
# This key must be kept private and not shared because google has billing credit card so anyone could
# do bad things to your account. We recommend to secure your private key to be used only from a certain IP

places$GG_result <- 'Not compared'
for (i in 1:length(places$city)) {
  #
  # So, ggmaps returns a list with all posible addresses it found. However the structure is NOT the same for each
  # address it returns. So, we can not search of a given element inside the lists of lists it returns. We would need to use another 
  # approach. One non-efficient but useful approach would be to loop inside all the info we get when calling revgeocode to search
  # for any coincidence to the data we retrieved from RevGeo, if we find it, then we can be sure we get the correct city
  # using revgeo. Otherwise, we would need to do a manual inspection of the data received from revgeocode
  # The search would be basically a deep-first search.
  if (places$placeID[i] %in% undef_cities$placeID) {   # We are just going to comprobate with those cities that were undefined before
    google_result <- revgeocode(c(places$longitude[i], places$latitude[i]), output = "all")$results
    found <- FALSE
    citynameOriginal <- places$city[i]
    compare_city <- as.character(lapply(as.character(citynameOriginal), tolower))
    compare_city <- gsub("[^a-z]","",compare_city)
    print(compare_city)
    #sprintf("Comparing with '%s'", compare_city)
    for (j in 1:length(google_result)) {
      address_comp <- google_result[[j]]$address_components
      for (k in 1:length(address_comp)) {
        long_name <- as.character(lapply(as.character(address_comp[[k]]$long_name), tolower))
        long_name <- gsub("[^a-z]","",long_name) 
        short_name <- as.character(lapply(as.character(address_comp[[k]]$short_name), tolower))
        short_name <- gsub("[^a-z]","",short_name)
        # We are not going to compare if the compare_city is at least substring of the google's result
        # We are not comparing it letter to letter because we found that google returns 'Soledad de Graciano Sanchez'
        # instead of just 'Soledad', which is the name of the city we got from revgeo. Therefore, comparing it using the
        # '==' operator will return FALSE, and it should be true, So we are going to use grep to check if one is substring
        # of the other
        if (grepl(compare_city, long_name, fixed = TRUE) || grepl(compare_city, short_name, fixed = TRUE)) {
          # print("FOUND:")
          # print(long_name)
          found = TRUE
          break
        }
      }
      if (found == TRUE) {
        break
      }
    }
    if (found == TRUE) {
      places$GG_result[i] = as.character(citynameOriginal)  # Just put the same city 
    } else {
      places$GG_result[i] <- 'Not the same'  # FUrther analysis will be needed
    }
  }
}

# So, let's check if the results fom Google and revgeo differs, if so, we are going to do something to choose 
# among the different cities. For this, if we do not find the city assigned by revgeo within the whole, I modified this payload that Google returns to us, we assign the dataframe the string 'Not the same'. 
# Now we just have to see if that string exists in the GG column with the google results.


differentResults <- filter(.data = places, GG_result == 'Not the same')
# Lets analyze these cases where we didn't get the same from google and revgeo
differentResults <- select(differentResults, -userID, -rating)  # Right now we are not using the userID nor rating
differentResults <- unique(differentResults)
differentResults  # So we didn't get any result diffeent from google from what we have already got from revgeo
# Now we have two columns, in city we have merged the info we had from the beginnign about city and now
# we have filled the missing data using the first package revgeo.
# The other column is called GG_result, and there we have either: 'Not compared" because the city info 
# wasn't missing since we imported the data set. "Not the same" found inconsistencies from what we get from
# regveo and google. Otherwise, it insert the same name as in city, simbolyzing that we got the same city data 
# both from regveo and google.

# We don't  do anything regarding the cuisine because there's no way of knowing but researching on internet.
# This is out of our scope becase 1. There's not needed for further analysis 2. Time contraints (Redactalo bonito LA)
# 3. They won't case any NA-related problem since we replace missing data with character '?'




# 3. What cities are the study restaurants from?
# To know this, we just need to refer to the variable cities
sprintf("There are: %s cities",nlevels(places$city))
levels(places$city) 


# 4. Si consideramos la popularidad de un lugar como aquellos que tienen la mayor cantidad
# de evaluaciones por parte de los usiarios (independientemente de si fue positiva o
# negativa la evaluación), obtener los nombres de los 10 restaurantes más
# evaluados/populares. ¿Cuántos restaurantes difernetes hay en total?

# Here, we just need to get the number of occurencies (observations) per placeID, since there is 
# one observation per vote.
t <- count(places, 'placeID')  # Getting frequency of each placeID
t <- merge(t, places, by='placeID')

t <- select(t, placeID, freq, name)                                        
t <- unique(x = t)


t <- t[order(-t$freq),]  # Ordering in descending order
popularPlaceID <- t$placeID[1:10]  # Here we have the 10 most popular places ID

mostPopular <- subset(places, placeID %in% popularPlaceID) # Here we extract the entire row where the 
# most popular placeID appears. Now we just want 1 row per name

mostPopular <- droplevels(unique(mostPopular$name))
mostPopular <- data.frame(popularPlaceID, mostPopular, t$freq[1:10])
colnames(mostPopular) <- c("placeID", "Restaurant", "Votes")
mostPopular

# 4.1 As an extra part, let's get those 'best rated' restaurants, this is the one with most 
# '2' as rating

best_rated <- filter(places, rating == "2")
best_rated <- select(.data = best_rated, placeID, name)
t <- count(best_rated, 'placeID')  # Getting frequency of each best-rated placeID
t <- t[order(-t$freq),] 

bestRatedPlaceID <- t$placeID[1:10]  # Here we have the 10 most popular and best rated places ID
bestRated <- subset(best_rated, placeID %in% bestRatedPlaceID) # Here we extract the entire row of top 10 best rated places
bestRated <- droplevels(unique(bestRated$name))
bestRated <- data.frame(popularPlaceID, bestRated, t$freq[1:10])
colnames(bestRated) <- c("placeID", "Restaurant", "Positive Votes")
bestRated
# Compare with
mostPopular




# 5 What type of food / cuisine are the 10 most popular restaurants found in the
# previous paragraph? How many different types of cuisine are there in total?
# Here we can obtain the type of combined kitchen that we created at the beginning:

mostPopular <- merge(mostPopular, cuisine, by='placeID')
mostPopular
# Getting only the cuisine
levels(droplevels(mostPopular$cuisine))
# Future work: Retrive this cuisine info directly from the first dataframe, before merging into single string





# 5. How many different types of cuisine are there in total?
# Here we can have three different results. 
# a.Types of cuisine within the dataset of the 130 places that we are analyzing already merged
sprintf("Number of types of cuisines: %s",nlevels(droplevels(places$cuisine)))
levels(places$cuisine) 


# b. Types of cuisine within the kitchen dataset that we import, without any processing
# since we have made a lot of modifications, we will import the dataset again

cuisine_original   <- read.csv(file = "RCdata/chefmozcuisine.csv", header = TRUE, sep = ',')
cuisine_original   <- select(.data = cuisine_original, placeID, Rcuisine)
nlevels(cuisine_original$Rcuisine)  # 59
# Recall that we have this many cuisine types because in this dataset we have placesID we are not analyzing


# c. Types of cuisine within the original kitchen dataset, but only of the 130 places we were analyzing
commonPlaceIDs <- as.factor(intersect(cuisine$placeID, rating$placeID))
cuisine_original_130 <- droplevels(subset(cuisine_original, placeID %in% commonPlaceIDs)) # Here we extract the entire row of top 10 best rated places
nlevels(cuisine_original_130$Rcuisine)  # 23 levels, they are less than the 'merged' cuisines because when merging, new levels are created
levels(cuisine_original_130$Rcuisine)




# 6. Generate the Utility matrix considering the rows with the userID variable, the
# columns with placeID, and the array values with rating. From said matrix of
# utility apply the SVD (Singular Value Decomposition) factorization to obtain the
# latent variable matrix for restaurants.

# First, let's get only the variables we will need in the simple-triple-matrix form
places_stm <- select(places, placeID, userID, rating, name)
head(places_stm)

# Altough we used rating as a categorical value previously, here we must convert to numerical again
places_stm$rating <- as.numeric(places_stm$rating)
utMx <- with(places_stm, tapply(rating, list(userID, name), sum, default=NA))
dim(utMx)  # Ok, we can observe that we created a matrix of 138x129, 138 correspond to the users we know we have
# but we should have 130 restaurant names, where is the missing one?
# If we recall, at the begining we identified two places with the same name. Until know, it hasn't been problematic
# but know, let's just change one of those places' name so we can identify it as separate restaurants
# Let's retrieve that name again
n_occur <- data.frame(table((geoPlaces$name)))
n_occur[n_occur$Freq > 1,]
geoPlaces[geoPlaces$name %in% n_occur$Var1[n_occur$Freq > 1],]
# Let's get the placeID of the first one
ID <- geoPlaces[geoPlaces$name %in% n_occur$Var1[n_occur$Freq > 1],][1,1]
ID

# Now, we will replace "Gorditas Dona Tota" to "Gorditas Dona Tota 2"
# Remember, since we will add a new factor, firts, de-factorize
backup <- places$name
places$name <- as.character(places$name)
for (i in 1:length(places$placeID)) {
  if (places$placeID[i] == ID) {
     places$name[i] = "Gorditas Dona Tota 2"
  }
}
places$name <- as.factor(places$name)  # Now we have the 130 factors in city
# Repeating the process again


places_stm <- select(places, placeID, userID, rating, name)
places_stm$rating <- as.numeric(places_stm$rating)
utMx <- with(places_stm, tapply(rating, list(userID, name), sum, default=NA))
dim(utMx)  # Ok, we have know the 138x130 matrix


utMx[1:10, 3:6]  # Here we can observe that it is a disperse matrix
place_names <- colnames(utMx)
head(place_names)

# Guardaremos la matrix de utilidad 
write.csv(utMx, file = "RCdata/Utility_Matrix.csv", na="")


# 6. From said matrix ofutility apply the SVD (Singular Value Decomposition) factorization 
# to obtain the latent variable matrix for restaurants.

#install.packages("irlba")
library(irlba)

# If we want to generate the latent variable matrix for restaurants, we should finish
# with a matrix u with dimensions 130x130


# Change NA inputs to 0, mathematically manageable
utMx[is.na(utMx)] <- 0
MSVD <- svd(t(utMx))  # in MSVD we have u, d and v that forms from the decomposition
dim(MSVD$u)  # We can observe we end with the right size of u, because we transpose the utility matrix in the line before
placesLatentMatix <- MSVD$u
# Result of subsection 6:
placesLatentMatix
placesLatentMatix[1:10,1:10]

# 7. If a user liked the restaurant "Gorditas Doña Gloria" with placeID: 132834,
#What other 12 restaurants (indicate names) could you recommend using the
# SVD decomposition with the "pearson" correlation method and considering the 10
# largest singular values of the SVD? Note: it doesn't matter for the moment what
# city be the restaurant.


utrunc <- MSVD$u[,1:10]  
# Truncate it for the first 10 latent columns, corresponding to the 10 latent values
# larger, Since the svd () function we used before Returns the singular values in d ordered 
# from highest to lowest
corr_mx <- cor( t(utrunc), use='pairwise.complete.obs', method = "pearson")# Here we obtain the correlation matrix using the correlation
#from pearson.  
df_cor <- as.data.frame(corr_mx)

library(ggplot2)
library(reshape2)

tmp <- melt(data.matrix(cor(df_cor)))
ggplot(data=tmp, aes(x=Var1,y=Var2,fill=value)) + geom_tile()  # We tried to have a look to the correlation matix graphically, 
# However, since there are a lot of variables is not recommendedd



place_names <- colnames(utMx)
# Now, from a restaurant name, lets get the index position in the corelation matrix:

# We select the name
name2look <- 'Gorditas Doa Gloria'
# We obtain the index of the correlation matrix
nameIndex <- which(place_names == name2look)


# Now we select all the column of these restaurant
restOptions <- corr_mx[,nameIndex]
# We obtain the indexes of the restaurants ordered from highest correlation to lowest correlation
orderedRest <- order(-restOptions)
# And then we use these indices to extract the names associated with those indices with greater correlation,
# We ignore the first index since it will be the one with correlation 1, that is, the same restaurant and we will not
# recommend the same restaurant
numberOfRecomm <- 12  # we will provide 12 Recommendations
RecommendedRest <- place_names[orderedRest[2:numberOfRecomm+1]]

for (i in 1:11){
  print(RecommendedRest[i])
}   # Here are the 12 recommended restaurants





