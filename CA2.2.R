# Step 1: Unzipping datasets. Amalgamating all of the crime data from each csv file into one dataset.
zipfile <- "NI Crime Data.zip"
unzip(zipfile)

setwd("NI Crime Data/")
getwd()

csv_files <- list.files(full.names = TRUE, recursive = TRUE)
csv_files
AllNICrimeData <- Reduce(rbind, lapply(csv_files, read.csv)) # 1)

setwd("../")
getwd()

write.csv(AllNICrimeData, "AllNICrimeData.csv")
nrow(AllNICrimeData)

head(AllNICrimeData)
str(AllNICrimeData)

# Step 2: Modifying the structure of the newly created AllNICrimeData csv file and removing certain attributes.
AllNICrimeData <- read.csv('AllNICrimeData.csv', header = TRUE, 
                           stringsAsFactors = FALSE, na.strings = c("", "NA"))

AllNICrimeData <- AllNICrimeData[c(1, 3, 6, 7, 8, 11)]
write.csv(AllNICrimeData, "AllNICrimeData.csv")

head(AllNICrimeData)
str(AllNICrimeData)

# Step 3: Factorising the Crime type attribute.
class(AllNICrimeData$Crime.type)
AllNICrimeData$Crime.type <- factor(AllNICrimeData$Crime.type)
class(AllNICrimeData$Crime.type)

head(AllNICrimeData)
str(AllNICrimeData)

# Step 4: Modifying the AllNICrimeData dataset so that the Location attribute contains only a street name.
AllNICrimeData$Location <- gsub("On or near ", "", AllNICrimeData$Location)

head(AllNICrimeData)
str(AllNICrimeData)

# Step 5: 
# Filling empty values with NA and deleting Location values that contain NA.
colSums(is.na(AllNICrimeData))
AllNICrimeData[AllNICrimeData == ""] <- NA
colSums(is.na(AllNICrimeData))
AllNICrimeData <- AllNICrimeData[complete.cases(AllNICrimeData[ , "Location"]),]
colSums(is.na(AllNICrimeData))

# Changing Location values to upper case to match the Primary Thorfare in PostcodeData.
AllNICrimeData$Location <- toupper(AllNICrimeData$Location)

# Choosing 1000 random samples of crime data from the AllNICrimeData dataset.
random_crime_sample <- AllNICrimeData[sample(1:nrow(AllNICrimeData), 1000, replace = FALSE),]

# Importing PostcodeData.
PostcodeData <- read.csv('CleanNIPostcodeData.csv', header = TRUE, stringsAsFactors = FALSE)

# Deleting Primary Thorfare values that contain NA.
colSums(is.na(PostcodeData))
PostcodeData <- PostcodeData[complete.cases(PostcodeData[ , "Primary.Thorfare"]),]
colSums(is.na(PostcodeData))

# Only including the necessary attributes.
PostcodeData <- PostcodeData[c("Primary.Thorfare", "Postcode")]

#install.packages("dplyr")
library(dplyr)

# Creating a function called find_a_postcode that takes as an input each location attribute 
# from random_crime_sample and finds a suitable postcode value from the postcode dataset.

find_a_postcode <- function(location) {
  
  matched_location <- filter(PostcodeData, Primary.Thorfare == location)
  
  Postcode <- names(which.max(table(matched_location$Postcode)))
  
  return(Postcode)
}

# Finding the postcode for each location using lapply on the find_a_postcode function.
Postcode <- as.character(lapply(random_crime_sample$Location, find_a_postcode))

# Appending the data output from the find_a_postcode function to the random_crime_sample dataset.
random_crime_sample$Postcode <- Postcode

# Making sure there are no missing postcodes in the output from the function.
colSums(is.na(random_crime_sample))

# Counting the number of records in the modified random_crime_sample data frame.
nrow(random_crime_sample)

# Saving the modified random crime sample data frame as random_crime_sample.csv.
write.csv(random_crime_sample, file = "random_crime_sample.csv")

head(random_crime_sample)
str(random_crime_sample)

# Step 6: 
# Updating the random sample so that it contains only certain items.
updated_random_sample <- random_crime_sample[c(2:7)]

# Sorting chart_data by postcode where the postcode contains “BT1” and then by crime type.
chart_data <- tbl_df(updated_random_sample)
chart_data <- chart_data[order(chart_data$Postcode, chart_data$Crime.type),]
chart_data <- filter(chart_data, grepl("BT1", Postcode))

# Showing the summary statistics for the crime type from this chart_data data frame.
table(chart_data$Crime.type)
names(which.max(table(chart_data$Crime.type)))

# Step 7: Creating a bar plot of the crime type from the chart_data data frame.
crime_type <- c(table(chart_data$Crime.type))

a = barplot(crime_type,
        main = "The number of Occurences for each Crime Type",
        xlab = "Crime Type",
        ylab = "Occurences",
        col = "purple",
        border = "black",
        las = 1,
        names.arg="")

text(a[,1], -3.7, srt = 60, adj = 1, xpd = TRUE, labels = names(crime_type), cex= 1.2) # 2)

# References: 1) https://gist.github.com/MarkEdmondson1234/c119bfe81af5d5ab81c8
#             2) https://www.r-graph-gallery.com/213-rotating-x-axis-labels-on-barplot/
