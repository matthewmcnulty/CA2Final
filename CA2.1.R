# Step 1: Importing dataset and replacing missing entries with "NA".
NIPostcodes <- read.csv('NIPostcodes.csv', header = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA"))

head(NIPostcodes)
str(NIPostcodes)

# Step 2: Show the total number of rows, the structure of the data frame, 
# and first 10 rows of the data frame containing all of the NIPostcode data.
nrow(NIPostcodes)

head(NIPostcodes, 10)
str(NIPostcodes)

# Step 3: Add a suitable title for each attribute of the data.
colnames(NIPostcodes) <- c("Organisation Name", "Sub-building Name", "Building Name", 
                           "Number", "Primary Thorfare", "Alt Thorfare", 
                           "Secondary Thorfare", "Locality", "Townland", 
                           "Town", "County", "Postcode", 
                           "x-coordinates", "y-coordinates", "Primary Key (identifier)")

head(NIPostcodes)
str(NIPostcodes)

# Step 4: Show the total number and mean missing values 
# for each column in the postcode data frame.
sum(is.na(NIPostcodes))
mean(is.na(NIPostcodes))
colSums(is.na(NIPostcodes))
colMeans(is.na(NIPostcodes))
sum(complete.cases(NIPostcodes))

# Checking if any of the following columns appear with each other or not. 
# If they do not, we can merge them to condense the data without losing any data.
sum(complete.cases(NIPostcodes[c("Organisation Name", "Sub-building Name")]))
sum(complete.cases(NIPostcodes[c("Organisation Name", "Building Name")]))
sum(complete.cases(NIPostcodes[c("Sub-building Name", "Building Name")]))

# Checking if any of the following columns appear with each other or not. 
# If they do not, we can merge them to condense the data without losing any data.
sum(complete.cases(NIPostcodes[c("Alt Thorfare", "Secondary Thorfare")]))
sum(complete.cases(NIPostcodes[c("Alt Thorfare", "Locality")]))
sum(complete.cases(NIPostcodes[c("Secondary Thorfare", "Locality")]))

head(NIPostcodes)
str(NIPostcodes)

# Step 5:
install.packages("dplyr")
library(dplyr)

# Remove or replace missing entries with a suitable identifier. 
# Decide whether it is best to remove missing data or to recode it.

# Since the 'Alt Thorfare' and 'Secondary Thorfare' never appear together, 
# it is a good idea to merge the columns together since they both have near identical meaning.
NIPostcodes$`Alt/Secondary Thorfare` <- coalesce(NIPostcodes$`Alt Thorfare`, 
                                                 NIPostcodes$`Secondary Thorfare`)
# Deleting the columns we had originally merged together, 
# since we now have a column containg the data from both.
NIPostcodes <- NIPostcodes[c(1:5, 16, 8:15)]

# Deleting 8900 rows without a Postcode. 
# Postcode is an important component of the address, and is essential in the answering of section 2.
NIPostcodes <- NIPostcodes[complete.cases(NIPostcodes[ , "Postcode"]),]

head(NIPostcodes)
str(NIPostcodes)

# Step 6: Modify the County attribute to be a categorising factor.
class(NIPostcodes$County)
NIPostcodes$County <- factor(NIPostcodes$County)
class(NIPostcodes$County)

head(NIPostcodes)
str(NIPostcodes)

# Step 7: Move the primary key identifier to the start of the dataset.
NIPostcodes <- NIPostcodes[c(14, 1:13)]

head(NIPostcodes)
str(NIPostcodes)

# Step 8: Create a new dataset called Limavady_data. 
# Store within it only information that has locality, 
# townland and town containing the name Limavady. 
# Store this information in an external csv file called Limavady.
attach(NIPostcodes)
Limavady_data <- subset(NIPostcodes, NIPostcodes$Town == "LIMAVADY", 
                        select = c(Locality, Townland, Town))
detach(NIPostcodes)
write.csv(Limavady_data, file = "Limavady_data.csv")

head(Limavady_data)
str(Limavady_data)

# Step 9: Save the modified NIPostcode dataset 
# in a csv file called CleanNIPostcodeData.
write.csv(NIPostcodes, file = "CleanNIPostcodeData.csv")

head(NIPostcodes)
str(NIPostcodes)