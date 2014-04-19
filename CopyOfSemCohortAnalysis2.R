
library(lubridate)
library(plyr)
library(ggplot2)
library(ggthemes)
library(sqldf)

# Read Data File - Activity by Day and PubID
Cohort <- read.csv("~/Dropbox/R/CohortAnalysis/NovTrafficByPubid.csv")

# Function to convert number with comas as numberic
conv_to_num <- function(x) {as.numeric(gsub(",", "", x))}
  
# Function to compute relative weight of a vector 
fn <- function(x) x/sum(x)
fnrow <- function(x) length(x)


# Load PubId Lookuphead for Pub & Pub Groupd IDs
PubId <- read.csv("~/Dropbox/R/CohortAnalysis/PubId.csv")


#Convert column into right Format
Cohort$Visits <- conv_to_num(Cohort$Visits)
Cohort$Unique_Visitors <- conv_to_num(Cohort$Unique_Visitors)
Cohort$New_Visits <- conv_to_num(Cohort$New_Visits)
Cohort$Bounce_Rate <- as.numeric(sub("%","",Cohort$Bounce_Rate))/100
Cohort$Seller_Clicks <- conv_to_num(Cohort$Seller_Clicks)
Cohort$Avg_Duration <- period_to_seconds(hms(Cohort$Avg_Duration))

#Look at lookup for Publisher Name
Cohort <- merge(PubId, Cohort, by = 'PubId')

# Remove data points with less than 100 visits
Cohort <- subset(Cohort, Visits > 100)

# Add Column Clicks_Visits
Cohort$Clicks_Visits <- Cohort$Seller_Clicks / Cohort$Visits

# Add Column Repeat_Visits
Cohort$Repeat_Visits <- Cohort$Visits - Cohort$New_Visits
Cohort$Pct_Repeat_Visits <- Cohort$Repeat_Visits / Cohort$Visits
Cohort$Pct_Search <- Cohort$Searches / Cohort$Visits
# Add VisitRatio
Cohort <- ddply(Cohort, .(PubId), transform, VisitRatio=fn(Visits))
Cohort <- ddply(Cohort, .(PubId), transform, DataCount=fnrow(Visits) )

# Aggregate by pub group id
counts <- ddply(Cohort, .(PubGroupName, PubGroupId), nrow)

print(counts)

# Plot interesting variables
print(plot(Cohort[,c(9,10,11,14)]))

# Plot Violin Graph for Bounce Rate
print(
  ggplot(subset(Cohort, PubGroupId %in% 
                  c(719, 559,557, 363,  6, 337, 253, 61, 62, 990, 739  ) ),
#                  c(  6, 739, 719, 253, 337, 559 ) ),
                  aes( Bounce_Rate * 100   , x=PubGroupName) ) 
      + geom_boxplot() 
      + geom_point(aes(size = 8 * VisitRatio * DataCount ))  
      + theme_wsj() 
      +  ylim(0,100)
     + theme(legend.position = "none")
   )

#print(ggplot(Cohort, aes(x=Bounce_Rate, y=Clicks_Visits) )
#       + geom_point()
#       + geom_smooth(method="lm") ) 

#print(ggplot(Cohort, aes(x=Bounce_Rate, y=Clicks_Visits) )
#      + geom_point()
#      + geom_smooth(method="lm")
#      + xlim(0,1)
#      + ylim(0, 1)
#      + theme_wsj()
#      ) 
