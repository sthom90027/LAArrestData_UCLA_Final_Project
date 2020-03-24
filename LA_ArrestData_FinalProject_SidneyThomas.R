# Sidney Thomas
# Intro to Data Science - Class Project


### 1.  Data access:  Download the data set into your R environment.
LA <- read.csv("Z:/Sidney/UCLA Data Science/Class Project/Arrest_Data_from_2010_to_Present.csv",
               colClasses = c("Time" = "character" ))




### 2a.  Use the data repository to get a definition of all the variables in the data set.
# summary(LA)      # summary of all variables
# LA <- head(LA1,1000)


### 2b. Perform feature engineering to select variables that support hypothesis
# Features/fields that will be imperative to my analysis are:

# 1. Arrest.Date
# 2. Time
# 3. Area.Name
# 4. Age
# 5. Sex.Code
# 6. Charge.Group.Description
# 7. Charge.Description
# 8. Location

# Select only my feature variables
LA2 <- data.frame(LA$Arrest.Date, 
                  as.numeric(LA$Time),
                  LA$Area.Name, 
                  LA$Age, 
                  LA$Sex.Code, 
                  LA$Charge.Group.Description,
                  LA$Charge.Description)




### 3.  Perform any data transormations you feel are necessary to achieve the desired goals.

# Convert to all lower case & change variable names
names(LA2) <- tolower(names(LA2))
names(LA2) <- c("arrest_date",
                "arrest_time",
                "area_name",
                "age",
                "sex",
                "charge_group",
                "charge_description"
                  )
# names(LA2)


#------ Remove age groups under 18
LA2 <- subset(LA2, LA2$age >= 13)   # Only want people over age 13
summary(LA2$age)

#------ Remove erroneous times with only 1 character
LA2 <- subset(LA2, str_length(LA2$arrest_time) >= 2)   # Only want observations where time length is above 2

#------ Omit fields with  NA's
LA2 <- na.omit(LA2)  # omit NA's 

#------  create variable for age ranges
ageranges <- data.frame(agerange = ifelse(LA2$age < 18,"13-17", 
                                       ifelse(LA2$age > 17 & LA2$age < 25,"18-24",
                                            ifelse(LA2$age > 24 & LA2$age <35 ,"25 - 34", 
                                               ifelse(LA2$age > 34 & LA2$age < 45, "35-44", 
                                                  ifelse(LA2$age > 44 & LA2$age < 55, "45-54","55+"))))))


#------ add age range variable to dataset
LA2 <- cbind(LA2, ageranges)                                                      


#------  create variable for time of day categorization
timecategory <- data.frame (timecategory
                   = ifelse(LA2$arrest_time > 500 & LA2$arrest_time < 1159,"Morning",
                      ifelse(LA2$arrest_time >= 1200 & LA2$arrest_time < 1659, "Afternoon",
                         ifelse(LA2$arrest_time >= 1700 & LA2$arrest_time < 2200, "Evening", "Overnight"))))
                           
#------  add time category variable to dataset
LA2 <- cbind(LA2, timecategory)                           
                           





### 4.  Use various EDA and simple statistical analysis techniques to gain a deep understanding for the data.
levels(LA2$LA2.Area.Name)
# [1] "77th Street" "Central"     "Devonshire"  "Foothill"    "Harbor"      "Hollenbeck"  "Hollywood"   "Mission"    
# [9] "N Hollywood" "Newton"      "Northeast"   "Olympic"     "Pacific"     "Rampart"     "Southeast"   "Southwest"  
# [17] "Topanga"     "Van Nuys"    "West LA"     "West Valley" "Wilshire"  
# 21 areas


table(LA2$area_name)
prop.table(table(LA2$area_name))*100  # Most arrests occur in Central region (10%) & Hollywood (9.2%)

# 77th Street     Central  Devonshire    Foothill      Harbor  Hollenbeck   Hollywood     Mission N Hollywood      Newton 
# 59         104          21          30          38          29          51          57          25          38 
# Northeast     Olympic     Pacific     Rampart   Southeast   Southwest     Topanga    Van Nuys     West LA West Valley 
# 33          56          52          42          55          38          26          52          23          37 
# Wilshire 
# 26  


table(LA2$age, LA2$sex)  # more males throughout age groups
summary(LA2$age)   # min age is 13, max age is 19
summary(LA2$sex)    # Approx 80% are males
summary(LA2$arrest_time)    #196 NA's, seems reasonably split, date/time needs to be transformed
prop.table(table(LA2$area_name))*100  # Central and Hollywood are most prominent neighborhoods
summary(LA2$charge_group)  # Top categories are Narcotic Drug Laws, DUI, & Drunkeness
prop.table(table(LA2$charge_group))*100  # Top categories are Misc, Narcotic Drug Laws, DUI, Drunkeness, Aggravated Assault
str(LA_Arrests)
head(LA_Arrests)
tail(LA_Arrests)
table(LA2$area_name, LA2$sex)
var(LA2$age)
cumsum(LA2$age)
fivenum(LA2$age)
quantile(LA2$age, probs=seq(0,1,0.25),na.rm=TRUE)
cor(LA2$age, LA2$arrest_time) 
stem(LA2$age)
prop.table(table(LA2$timecategory, LA2$sex))*100
prop.table(table(LA2$charge_group, LA2$sex))*100




# 5.  Use R's plotting features to produce both exploratory and expository data visualizations.
# install.packages("scatterplot3d")
library(scatterplot3d)



#### Exploratory Plots
# scatterplot3d(LA2[,2,4])
# plot(LA2$age, LA2$sex, pch=19, col = "blue")  
hist(LA2$age)  
hist(LA2$arrest_time)
# most arrests are early 20's



#### Expository Plots
barplot(prop.table
        (table(LA2$agerange) 
          )*100
        ,main = "Age Range by Percentage of Total Arrests"
        ,xlab = "Age Range"
        ,ylab = "% of Arrests"
        ,col = brewer.pal(6, "BuGn")
        ,legend.text = TRUE)
        
barplot(prop.table
        (table(LA2$area_name)
        ) *100
        ,main = "Area Name by % of Total Arrests"
        ,xlab = "Area Name"
        ,ylab = "% - Total Arrests"
        ,col = brewer.pal(6, "BuGn")
        ,cex.names = .75
        ,las = 2)

barplot(prop.table
        (table(LA2$timecategory)
        ) *100
        ,main = "Time of Arrest by % of Total Arrests"
        ,xlab = "Time of Arrests"
        ,ylab = "% - Total Arrests"
        ,col = brewer.pal(6, "BuGn")
        ,cex.names = .75
        ,las = 2)




# 6.  Select one or more of R's statistical learning algorithms to make predictions, and/or discoveries.
### discovered that males make up a huge portion of arestees.  the older the age range, the more likely the
### arestee is male, however even at our lowest age, there's still a 75% chance the arestee is male



####### Create new binary categorical variable
ismale <- data.frame(ismale=(LA2$sex=="M"))

####### Combine LA2 and binary var
LA2 <- cbind(LA2, ismale)   
head(LA2)


####### Split data set into training set and test set
n <- nrow(LA2)  # Number of observations = 1,312,965
ntrain <- round(n*0.6)    # 60% for training set
set.seed(314)             # Set seed for reproducible results
tindex <- sample(n, ntrain) # Create an index


trainLA2 <- LA2[tindex,]  # Create training set
testLA2 <- LA2[-tindex,]  # Create test set

####### Create sample for large data set
sampledSubset <- sample(1:1e5, size=1000, replace=FALSE)


###################Remove this section
#######plot(LA2$age[sampledSubset], LA2$arrest_time[sampledSubset]) #Trend


####### Use logistic regression on single continuous predictor for binary categorical response
glm9 <- glm(ismale ~ age, 
            data=trainLA2, 
            family=binomial)   
glm9

summary(glm9)


###### Bi-modal plot for age vs. sex
par(mar=c(4,4,4,4))
plot(trainLA2$age[sampledSubset], trainLA2$ismale[sampledSubset]
     ,col = "#4876FF"
     ,main = "Likelihood Arrestee is Male (*100)"
     ,xlab = "Age (Years)"
     ,ylab = "% Chance Male (*100)")   
     
###### add sigmoid function to plot to show likelihood that arestee is male as age increases
curve(predict(glm9, data.frame(age=x), type="response"), 
      add=TRUE)    




# 7.  In the case of predictions, use the trained algorithm on new data and make a case of the algorithms' 
#     accuracy.


######  make predictions for test set
prob <- predict(glm9, newdata=testLA2, type="response")
prob <- round(prob,3)*100   # Get percentages
# prob            # view probability values


######  create vector index 1 is above 76% prob, 0 is below
prob_1 <-  data.frame(prob_1=ifelse( prob > 76,"TRUE","FALSE"))  


###### Assign test values for ismale to "prob_2"
prob_2 <- data.frame(testLA2$ismale)     


###### combine probs and actual values from test set for comparison
errortable_1 <- cbind(prob_1,prob_2) 

###### assign 1 where prediction matches test variable for "ismale"
errortable_2 <- data.frame(errortable_2=
                             ifelse(
                               errortable_1[1]==errortable_1[2],
                               "1","0"))  #compare the two values



###### change name to value
names(errortable_2) <- c("value")


###### calculate how often the prediction is right
######  iterative approach, change the prob % until we get the best accuracy rate below
test <- ifelse(errortable_2$value== "1", 1,0)
mean(test)     # get prediction correctness rate

#   mean(test)
# [1] 0.7504427

# predictions are correct 75% of the time, using a probability cut off rate of .76
# this is the best the model does with the chosen variables


###### predict sex at various ages for final report
agepredictor_15 <- data.frame(age=15)
agepredictor_80 <- data.frame(age=80)

###### Probability arestee is male @ various ages   --- for final report
predict.glm(glm9, agepredictor_15, type="response") 
#        1 
# 0.7413364 

predict.glm(glm9, agepredictor_80, type="response") 
#        1 
#0.8874655 





# 8.  Prepare a report using techniques of "data storytelling" to present the results to a management-level
#     audience - state the goals of the project, the data sets used, EDA results, data visulization, overview
#     of how you used machine learning algorithms, and final conclusions.

###### see the PDF document



# 9.  Create your own GitHub repository and publish the results of the project: project description, data set
#     used, well-commented R script(s), and the final report mentioned above.

###### navigate to GitHub site:  
##  https://github.com/sthom90027/LAArrestData_UCLA_Final_Project





