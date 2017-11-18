require(readr)
require(plotly)

#load data
responses <- read_csv("../Data/imputed_responses.csv")

#removing any fake/incorrect entries considering no one has BMI over 100
BMI <- responses$Weight/(responses$Height * responses$Height / 10000)
check <- data.frame(BMI, responses$Age)
fake.indices = which(check$BMI > 100)
responses <- responses[-fake.indices,]

#Create subset for people from city or village
city_people <- subset(responses, responses$`Village - town`=='city')
village_people <- subset(responses, responses$`Village - town`=='village')

#calculataing BMI for people living in cities vs those living in villages
BMI_city <- data.frame(BMI = city_people$Weight/(city_people$Height * city_people$Height / 10000)  ,  Age = city_people$Age)
BMI_village <- data.frame(BMI = village_people$Weight/(village_people$Height * village_people$Height / 10000)  ,  Age = village_people$Age)

#see the average BMI for each age group people, in cities and villages

age <- c(15:30)
Avg_BMI_city <- data.frame(Age = age)
Avg_BMI_village <- data.frame(Age = age)
BMI.city <- c()
BMI.village = c()

for(i in 1:length(age)){
  BMI.city[i] <- mean(subset(BMI_city, BMI_city$Age==age[i])$BMI)
  BMI.village[i] <- mean(subset(BMI_village, BMI_village$Age==age[i])$BMI)

}

Avg_BMI_city <- cbind(Avg_BMI_city, BMI = BMI.city)
Avg_BMI_village <- cbind(Avg_BMI_village, BMI = BMI.village)

#plot graph for Age vs BMI
p <- plot_ly(x = ~Avg_BMI_city$Age, y = ~Avg_BMI_city$BMI, type = 'scatter', mode = 'line', name = "City") %>%
  add_trace(y = ~Avg_BMI_village$BMI, type = 'scatter', mode = 'line', name = "Village") %>%
  layout(yaxis = list(title = "BMI"), xaxis = list(title = "Age"), title = "BMI vs Age")
print(p)

#Here we can see how the trend for BMI increases with an increase in age.
#Although we dont see any clear difference between people from the city vs those from the village.


#t-test to check if there is any significant difference in the BMI for city vs village

t.test(BMI_city$BMI, BMI_village$BMI, var.equal = F)

#Since the p value is 0.202, we cannot reject the null hypothesis and hence there is no significant difference between the two


#check if there is any difference between BMI of males vs females

female <- responses[responses$Gender=="female",c(142,143)]
male <- responses[responses$Gender=="male", c(142,143)]
BMI.female <- female$Weight/ (female$Height/100)**2
BMI.male <- male$Weight/ (male$Height/100)**2

t.test(BMI.male, BMI.female, var.equal = F)

#Since the p value is less than 0.05, we can reject the null hypothesis and conclude that BMI of males is significantly greater than the BMI of Females.

#plot for BMI for male vs BMI for female

male <- data.frame(BMI = BMI.male)
female <- data.frame(BMI = BMI.female)

#Now, combine your two dataframes into one.  First make a new column in each.
male$gender <- 'male'
female$gender <- 'female'

#and combine into your new data frame vegLengths
plotBMI <- rbind(male,female)

#now make your lovely plot
p <- ggplot(plotBMI, aes(BMI, fill = gender)) + geom_density(alpha = 0.2)

p <- ggplotly(p)
p

#As proven by the t-test, BMI of males is significantly greater than the BMI of females on average.

# Test to check if there is difference in BMI across different age groups in city/village

BMI_city_15to20 <- subset(BMI_city, BMI_city$Age>=15 & BMI_city$Age <=20)
BMI_city_21to25 <- subset(BMI_city, BMI_city$Age>=21 & BMI_city$Age <=25)
BMI_city_26to30 <- subset(BMI_city, BMI_city$Age>=26 & BMI_city$Age <=30)

BMI_village_15to20 <- subset(BMI_village, BMI_village$Age>=15 & BMI_village$Age <=20)
BMI_village_21to25 <- subset(BMI_village, BMI_village$Age>=21 & BMI_village$Age <=25)
BMI_village_26to30 <- subset(BMI_village, BMI_village$Age>=26 & BMI_village$Age <=30)

t.test(BMI_city_15to20, BMI_village_15to20, var.equal = F)
t.test(BMI_city_21to25, BMI_village_21to25, var.equal = F)
t.test(BMI_city_26to30, BMI_village_26to30, var.equal = F)


#Since the p-value is greater than 0.05 in all the cases, we cannot reject the null hypothesis and therefore we conclude that there is no significant difference even across age groups comparing city to village.
