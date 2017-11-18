# Load all the libraries
require(readr)
require(mice)

# Load data
responses <- read_csv("../Data/responses.csv")

# Converting character columns to factors
y <- which(sapply(responses,class) == 'character')
responses[,names(y)] <- lapply(responses[,names(y)], factor)
summary(responses$Smoking)
# Imputing all NA with mice package
response_impute <- mice(responses, maxit = 1, m= 1)

# This is Imputed Data
imputed_response <- complete(response_impute, 1)
which(is.na(imputed_response))
write_csv(imputed_response, "../Data/imputed_responses.csv")
