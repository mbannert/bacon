library(fivethirtyeight)

# let's explore all of 538's datasets
d <- data(package = "fivethirtyeight")
dnames <- d$results[,"Item"]

# apply family function
l <- lapply(dnames,function(x) get(x))
names(l) <- dnames
print(object.size(l),units = "Mb")

s <- sapply(dnames, function(x) nrow(get(x)))
s[which.max(s)]

# read .RData

# read .csv (for example KOF Barometer)
# checkout these multi-line comments
# How many observations are in the dataset?
# Who is the tallest / smallest player?
# Are players older than 32 significantly heavier than younger players?
# What's the average size by position
# What’s the size of these players in centimeters?

# already fed up with baseball?
# how bout some text mining?

# playin with graphics: https://rpubs.com/mbannert/235246
