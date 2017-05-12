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

# 



# read .RData
load("../boar/data/mlb.RData")
write.csv2(mlb,"data/mlb.csv")

# read .csv (for example KOF Barometer)
mlb <- read.csv2("data/mlb.csv")


# checkout these multi-line comments
# How many observations are in the dataset?
# Who is the tallest / smallest player?
# Are players older than 32 significantly heavier than younger players?
# What's the average size by position
# What’s the size of these players in centimeters?
# write a function!


# graphs

# for the entire dataset
cor_cars <- cor(mtcars)

# visualize correlation in a heatmap
image(cor(mtcars[, 1:7]), col = heat.colors(256), xaxt = "n", yaxt = "n", main = "Correlation Heatmap")

# add some more meaningful axis
axis(1, at = seq(0, 1, length.out = ncol(mtcars[, 1:7])), labels = colnames(mtcars[, 
                                                                                   1:7]))

# las rotates labels
axis(2, at = seq(0, 1, length.out = ncol(mtcars[, 1:7])), labels = colnames(mtcars[, 
                                                                                   1:7]), las = 2)


