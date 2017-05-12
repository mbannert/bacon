# take a way for the book: 
# need a guideline: what to with an unknown dataset
# guide to descriptive statistics - cheat sheet / reference / guide
# 


library(fivethirtyeight)
d <- data(package="fivethirtyeight")
d_name_538 <- d$results[,"Item"]

dlist <- lapply(d_name_538,get)
names(dlist) <- d_name_538

head(dlist$unisex_names)


library(googleVis)
# only needed vor markdown... 
# op <- options(gvis.plot.tag='chart')

Geo <- gvisGeoChart(drinks, locationvar='country',
                    colorvar='beer_servings', 
                    options=list(height=300, width=750)) 
Tbl <- gvisTable(drinks, options=list(height=300, width=200))
plot(Geo)
plot(gvisMerge(Geo, Tbl, horizontal=TRUE))




lapply(dlist,function(x) c(nrow(x),ncol(x)))
lapply(dlist,rowsAndCols)

summaries <- lapply(dlist,summary)

write.csv2(trump,
           file="data/trump.csv")

lapply(names(dlist),function(x){
  ds <- get(x)
  fname <- paste("data/",x,".csv",sep = "")
  write.csv2(ds,file = fname)
})




trump <- dlist$trump_news
rowsAndCols(trump)
lapply(dlist,rowsAndCols)









lapply(dlist,nrow)






nameDerFunktion <- function(datensatz){
  
}


meineFunktion(trump)
dim(trump)


lapply(dlist,dim)



sapply(dlist,nrow)
sapply(dlist,ncol)










trump <- get("trump_news")











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


