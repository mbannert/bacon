library(fivethirtyeight)
biopics <- fivethirtyeight::biopics

l <- list()
l$biopics <- biopics
l$unisex <- uni_names

lapply(l,nrow)


names(biopics)

t.test(box_office ~ person_of_color,
       data = biopics)

fit1 <- lm(box_office ~ person_of_color + 
             year_release + country,
           data = biopics)
summary(fit1)

# Example 2
uni_names <- fivethirtyeight::unisex_names
uni_names$no_char <- nchar(uni_names$name)

fit2 <- lm(gap ~ no_char,data = uni_names)
summary(fit2)

# Example 3 choropleth maps
library(googleVis)
# only needed vor markdown... 
# op <- options(gvis.plot.tag='chart')
drinks <- fivethirtyeight::drinks

Geo <- gvisGeoChart(drinks, locationvar='country',
                    colorvar = 'beer_servings', 
                    options=list(height=300,
                                 width=750)) 

Tbl <- gvisTable(drinks,
                 options=list(height=300,
                              width=200))

plot(Geo)
plot(gvisMerge(Geo, Tbl, horizontal=TRUE))




