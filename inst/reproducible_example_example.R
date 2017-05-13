# minimal dataset for exemplary run
# created by dput
mlb_min <- structure(list(Name = c("Adam_Donachie", "Paul_Bako", "Ramon_Hernandez", 
                        "Kevin_Millar", "Chris_Gomez", "Brian_Roberts"),
               height = c(74L, 
                          74L, 72L, 72L, 73L, 69L)),
          .Names = c("Name", "height"),
          class = "data.frame", row.names = c(NA, 
                                              6L))

#' conversion function 
#' 
#' this is my description
#' 
#' @param inches numeric US inches 
#' @author alle
#' @export
inchToCm <- function(inches){
  inches*2.54
}

# function call 
mlb_min$height_in_cm <- inchToCm(mlb_min$height)


# Example 2: simulated data 
nv <- rnorm(10)

set.seed(123)
nv2 <- rnorm(10)
nv3 <- rnorm(10000)
tv <- rt(100,5)

plot(density(rnorm(100)))










