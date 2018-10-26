# load packages
#-------------------------------------------------------------------------------

packages <- c("here", "dplyr", "funModeling", "ggplot2", "magrittr")
sapply(packages, library,
       character.only = TRUE, logical.return = TRUE, quietly = TRUE)

# read data
#-------------------------------------------------------------------------------

houses <- read.csv(
    file = here("data", "raw", "train.csv"),
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE
)

dim(houses)

str(houses)

# SalePrice
#-------------------------------------------------------------------------------

houses %>% select(SalePrice) %>% pull() %>% summary()

houses %>% select(SalePrice) %>% ggplot() + geom_histogram(aes(SalePrice))


# MSSubClass
#-------------------------------------------------------------------------------

houses %>% transmute(MSSubClass = factor(MSSubClass)) %>% pull() %>% table() %>% prop.table() %>% round(digits = 3) %>% multiply_by(100) %>% sort()

houses %>% ggplot() + geom_boxplot(aes(x = factor(MSSubClass), y = SalePrice))

data.frame(x = sort(coef(lm(SalePrice ~ factor(MSSubClass), data = houses))))
    