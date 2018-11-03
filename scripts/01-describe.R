# load packages
#-------------------------------------------------------------------------------

packages <- c("here", "dplyr", "funModeling", "ggplot2", "magrittr", "tibble", "gtools", "minerva")
sapply(packages, library,
       character.only = TRUE, logical.return = TRUE, quietly = TRUE)

# read data
#-------------------------------------------------------------------------------

houses <- read.csv(
    file = here("funky","data", "raw", "train.csv"),
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

# houses %>%
#     transmute(MSSubClass = factor(MSSubClass)) %>%
#     pull() %>%
#     table() %>%
#     prop.table() %>%
#     round(digits = 3) %>%
#     multiply_by(100) %>%
#     sort(decreasing = TRUE) %>% 
#     as.data.frame()

houses %>% 
    ggplot() +
    geom_boxplot(aes(x = factor(MSSubClass), y = SalePrice))

(MSSubClass_summary <- houses %>%
        mutate(MSSubClass = factor(MSSubClass)) %>%
        group_by(MSSubClass) %>%
        dplyr::summarize(n = n(), mean_price = mean(SalePrice)) %>%
        arrange(desc(n)))

set.seed(1)
MSSubClass_clusters <- kmeans(
    x = MSSubClass_summary$mean_price,
    centers = 3
    ) %>% 
    extract2("cluster")

MSSubClass_coef <- lm(SalePrice ~ factor(MSSubClass), data = houses) %>%
    summary() %>% 
    extract2("coefficients") %>%
    as.data.frame() %>% 
    rownames_to_column() %>%
    as.tibble() %>%
    mutate(MSSubClass = replace(rowname, 1, "factor(MSSubClass)20")) %>%
    transmute(
        MSSubClass = substring(MSSubClass, first = 19),
        signif = stars.pval(`Pr(>|t|)`)
    )


MSSubClass_summary %>%
    cbind(MSSubClass_clusters) %>%
    merge(MSSubClass_coef, sort = FALSE) %>% 
    arrange(MSSubClass_clusters)


# LotArea
#-------------------------------------------------------------------------------

houses %>% ggplot() + geom_histogram(aes(LotArea))

houses %>% ggplot() + geom_point(aes(LotArea, SalePrice))

houses %>%
    ggplot(aes(x = LotArea, y = SalePrice)) +
    geom_point() +
    xlim(0, 50000) +
    stat_smooth(method = "lm", formula = y ~ x)

# Street
#-------------------------------------------------------------------------------
street_summary <- describe(houses$Street)
street_summary


## 99.6% of streets are paved.. maybe we can get riid of this parameter


#Alley
#-------------------------------------------------------------------------------
#Grvl	Gravel = 50
#Pave	Paved = 41
#NA 	No alley access = 1369

describe(houses$Alley)
# I would suggest to rename NA to NO_alley or so...
houses$Alley[is.na(houses$Alley)] <- "no_access"
describe(houses$Alley)

houses %>% select(Alley) %>% mutate(replace(houses$Alley, list = which(is.na(houses$Alley)), values = "No_alley" )) %>% pull %>% as.data.frame() %>%
    ggplot() + geom_boxplot(aes(x = as.factor(Value), y = SalePrice))
alley <- houses %>% select(Alley) %>% mutate(replace(houses$Alley, list = which(is.na(houses$Alley)), values = "No_alley" )) %>% pull %>% as.factor()
houses %>% ggplot() + geom_boxplot(aes(x = alley, y = SalePrice) )
aov_fit <- aov(SalePrice ~ alley, data = houses)
summary(aov_fit)                        
TukeyHSD(aov_fit)
## looks like there is not much of difference between No_allay and Paved in terms of explaining SalePrice, one may think to merge them
## but their is significant difference between Gravel and the rest.

#LotShape
#-------------------------------------------------------------------------------
describe(houses$LotShape)
houses %>% ggplot() + geom_boxplot(aes(x = LotShape, y = SalePrice))
lot_price_fit <- aov(SalePrice ~ LotShape, data = houses)
TukeyHSD(lot_price_fit) 

## IR3 encompasses 7% and is not different from  IR1, IR2 and IR3 are not different, let up merge IR1+IR3 to get IR1, IR2, Reg 
LotShape_mut <- houses %>% select(LotShape) %>% mutate(LotShape = replace(LotShape, LotShape == "IR3", "IR1" )) %>% select(LotShape) %>% pull %>%as.factor()
houses %>% ggplot() + geom_boxplot(aes(x = LotShape_mut, y = SalePrice)) 
aov_fit_lot_shape <- aov(SalePrice ~ LotShape_mut, data = houses)
summary(aov_fit_lot_shape)                        
TukeyHSD(aov_fit_lot_shape) ## all comparisons have very low pvalues...

## I think it is not a bad idea to merge IR1 with IR3 and keep the rest.

