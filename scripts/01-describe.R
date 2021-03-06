# load packages
#-------------------------------------------------------------------------------

packages <- c("here", "dplyr", "funModeling", "ggplot2", "magrittr", "tibble", "gtools", "minerva", "mgcv", "ggpubr")
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

###############################################################################




# this function orders levels of variable to display ordered by SalePrice boxplot
# as input takes name of variable OR it's index as integer as a string and returns ordered boxplot
# number on top of boxes represent portion of this level
ordered_box_plot <- function(var = NULL, clust = 1){
    if (length(clust) ==1){
        clust <- 1
    } else {
        clust <- clust
    }
    if (is.null(var)){
        message("define variable")
        return(NULL)
    }
    if (is.character(var)){
        var <- grep(var, colnames(houses))
    }
    if (is.numeric(var)) {
        if (var >= ncol(houses)){
            message("variable index is greater than number of columns")
            return(NULL)
        }
        var_ind <- var
        var <- houses[, var]
        var_name <- colnames(houses)[var_ind]
        var_description <- describe(var)
        var_proportions <- round((var_description$values$frequency / nrow(houses)), 3)
        var_by_price <- houses %>% group_by(get(var_name)) %>% dplyr::summarize(n = n(), mean_price = mean(SalePrice)) %>%
            arrange(desc(n)) %>% as.data.frame()
        var_by_price$proportion <- var_by_price$n / nrow(houses)
        var_reordered <- factor(var, levels = var_by_price[,1][order(var_by_price[,3])])
        ggp <- houses %>% ggplot() + geom_boxplot(aes(x = var_reordered, y = SalePrice, fill = clust)) + labs(x = names(var) )
        proportion <- round(var_by_price$proportion[order(var_by_price[,3])], 3)
        ggp + annotate("text", label = proportion, x = 1:length(unique(var)), y = max(houses$SalePrice)*0.9 ) 
    } else {
        message("type of input variable should be either character or integer")
    }
}
ordered_box_plot('RoofMatl')
###############################################################################

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
describe(houses$Street)
houses %>% group_by(Street) %>% dplyr::summarize(n = n(), mean_price = mean(SalePrice)) %>%
    arrange(desc(n))
houses %>% group_by(Street) %>% dplyr::summarize(n = n(), sd_price = sd(SalePrice)) %>%
    arrange(desc(n))
houses %>% ggplot() + geom_boxplot(aes(x = Street, y = SalePrice))
summary(lm(SalePrice ~ Street, data =houses))

##  99.6% of streets are paved... And only 0.4% gravel, still gravel group looks like different 


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


#LandContour: Flatness of the property
#-------------------------------------------------------------------------------
describe(houses$LandContour)
compare_means(SalePrice ~ LandContour, data = houses) ## quite useful function from ggpubr
houses %>% ggplot() + geom_boxplot(aes(x = LandContour, y = SalePrice)) 
land_contour_fit <- aov(SalePrice ~ LandContour, data = houses)
TukeyHSD(land_contour_fit) 
## don't think we can merge groups here, for now I would keep them as they are


#Utilities: Type of utilities available
#-------------------------------------------------------------------------------
describe(houses$Utilities)
# as you see only one NoSeWa, don't think we can use this variable

#LotConfig: Lot configuration
#-------------------------------------------------------------------------------
describe(houses$LotConfig)
compare_means(SalePrice ~ LotConfig, data = houses) ## quite useful function from ggpubr
houses %>% ggplot() + geom_boxplot(aes(x = LotConfig, y = SalePrice)) 
lot_config_fit <- aov(SalePrice ~ LotConfig, data = houses)
TukeyHSD(lot_config_fit) 

## merge CulDsac with FR3, merge Corner, FR2, inside
summary(lm(SalePrice ~ LotConfig, data = houses))
LC <- gsub("FR3|CulDSac", "Cat1", houses$LotConfig, perl = TRUE)
LC <- gsub("Corner|FR2|Inside", "Cat2", LC)
summary(lm(SalePrice ~ LC, data = houses))
boxplot(houses$SalePrice~LC)

# LandSlope: Slope of property
#-------------------------------------------------------------------------------
describe(houses$LandSlope)
houses %>% ggplot() + geom_boxplot(aes(x = LandSlope, y = SalePrice))
TukeyHSD(aov(SalePrice ~ LandSlope, data = houses))
## they don't look like really different. Should we get rid of them ?

#Neighborhood
#-------------------------------------------------------------------------------
describe(houses$Neighborhood)
houses %>% ggplot() + geom_boxplot(aes(x = Neighborhood, y = SalePrice))
price_by_neib<- houses %>% group_by(Neighborhood) %>% dplyr::summarize(n = n(), mean_price = mean(SalePrice)) %>%
    arrange(desc(n))
neigh_reordered <- factor(houses$Neighborhood, levels = price_by_neib$Neighborhood[order(price_by_neib$mean_price)])
houses %>% ggplot() + geom_boxplot(aes(x = neigh_reordered, y = SalePrice))
neigh_tukeyHSD <- TukeyHSD(aov(SalePrice ~ Neighborhood, data = houses))$Neighborhood %>% as.data.frame()

## my impression is that we have quite smooth gdarient of mean / median value of SalePrice as a function of Neighborhood
## we can intorduce bins, but will do it rather arbitrary. 
## I don't know whether it's a good idea to substitute names of neighborhood
## by intergers (or maybe levels) representing rank of each neighborhood.

# Condition1:
#-----------------------------------------------------------------------------------
describe(houses$Condition1)
houses %>% ggplot() + geom_boxplot(aes(x = Condition1, y = SalePrice))
ordered_box_plot("Condition1")

#-----------------------------------------------------------------------------------
describe(houses$Condition2)
ordered_box_plot("Condition2")
# the same, just a gradient

#BldgType: Type of dwelling
#----------------------------------------------------------------------------------
describe(houses$BldgType)
ordered_box_plot("BldgType")

#HouseStyle: Style of dwelling
#----------------------------------------------------------------------------------
ordered_box_plot("HouseStyle")

#OverallQual
#----------------------------------------------------------------------------------
describe(houses$OverallQual)
# currently it is integer
houses %>% ggplot(aes(x = OverallQual, y = SalePrice)) + geom_point() +
    stat_smooth(method = "loess", formula = y ~ x)
houses %>% ggplot(aes(x = OverallQual, y = SalePrice)) + geom_point() +
    stat_smooth(method = "lm", formula = y ~ x)
compare_means(SalePrice ~ OverallQual, data = houses) %>% as.data.frame() %>% filter(p.signif == 'ns')
## my impression is that Overalqual with values 1 and 2 are identical and I would group them together:
qm_houses <- houses %>% mutate(OverallQual_merged = ifelse(OverallQual <2,2,OverallQual )) 
qm_houses %>% ggplot(aes(x = OverallQual_merged, y = SalePrice)) + geom_point() +
    stat_smooth(method = "loess", formula = y ~ x)
## to my mind loess better approximate relationship between OverallQual and SalePrice,
## can we use polynomial fir for OverrallQual?
## or we just coerce to factor and let the model choose coefficients

summary(lm(SalePrice ~ as.factor(OverallQual), data = houses))
summary(lm(SalePrice ~ OverallQual, data = houses))
## here I compared sum of residuals of two model : intact OverallQual and as.factor() and I've got 9% reduction of total residuels
sum(abs(residuals(lm(SalePrice ~ OverallQual, data = houses)))) / sum(abs(residuals(lm(SalePrice ~as.factor( OverallQual ), data = houses))))

#OverallCond
#-----------------------------------------------------------------------------------
describe(houses$OverallCond)
houses %>% ggplot(aes(x = OverallCond, y = SalePrice)) + geom_point() +
    stat_smooth(method = "lm", formula = y ~ x)
houses%>% filter(OverallCond == 2) %>% select(OverallCond, SalePrice)
## not sure it's a good predictor

summary(lm(SalePrice ~ as.factor(OverallQual), data = houses))
sum(abs(residuals(lm(SalePrice ~ as.factor(OverallQual), data = houses)))) / sum(abs(residuals(lm(SalePrice ~as.factor(OverallQual) + as.factor(OverallCond), data = houses))))
## now I sure wee need to get rid of it beause it does not bring anything


#YearBuilt
#--------------------------------------------------------------------------------
describe(houses$YearBuilt)
houses %>% ggplot(aes(x = YearBuilt, y = SalePrice)) + geom_point() + stat_smooth(method = "lm", formula = y ~ x)
summary(lm(SalePrice ~ YearBuilt, data = houses)) #Adjusted R-squared:  0.2729 


#YearRemodAdd
#---------------------------------------------------------------------------------
describe(houses$YearRemodAdd)

houses %>% ggplot(aes(x = YearRemodAdd, y = SalePrice)) + geom_point() + stat_smooth(method = "lm", formula = y ~ x)
summary(lm(SalePrice ~ YearRemodAdd , data = houses))  #Adjusted R-squared:  0.2566
summary(lm(SalePrice ~ YearRemodAdd + YearBuilt, data = houses))  #Adjusted R-squared:  0.3324 

sum(abs(residuals(lm(SalePrice ~YearRemodAdd, data = houses)))) / sum(abs(residuals(lm(SalePrice ~YearBuilt + YearRemodAdd, data = houses))))
#1.05, so addition of YearBuilt on top of YearRemodAdd reduces residuals by 5 % 

# I would keep YearRemodAdd only


#RoofStyle
#-------------------------------------------------------------------------------
ordered_box_plot("RoofStyle")
compare_means(SalePrice ~ RoofStyle, data = houses)
# 1 out 15 comparisons has pvalue < 0.05 and one level has 78% of cases.
#don't think it's meaningfull parameter



#RoofMatl
#-------------------------------------------------------------------------------
ordered_box_plot('RoofMatl')
compare_means(SalePrice ~ RoofMatl, data = houses) %>% as.data.frame() %>% arrange(p.adj)

## either get rid of this variable of keep CompShg and WdShngl

#Exterior1st
#--------------------------------------------------------------------------------------

ordered_box_plot('Exterior1st', clust = tr)
compare_means(SalePrice ~ Exterior1st, data = houses) %>% as.data.frame() %>% arrange(p.adj)
t <- houses %>% group_by(Exterior1st) %>% dplyr::summarize(n = n(), mean_price = mean(SalePrice)) %>%
    arrange(desc(n)) %>% as.data.frame()
tk <- kmeans(t$mean_price, centers = 3)
tk2 <- kmeans(t$mean_price, centers = 3)
mean(tk$cluster == tk2$cluster)
houses %>% ggplot(aes(x = Exterior1st, y = SalePrice, fill = tr))+ geom_boxplot()

tr <- c()
for (i in 1:length(t$n)){
    tr <- c(tr, rep(tk$cluster[i] , t$n[i]))
}
ht <- houses
ht$tr <- tr
ht %>% filter(tr == 1) %>% select(tr, Exterior1st)
