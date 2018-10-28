# load packages
#-------------------------------------------------------------------------------

packages <- c("here", "dplyr", "funModeling", "ggplot2", "magrittr", "tibble", "gtools")
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
