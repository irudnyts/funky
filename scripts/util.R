#' @param x an atomic vector of arbitrary type ...
#'
describe_avector <- function(x, inf.na = FALSE) {
    
    # validate arguments
    stopifnot(exprs = {
        is.atomic(x)
        length(x) > 0
        is.logical(inf.na)
        length(inf.na) == 1
        !is.na(inf.na)
    })
    
    # change Inf to NA, if needed
    if (inf.na) x[is.infinite(x)] <- NA
    
    
    if (is.numeric(x)) {
        
        if (is.integer(x)) {
            
        } else {
            # are they equidistant?
        }
        
    } else if (is.character(x)) {
        
        # is it possible to coerce to numeric?
    
    } else if (is.logical(x)) {
    
        
            
    } else if (is.factor(x)) {
        is_ordered <- is.ordered(x)
    }
    
    # how many missing values?
    
    # how many unique?
    
    
    # norm <- "not applicable"
    # if (is.numeric(x)) {
    #     ctn <-  "already number"
    #     norm <- shapiro.test(x)
    #     npos <- sum(x > 0, na.rm = T)
    #     nneg <- sum(x < 0, na.rm = T)
    # } else if (is.character(x))  {
    #     if (mean(is.na(as.numeric(x))) <0.9){
    #         ctn <- "yes"
    #     }
    # } else if (is.factor(x)){
    #     ctn <- "better keep as factor"
    # } else {ctn <- "no"}
    # dt <- data.frame(type = typeof(x),
    #                  class = class(x),
    #                  N_observation = length(x),
    #                  NAs_portion = mean(is.na(x)),
    #                  missing = sum(is.na(x)),
    #                  distinct = length(unique(x)),
    #                  ctn = ctn,
    #                  infinite = sum(is.infinite(x)) )
    # if (is.numeric(x)){
    #     dt$positives <- npos
    #     dt$negatives <- nneg
    # }
    # out <- list(dt, summary = summary(x), normality_test = norm)
    # return(out)
}


# Test function
# v <- rnorm(n =100,mean = 50,sd =20)
# vchar <- as.character(v)
# vfac <- as.factor(c(rep("a",5), rep('b',10)))
# 
# descVar(vi,inf.na = T)
# descVar(vchar)
# descVar(vfac)
