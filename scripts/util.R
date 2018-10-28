#' @param x an atomic vector of arbitrary type ...
#'
describe_avector <- function(x) {
    
    # check if magrittr is loaded
    stopifnot("magrittr" %in% (.packages()))
    
    # validate argument
    stopifnot(exprs = {
        is.atomic(x)
        length(x) > 0
    })
    
    if (is.numeric(x)) {
        
        is_equidistant <- ifelse(
            unique(x) %>% sort() %>% diff() %>% length() == 1,
            yes = TRUE, no = FALSE
        )
        
        n_positive <- sum(x > 0, na.rm = TRUE)
        n_negative <- sum(x < 0, na.rm = TRUE)
        
        # is_symmetric <-
        
    } else if (is.character(x)) {
        
        is_coercable <- x[!is.na(x)] %>%
            suppressMessages(as.numeric(.)) %>%
            is.na() %>%
            magrittr::not() %>%
            all()
    
    } else if (is.factor(x)) {
        
        is_ordered <- is.ordered(x)
        
        is_coercable <- x[!is.na(x)] %>%
            as.character() %>%
            suppressMessages(as.numeric(.)) %>%
            is.na() %>%
            magrittr::not() %>%
            all()
    }
    
    rval <- list(
        # compute common measures
        type = typeof(x),
        length = length(x),
        n_missing = is.na(x) %>% sum(),
        p_missing = n_missing / length(x),
        n_distinct = unique(x) %>% length()
        # ...
    )
    
    class(rval) <- "description"
    
    return(rval)
    
}

#' 
print.description <- function(x) {
    # print description
}
