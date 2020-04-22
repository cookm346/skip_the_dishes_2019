data <- read.csv("train_orders_data_with_targets.csv", stringsAsFactors = FALSE)
# data <- read.csv("test_orders_data_without_targets.csv", stringsAsFactors = FALSE)

menu <- read.csv("menu_items_details_complete.csv", stringsAsFactors = FALSE)
menu$category[menu$category == ""] <- "Other"

wxd <- read.csv("wxd.csv", row.names = 1)

data$datetime <- as.POSIXct(data$datetime)
data$day <- as.numeric(format(as.POSIXct(strptime(data$datetime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%d"))
data$hour <- as.numeric(format(as.POSIXct(strptime(data$datetime,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H"))

#number of items
data$n_items <- rowSums(data[ , paste0("quantity_", 1:10)], na.rm = TRUE)

#number of unique items
data$n_unique_items <- apply(data, 1, function(x){sum(!is.na(x[paste0("quantity_", 1:10)]))})

#proportion of unique items
data$prop_unique <- data$n_unique_items / data$n_items

#category counts
category_counts <- function(data, menu){
    cc <- matrix(0, nrow(data), length(unique(menu$category)))
    colnames(cc) <- unique(menu$category)
    
    for(i in 1:nrow(data)){
        items <- data[i, paste0("item_", 1:10)]
        items <- items[items != ""]
        counts <- data[i, paste0("quantity_", 1:10)]
        counts <- counts[!is.na(counts)]
        
        x <- menu$category[menu$menu_item_id %in% items]
        x <- aggregate(counts, by=list(x), sum)
        cc[i, x$Group.1] <- x$x
    }
    return(cc)
}

data <- data.frame(data, category_counts(data, menu))

data$n_cat <- apply(data[ , c("Other", "Cookies", "Cakes", "Pies", "Soup", "Salads.and.Dressings")], 
                    1, function(x){sum(x > 0)})

menu$title_length <- sapply(menu$title, function(x){length(strsplit(x, " ")[[1]])})
menu$direction_length <- sapply(menu$direction, function(x){length(strsplit(x, " ")[[1]])})

word_counts <- function(data){
    cc <- matrix(0, nrow(data), 14)      #summary (and sds) for both title and directions
    colnames(cc) <- c("min_title", "fq_title", "median_title", "mean_title", "tq_title", "max_title", "sd_title",
                      "min_dir", "fq_dir", "median_dir", "mean_dir", "tq_dir", "max_dir", "sd_dir")
    
    for(i in 1:nrow(data)){
        items <- data[i, paste0("item_", 1:10)]
        items <- items[items != ""]
        counts <- data[i, paste0("quantity_", 1:10)]
        counts <- counts[!is.na(counts)]
        
        title_lengths <- menu$title_length[menu$menu_item_id %in% items]
        title_lengths <- title_lengths * counts
        
        dir_lengths <- menu$direction_length[menu$menu_item_id %in% items]
        dir_lengths <- dir_lengths * counts
        
        cc[i, ] <- c(summary(title_lengths), sd(title_lengths),
                     summary(dir_lengths), sd(dir_lengths))
    }
    return(cc)
}


wc <- word_counts(data)
wc[is.na(wc)] <- 0
data <- data.frame(data, wc)
rm(wc)


#generate interaction variables
int_data <- model.matrix( ~.^2 -1, data=data[ , which(names(data) == "day"):which(names(data) == "sd_dir")])


build_semantic_orders <- function(data, wxd){
    vecs <- matrix(0, nrow(data), ncol(wxd))
    for(i in 1:nrow(data)){
        items <- data[i, paste0("item_", 1:10)]
        items <- items[items != ""]
        counts <- data[i, paste0("quantity_", 1:10)]
        counts <- counts[!is.na(counts)]
        
        r <- wxd[items, ] * counts
        
        if(! is.null(dim(r)))
            vecs[i, ] <- colMeans(r)
        else
            vecs[i, ] <- r
    }
    return(vecs)
}

order_vectors <- build_semantic_orders(data, wxd)

ids <- data$order_id
# data <- data.frame(int_data, 
#                    order_vectors, 
#                    food_prep_time_minutes = data$food_prep_time_minutes)
data <- data.frame(int_data, 
                   order_vectors)
rownames(data) <- ids

write.csv(data, "train.csv", row.names = TRUE)
# write.csv(data, "test.csv", row.names = TRUE)

