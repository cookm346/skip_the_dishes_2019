setwd("/home/share/mcook/DataScienceChallange/Skip/analysis")

cleanText <- function(x){
    return(tolower(strsplit(gsub("[^[:alnum:] ]", " ", x), " +")[[1]]))
}

build_word_counts <- function(menu){
    words <- paste(c(menu$title, menu$directions), collapse = " ")
    words <- cleanText(words)
    unique_words <- sort(unique(words))
    
    wxd <- matrix(0, length(unique_words), nrow(menu))
    rownames(wxd) <- unique_words
    colnames(wxd) <- menu$title
    
    for(i in 1:nrow(menu)){
        title_and_directions <- paste(menu$title[i], menu$directions[i], collapse = " ")
        wxd[ , i] <- sapply(unique_words, function(x)sum(x == cleanText(title_and_directions)))
    }
    return(wxd)
}

menu <- read.csv("menu_items_details_complete.csv", stringsAsFactors = FALSE)
menu$category[menu$category == ""] <- "Other"

directions <- menu$directions
directions <- paste(directions, collapse = " ")
directions <- cleanText(directions)
unique_words <- sort(unique(directions))

wxd <- matrix(0, length(unique_words), nrow(menu))
rownames(wxd) <- unique_words
colnames(wxd) <- menu$title


wxd <- build_word_counts(menu)

wxd <- t(wxd)
rownames(wxd) <- menu$menu_item_id

write.csv(wxd, "wxd.csv")

