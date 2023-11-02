#' Clean tweets
#'
#' This function cleans tweets from a rtweet data.frame, erasing links, emojis, mentions and hashtags, which are saved
#' already in your data.frame. It can also filter tweets from a certain year.
#'
#' @param df A rtweet data.frame (timeline, tweets collected, etc.). No default.
#' @param year Year to filter your tweets. Default is ".", extracting all data from your df.
#'
#' @export
GCA_clean_tweets <- function(df, year = "."){
    text_ref <- df$text[grep(year, df$created_at)] %>%
      gsub(pattern = "–", replacement =  " ", x =  .) %>%
      gsub(x = ., pattern = "#.*?[[:blank:]]|#.*?[[:punct:]]", replacement = "") %>%
      gsub(x = ., pattern = "http.*?[[:blank:]]", replacement = "") %>%
      gsub(x = ., pattern ="<(.*?)>|“|”", replacement = "") %>%
      emoji::emoji_replace_all(string = ., replacement = "") %>%
      gsub(x = ., pattern = "@.+?[[:punct:]]|@.+?[[:blank:]]|@.+?[[:space:]]", replacement = "") %>%
      gsub(pattern = "#.+$|http.+$", replacement = "", x = .)
    return(text_ref)
  }
#' Clean text
#'
#' This function cleans texts in a character vector, erasing links, emojis, mentions and hashtags.
#'
#' @param vec A character vector
#'
#' @export
GCA_clean_text <- function(vec){
  text_ref <- vec %>%
    gsub(pattern = "–", replacement =  " ", x =  .) %>%
    gsub(x = ., pattern = "#.*?[[:blank:]]|#.*?[[:punct:]]", replacement = "") %>%
    gsub(x = ., pattern = "https.*?[[:blank:]]", replacement = "") %>%
    gsub(x = ., pattern ="<(.*?)>|“|”", replacement = "") %>%
    emoji::emoji_replace_all(string = ., replacement = "") %>%
    gsub(x = ., pattern = "@.+?[[:punct:]]|@.+?[[:blank:]]|@.+?[[:space:]]", replacement = "") %>%
    gsub(pattern = "#.+$|https.+$", replacement = "", x = .)
  return(text_ref)
}
#'
#' Get Video Corpus
#'
#' This function retrieves the transcript from a video and returns it as a character vector.
#'
#' Requires: dplyr, youtubecaption
#'
#' @param chosen_url Url from a youtube video
#' @param lang Language to find transcripts
#'
GCA_get_video_corpus <- function(chosen_url, lang = "pt"){
  vec <- try(youtubecaption::get_caption(chosen_url, language = lang), silent = T)

  if (class(vec) == "try-error"){
    return(NA)
  } else {
    vec <- vec  %>% .$text %>% paste0(., collapse = " ")
    return(vec)}
}
#'
#' Get corpus
#'
#' This function cleans a vector which contains your text to be analyzed, returning a corpus in a dtm format
#' It's basically a wrapper of different function from the tm package
#'
#' @param vec Your text to be cleaned and transformed into a corpus
#' @param stopwords_chos A character vector with stopwords to be removed
#'
#' @export
GCA_get_corpus <- function(vec, stopwords_chos = tm::stopwords("pt")){

  corpus <- tm::Corpus(tm::VectorSource(vec)) %>%  tm::tm_map(tm::content_transformer(tolower)) %>% tm::tm_map(tm::content_transformer(tm::removeNumbers)) %>%
    tm::tm_map(tm::content_transformer(tm::removePunctuation)) %>% tm::tm_map(tm::content_transformer(tm::removeWords), stopwords_chos) %>%
    tm::tm_map(tm::content_transformer(tm::stripWhitespace)) %>% tm::DocumentTermMatrix()

  return(corpus)
}
#'
#' Get main terms
#'
#' This function returns the top main terms from a corpus matrix (supports dtm, matrix and data.frame) in a vector.
#'
#' @param fir_mx Your text to be cleaned and transformed into a corpus
#' @param n An integer value, defaulted to 10, to choose how much main terms your are looking for
#'
#' @export
GCA_get_main_terms <- function(fir_mx, n = 10){
  fir_term_freq <- apply(fir_mx, 2, sum)
  fir_term_freq <- sort(fir_term_freq, decreasing = T)[1:n]
  return(names(fir_term_freq))
}
#'
#' Get main terms proportion
#'
#' This function returns the proportions of each main term into the matrix passed into our function.
#' Returns a named numeric vector
#'
#' @param mx matrix to be analyzed each main terms value
#' @param main_terms a character vector with the main terms to be searched into our matrix
#' @param get_prop logical value to decide if it's going to convert absolute into proportional value
#'
#' @export
GCA_get_main_terms_prop <- function(mx, main_terms, get_prop = T){
  vec <- vector(mode = "numeric", length = length(main_terms))
  for (i in 1:length(main_terms)){
    vec[i] <- sum(mx[, grep(paste0("^", main_terms[i], "$"), colnames(mx))])
  }

  if(get_prop){
  vec <- vec/sum(mx)
  }

  names(vec) <- main_terms
  return(vec)
}