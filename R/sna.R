#' Create term matrix
#'
#' Filter a matrix to limit itself to a chosen term
#'
#' @param df A matrix generated from GCA_get_corpus
#' @param term A character element used to filter your corpus
#' @param agent Your agent name, to make it easier to link them
#'
#' @export
GCA_create_term_matrix <- function(df, term, agent){
  b <- as.data.frame(as.matrix(df))
  b <- b[which(b[[term]] > 0),]

  chosen_cols <- logical(ncol(b))
  for (i in 1:ncol(b)){
    chosen_cols[i]  <- any(b[i] > 0)
  }

  b <- b[,chosen_cols]
  row.names(b) <- paste0(agent, ".", row.names(b))
  return(b)
}
#' Create edges data.frame
#'
#' Creates an edges df from your term filtered matrix. Use this method preferably with a small sample.
#'
#' @param df A df from GCA_create_term_matrix
#'
#' @export
GCA_create_edges_df <- function(df){

  list_edges <- list()

  for (i in 1:nrow(df)){
    list_edges[[i]] <- data.frame(tweet = rep(x = row.names(df)[i], length(which(df[i,] > 0))),
                                  term = names(df[which(df[i,] > 0)])
    )
  }

  edges_df <- do.call(what = dplyr::bind_rows, args = list_edges)

  return(edges_df)
}
