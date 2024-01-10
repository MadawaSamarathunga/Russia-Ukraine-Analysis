calculate_and_plot_tfidf <- function(article_data,articles,journal) {
  
  
  
  
  
  # Tokenize the articles, remove stopwords
  tfidf_data <- article_data%>%
    mutate(articles = as.character(articles)) %>%
    unnest_tokens(word, articles) %>%
    anti_join(stop_words, by = "word") %>%
    count(journal, word) %>%
    group_by(journal) %>%
    bind_tf_idf(word, journal, n)
  
  # Extract top 10 tf-idf words for each journal
  top_tfidf_words <- tfidf_data %>%
    group_by(journal) %>%
    slice_max(tf_idf, n = 10, with_ties = TRUE) %>%
    ungroup()
  
  # Reorder words based on tf-idf for plotting
  top_tfidf_words <- top_tfidf_words %>%
    arrange(journal, desc(tf_idf)) %>%
    mutate(word = fct_reorder(word, tf_idf))
  
  # Plot the top tf-idf words
  plot <-ggplot(top_tfidf_words, aes(x = word, y = tf_idf, fill = journal)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~journal, scales = "free") +
    coord_flip() +
    labs(title = "Highest tf-idf Words in Ukraine war articles in 2022", x = "", y = "tf−idf index") +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 8))
  return(plot)
  
}