plot_zipfs_law <- function(article_data,articles,journal) { 
  
  # Tokenize the words
  zipf_data <- article_data %>%
    mutate(articles = as.character(articles)) %>%
    unnest_tokens(word, articles) %>%
    count(journal, word) %>%
    bind_tf_idf(word, journal, n)
  
  # rank calculation 
  zipf_data <- zipf_data %>%
    group_by(journal) %>%
    arrange(desc(tf)) %>%
    mutate(rank = row_number()) %>%
    ungroup()
  
  # linear regression analysis
  regression_results <- lm(log(tf) ~ log(rank), data = zipf_data)
  
  # Plot log-log graph with regression line and different colors for each journal
  plot <- ggplot(zipf_data, aes(x = rank, y = tf, color = journal)) +
    geom_line(size = 1.5) + 
    geom_smooth(method = "lm", size = 1, se = FALSE, color = "black", aes(group = 1)) +
    scale_x_log10() +
    scale_y_log10(labels = scales::label_number()) + 
    labs(title = "Zipf's Law for Ukraine war articles in 2022 with Linear Regression",
         x = "World rank",
         y = "Term Frequency(linear regression)") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(list(article_data = zipf_data,regression = summary(regression_results),plot))
}