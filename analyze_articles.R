analyze_articles <- function(article_data, published, journal) {
  
  
  # Date Analysis: Distribution of articles over time
  article_data$month_year <- floor_date(article_data$published, "month")
  articles_per_month <- article_data %>% 
    count(month_year) %>% 
    arrange(month_year)
  
  # Journal Analysis: Count of articles per journal
  articles_per_journal <- article_data %>% 
    count(journal)
  
  # Creating plots
  
  # Plot for Article Distribution Over Time
  plot1 <- ggplot(articles_per_month, aes(x=month_year, y=n)) +
    geom_bar(stat="identity", fill="steelblue") +
    theme_minimal() +
    labs(title="Article Distribution Over Time", x="Month", y="Number of Articles")
  
  # Plot for Journal Article Counts
  plot3 <- ggplot(articles_per_journal, aes(x=reorder(journal, n), y=n)) +
    geom_bar(stat="identity", fill="steelblue") +
    coord_flip() +
    theme_minimal() +
    labs(title="Articles per Journal", x="Journal", y="Number of Articles")
  
  
  
  grid.arrange(plot1, plot3, ncol=1)
}
