% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_and_plot_tfidf.R
\name{calculate_and_plot_tfidf}
\alias{calculate_and_plot_tfidf}
\title{Calculate and Plot TF-IDF}
\usage{
calculate_and_plot_tfidf(article_data, articles, journal)
}
\arguments{
\item{article_data}{A dataframe containing the articles and their metadata.}

\item{articles}{The column name in `article_data` that contains the article text.}

\item{journal}{The column name in `article_data` that contains the journal names.}
}
\value{
A ggplot object representing the top TF-IDF words for each journal.
}
\description{
This function calculates the Term Frequency-Inverse Document Frequency (TF-IDF) for words in a collection of articles,
grouped by journal. It then plots the top TF-IDF words for each journal, providing a visualization of the most
distinctive words used in different journals' articles about the Ukraine war in 2022.
}
\examples{
# Example usage:
calculate_and_plot_tfidf(article_data,"articles","journal")
}
