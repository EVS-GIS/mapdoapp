% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_utils.R
\name{utils_normalize_string}
\alias{utils_normalize_string}
\title{Normalize a string by removing spaces, accents, and special characters.}
\usage{
utils_normalize_string(input_string)
}
\arguments{
\item{input_string}{The input string to be normalized.}
}
\value{
The normalized string with spaces, accents, and special characters removed, and in lowercase.
}
\description{
This function takes an input string and normalizes it by removing spaces, accents, diacritics,
and special characters. It then converts the string to lowercase.
}
\examples{
original_string <- "Thïs is à sâmplè strîng with spèciál chàracters!"
normalized_string <- utils_normalize_string(original_string)
cat(normalized_string)  # "thisisasamplestringwithspecialcharacters"

}
