### Regular Expressions and Subsetting ###

# Combine strings
paste("abc", "def")  # Default sep is a space
paste("abc", "def", sep = "-")
paste0("abc", "def")

# Split a string


# Extract word(s) from a string
library(stringr)
x <- "This is a test, so be patient."
word(x, 2)
word(x, 1, 3)
word(x, 4)  # Be aware that punctuation is included in the words

# Replace FIRST instance of a string
sub("a", "x", c("abc", "bafa", "xyz"))

# Replace ALL instances of a string
gsub("a", "x", c("abc", "bafa", "xyz"))

# Trime white space on each side
str_trim("  abc   ")
