library(rvest)

url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'
webpage <- read_html(url)

# Scrape Ranks of Films
rank.html <- html_nodes(webpage, ".text-primary")
rank <- html_text(rank.html)
rank <- as.numeric(rank)

# Scrape Names of Films
film.html <- html_nodes(webpage, ".lister-item-header a")
film <- html_text(film.html)

# Scrape Film Description
description.html <- html_nodes(webpage, "#main p:nth-child(4)")
description <- html_text(description.html)
description <- gsub("\n", "", description)

# Scrape Runtimes
runtime.html <- html_nodes(webpage, ".text-muted .runtime")
runtime <- html_text(runtime.html)
runtime <- gsub(" min", "", runtime)
runtime <- as.numeric(runtime)

# Scrape Genres
genre.html <- html_nodes(webpage, ".genre")
genre <- html_text(genre.html)
genre <- gsub("\n", "", genre)
genre <- gsub(" ","", genre)
genre <- gsub(",.*", "", genre)  # Remove white space and only take first genre
genre<-as.factor(genre)

# Scrape Film Ratings
rating.html <- html_nodes(webpage, '.ratings-imdb-rating strong')
rating <- html_text(rating.html)
rating <- as.numeric(rating)

# Scrape Number of Votes
votes.html <- html_nodes(webpage, ".sort-num_votes-visible span:nth-child(2)")
votes <- html_text(votes.html)
votes <- gsub(",", "", votes)
votes <- as.numeric(votes)

# Scrape the Directors
directors.html <- html_nodes(webpage, ".text-muted+ p a:nth-child(1)")
directors <- html_text(directors.html)
directors <- as.factor(directors)

# Scrape the Actors
actors.html <- html_nodes(webpage, ".lister-item-content .ghost+ a")
actors <- html_text(actors.html)
actors <- as.factor(actors)

data <- data.frame(rank = rank,
                   film = film,
                   description = description,
                   runtime = runtime,
                   genre = genre,
                   rating = rating,
                   votes = votes,
                   director = directors,
                   actor = actors)
