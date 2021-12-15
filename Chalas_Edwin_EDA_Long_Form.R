#Needed libraries
library(tidyverse)
library(readxl)

#Setting up the data

#the IMDb Data
#imdb_data <- read_tsv("data/unprocessed/title.basics.tsv")
#write_rds(imdb_data, "data/processed/imdb_data.rds")
imdb_data <- read_rds("data/processed/imdb_data.rds")

#the film gross data
#highest_grossing <- read_xlsx("data/unprocessed/highest.grossing.xlsx", sheet = 2, skip = 6, col_names = FALSE)
#highest_grossing <- highest_grossing %>%
#  rename(c("movie"="...1", "gross"="...2"))
#write_rds(highest_grossing, "data/processed/highest_grossing.rds")
highest_grossing <- read_rds("data/processed/highest_grossing.rds")

#separating the year and movie title
highest_grossing <- highest_grossing %>% 
  separate(movie, into = c("year", "movie"), sep = " - ")

#Removing 2020 and 1968 
#(1968 is based only on domestic box office data, so could be misleading in our results)
#along with filtering so that it's only films from 1959 to 2019
highest_grossing <- highest_grossing %>%
  filter(year != "****2020") %>%
  filter(year != "1968") %>%
  filter(year >= 1959) %>%
  filter(year <= 2019)

highest_grossing <- highest_grossing %>%
  mutate(year = as.double(year))

#Filtering only films and TV from 1959 to 2019
imdb_final_data <- imdb_data %>%
  filter(startYear >= 1959) %>%
  filter(startYear <= 2019)

#Dividing genres into one genre and one subgenre
imdb_final_data <- imdb_final_data %>% 
  separate(genres, into = c("genre", "subgenre"), sep = ",")

#Setting up the movie data
imdb_movies <- read_rds("data/processed/imdb_movies.rds")

#imdb_movies <- imdb_final_data %>%
#  filter(titleType == "movie") %>%
#  filter(genre != "\\N") %>%
#  group_by(genre) %>%
#  mutate(genre_count = sum(!is.na(genre))) %>%
#  mutate(subgenre_count = sum(!is.na(subgenre)))

#write_rds(imdb_movies, "imdb_movies.rds")
  
imdb_movies <- imdb_movies %>%
  group_by(startYear) %>%
  mutate(genre_year_count = sum(!is.na(genre))) %>%
  mutate(subgenre_year_count = sum(!is.na(subgenre)))

top_film <- inner_join(highest_grossing, imdb_movies, 
                       c("movie" = "primaryTitle", "year" = "startYear")) %>%
  select(-movie, year)

#Graphing top film genres and subgenres

imdb_movies %>%
  filter(rank(genre_count) > 1000) %>%
  mutate(genre = fct_reorder(genre, genre_count)) %>%
  ggplot() +
  geom_bar(aes(x = genre)) +
  labs(x = "Genres", y = "Number of films", title = "Top film genres from 1959-2019") +
  theme(axis.text.x = element_text(angle = 90))

imdb_movies %>%
  filter(rank(subgenre_count) > 1000) %>%
  filter(!is.na(subgenre)) %>%
  mutate(subgenre = fct_reorder(subgenre, subgenre_count)) %>%
  ggplot() +
  geom_bar(aes(x = subgenre)) +
  labs(x = "Subgenres", y = "Number of films", title = "Top film subgenres from 1959-2019") +
  theme(axis.text.x = element_text(angle = 90))


#Using count to rank genres
genres_over_time <- count(imdb_movies, genre, sort = TRUE) %>%
  group_by(startYear)
subgenres_over_time <- count(imdb_movies, subgenre, sort = TRUE) %>%
  filter(!is.na(subgenre)) %>%
  group_by(startYear)

top_films_over_time <- full_join(top_film, genres_over_time, 
                       c("year" = "startYear", "genre" = "genre")) %>%
  filter(!is.na(originalTitle))

top_subs_over_time <- full_join(top_film, subgenres_over_time, 
                                 c("year" = "startYear", "subgenre" = "subgenre")) %>%
  filter(!is.na(originalTitle))

#Graphing genres over time

top_genres <- pull(genres_over_time, genre)
top_genres <- as.factor(top_genres)

graph_genre_trend <- genres_over_time %>%
  fct_lump_n(f = top_genres, n = 10, w = genres_over_time$n)

ggplot() +
  geom_line(data = genres_over_time, mapping = aes(x = startYear, y = n, color = genre)) +
  geom_point(data = top_films_over_time, mapping = aes(x = year, y= n, color = genre)) +
  labs(x = "Year", y = "Number of films globally", title = "Trends in film genres globally from 1959-2019")

ggplot(subgenres_over_time) +
  geom_line(aes(x = startYear, y = n, color = subgenre)) +
  geom_point(data = top_subs_over_time, mapping = aes(x = year, y= n, color = subgenre)) +
  labs(x = "Year", y = "Number of films globally", title = "Trends in film subgenres globally from 1959-2019")


heatmap <- imdb_movies %>% mutate(startYear = as.character(startYear)) %>%
  mutate(startYear = fct_collapse(startYear,
 "2010s" = c("2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"), 
 "2000s" = c("2009", "2008", "2007", "2006", "2005", "2004", "2003", "2002", "2001", "2000"),
 "1990s" = c("1999", "1998", "1997", "1996", "1995", "1994", "1993", "1992", "1991", "1990"),
 "1980s" = c("1989", "1988", "1987", "1986", "1985", "1984", "1983", "1982", "1981", "1980"),
 "1970s" = c("1979", "1978", "1977", "1976", "1975", "1974", "1973", "1972", "1971", "1970"),
 "1960s" = c("1969", "1968", "1967", "1966", "1965", "1964", "1963", "1962", "1961", "1960")
  )) %>%
  select(genre, startYear) %>%
  filter(startYear != "1959")

heatmap <-count(heatmap, genre, sort = TRUE) %>%
  group_by(startYear) %>%
  mutate(prop = n / sum(n))

ggplot() +
  geom_tile(data = heatmap, mapping = aes(startYear, genre, fill = prop)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Decade", y = "Film genre", fill = "Proportion", title = "Proportion of genre vs. decade film output")

#Setting up TV data
imdb_tv <- read_rds("data/processed/imdb.tv.rds")

#imdb_tv <- imdb_final_data %>%
#  filter(titleType == "tvEpisode" | titleType == "tvMovie") %>%
#  filter(genre != "\\N")%>%
#  group_by(genre) %>%
#  mutate(genre_count = sum(!is.na(genre))) %>%
#  mutate(subgenre_count = sum(!is.na(subgenre)))

#write_rds(imdb_tv, "imdb.tv.rds")

#making a tv heatmap
tv_heatmap <- imdb_tv %>% mutate(startYear = as.character(startYear)) %>%
  mutate(startYear = fct_collapse(startYear,
                                  "2010s" = c("2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"), 
                                  "2000s" = c("2009", "2008", "2007", "2006", "2005", "2004", "2003", "2002", "2001", "2000"),
                                  "1990s" = c("1999", "1998", "1997", "1996", "1995", "1994", "1993", "1992", "1991", "1990"),
                                  "1980s" = c("1989", "1988", "1987", "1986", "1985", "1984", "1983", "1982", "1981", "1980"),
                                  "1970s" = c("1979", "1978", "1977", "1976", "1975", "1974", "1973", "1972", "1971", "1970"),
                                  "1960s" = c("1969", "1968", "1967", "1966", "1965", "1964", "1963", "1962", "1961", "1960")
  )) %>%
  filter(startYear != "1959")

tv_heatmap <- tv_heatmap %>% group_by(startYear) %>%
  count(genre, sort = TRUE) %>%
  mutate(prop = n / sum(n))

#Graphing top TV genres
imdb_tv %>%
  filter(rank(genre_count) > 1000) %>%
  mutate(genre = fct_reorder(genre, genre_count)) %>%
  ggplot() +
  geom_bar(aes(x = genre)) +
  labs(x = "Genres", y = "Number of TV episodes/movies", title = "Top TV genres from 1959-2019") +
  theme(axis.text.x = element_text(angle = 90))

#Using count to rank genres
tv_genres_over_time <- imdb_tv %>% group_by(startYear) %>% count(genre, sort = TRUE)
tv_subgenres_over_time <- imdb_tv %>% group_by(startYear) %>% count(subgenre, sort = TRUE) %>%
  filter(!is.na(subgenre))

#Graphing genres over time

ggplot(tv_genres_over_time) +
  geom_line(aes(x = startYear, y = n, color = genre)) +
  labs(x = "Year", y = "Number of episodes/movies globally", title = "Trends in TV genres globally from 1959-2019") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(tv_subgenres_over_time) +
  geom_line(aes(x = startYear, y = n, color = subgenre)) +
  labs(x = "Year", y = "Number of episodes/movies globally", title = "Trends in TV subgenres globally from 1959-2019") +
  theme(axis.text.x = element_text(angle = 90))

