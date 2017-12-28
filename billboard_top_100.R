library(httr)
library(Rspotify)
library(jsonlite)
library(anytime)
library(dplyr)
library(ggplot2)
library(reshape)

# Load functions---------
add_artists_info <- function(artist_ids){
  df <- data.frame()
  for (i in artist_ids){
    aristst_df <- getArtistinfo(i,token=keys)
    
    df <- rbind(df, aristst_df)
  }
  df
}

TopArtistDiscography <- function(artist_ids, token){
  df <- data.frame()
  for (i in artist_ids) {
    info <- getDiscographyInfo(i,token=token)
    df <- rbind(df, info)
    print(unique(info$artist))
  }
  df
}

getDiscographyInfo <- function(id,token){
  artist_info <- getAlbums(id,token=token)
  ids <- artist_info$id
  df <- data.frame()
  for (i in ids){
    info <- getAlbumInfo(i, token=keys)
    df <- rbind(df, info)
  }
  df
}

getAlbumInfo<-function(id,token){
  req<-httr::GET(paste0("https://api.spotify.com/v1/albums/",id),httr::config(token = token))
  json1<-httr::content(req)
  json2<-jsonlite::fromJSON(jsonlite::toJSON(json1))
  
  df <- data.frame("id" = json2$id, "artist" = as.character(json2$artists$name),"name" = json2$name, "label" = json2$label, "popularity" = json2$popularity, "release_date" = json2$release_date, "release_date_precision" = json2$release_date_precision, "album_type" = json2$album_type, "track_total" = json2$tracks$total)
  df
}

RemoveAllDubAlbums <- function(top_df){
  artists <- unique(top_df$artist)
  
  rm_dubs <- data.frame()
  for (i in artists){
    df <- filter(top_df, artist==i)
    df <- RemoveDubAlbums(df)
    df <- as.data.frame(df)
    rm_dubs <- rbind(rm_dubs, df)
  }
  rm_dubs
}

RemoveDubAlbums <- function(artist_df){
  artist_df$release_date <- as.Date(artist_df$release_date)
  
  df <- artist_df
  df <-
    artist_df %>%
    group_by(release_date, popularity) %>%
    arrange(desc(popularity))
  
  df = df[!duplicated(df$name),]
  df = df[!duplicated(df$release_date),]
  df
}

add_reg_slope <- function(df){
  artist_names <- unique(df$artist)
  df3 <- data.frame()
  
  for(i in artist_names){
    df1 <- filter(df, artist == i)
    model <- lm(df1$popularity ~ df1$release_date)
    slope <- model$coefficients[2] 
    df1$slope <- slope
    df3 <- rbind(df3, df1)
  }
  df3
}
# Load Local Data and API keys ---------
source("keys.r")
call_service(app_id, client_id, client_secret)
keys <- spotifyOAuth(app_id,client_id,client_secret)

billboard_top_100_info <- read.csv("billboard_100_info.csv")
original_billboard <- billboard_top_100_info

# API calls---------
# Get artists info from API
billboard_top_100_artist_info <- add_artists_info(billboard_top_100_info$id)

# Create master discogrphy for each artists split into 4 parts then join
# Missing 5 artists "Elvis Presley" "The Rolling Stones" "The Beach Boys" "James Brown" "JAY Z"             
# billboard_top_100_df <- TopArtistDiscography(billboard_top_100_artist_info$id,token=keys)
a <- TopArtistDiscography(billboard_top_100_artist_info$id[1:25],token=keys)
b <- TopArtistDiscography(billboard_top_100_artist_info$id[26:50],token=keys)
c <- TopArtistDiscography(billboard_top_100_artist_info$id[51:75],token=keys)
d <- TopArtistDiscography(billboard_top_100_artist_info$id[76:100],token=keys)
billboard_top_100_df <- do.call("rbind", list(a, b, c, d))
rm(a, b, c, d)

# Clean data---------
# Remove random artists
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Otto Von Wernherr", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Leon Russell", ]               
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Academy of St. Martin in the Fields", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Gavin Greenaway", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "London Symphony Orchestra", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Loma Mar Quartet", ]                   
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Lawrence Foster", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Andrea Quinn", ]                 
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Wings", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Linda McCartney", ] 
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "3RDEYEGIRL", ]                         
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "John Farnham", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Beth Nielsen Chapman", ]               
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Amy Sky", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "John Travolta", ]                      
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "King Curtis", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Tammi Terrell", ]                      
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Mary Wells", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Hyung-ki Joo", ]                       
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Plácido Domingo", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "José Carreras", ]                      
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Blinky Williams", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Michel Legrand", ]                     
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Smokey Robinson & The Miracles", ]  
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Dolly Parton", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Happy Singers", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Die Happy Singers", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Kris Kristofferson", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Hans Zimmer", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Sonny & Cher", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "George Burns", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Miami Sound Machine", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Milt Jackson", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Tyga", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Dawn", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "The Dawn", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Tony Bennett", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Royal Philharmonic Orchestra", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Emmylou Harris", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Mitchell Owens", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "DJ Whoo Kid", ]
billboard_top_100_df <- billboard_top_100_df[billboard_top_100_df$artist != "Diana Ross & The Supremes", ]

# Check for different artists
setdiff(billboard_top_100_artist_info$name, billboard_top_100_df$artist)
setdiff(billboard_top_100_df$artist, billboard_top_100_artist_info$name)

# Remove duplicate albums keeping more popular 
billboard_top_100_df$release_date <- anydate(billboard_top_100_df$release_date)
# billboard_top_100_df$release_date <- as.Date(billboard_top_100_df$release_date)
billboard_top_100_df <- RemoveAllDubAlbums(billboard_top_100_df)
billboard_top_100_df <- na.omit(billboard_top_100_df)

# Remove Live albums
df <- billboard_top_100_df
toMatch <- c("Live At", "Live at", "Live In", "Live in", "Live Version", " Live: ", "Live from", " (Live) ", '1 (Remastered)')
df <- df[grep(paste(toMatch,collapse="|"), df$name) , ]
billboard_top_100_df <- billboard_top_100_df[!(billboard_top_100_df$name %in% df$name),]
rm(toMatch, df)

original_billboard$id <- as.character(original_billboard$id)
top95 <- billboard_top_100_df
top95$id <- as.character(top95$id)
top95 <- left_join(original_billboard, top95, by=c("id"="id"))
top95<-na.omit(top95)

# Calculate artist trajectory and other new varibles---------
# Add slope to the data frame
billboard_top_100_df <- add_reg_slope(billboard_top_100_df)
top95 <- unique(data.frame(name = billboard_top_100_df$artist, slope = billboard_top_100_df$slope))
top95$name <- as.character(top95$name)
billboard_top_100_artist_info$name <- as.character(billboard_top_100_artist_info$name)
top95 <- left_join(top95, billboard_top_100_artist_info,  by = c("name" = "name"))
rm(df)


# Create career length
career <-
  billboard_top_100_df %>%
    group_by(artist) %>%
    summarise(career_length = as.numeric(max(release_date) - min(release_date)) / 365,
              first_album = min(release_date), 
              latest_album = max(release_date))

career$artist <- as.character(career$artist)
top95 <- left_join(top95, career, by = c("name" = "artist"))
top95$pos_neg <- ifelse(top95$slope > 0,"Positive", "Negative")  
top95$career_bin <- ifelse(top95$career_length < 10,"Under 10",NA)  
top95$career_bin <- ifelse(top95$career_length > 10 && top95$career_length < 20,"10 - 20", '')  

career_if_fun <- function(x){
  if (x < 10) {
    '10 or Less'
  } else if (x >= 10 && x < 20) {
    '10 to 20'
  } else if (x >= 20 && x < 30) {
    '20 to 30'
  } else if (x >= 30 && x < 40) {
    '30 to 40'
  } else if (x >= 40 && x < 50) {
    '40 to 50'
  } else {
    '50 Plus'
  }
}

top95$career_bin <- sapply(top95$career_length,career_if_fun)

# add album count
df <- as.data.frame(table(billboard_top_100_df$artist))
df <- df[df$Freq != 0, ]
df$Var1 <- as.character(df$Var1)
top95 <- left_join(top95, df, by = c("name" = "Var1"))
top95 <- rename(top95, c("Freq"="album_count"))

# Create artist slope df
df <- top95[order(top95$slope), ]
df$name <- factor(df$name, levels = df$name)  # convert to factor to retain sorted order in plot.

artists_slopes <-
  as.data.frame(unique(
    billboard_top_100_df %>%
      select(artist, slope) %>%
      group_by(artist, slope) %>%
      arrange(desc(slope))))

# Create Plots---------
df <- top95
df <- df[order(df$slope), ]
df$name <- factor(df$name, levels = df$name) 
all_artists_bar <-
  ggplot(df, aes(x=name, y=slope, label=slope)) + 
    geom_bar(stat='identity', aes(fill=pos_neg), width=.5)  +
    coord_flip() +
    theme(legend.position="none") +
    labs(title = 'All Artists',
         x = 'Artist',
         y = 'Slope')

slope_hist <-
  ggplot(top95) +
    geom_histogram( aes(slope, fill=pos_neg)) +
    theme(legend.position="none") +
    labs(title = 'Slope Histogram',
         x = 'Slope',
         y = 'Count of Artists')

slope_bar <-
  ggplot(top95, aes(pos_neg, fill=pos_neg)) +
    coord_flip() +
    geom_bar() +
    theme(legend.position="none") +
    labs(title = 'Count of Slopes',
         y = 'Count of Artists',
         x = '')

slope_bar_year <-
  ggplot(top95, aes(pos_neg, fill=pos_neg)) + 
  geom_bar() +
  facet_grid(.~career_bin) +
  theme(legend.position="none") +
  labs(title = 'Count of Slopes by Career Length',
       y = 'Count',
       x = '') 

album_count_slope <-
  ggplot(top95, aes(y=slope,x=album_count)) +
    geom_point(aes(col=pos_neg)) +
    geom_smooth(se=FALSE,method = "lm", formula = (y~x)) +
    theme(legend.position="none") +
    labs(title = 'Number of Albums vs Slope',
         y = 'Slope',
         x = 'Number of Albums')

billboard_rank <-
  ggplot(top95, aes(x=billboard_rank,y=slope)) +
    geom_point(aes(col=pos_neg)) +
    geom_smooth(se=FALSE,method = "lm", formula = (y~x)) +
    theme(legend.position="none") +
    labs(title = 'Billboard Rank vs Slope',
       y = 'Slope',
       x = 'Billboard Rank')

# Save objects
save.image("billboard_objects.RData")
