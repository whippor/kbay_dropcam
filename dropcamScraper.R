
# Load required libraries 
library(rvest) 
library(httr) 

# URL of the webpage with video links 
url <- "https://products.coastalscience.noaa.gov/biomapper_explorer/index.php?path=K25VVzRCUlNwSDdJcDZvQ3FBNnFGUT09&uri=YmdISDNIakc0a2poU1hsNFkwZ280Tll0aVZVZ0VNV01tc1lHaCtmZDR1N1NzTW9sVlJ5UmtHVHB5YUhnYVA3YysrOGFhWDVqcW5hN3ZMakFKRDJKRWpqcHJuT01LekVOZUdZVkNLekpxc1k9&type=RXJFNEROUHZHaEppZEhNd2VhRnFHUT09" 

# Create a directory on the Windows 10 desktop to store the videos 
download_dir <-
  file.path(Sys.getenv("USERPROFILE"), "Desktop", "VideoDownloads")
dir.create(download_dir, showWarnings = FALSE, recursive = TRUE) 

# Function to download a video 
download_video <- 
  function(video_url, download_dir, index) { 
  response <- GET(video_url) 
  if (http_type(response) == "video/mp4") { 
    video_name <-
      paste0("video_", index, ".mp4")
    download_path <- file.path(download_dir, video_name)
    writeBin(content(response), download_path) 
    cat("Downloaded:", video_name, "\n") 
    } else { 
      cat("Skipped non-video link:", video_url, "\n") 
      } 
  } 

# Scrape video links from the webpage 
page <- read_html(url) 
video_links <- page %>%
  html_nodes("a[href^='https://products.coastalscience.noaa.gov/biomapper_explorer/index.php?path=']") %>%
  html_attr("href")

for (i in 1:length(video_links)) {
  video_url <-
    URLencode(video_links[i])
  download_video(video_url, download_dir, i)
}

cat("All downloads completed.]n")






# Rename files
library(rvest)
library(stringr)
library(tools)

# scrape titles
scrape_titles <- function(url) {
  page <- read_html(url)
  titles <- page %>%
    html_nodes(xpath = "//a[contains(@title, 'KBAY')]") %>%
    html_attr("title")
  return(titles)
}

# URL of the webpage with video links 
url <- "https://products.coastalscience.noaa.gov/biomapper_explorer/index.php?path=K25VVzRCUlNwSDdJcDZvQ3FBNnFGUT09&uri=YmdISDNIakc0a2poU1hsNFkwZ280Tll0aVZVZ0VNV01tc1lHaCtmZDR1N1NzTW9sVlJ5UmtHVHB5YUhnYVA3YysrOGFhWDVqcW5hN3ZMakFKRDJKRWpqcHJuT01LekVOZUdZVkNLekpxc1k9&type=RXJFNEROUHZHaEppZEhNd2VhRnFHUT09" 

# call function to get the titles
titles <- scrape_titles(url)

# specify folder path
folder_path <- "C:/Users/Ross.Whippo/Desktop/VideoDownloads"

# list all files in folder
list.files(path = folder_path, full.names = TRUE)

# rename the files
if(length(titles) > 0) {
  for (i in seq_along(files)) {
    new_name <- 
      str_replace_all(titles[i], "[^alnum:]]", "_")
    new_name <- tolower(new_name)
    new_name <- substr(new_name, 1, 50)
    new_name <- sprintf("%s%s", new_name, file_ext(files[i]))
    new_path <- 
      file.path(folder_path, new_name)
                file.rename(files[i], new_path)
                cat("Renamed:", files [i], "to", new_name, "\n")
                }
  } else {
    cat("No titles found on the webpage.\n")
  }
    

