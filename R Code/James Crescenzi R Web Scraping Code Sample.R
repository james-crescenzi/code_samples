## Task
## Collect the abstracts for all articles that were published
## in the American Journal of Political Science between 2003 and 2019
## Build a database containing the following pieces of information:

## - Title
## - Author name
## - Author affiliation
## - Publication volume
## - Publication issue
## - Publication month
## - Publication year
## - Page range
## - Abstract

## https://onlinelibrary.wiley.com/loi/15405907/year/2014

library(stringr)
library(httr)
library(readr)
library(rvest)
library(curl)

year <- 2010:2019
volume <- 47:64

for(i in 1:length(year)){
  for(j in 1:4){



      url <- str_c(
      "https://onlinelibrary.wiley.com/toc/15405907/",
      year[i],
      "/",
      volume[i],
      "/",
      j
      )
      
      Sys.sleep(abs(rnorm(1, 1)))

      html <- read_html(url)

      write_html(
          html, 
           file = str_c(
           "~/webScraping/AJPS2010-2019/volume_",
           volume[i],
           "_issue_",
           j,
           ".html"
           )
)
  }
}
           

FOLDER <- "~/webScraping/AJPS2002-2020/"

files <- list.files(FOLDER)

INDEX <- 1

for(i in 1:40){
  

  html <- read_html(
    str_c(
      FOLDER,
      files[i]
    )
  )
  
  nodes <- html_nodes(html, xpath = "//a[@title='Abstract']")
  
  links <- html_attr(nodes, name = "href")
  
  for(j in 1:length(links)){
    
    url <- str_c(
      "https://onlinelibrary.wiley.com",
      links[j]
    )
    
    #read_html(curl('http://google.com', handle = curl::new_handle("useragent" = "Mozilla/5.0")))
    html_article <- read_html(curl(url), handle = curl::new_handle("useragent" = "Mozilla/5.0"))
    
    write_html(
      html_article,
      file = str_c(
        "~/webScraping/abstracts/",
        INDEX,
        ".html"
      )
    )
    
    INDEX <- INDEX + 1
    
    Sys.sleep(abs(rnorm(1,1)))
    
  }
}

## - Title
## - Author name
## - Author affiliation
## - Publication volume
## - Publication issue
## - Publication month
## - Publication year
## - Page range
## - Abstract

dat <- data.frame(
  title = character(),
  author = character(),
  author_affiliation = character(),
  abstract = character(),
  publication_volume = numeric(),
  publication_issue = numeric(),
  publication_month = character(),
  publication_year = numeric(),
  page_range = character()
)

for(i in 1:642){

  html <- read_html(
    str_c(
      "~/webScraping/ajps_articles/",
      i,
      ".html"
    )
  )
  
  # Title
  
  title_node <- html_nodes(
    html,
    xpath = "//h1[@class='citation__title']"
    )
  
  title <- html_text(title_node)
 # title2 = title
  
  # Author
  
  author_node <- html_nodes(
    html,
    xpath = "//a[@class='author-name accordion-tabbed__control']"
  )
  
  author <- html_text(author_node)
  
  author_selection <- !duplicated(author)
  
  a_multi <- author[!duplicated(author)]
  
  author <- a_multi[1]
  
  if(!is.na(a_multi[2]) == TRUE){
    for(h in 2:length(a_multi)){
    author = str_c(author, ", ",a_multi[h])
    }
}
  #author2 = author
  # Author Affiliation
  
  affiliation_node <- html_nodes(
    html,
    xpath = "//div[@class='author-info accordion-tabbed__content']/p"
  )
  affiliationtwo <- html_text(affiliation_node)
  author_affiliation_multi_raw <- NULL
  for(y in 1:length(affiliationtwo)/2){
  author_affiliation_multi_raw[y] <- affiliationtwo[y]
}
  #author_affiliation_multi <- affiliation[author_selection]
  
  affiliation_index <- 1
  for(r in 1:length(author_affiliation_multi_raw)){
    if(!is.na(str_extract(author_affiliation_multi_raw[r], "\n"))
       || !is.na(str_extract(author_affiliation_multi_raw[r], "E-mail address:"))
       || !is.na(str_extract(author_affiliation_multi_raw[r], "Corresponding Author"))){
      author_affiliation_multi[r] <- author_affiliation_multi_raw[r]
    }
    affiliation_index++
  }
  author_affiliation <- author_affiliation_multi[1]
  
  if(!is.na(author_affiliation_multi[2]) == TRUE){
    for(m in 2:length(author_affiliation_multi)){
      author_affiliation = str_c(author_affiliation, ", ",author_affiliation_multi[m])
    }
  }
  
  #author_affiliation2 = author_affiliation
  # Abstract
  
  abstract_node <- html_nodes(
    html,
    xpath = "//div[@class='article-section__content en main']"
  )
  
  abstract <- html_text(abstract_node)
  abstract <- str_trim(abstract)

  # Publication volume
  
  publication_volume_node <- html_nodes(
    html,
    xpath = "//p[@class='volume-issue']"
  )
  
  publication_raw <- html_text(publication_volume_node)
  
  publication <- strsplit(publication_raw, ",")
  
  publication_volume <- parse_number(publication[[1]][1])
  
  # Publication issue
  
  publication_issue <- parse_number(publication[[1]][2])
  
  ## - Publication month
  
  detail_node <- html_nodes(
    html,
    xpath = "//div[@class='extra-info-wrapper cover-image__details']/p"
  )
  
  date_node <- detail_node[2]
  
  date_node <- html_text(date_node)
  
  date_node <- as.character(date_node)
  
  date_node <- strsplit(date_node, " ")
  
  publication_month <- date_node[[1]][1]
  
  ## - Publication year
  
  publication_year <- parse_number(date_node[[1]][2])
  
  ## - Page range
  
  page_range_node <- detail_node[3]
  
  page_range <- html_text(page_range_node)
 
  
  dat <- rbind(
   dat,
    data.frame(
      title <- title,
      author <- author,
      author_affiliation <- author_affiliation,
      abstract <- abstract,
      publication_volume <- publication_volume,
      publication_issue <- publication_issue,
      publication_month <- publication_month,
      publication_year <- publication_year,
      page_range <- page_range
    )
  )
  }


