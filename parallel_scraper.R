if (!require("foreach")) install.packages("foreach")
library(foreach)
if (!require("doSNOW")) install.packages("doSNOW")
library(doSNOW)
if (!require("parallel")) install.packages("parallel")
library(parallel)


# Start our search with Chael P. Sonnen. Can replace this with ANY fighter's Sherdog link.
link = "/fighter/Chael-Sonnen-4112"


# Scrapes a single page and creates list
scrape <- function(link) {
  if (!require("rvest")) install.packages("rvest")
  library(rvest)
  if (!require("httr")) install.packages("httr")
  library(httr)
  if (!require("dplyr")) install.packages("dplyr")
  library(dplyr)
  if (!require("xml2")) install.packages("xml2")
  library(xml2)

  # Initialize scraper
  searched_links <- c()
  links_to_search <- c()
  fights_table <- data.frame(matrix(ncol=11,nrow=0))
  fn_nn <- list()
  
  
  
  no_errors = TRUE
  # read the webpage page of the fighter that we are interested in
  fighter_page <- tryCatch({
    read_html(paste0("http://www.sherdog.com/fighter",link))
  }, 
  error=function(cond) {
    message(cond)
    no_errors = FALSE
    return(fighter_page)
  },
  warning=function(cond) {
    message(cond)
    no_errors = FALSE
    return(fighter_page)
  })
  
  
  # Most pages have the fight history as the 3rd table on the page, although for some
  # it can be the 4th or 5th table. This checks which table is the fight history table.
  tbl_num <- 3
  loop = TRUE
  while (loop == TRUE) {
    error_check <- fighter_page %>%
      html_nodes(paste0("section:nth-child(",tbl_num,") h2")) %>%
      html_text()
    # Links to opponent pages
    if(!("Fight History - Pro" %in% error_check)){
      tbl_num <- tbl_num + 1
    } else {
      loop = FALSE
    }
  }
  
  # Opposing fighters' links
  fighter_links <- fighter_page %>%
    html_nodes(paste0("section:nth-child(",tbl_num,") td:nth-child(2) a")) %>%
    html_attr("href")  
  
  # Track Fighter name
  fighter_names <- fighter_page %>%
    # use CSS selector to extract relevant entries from html
    html_nodes(".nickname em , .fn") %>%
    # turn the html output into simple text fields
    html_text
  
  
  # Using our same fight page from before
  fighter_table <- fighter_page %>%
    # extract fight history
    html_nodes(paste0("section:nth-child(",tbl_num,") td")) %>%
    # not a well-behaved table so it is extracted as strings
    html_text() %>%
    # wrap text to reform table
    matrix(ncol = 6, byrow = T)
  
  
  # Add column names from first entries of table
  colnames(fighter_table) <- fighter_table[1,]
  fighter_table <- fighter_table[-1,, drop = F]
  fighter_table <- as.data.frame(fighter_table, stringsAsFactors = F)
  fighter_table$Fighter1 <- fighter_names[[1]]
  
  # Split Method/Referee columns
  fighter_table$Referee <- html_text(html_nodes(fighter_page, paste0("section:nth-child(",tbl_num,") td:nth-child(4) .sub_line"))) %>% na.omit()
  fighter_table$Method_d <- sapply(fighter_table$`Method/Referee`,function(x) gsub("N/A","",gsub('\\)(.*)','',gsub('^(.*)\\(','',x))))
  fighter_table$Method <- sapply(fighter_table$`Method/Referee`,function(x) gsub("N/A","",gsub('\\(.*','',x)))
  
  # Fix Date/Event column
  fighter_table$Date <- html_text(html_nodes(fighter_page, paste0("section:nth-child(",tbl_num,") td:nth-child(3) .sub_line"))) %>%
    as.Date(format="%B / %d / %Y") %>% na.omit()
  fighter_table$Event <- html_text(html_nodes(fighter_page, paste0("section:nth-child(",tbl_num,") td:nth-child(3) a"))) %>% na.omit()
  
  # Add both fighters' links
  fighter_table$Link1 <- link
  fighter_table$Link2 <- fighter_links
  
  fighter_table <- fighter_table %>%
    tbl_df() %>%
    # reorder
    select(Fighter1, Result, Fighter2=Fighter, Method, Method_d, R, Time, Referee, Event, Date, Link1, Link2)
  
  # Remove rows which are already in final table
  fighter_table <- subset(fighter_table, !(Link2 %in% searched_links))
  
  fights_table <- rbind(fights_table, fighter_table)
  
  if (no_errors == TRUE) {
    searched_links <- unique(append(searched_links, link))
    links_to_search <- fighter_links[fighter_links != "javascript:void();"]
  }
  
  return(list("fights" = fights_table,
              "searched" = data.frame(searched_links, "chael_Index" = 0),
              "toSearch" = links_to_search))
  
}

# Combines the scraped results
bind <- function(a, b) {
  fights_table = rbind(a[[1]], b[[1]])
  searched = rbind(a[[2]], b[[2]])
  toSearch = append(a[[3]], b[[3]]) %>% setdiff(searched$searched_links)
  
  return(list("fights"=fights_table, "searched"=searched, "toSearch"=toSearch))
}


# Initialize fights table
fights_table <- list("fights" = NULL,
                     "searched" = data.frame("searched_links" = NA, "chael_Index" = -1),
                     "toSearch" = link)


scrapeAll <- function (iterations) {
  for (i in 1:iterations) {
    print(system.time({
      pb <- txtProgressBar(max = length(fights_table[[3]]), style = 3)
      progress <- function(n) setTxtProgressBar(pb, n)
      opts <- list(progress = progress)
      fights_table2 <- foreach(link=fights_table[[3]], .combine='bind', .multicombine=TRUE, .maxcombine=2, .export=c("link", "scrape"), .options.snow = opts) %dopar% {
        scrape(link)
      }
      fights_table2$searched$chael_Index <- tail(fights_table$searched$chael_Index, 1) + 1
      fights_table <<- bind(fights_table, fights_table2)
      print(i)
    }))
  }
  fights_table$searched <<- fights_table$searched[-1,]
}

NumberOfCluster <- detectCores()
cl <- makeCluster(NumberOfCluster, outfile="log.txt")
registerDoSNOW(cl)
# Will run until all fighters to scrape are exausted.
# Should take about 2 seconds for first iteration, 10 seconds for second, 2 minutes for third, to upwards of 1.5 hours for some iterations. Should only be about 25 iterations.
# Script may stop from timing out. You can simply resume from where you left off.
while(length(fights_table$toSearch > 1)) {
  scrapeAll(1)
}
stopCluster(cl)
rm(NumberOfCluster)

