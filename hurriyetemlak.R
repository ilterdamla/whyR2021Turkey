rm(list = ls())
options(scipen = 999)

library(rvest)
library(tidyverse)

pages <- seq(1, 100, 1)

url <-
  str_c("https://www.hurriyetemlak.com/sile-satilik/daire?page=", #url deðiþecek
        pages)

urls <- data.frame()

for(i in 1:length(url)){
  
  skip_to_next <- FALSE
  
  tryCatch(
    
    expr = {
      
      download.file(url[i], destfile = "scrapedpage.html", method="curl", quiet=TRUE)
      content <- read_html("scrapedpage.html")
      tblurls <- content %>% html_nodes("div.links a") %>% html_attr("href") %>% as.data.frame()
      urls <- urls %>% bind_rows(tblurls)
      
    },
    
    error = function(e){
      
      skip_to_next <<- TRUE
      
    }
    
  )
  
  if(skip_to_next){
    
    next
    
  }
  
  Sys.sleep(time = 3)
  print(paste0(i,". iþlem bitti..."))
  
}

urls <- urls %>%
  rename("urls" = 1) %>% 
  mutate(urls = ifelse(
    grepl("https://www.hurriyetemlak.com", urls),
    urls,
    paste0("https://www.hurriyetemlak.com", urls)
  ))

####################################################

resultdf <- data.frame(matrix("", nrow = nrow(urls), ncol = 3)) %>% 
  rename(
    "fiyat" = 1,
    "detay" = 2,
    "emlak" = 3
  )

for(i in 1:nrow(urls)){
  
  skip_to_next <- FALSE
  
  tryCatch(
    
    expr = {
      
      #download.file(urls[i,1], destfile = "scrapedpage.html", method="wininet", quiet=TRUE)
      #content <- read_html("scrapedpage.html")
      content <- read_html(as.character(urls$urls[i]))
      resultdf[i,1] <- content %>% html_nodes("div.right p") %>% html_text()
      resultdf[i,2] <- content %>% html_nodes("div.det-adv-info") %>% html_text()
      resultdf[i,3] <- content %>% html_nodes("div.firm-link") %>% html_text() %>% .[2]
      
    },
    
    error = function(e){
      
      skip_to_next <<- TRUE
      
    }
    
  )
  
  if(skip_to_next){
    
    next
    
  }
  
  Sys.sleep(time = 3)
  openxlsx::write.xlsx(resultdf, "sile.xlsx") #burasý deðiþecek!
  print(paste0(i,". iþlem bitti..."))
  
}

resultdf <- resultdf %>% 
  filter(fiyat != "") %>% 
  na.omit() #boþ gelenleri gönder

openxlsx::write.xlsx(resultdf, "sile.xlsx") #kaydet
