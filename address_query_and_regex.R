
library(tidyverse)
library(stringi)
library(jsonlite)
library(rvest)


### Our geocode() function

geocode <- function(name, address, city, state, zipcode){
    src_url <- "https://nominatim.openstreetmap.org/search?q="
    
    ## create a full address
    addr <- paste(address, city, state, zipcode, sep = "%2C")
    
    ## create a search url based on Nominatim API, and return geojson
    requests <- paste0(src_url, query, "&format=geojson")
    
    ## iterate over the urls and make request to the API 
    for (i in 1:length(requests)){
        
        ## query the API transform response from json to R list
        response <- read_html(requests[i]) %>% 
            html_node("p") %>%
            html_text() %>% 
            fromJSON()
        
        ## from the response extract lat and lon coordinates
        lon <- response$features$geometry$coordinates[[1]][1]
        lat <- response$features$geometry$coordinates[[1]][2]
        
        ## create coordinates data.frame
        if(i == 1){
            loc <- tibble(name = name[i],
                          address = str_replace_all(addr[i], "%2C", ","),
                          latitude = lat,
                          longitude = lon)
        }
        else{
            df <- tibble(name = name[i],
                         address = str_replace_all(addr[i], "%2C", ","), 
                         latitude = lat,
                         longitude = lon)
            loc <- bind_rows(loc, df)
        }
    }
    return(loc)
}


## The data
mydata <- read_csv("food_inspection_sample.csv")

## Cleaning the data
## Remove spaces from col names
colnames(mydata) <- str_replace_all(colnames(mydata), " ", "_")

## Remove spaces from results
mydata$Results <- mydata$Results %>%
    str_replace_all("w/", "with") %>%
    str_replace_all(" ", "_")

## Extract risk level
mydata$Risk <- tolower(mydata$Risk) %>% 
    str_extract("\\(.*\\)") %>% 
    str_extract("[a-z]+")

## Extract the address variable
myAdd <- mydata$Address
head(myAdd)

## Clean special cases
query <- str_replace_all(string = myAdd,
                         pattern = "BLDG",
                         replacement = " ")
## Clean special cases
query <- stri_replace(str = query,
                      replacement = " ",
                      regex = "(-[0-9] + \\s)")

## Replace spaces or commas, with plus sign
query <- str_replace_all(string = query,
                         pattern = "\\s|,",
                         replacement = "+")

head(query)

df <- geocode(name = mydata$DBA_Name,
              address = query,
              city = mydata$City,
              state = mydata$State,
              zipcode = mydata$Zip)


ls()



### Example with reg expressions
### 
ds <- data.frame(name = c("Doe, Mr. John", "Roe, Ms. Jane"), 
                 profession = c("Engineer", "Lawyer"), 
                 address = c("Insurgentes Sur 892", "Primavera 22"),
                 stringsAsFactors = FALSE)

nonsp <- "[[:alnum:][:punct:]]+"
sp <- "[[:blank:]]+"

newds <- ds %>% 
    mutate(title = stri_match_all_regex(
        name, nonsp %s+% sp %s+% "(" %s+% nonsp %s+% ")" %s+% sp %s+% nonsp) %>% 
            map_chr(2))

nums <- "[:digit:]"

newds <- newds %>% 
    mutate(add_num = stri_match_all_regex(
        address, nums) %>% map_chr(2))
