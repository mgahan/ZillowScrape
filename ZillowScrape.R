

# Bring in libraries
library(rvest)
library(curl)
Sys.setenv("ZILLLOW_API_KEY"="key248012")


# URL_Build <- "https://www.zillow.com/homedetails/270-Brookview-Ct-Santee-CA-92071/16884697_zpid/?fullpage=true"
# zillowListings <- read_html(curl(URL_Build, handle = curl::new_handle("useragent" = "Mozilla/5.0")))
# zillowListings #hdp-price-history 
# zillowListings %>% 
#     html_nodes("#hdp-price-history")  %>%
#     html_table()

# Scrape IDs
ScrapeIDs <- function(PageNum) {
  
  # Build URL
  print(PageNum)
  Location <- "San-Diego-County-CA"
  House <- "house,townhouse_type"
  Bedrooms <- "2-_beds"
  Bathrooms <- "2-_baths"
  PriceRange <- "0-1000000_price"
  URL_Build <- paste0("https://www.zillow.com/homes/for_sale/",
                     Location,"/",
                     House,
                     "/2841_rid/",
                     Bedrooms,"/",
                     Bathrooms,"/",
                     PriceRange,"/",
                     PageNum,"_p/")

  # Scrape html data
  ResultDat <- tryCatch({
    zillowListings <- read_html(curl(URL_Build, handle = curl::new_handle("useragent" = "Mozilla/5.0")))
    #zillowListings <- read_html(URL_Build, options = c("RECOVER"))
    ListingIDs <- zillowListings %>% 
      html_nodes(".hdp-link") %>% 
      html_attr('href')
    ListingsDat <- as.data.table(ListingIDs)
    ListingsDat[, PageNum := PageNum]
  }, error = function(e) {
    ListingsDat <- data.table(PageNum=PageNum)
  })

  
  # Format table
  #ListingsDat[, paste0("V",1:4) := tstrsplit(ListingIDs,"/")]
  
  return(ResultDat[])
}

# Loop through pages
allDatList <- lapply(1:20, ScrapeIDs)
allDat <- rbindlist(allDatList, fill=TRUE)
allDat[, PageCount := .N, keyby=.(PageNum)]
allDat[PageCount==1]

################################## GetUpdatedPropertyDetails ##################################
KeyURL <- paste0("http://www.zillow.com/webservice/GetUpdatedPropertyDetails.htm?zws-id=",Sys.getenv("ZILLLOW_API_KEY"),"&zpid=16884697")
tmp <- GET(url=KeyURL)
tmp2 <- content(tmp, "text")
keyvars <- c("</?zpid>","</?pageViewCount>","</?latitude>","</?longitude>",
             "</?status>","</?lastUpdatedDate>","</?bedrooms>",
             "</?bathrooms>","</?finishedSqFt>","</?lotSizeSqFt>","</?yearBuilt>",
             "</?parkingType>","</?heatingSystem>","</?coolingSystem>",
             "</?appliances>","</?floorCovering>","<?/rooms>","<?price>?",
             "</?homeDescription>","</?neighborhood>","</?schoolDistrict>",
             "</?elementarySchool>","</?middleSchool>")
scrapeVar <- function(x) {
  check1 <- tstrsplit(tmp2,x, fixed=FALSE)
  if (length(check1)==1) {
    check1Dat <- data.table(Var=x, Value=NA_character_)
  } else {
    check1 <- check1[2]
    check1Dat <- data.table(Var=x, Value=sapply(check1, I))
  }
  return(check1Dat[])
}
allDat <- rbindlist(lapply(keyvars, scrapeVar), fill=TRUE)
allDat[, Type := "GetUpdatedPropertyDetails"]
#########################################################################################################

################################## GetDeepSearchResults ################################## 
KeyURL <- paste0("http://www.zillow.com/webservice/GetDeepSearchResults.htm?zws-id=",Sys.getenv("ZILLLOW_API_KEY"),"&zpid=16884697&address=270+Brookview+Ct&citystatezip=Santee%2C+CA")
tmp <- GET(url=KeyURL)
tmp2 <- content(tmp, "text")
keyvars <- c("</?zpid>","</?taxAssessment>","lastSoldDate>","<?lastSoldPrice>?",
             "</?zindexValue>","</?valuationRange>")
scrapeVar <- function(x) {
  check1 <- tstrsplit(tmp2,x, fixed=FALSE)
  if (length(check1)==1) {
    check1Dat <- data.table(Var=x, Value=NA_character_)
  } else {
    check1 <- check1[2]
    check1Dat <- data.table(Var=x, Value=sapply(check1, I))
  }
  return(check1Dat[])
}
allDat <- rbindlist(lapply(keyvars, scrapeVar), fill=TRUE)
allDat[, Type := "GetDeepSearchResults"]

########################################################################################################

################################## GetZestimate. ##################################
KeyURL <- paste0("http://www.zillow.com/webservice/GetZestimate.htm?zws-id=",Sys.getenv("ZILLLOW_API_KEY"),"&zpid=16884697")
tmp <- GET(url=KeyURL)
tmp2 <- content(tmp, "text")
keyvars <- c("</?zpid>","<?amount>?")
scrapeVar <- function(x) {
  check1 <- tstrsplit(tmp2,x, fixed=FALSE)
  if (length(check1)==1) {
    check1Dat <- data.table(Var=x, Value=NA_character_)
  } else {
    check1 <- check1[2]
    check1Dat <- data.table(Var=x, Value=sapply(check1, I))
  }
  return(check1Dat[])
}
allDat <- rbindlist(lapply(keyvars, scrapeVar), fill=TRUE)
allDat[, Type := "GetZestimate"]

#########################################################################################################

tstrsplit(tmp2,"</")
