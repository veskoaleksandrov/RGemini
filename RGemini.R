oauth2.0_refresh <- function(endpoint, app, access_token, type = NULL) {
  
  if (!require("httr")) install.packages("httr", repos = "https://cloud.r-project.org/")
  stopifnot(library(httr, logical.return = TRUE))
  
  # endpoint <- yahoo
  # app <- myapp
  # access_token <- refresh_token
  # type <- NULL
  
  req <- POST(url = endpoint$access,
              body = list(client_id = app$key,
                          client_secret = app$secret,
                          grant_type = "refresh_token",
                          refresh_token = access_token$refresh_token, 
                          redirect_uri = "oob"), 
              encode = "form")
  
  stopifnot(content(req, type = type)$refresh_token == access_token$refresh_token)
  return(content(req, type = type))
}
geminiRequestReport <- function(advertiserId, from, to, endpoint, app, access_token) {
  
  if (!require("httr")) install.packages("httr", repos = "https://cloud.r-project.org/")
  stopifnot(library(httr, logical.return = TRUE))
  
  # advertiserId <- 1082866
  # from <- "2018-05-01"
  # to <- "2018-05-01"
  # endpoint <- yahoo
  # app <- myapp
  # access_token <- refresh_token$access_token
  
  # define the report
  reportDefinition <- list(cube = "search_stats", 
                           fields = data.frame(field = c("Advertiser ID", "Campaign ID", "Ad Group ID", "Ad ID", "Keyword ID", 
                                                         "Advertiser Name", "Campaign Name", "Ad Group Name", "Ad Title", "Keyword Value", 
                                                         "Advertiser Timezone", "Advertiser Currency", "Ad Landing URL", "Keyword Match Type", 
                                                         "Source", "Price Type", "Day", 
                                                         "Impressions", "Clicks", "Spend", "Average Position", "Conversions"), 
                                               stringsAsFactors = FALSE), 
                           filters = data.frame(field = c("Advertiser ID", "Day"), 
                                                operator = c("=", "between"), 
                                                value = c(advertiserId, NA), 
                                                from = c(NA, from), 
                                                to = c(NA, to), 
                                                stringsAsFactors = FALSE))
  
  # request report
  url <- "https://api.gemini.yahoo.com/v3/rest/reports/custom"
  body <- reportDefinition
  req <- POST(url = url, 
              config = add_headers(c("Authorization" = paste0("Bearer ", access_token))), 
              body = body, 
              encode = "json")
  
  return(content(req))
}
geminiGetReportStatus <- function(jobId, advertiserId, access_token) {
  
  if (!require("httr")) install.packages("httr", repos = "https://cloud.r-project.org/")
  stopifnot(library(httr, logical.return = TRUE))
  
  # get report status
  url <- "https://api.gemini.yahoo.com/v3/rest/reports/custom/"
  url <- parse_url(url)
  url$path <- paste0(url$path, jobId)
  url <- modify_url(url = url, query = c(paste0("advertiserId=", advertiserId)))
  report_status <- GET(url = url, 
                       config = add_headers(c("Authorization" = paste0("Bearer ", access_token))))
  report_status <- content(report_status)
}
geminiGetAuthorizationCode <- function(endpoint, app) {
  
  if (!require("httr")) install.packages("httr", repos = "https://cloud.r-project.org/")
  stopifnot(library(httr, logical.return = TRUE))
  
  # 
  # Interactive environment is required. 
  # 
  # 1. Get an authorization URL and authorize access
  # 2. Redirect to Yahoo where the user can authorize access
  # 3. Once the user authorizes access, Yahoo generates an authorization code. 
  #    Hardcode the code value in configuration section. 
  # 
  
  oauth2.0_token(endpoint, 
                 app, 
                 user_params = c(response_type = "code"))
}
geminiGetAdvertiser <- function(access_token) {
  
  if (!require("httr")) install.packages("httr", repos = "https://cloud.r-project.org/")
  stopifnot(library(httr, logical.return = TRUE))
  if (!require("jsonlite")) install.packages("jsonlite", repos = "https://cloud.r-project.org/")
  stopifnot(library(jsonlite, logical.return = TRUE))
  
  # get advertisers
  url <- parse_url("https://api.gemini.yahoo.com/v3/rest/advertiser/")
  req <- GET(url = url, 
             config = add_headers("Content-Type" = "application/json", 
                                  "Authorization" = paste0("Bearer ", access_token$access_token)))
  
  return(fromJSON(content(req, "text"))$response)
}
geminiGetRefreshToken <- function(endpoint, app, code) {
  
  if (!require("httr")) install.packages("httr", repos = "https://cloud.r-project.org/")
  stopifnot(library(httr, logical.return = TRUE))
  
  # get refresh token 
  return(oauth2.0_access_token(endpoint, app, code))
}

if (!require("httr")) install.packages("httr", repos = "https://cloud.r-project.org/")
stopifnot(library(httr, logical.return = TRUE))
if (!require("plyr")) install.packages("plyr", repos = "https://cloud.r-project.org/")
stopifnot(library(plyr, logical.return = TRUE))

from <- "2017-04-01"
from <- as.character(input$Max_Day)
to <- as.character(Sys.Date() - 1)
setwd("c:/Users/alteryx/Desktop/Alteryx Projects/Yahoo/")

if(from >= to) {
  stop("'from' is greater than or equal to 'to'")
}


#----------------------- Configuration section ----------------------------------------------------
# 
# 1. Find OAuth settings for yahoo:
# Endpoints described here... 
# https://developer.yahoo.com/oauth2/guide/flows_authcode/
yahoo <- oauth_endpoint(request = "get_token",                       
                        authorize = "request_auth", 
                        access = "get_token",
                        base_url = "https://api.login.yahoo.com/oauth2")

# 2. Register an application at...
# https://developer.apps.yahoo.com/dashboard/createKey.html
# Insert your values below - if secret is omitted, it will look it up in
#    the YAHOO_CONSUMER_SECRET environmental variable.
myapp <- list(lulu = oauth_app(appname = "yahoo", 
                               key = "", 
                               secret = "", 
                               redirect_uri = "oob"), 
              ib = oauth_app(appname = "yahoo", 
                             key = "", 
                             secret = "", 
                             redirect_uri = "oob"))

# keep authorization codes updated
authorizationCode <- list(lulu = "gsec6ja", 
                          ib = "bepu7w4")

refresh_token <- list(lulu = readRDS(file = "C:/Users/alteryx/Desktop/Alteryx Projects/Yahoo/Authorisation Tokens/gemini-lulu.rds"), 
                      ib = readRDS(file = "C:/Users/alteryx/Desktop/Alteryx Projects/Yahoo/Authorisation Tokens/gemini-ib.rds"))

report <- NULL
for(t in 1:length(refresh_token)) {
  
  print(paste0(Sys.time(), " - Token ", t, ": ", refresh_token[[t]]))
  
  # refresh stale tokens
  my_token <- oauth2.0_refresh(endpoint = yahoo, 
                               app = myapp[[t]], 
                               access_token = refresh_token[[t]])
  
  # get advertiser 
  advertiser <- geminiGetAdvertiser(access_token = my_token)
  
  # loop through each advertiser
  for(a in 1:nrow(advertiser)) {
    print(paste0(Sys.time(), " - Advertizer: ", advertiser$advertiserName[a]))
    requestReport <- geminiRequestReport(advertiserId = advertiser$id[a],
                                         from = from, 
                                         to = to, 
                                         endpoint = yahoo, 
                                         app = myapp[[t]], 
                                         access_token = my_token$access_token)
    print(paste0(Sys.time(), " - Job ID: ", requestReport$response$jobId))
    
    repeat({
      statusReport <- geminiGetReportStatus(jobId = requestReport$response$jobId, 
                                            advertiserId = advertiser$id[a], 
                                            access_token = my_token$access_token)
      print(paste0(Sys.time(), " - Report status: ", statusReport$response$status))
      
      if(statusReport$response$status == "completed") {
        break()
      }
      
      Sys.sleep(15)
    })
    
    print(paste0(Sys.time(), " - Download report: ", statusReport$response$jobResponse))
    temp <- read.csv(file = statusReport$response$jobResponse, 
                     stringsAsFactors = FALSE)
    temp$Day <- as.Date(temp$Day)
    
    fileName <- paste0("yahoo_", advertiser$id[a], "_", from, "_", to, ".csv")
    print(paste0(Sys.time(), " - Write report: ", fileName))
    write.csv(x = temp, 
              file = fileName, 
              row.names = FALSE, 
              fileEncoding = "UTF-8")
    
    # append the report 
    print(paste0(Sys.time(), " - Appending ", nrow(temp), " row(s)."))
    report <- rbind.fill(report, temp)
  }
}

write.Alteryx(report, 1)