library(leaflet)
library(leaflet.extras)
library(htmltools)
library(rhandsontable)
library(DT)
library(digest)
#library(aws.s3)
library(shinyjs)
library(stringr)
library(scales)
#setwd("C:/Users/csmithxc/Documents/RealEstate_Rev2/Rev2")
#taxlien <- read.csv("TaxLien_Data_Full_LL_12_16_2017.csv", header=T)
sheriff <- read.csv("sheriff_parcels_complete_5_9_2018.csv", header=T)
#taxlien_personal <- read.csv("TaxLien_Personal_12_16_17.csv", header=T)
#sheriff_personal <- read.csv("Sheriff_Personal_12_16_17.csv", header=T)
#colnames(taxlien_personal)[1] <- "Address"
#colnames(sheriff_personal)[1] <- "Address"
sheriff$SellDate <- as.Date(sheriff$SellDate, "%m/%d/%Y")
sheriff$Plaintiff <- gsub("AS TRUSTEE.*","",sheriff$Plaintiff)
#sheriff$Address <- gsub(" [A-z]*,| ..$", "", sheriff$Address)

#taxlien$Address <- as.character(taxlien$Address)
sheriff$Address <- as.character(sheriff$Address)
sheriff$OpeningBid <- as.numeric(gsub("\\$|,", "", sheriff$OpeningBid))
sheriff$Status <- as.character(sheriff$Status)
colnames(sheriff)[colnames(sheriff) == "ZipCode"] <- "Zip"
#taxlien_personal$Address <- as.character(taxlien_personal$Address)


redIcon <- makeIcon(
  iconUrl = "red-marker.png",
  iconWidth = 36, iconHeight = 36,
  iconAnchorX = 0, iconAnchorY = 0
)

num_fails_to_lockout <- 3

Sys.setenv("AWS_ACCESS_KEY_ID" = "####",
           "AWS_SECRET_ACCESS_KEY" = "####",
           "AWS_DEFAULT_REGION" = "us-east-1")
