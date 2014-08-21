---
title: "PA2"
author: "GianLuca Colussi"
date: "08/16/2014"
output: html_document
---

### Peer assessment 2 - Exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database

## Download and preprocessing data

Set work directory, download and extract bzipped dataset

```r
setwd("~/NOAAstorm_PeerAssesment2")
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url,"NOAAstorm.csv.bz2",method="curl")
```

