---
title: "PA2"
author: "GianLuca Colussi"
date: "08/16/2014"
output: html_document
---

## Peer assessment 2 - Exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database

### Download and preprocessing data

Set work directory, download and extract bzipped dataset

```r
library(R.utils)
```

```
## Loading required package: R.oo
## Loading required package: R.methodsS3
## R.methodsS3 v1.6.1 (2014-01-04) successfully loaded. See ?R.methodsS3 for help.
## R.oo v1.18.0 (2014-02-22) successfully loaded. See ?R.oo for help.
## 
## Attaching package: 'R.oo'
## 
## The following objects are masked from 'package:methods':
## 
##     getClasses, getMethods
## 
## The following objects are masked from 'package:base':
## 
##     attach, detach, gc, load, save
## 
## R.utils v1.32.4 (2014-05-14) successfully loaded. See ?R.utils for help.
## 
## Attaching package: 'R.utils'
## 
## The following object is masked from 'package:utils':
## 
##     timestamp
## 
## The following objects are masked from 'package:base':
## 
##     cat, commandArgs, getOption, inherits, isOpen, parse, warnings
```

```r
setwd("~/NOAAstorm_PeerAssesment2")
dir <- dir()
fileex <- dir=="NOAAstorm.csv.bz2"
fileex1 <- sum(fileex*1)
if (fileex1==0) {
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url,"NOAAstorm.csv.bz2",method="curl")        
}
fileex2 <- dir=="NOAAstorm.csv"
fileex3 <- sum(fileex2*1)
if (fileex3==0) {
bunzip2("NOAAstorm.csv.bz2","NOAAstorm.csv",remove=FALSE)
} 
db <- read.csv("NOAAstorm.csv")
```

```
## Warning: EOF within quoted string
```

