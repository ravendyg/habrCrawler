dateProc <- function(dates, i, months) { # takes list of dates and index of the currently processing record
  dates[i] = gsub("[\r\n]|^\\s+|\\s+$", "", dates[i])
  q = unlist(strsplit(dates[i], " "))
  if (q[1] == "сегодня") { # today record
    q = Sys.Date();
  } else if (q[1] == "вчера") { # yesterday record
    q = Sys.Date() - 1;
  }
  else {
    q = q[q != ""][c(1, 2, 3)] # need only day and month; presume you aren't gonna extract 
    q[2] = which(months==q[2])
    if (nchar(q[3])!=4) {
      # current year
      # 
      q = as.Date(paste(format(Sys.Date(), "%Y"), q[2], q[1], sep="/"))
    } else {
      q = as.Date(paste(q[3], q[2], q[1], sep="/"))
    }
  }
  return(q);  # returns the date in Date format
}

library(bitops)
library(RCurl)
library(XML)
      # read every hab and collect records
      # time control
tm = Sys.time() # time control

# some technicalities for date processing
curYear = unlist(strsplit(toString(Sys.Date()), "-"))[1] # current year
prevYear = unlist(strsplit(toString(Sys.Date()-365), "-"))[1] # smtimes need previous year
if (curYear == prevYear) { # rare case of leap year + 31 of December
  prevYear = unlist(strsplit(toStriqng(Sys.Date()-366), "-"))[1]
}
curMonth = unlist(strsplit(toString(Sys.Date()), "-"))[2] # current month
monthConvert = c("января", "февраля", "марта", "апреля", "мая", "июня", "июля", "августа", "сентября", "октября", "ноября", "декабря")
monthConvertHabr = c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", "Ноябрь", "Декабрь")

# iterate over specified hab
readHabr <- function (basePath, months, limitNumber = 0, startFrom = 1, dayShift = 7, stopAt = 0) {
  # don't check startFrom, limitNumber validity
  j = startFrom
  repeat {
    url = paste(basePath, "page", as.character(j), '/', sep = '')
    urlContent = getURL(url)
    html = htmlTreeParse(urlContent, useInternalNodes = T, encoding = "UTF-8")
    # titles
    #titles = xpathSApply(html, "//a[@class='post_title']", xmlValue)
    titles = xpathSApply(html, "//a[@class='post__title_link']", xmlValue)
    if (length(titles) == 0) {break}
    titles = gsub(",", "", titles)
    titles = gsub(";", "", titles)
    # dates
    #dates = xpathSApply(html, "//div[@class='published']", xmlValue)
    dates = xpathSApply(html, "//span[@class='post__time_published']", xmlValue)
    # content
    content = xpathSApply(html, "//div[@class='content html_format']", xmlValue)  
    # id      
    #id = xpathSApply(html, "//a[@class='post_title']", xmlAttrs)
    id = xpathSApply(html, "//a[@class='post__title_link']", xmlAttrs)     
    if (typeof(id) == "list") {id = unlist(id)}  
    id_key = seq(1,length(id),2)
    id = id[id_key]
    # author 
    auth = xpathSApply(html, "//a[@class='post-author__link']", xmlValue)
    
    # write to csv
    for (i in 1:length(id)) {
      # will read at least one page before date condition will kick in
      # modify content
      w = gsub(",", "", content[i])
      w = gsub(";", "", w)
      w = gsub("\t", "", w)
      w = gsub("\n", "", w)
      w = gsub("\r", "", w)
      #w = unlist(strsplit(content[i], ","))
      #w = unlist(strsplit(w, "\n"))
      #w = unlist(strsplit(w, "\t"))
      #w = unlist(strsplit(w, "\r"))
      #w = paste(w[w!=''], collapse='')
      w = strtrim(w, 500)
      # convert readed dates into R recognizable smth
      q=dateProc(dates, i, months)         
      # extract author name
      authName = strsplit(strsplit(auth[i], "@")[[1]][2], "\n")[[1]][1]
      if (traceAuthors && authName %in% authorsList)            {
        titles[i] = paste('@@@', titles[i], sep=' ')
      } else if (markAuthorsNegative && authName %in% negativeAuthorsList) {
        titles[i] = paste('`---', titles[i], sep=' ')
      }
      # write
      fileCon = file(paste(where,outFile,sep=''), open="a") # open connection for appending
      writeLines(paste(id[i], q, authName, titles[i], w, sep = ','), fileCon)
      close(fileCon)
    }
    print(j)
    if ((dayShift > 0 && q+dayShift<Sys.Date()) || # old data
        (limitNumber > 0 && (j-startFrom+1) == limitNumber) ||
        (stopAt > 0 && j >= stopAt)) {  # artificially introduced limit
      break
    }
    j = j + 1
  }
}

# path settings
where = "/home/me/Dropbox/Habr/"          # file name. Goes to Documents by default
outFile = "habrnewWithAuthor.csv"
#outFile = "testHabr2.csv"
authorsListSrc = "habrAuthors.csv" # list of the authors we want to be notified about
negativeAuthorsListSrc = "habrAuthorsNegative.csv" # list of the authors we want to be notified about, but differently

traceAuthors = TRUE
markAuthorsNegative = TRUE

if (traceAuthors) {
  authorsList = read.csv(paste(where,authorsListSrc, sep=''), header = F, stringsAsFactors=FALSE)[,1]
}
if (markAuthorsNegative) {
  negativeAuthorsList = read.csv(paste(where,negativeAuthorsListSrc, sep=''), header = F, stringsAsFactors=FALSE)[,1]
}

# habrahabr
readHabr("https://habrahabr.ru/all/", monthConvert)
# geektimes
readHabr("https://geektimes.ru/all/", monthConvert)
# any hub
#readHabr("https://geektimes.ru/hub/biotech/", limitNumber = 196, startFrom = 1, stopAt = 55, dayShift = -1)
#readHabr("https://geektimes.ru/hub/gadgets/", limitNumber = 196, startFrom = 1, stopAt = 92, dayShift = -1)

print(Sys.time() - tm)

#library(RCurl)

#j=1
#url = paste("https://habrahabr.ru/all/", "page", as.character(j), '/', sep = '')
#readLines(url, encoding = "UTF-8")
#readHabr("https://habrahabr.ru/all/", limitNumber = 1)
