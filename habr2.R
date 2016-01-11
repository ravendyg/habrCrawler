dateProc <- function(dates, i) { # takes list of dates and index of the currently processing record
  q = unlist(strsplit(dates[i], " "))
  if (q[1] == "сегодня") { # today record
    q = Sys.Date();
  } else if (q[1] == "вчера") { # yesterday record
    q = Sys.Date() - 1;
  }
  else {
    q = q[q != ""][c(1, 2)] # need only day and month; presume you aren't gonna extract 
    q[2] = which(monthConvert==q[2])
    if (curMonth == "01" && q[2] == 12) { # new year cross
      q = as.Date(paste(prevYear, q[2], q[1], sep="/"))
    } else {
      q = as.Date(paste(curYear, q[2], q[1], sep="/"))
    }
  }
  return(q);  # returns the date in Date format
}

traceAuthors = TRUE

library(XML)
      # read every hab and collect records
      # time control
tm = Sys.time() # time control
where = "/media/slava/Seagate Expansion Drive/Seagate/9/users/"          # file name. Goes to Documents by default
outFile = "habrnewWithAuthor.csv" # 
if (traceAuthors) {
  authorsListSrc = "habrAuthors.csv" # list of the authors we want to be notified about
  authorsList = read.csv(paste(where,authorsListSrc, sep=''), header = F, stringsAsFactors=FALSE)[,1]
}
      
# some technicalities for date processing
dayShift = 7 # how deep to look (in days)
curYear = unlist(strsplit(toString(Sys.Date()), "-"))[1] # current year
prevYear = unlist(strsplit(toString(Sys.Date()-365), "-"))[1] # smtimes need previous year
if (curYear == prevYear) { # rare case of leap year + 31 of December
  prevYear = unlist(strsplit(toStriqng(Sys.Date()-366), "-"))[1]
}
curMonth = unlist(strsplit(toString(Sys.Date()), "-"))[2] # current month
monthConvert = c("января", "февраля", "марта", "апреля", "мая", "июня", "июля", "августа", "сентября", "октября", "ноября", "декабря")

# iterate over specified hab
readHabr <- function (basePath, optionalLimit = 0) {
  j = 1
  repeat {
    url = paste(basePath, "page", as.character(j), sep = '')
    html = htmlTreeParse(url, useInternalNodes = T, encoding = "UTF-8")
    # titles
    titles = xpathSApply(html, "//a[@class='post_title']", xmlValue)
    if (length(titles) == 0) {break}
    # dates
    dates = xpathSApply(html, "//div[@class='published']", xmlValue)
    # content
    content = xpathSApply(html, "//div[@class='content html_format']", xmlValue)  
    # id      
    id = xpathSApply(html, "//a[@class='post_title']", xmlAttrs)     
    if (typeof(id) == "list") {id = unlist(id)}  
    id_key = seq(1,length(id),2)
    id = id[id_key]
    # author 
    auth = xpathSApply(html, "//a[@class='post-author__link']", xmlValue)
    
    # write to csv
    for (i in 1:length(id)) {
      # modify content
      w = unlist(strsplit(content[i], ","))
      w = unlist(strsplit(w, "\n"))
      w = unlist(strsplit(w, "\t"))
      w = unlist(strsplit(w, "\r"))
      w = paste(w[w!=''], collapse='')
      w = strtrim(w, 500)
      # convert readed dates into R recognizable smth
      q=dateProc(dates, i)         
      # extract author name
      authName = strsplit(strsplit(auth[i], "@")[[1]][2], "\n")[[1]][1]
      if (traceAuthors && authName %in% authorsList)            {
        titles[i] = paste('@@@', titles[i], sep=' ')
      }
      # write
      fileCon = file(paste(where,outFile,sep=''), open="a") # open connection for appending
      writeLines(paste(id[i], q, authName, titles[i], w, sep = ','), fileCon)
      close(fileCon)
    }
    print(j)
    if (q+dayShift<Sys.Date() || # old data
        (optionalLimit > 0 && j == optionalLimit)) {  # artificially introduced limit
      break
    }
    j = j + 1
  }
}

# habrahabr
readHabr("http://habrahabr.ru/all/",2)
# geektimes
readHabr("http://geektimes.ru/all/",2)
# megamozg
#readHabr("http://megamozg.ru/all/")

print(Sys.time() - tm)