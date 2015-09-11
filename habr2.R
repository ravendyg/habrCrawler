dateProc <- function(date, i) { # takes list of dates and index of the currently processing record
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

library(XML)
      # read every hab and collect records
      # time control
tm = Sys.time() # time control
where = "habrnew.csv"          # file name. Goes to Documents by default
      
# some technicalities for date processing
dayShift = 7 # how deep to look (in days)
curYear = unlist(strsplit(toString(Sys.Date()), "-"))[1] # current year
prevYear = unlist(strsplit(toString(Sys.Date()-365), "-"))[1] # smtimes need previous year
if (curYear == prevYear) { # rare case of leap year + 31 of December
  prevYear = unlist(strsplit(toStriqng(Sys.Date()-366), "-"))[1]
}
curMonth = unlist(strsplit(toString(Sys.Date()), "-"))[2] # current month
monthConvert = c("января", "февраля", "марта", "апреля", "мая", "июня", "июля", "августа", "сентября", "октября", "ноября", "декабря")

# iterate over all habs
# habrahabr
j = 1
repeat {
      url = paste("http://habrahabr.ru/all/", "page", as.character(j), sep = '')
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
      
      # write to csv
      for (i in 1:length(id)) {
                  # modify content
            w = unlist(strsplit(content[i], ","))
            w = unlist(strsplit(w, "\n"))
            w = unlist(strsplit(w, "\t"))
            w = unlist(strsplit(w, "\r"))
            w = paste(w[w!=''], collapse='')
                # convert readed dates into R recognizable smth
            q=dateProc(dates, i)           
                  # write
            fileCon = file(where, open="a") # open connection for appending
            writeLines(paste(id[i], q, titles[i], w, sep = ','), fileCon)
            close(fileCon)
      }
      print(j)
      if (q+dayShift<Sys.Date()) {break} # old data
      j = j + 1
}

# geektimes
j = 1
repeat {
      url = paste("http://geektimes.ru/all/", "page", as.character(j), sep = '')
      html = htmlTreeParse(url, useInternalNodes = T, encoding = "UTF-8")
      # titles
      titles = xpathSApply(html, "//a[@class='post_title']", xmlValue)
      if (length(titles) == 0) {break}
      # dates
      dates = xpathSApply(html, "//div[@class='published']", xmlValue)
      # content
      content = xpathSApply(html, "//div[@class='content html_format']", xmlValue)  
      # id
      #id = xpathSApply(html, "//div[@class='post shortcuts_item']", xmlAttrs)
      
      id = xpathSApply(html, "//a[@class='post_title']", xmlAttrs)     
      #id2 = xpathSApply(html, "//div[@class='post translation shortcuts_item']", xmlAttrs)
      ##id=c(id, id2)
      if (typeof(id) == "list") {id = unlist(id)}  
      #key = seq(from = 2, to = length(id), by = 2)
      #id  = id[key]
      id_key = seq(1,length(id),2)
      id = id[id_key]
      
      #id = unlist(strsplit(id, "/"))
      #id = as.integer(id)
      #id = id[!is.na(id)]
      #key = seq(from = 2, to = length(id), by = 2)
      #id  = as.integer(id[key])
      # write to csv
      for (i in 1:length(id)) {
            # modify content
            w = unlist(strsplit(content[i], ","))
            w = unlist(strsplit(w, "\n"))
            w = unlist(strsplit(w, "\t"))
            w = unlist(strsplit(w, "\r"))
            w = paste(w[w!=''], collapse='')
            # convert readed dates into R recognizable smth
            q=dateProc(dates, i) 
            # write
            fileCon = file(where, open="a") #open connection for appending
            writeLines(paste(id[i], dates[i], titles[i], w, sep = ','), fileCon)
            close(fileCon)
      }
      print(j)
      if (q+dayShift<Sys.Date()) {break} # old data
      j = j + 1
}

# megamozg
j = 1

repeat {
  url = paste("http://megamozg.ru/all/", "page", as.character(j), sep = '')
  html = htmlTreeParse(url, useInternalNodes = T, encoding = "UTF-8")
  # titles
  titles = xpathSApply(html, "//a[@class='post_title']", xmlValue)
  if (length(titles) == 0) {break}
  # dates
  dates = xpathSApply(html, "//div[@class='published']", xmlValue)
  # content
  content = xpathSApply(html, "//div[@class='content html_format']", xmlValue)  
  # id
  #id = xpathSApply(html, "//div[@class='post shortcuts_item']", xmlAttrs)
  
  id = xpathSApply(html, "//a[@class='post_title']", xmlAttrs)     
  #id2 = xpathSApply(html, "//div[@class='post translation shortcuts_item']", xmlAttrs)
  ##id=c(id, id2)
  if (typeof(id) == "list") {id = unlist(id)}  
  #key = seq(from = 2, to = length(id), by = 2)
  #id  = id[key]
  id_key = seq(1,length(id),2)
  id = id[id_key]
  
  #id = unlist(strsplit(id, "/"))
  #id = as.integer(id)
  #id = id[!is.na(id)]
  #key = seq(from = 2, to = length(id), by = 2)
  #id  = as.integer(id[key])
  # write to csv
  for (i in 1:length(id)) {
    # modify content
    w = unlist(strsplit(content[i], ","))
    w = unlist(strsplit(w, "\n"))
    w = unlist(strsplit(w, "\t"))
    w = unlist(strsplit(w, "\r"))
    w = paste(w[w!=''], collapse='')
    # convert readed dates into R recognizable smth
    q=dateProc(dates, i)
    # write
    fileCon = file(where, open="a") #open connection for appending
    writeLines(paste(id[i], dates[i], titles[i], w, sep = ','), fileCon)
    close(fileCon)
  }
  print(j)
  if (q+dayShift<Sys.Date()) {break} # old data
  j = j + 1
}

print(Sys.time() - tm)
