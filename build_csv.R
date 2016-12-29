library(XML)
library(lubridate)
library(stringr)

doc.html <- htmlTreeParse('http://www.nadc.nebraska.gov/cf/active_pacs.html', useInternal = TRUE)

doc.html
doc.text <- unlist(xpathApply(doc.html, '//p', xmlValue))
doc.text

pac.df <- data.frame()

for (i in 1:length(doc.text)) {
  pac <- unlist(strsplit(doc.text[i], split = "\\n"))

  # Skip the right column 
  if (str_detect("Annual Campaign Statement for calendar year.*", pac[1])) {
    break
  }

  pac.name <- NA
  pac.name2 <- NA
  dissolved <- NA
  contact <- NA
  position <- NA
  addr1 <- NA
  addr2 <- NA
  city <- NA
  state <- NA
  zip <- NA
  phone <- NA

  pac.name <- gsub(pattern = " - Dissolved.*", replacement = "", trimws(pac[1]))
  diss <- gsub(pattern = ".* - Dissolved (.*)", replacement = "\\1", trimws(pac[1]))

  if (str_detect(diss, "[0-9]+/[0-9]+/[0-9]+")) {
    dissolved <- mdy(diss)
  } else {
    dissolved <- NA
  }

  if (length(pac) == 6) {
    # Either the next line is the treasurer, or it's the name of the pac for mailing purposes
    if (str_detect(pac[2], "Treasurer")) {
      # If it's the treasurer line, then there is probably a 2 part address line
      contact <- gsub('[ ]?,[ ]?.*', "", trimws(pac[2]))
      position <- gsub('.*[ ]?,[ ]?(.*)$', "\\1", trimws(pac[2]))
      addr1 <- trimws(pac[3])
      addr2 <- trimws(pac[4])
    } else {
      # This is probably the name of the PAC again 
      pac.name2 <- trimws(pac[2])
      contact <- gsub('[ ]?,[ ]?.*', "", trimws(pac[3]))
      position <- gsub('.*[ ]?,[ ]?(.*)$', "\\1", trimws(pac[3]))
      addr1 <- trimws(pac[4])
    }
   
    city <- gsub("([A-Za-z]+)[,]? .*", "\\1", trimws(pac[5])) 
    state <- gsub("([A-Za-z]+)[,]?[ ]+,([A-Z][A-Z]).*", "\\2", trimws(pac[5])) 
    zip <- gsub(".*[ ]+([0-9]{5}(-[0-9]{4})?)", "\\1", trimws(pac[5])) 
  
    phone <- trimws(pac[6])
  } else {
      contact <- gsub('[ ]?,[ ]?.*', "", trimws(pac[2]))
      position <- gsub('.*[ ]?,[ ]?(.*)$', "\\1", trimws(pac[2]))
      addr1 <- trimws(pac[3])
   
      city <- gsub("([A-Za-z]+)[,]? .*", "\\1", trimws(pac[4])) 
      state <- gsub("([A-Za-z]+)[,]?[ ]+([A-Z][A-Z]).*", "\\2", trimws(pac[4])) 
      zip <- gsub(".*[ ]+([0-9]{5}(-[0-9]{4})?)", "\\1", trimws(pac[4])) 
  
      phone <- trimws(pac[5])
  }
  test <- "Lincoln NE  68508"
 
  print(temp)
  pac.df <- rbind(pac.df, data.frame(pac.name,
                                     pac.name2,
                                     dissolved,
                                     contact,
                                     position,
                                     addr1,
                                     addr2,
                                     city,
                                     state,
                                     zip,
                                     phone))
}

colnames(pac.df) <- c('pac.name', 'pac.name2', 'dissolved', 'contact', 'position', 'addr1', 'addr2', 'city', 'state', 'zip', 'phone')
pac.df
pac.df[2,]
head(pac.df)

