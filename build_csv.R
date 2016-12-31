library(XML)
library(lubridate)
library(stringr)

doc.html <- htmlTreeParse('http://www.nadc.nebraska.gov/cf/active_pacs.html', useInternal = TRUE)

doc.html
doc.text <- unlist(xpathApply(doc.html, '//p', xmlValue))
doc.text

pac.df <- data.frame()

# Some of the PAC names are in <p>'s of their own
# If this is the case, we'll carry them forward to the next <p>
carry.forward <- NA

for (i in 1:length(doc.text)) {

  pac <- unlist(strsplit(doc.text[i], split = "\\n"))

  # Skip the right column 
  if (str_detect("Annual Campaign Statement for calendar year.*", pac[1])) {
    print("Stopping at the right column which is not data")
    break
  }

  pac.name <- NA
  pac.name2 <- NA
  contact <- NA
  position <- NA
  addr1 <- NA
  addr2 <- NA
  city <- NA
  state <- NA
  zip <- NA
  phone <- NA
  dissolved <- NA

  if (length(pac) > 1) {

    if (!is.na(carry.forward)) {
      pac.name <- gsub(pattern = "- Dissolved.*", replacement = "", carry.forward)
      diss <- gsub(pattern = "^.* - Dissolved (.*)$", replacement = "\\1", carry.forward)
      carry.forward <- NA
    } else {
      pac.name <- gsub(pattern = "- Dissolved.*", replacement = "", trimws(pac[1]))
      diss <- gsub(pattern = "^.* - Dissolved (.*)$", replacement = "\\1", trimws(pac[1]))
    }

    # If the PAC was dissolved, get the date
    if (str_detect(diss, "[0-9]+/[0-9]+/[0-9]+")) {
      dissolved <- mdy(diss)
    }

    # Remove leading blank line
    if (trimws(pac[1]) == "") {
      pac <- pac[-1]
    }

    # Remove ending blank lines 
    if (trimws(pac[length(pac)]) == "") {
      pac <- pac[1:(length(pac) - 1)]
    }

    last.line <- trimws(pac[length(pac)])
    second.to.last.line <- trimws(pac[length(pac) - 1])

    if (last.line == "") {
      last.line <- trimws(pac[length(pac) - 1])  
      second.to.last.line <- trimws(pac[length(pac) - 2])
    }

    # If the last line is a phone number...
    if (str_detect(last.line, ".* [0-9]{3}-[0-9]{4}$")) {
      phone <- last.line

      city <- gsub("([A-Za-z]+)[,]?[ ]*.*", "\\1", second.to.last.line) 
      state <- gsub("([A-Za-z]+)[,]?[ ]*([A-Z][A-Z]).*", "\\2", second.to.last.line)
      zip <- gsub(".*[ ]+([0-9]{5}(-[0-9]{4})?)", "\\1", second.to.last.line)
      
    } else if (str_detect(last.line, ".*Independent Committee.*")) {
      
      city <- gsub("([A-Za-z]+)[,]?[ ]*.*", "\\1", second.to.last.line)
      state <- gsub("([A-Za-z]+)[,]?[ ]*([A-Z][A-Z]).*", "\\2", second.to.last.line)
      zip <- gsub(".*[ ]+([0-9]{5}(-[0-9]{4})?)", "\\1", second.to.last.line)
      
    } else {
      
      city <- gsub("([A-Za-z]+)[,]?[ ]*.*", "\\1", last.line)
      state <- gsub("([A-Za-z]+)[,]?[ ]*([A-Z][A-Z]).*", "\\2", last.line)
      zip <- gsub(".*[ ]+([0-9]{5}(-[0-9]{4})?)", "\\1", last.line)
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
    } else if (length(pac) == 5) {
        contact <- gsub('[ ]?,[ ]?.*', "", trimws(pac[2]))
        position <- gsub('.*[ ]?,[ ]?(.*)$', "\\1", trimws(pac[2]))
        addr1 <- trimws(pac[3])
    } else if (length(pac) == 4) {
        # If first line is contact
        if (str_detect(pac[1], ".*Treasurer.*")) {
          contact <- gsub('[ ]?,[ ]?.*', "", trimws(pac[1]))
          position <- gsub('.*[ ]?,[ ]?(.*)$', "\\1", trimws(pac[1]))
          addr1 <- trimws(pac[2])
        } else {
          contact <- gsub('[ ]?,[ ]?.*', "", trimws(pac[2]))
          position <- gsub('.*[ ]?,[ ]?(.*)$', "\\1", trimws(pac[2]))
          addr1 <- trimws(pac[3])
        }
    }
  
    pac.df <- rbind(pac.df, data.frame(pac.name,
                                       pac.name2,
                                       as.POSIXct(dissolved),
                                       contact,
                                       position,
                                       addr1,
                                       addr2,
                                       city,
                                       state,
                                       zip,
                                       phone))
  } else {
    carry.forward <- trimws(pac[1])
  }
}

colnames(pac.df) <- c('pac.name', 'pac.name2', 'dissolved', 'contact', 'position', 'addr1', 'addr2', 'city', 'state', 'zip', 'phone')



str(pac.df)
pac.df$pac.name <- as.character(pac.df$pac.name)
pac.df$contact <- as.character(pac.df$contact)
pac.df$position <- as.character(pac.df$position)
pac.df$addr1 <- as.character(pac.df$addr1)
pac.df$addr2 <- as.character(pac.df$addr2)
pac.df$city <- as.character(pac.df$city)
pac.df$state <- as.character(pac.df$state)
pac.df$zip <- as.character(pac.df$zip)

# Need to hand fix a few items
# rows 26, 29, 31, 36, 50, 78, 
pac.df$city[26] <- 'Lee\'s Summit'
pac.df$state[26] <- 'MO'

pac.df$city[29] <- 'Grand Island'
pac.df$state[29] <- 'NE'

pac.df$city[31] <- 'Papillion'
pac.df$state[31] <- 'NE'
pac.df$zip[31] <- '68046'
pac.df$phone[31] <- '(402)592-7474'

pac.df$state[36] <- 'MN'

pac.df$pac.name2[50] <- 'ACRE'
pac.df$contact[50] <- 'Troy Bredenkamp'
pac.df$position[50] <- 'Treasurer'
pac.df$addr1[50] <- 'PO Box 82048'
pac.df$addr2[50] <- '1244 K Street'

pac.df$contact[78] <- 'Creative Association Management'
pac.df$position[78] <- 'Treasurer'

pac.df$phone <- gsub("\\(\\(", "(", pac.df$phone)
pac.df$phone <- gsub("[ ]+", " ", pac.df$phone)

addresses <- paste0(pac.df$city, ", ", pac.df$state, " ", pac.df$zip)

# Get latitude, longitude pairs from Google Maps
# Note: Not sure how accurate these are after doing a spot check. :/
latlon <- geocode(addresses)

pac.df <- cbind(pac.df, latlon)

write.csv(x = pac.df, file = 'NE_PACs.csv')
