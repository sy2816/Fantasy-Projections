library(rvest)
library(dplyr)

week <- c(1:11)
index <- c(0, 40, 60, 80, 120, 140, 160, 200, 220, 240, 260)

cat <- 0 #Signifies QBs

url <- sprintf("http://games.espn.com/ffl/tools/projections?&scoringPeriodId=%s&seasonId=2017&slotCategoryId=%s&startIndex=%s",
               week[1], cat, index[1])

# Initialize empty data frame 
projqb <- data.frame()

for (w in c(1:length(week))){
  
    for(i in c(1:length(index))){
      
      # Create url string
      url <- sprintf("http://games.espn.com/ffl/tools/projections?&scoringPeriodId=%s&seasonId=2017&slotCategoryId=%s&startIndex=%s",
                     week[w], cat, index[i])
      
      #Read table
      data <- read_html(url) %>% html_node(xpath = '//*[@id="playertable_0"]') %>% html_table()
      
      #Clean column names
      colnames(data) <- data[1,]
      data <- data[-1,]

      print(sprintf("Week %s, index %s", w, i))
      if(nrow(data) == 0){
        break # breaks loop if empty data is detected
      }else{
        data$week <- w # record which week data is for
        
        byes <- grep(pattern = "** BYE **", x = data$OPP, fixed = TRUE)
        if(length(byes) > 0){
          data <- data[-byes,]
        }

        projqb <- rbind(projqb, data)
      }
    }
  }

colnames(projqb) <- c("player", "opp", "status", "catchAttempt", "passYards", "passTD", "passInt", "rushAttempt", "rushYards"
                      ,"rushTD", "rec", "recYards", "recTD", "projPoints", "week")

# Actual scores

weekReal <- c(1:11)
cat <- 0
indexReal <- c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600)

realqb <- data.frame()

for (w in c(1:length(week))){
  
  for(i in c(1:length(index))){
    
    urlReal <- sprintf("http://games.espn.com/ffl/leaders?&scoringPeriodId=%s&seasonId=2017&slotCategoryId=%s&startIndex=%s",
                       weekReal[w], cat, indexReal[i])
    
    data <- read_html(urlReal) %>% html_node(xpath = '//*[@id="playertable_0"]') %>% html_table()
    
    # clean data
    data <- data[-1,c(1,3,4,6,8,9,11,12,13,15,16,17,18,20,21,22,24)]
    colnames(data) <- data[1,]
    data <- data[-1,]
    
    print(sprintf("Week %s, index %s", w, i))
    if(nrow(data) == 0){
      break # breaks loop if empty data is detected
    }else{
      data$week <- w # record which week data is for
      
      byes <- grep(pattern = "** BYE **", x = data$OPP, fixed = TRUE)
      if(length(byes) > 0){
        data <- data[-byes,]
      }
      
      realqb <- rbind(realqb, data)
    }
    
  }
}

colnames(realqb) <- c("player", "opp", "status", "catchAttempt", "passTD", "passInt", "rushAttempt", "rushYards", "rushTD",
                      "rec", "recYards", "recTD", "target", "2pc", "fuml", "miscTD", "realPoints", "week")



players <- unique(realqb$player)

summaryData <- data.frame()

for(i in c(1:length(players))){
  dataProj <- filter(projqb, player == players[i])
  dataProj <- dataProj[,c(1,14,15)]
  dataReal <- filter(realqb, player == players[i])
  dataReal <- dataReal[,c(1,17,18)]
  
  joinedData <- inner_join(dataProj, dataReal, by = "week")
  
  joinedData$projPoints <- as.numeric(joinedData$projPoints)
  joinedData$realPoints <- as.numeric(joinedData$realPoints)
  
  joinedData$realMinusProj <- joinedData$realPoints - joinedData$projPoints
  
  meanDiff <- mean(joinedData$realMinusProj)
  medianDiff <- median(joinedData$realMinusProj)
  
  bindDat <- data.frame(player = players[i], meanDiff = meanDiff, medianDiff = medianDiff)
  summaryData <- rbind(summaryData, bindDat)
}
summaryData$meanDiff <- round(summaryData$meanDiff, digits = 1)
