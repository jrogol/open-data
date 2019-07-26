#Arqam Assignmnet Due Date Converter
library(lubridate) 
library(M3)
library(chron)
#Constants
current_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S")

#Read Arqam file
Week_File_1 = read.csv("Week19July.csv", header = TRUE, stringsAsFactors = FALSE)

#Rewrite dates:
Match_Date = as.Date(Week_File_1$Match.Date, "%d/%m/%Y")
Arqam_ETA_Day = as.Date(Week_File_1$ETA, "%m/%d/%Y")

#Arqam time of completion **
Night = chron(times = rep("23:59:59", 182))
Morning = chron(times = rep("15:00:00", 182))
Arqam_ETA = vector(mode = "character", length = 182)
for (k in 1:length(Match_Date)) {
  if (Week_File_1$Shift[k] == "Night") {
    Arqam_ETA[k] = as.character(combine.date.and.time(Arqam_ETA_Day[k], Night[k]))
  } else {
    Arqam_ETA[k] = as.character(combine.date.and.time(Arqam_ETA_Day[k], Morning[k]))
  }
}

#Add :00 or replace missing kick off values (ONLY RUN THIS ONCE)
for (j in 1:length(Week_File_1$Home.Team)) {
  if ((Week_File_1$Kick.Off.Time[j] == "") == TRUE) {
    Week_File_1$Kick.Off.Time[j] = "19:45:00"
  } else {
    Week_File_1$Kick.Off.Time[j] = paste0(Week_File_1$Kick.Off.Time[j], ":00")
  }
}

#Convert text to times
Kick_Off = chron(times = Week_File_1$Kick.Off.Time)

#Assigning match times to dates
Match_Time = combine.date.and.time(Match_Date, Kick_Off)

#Expected time to completion feed using priorities **
Priority_Expected_SBDB_Time = vector(mode = "character", length = 182)

for (i in 1:length(Match_Date)) {
  if (Week_File_1$Priority[i] == 1) {
    Priority_Expected_SBDB_Time[i] = as.POSIXct(as.character(update(Match_Time[i], hours = 12)), 
                                                format="%Y-%m-%d-%H.%M.%S")
  } else if (Week_File_1$Priority[i] > 1 && Week_File_1$Priority[i] < 4){
    Priority_Expected_SBDB_Time[i] = as.POSIXct(as.character(Match_Time[i] + ddays(x = 1)))
  } else if (Week_File_1$Priority[i] > 7) {
    Priority_Expected_SBDB_Time[i] = as.POSIXct(as.character(Match_Time[i] + ddays(x = 1)))
  } else {
    Priority_Expected_SBDB_Time[i] = as.POSIXct(as.character(Match_Time[i] + ddays(x = 1)))
  }
}

Expected_IQ_Time = vector(mode = "character", length = 182)

#Create and Data Table (table_tracker)
table_tracker = data.frame(Week_File_1$Competition, Week_File_1$Home.Team, Week_File_1$Away.Team, 
                           Match_Date, Week_File_1$Priority, Week_File_1$Shift, 
                           Week_File_1$Kick.Off.Time, Match_Time, Priority_Expected_SBDB_Time)

##Rename columns and Sort Data Table
sort_table_tracker = table_tracker[order(table_tracker$Match_Date, decreasing = TRUE), , ]
names(table_tracker)[2]<-"Home Team"
names(table_tracker)[3]<-"Away Team"
names(table_tracker)[5]<-"Priority"
names(table_tracker)[6]<-"Shift"
names(table_tracker)[7]<-"Kick Off"
names(table_tracker)[]<-"Due Date - Expected Due date"

#Estimated SBDB & IQ importer time 
ETA_SBDB = vector(mode = "character", length = 182)
ETA_IQ = vector(mode = "character", length = 182)

     #Find next half hour the SBDB checks for upload ***
for (l in 1:length(Arqam_ETA)) {
  if (minute(Arqam_ETA[l]) >= 30) {
    ETA_SBDB[l] = as.character(Arqam_ETA[l] + dminutes(x = 1))
  } else {
  }
}

#Time difference (between ETA_SBDB & Current Date)
live_time_difference = difftime(ETA_SBDB, current_date, units = "hours")
expected_time_difference = difftime(ETA_SBDB, Priority_Expected_SBDB_Time, units = "hours")

#########
#Link SB data with Arqam using match ID
new = intersect(g_status$`Match Id`, Week_File_1$matches.match_id)
Status_add = g_status$Status[new]

#List of Customers
Customers = c("Los Angeles FC", "AC Milan", "Inter Miami", "FC Cincinnati", 
              "Fleetwood", "Barnsley", "PSG", "Lyon", "FC Lorient", "Huddersfield", "QPR", 
              "Southampton", "Besiktas", "Wigan Athletic","Norwich")

