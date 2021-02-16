
#Goal: Acquires all holidays in Hawaii 3 years ago to 3 years into the future
#Output table is Completeholidays

#Initializing the variable that is to be returned from a function and associated libraries
acc <- NULL
library(timeDate)
library(lubridate)
library(dplyr)

#Obtaining most recent signup date
Input <- readxl::read_xlsx("G:/OPER_ADM/Service Evaluation/PowerBI/Executive Summary Report/Library/BookingToSignupDate.xlsx")

#Grabbing the year of the latest signup
C <- as.Date(max(Input$Date))
CYR <- format(C, "%Y")

#Defining the beginning of year
YRBegin <- "01-01"
YREnd <- "12-31"


#Years Forwards and Backwards
FYR1 <- as.numeric(CYR) + 1
FYR2 <- as.numeric(CYR) + 2
FYR3 <- as.numeric(CYR) + 3
PYR1 <- as.numeric(CYR) - 1
PYR2 <- as.numeric(CYR) - 2
PYR3 <- as.numeric(CYR) - 3
CYR <- as.numeric(CYR)

#Concatenates the year of the latest signup to the end of fiscal year date (ex. signup in 2019 becomes 2019-06-30)
PYR <- as.Date(paste(PYR3, YRBegin, sep = "-"))
FYR <- as.Date(paste(FYR3, YREnd, sep = "-"))


#Creating a blank calendar from 3 Years Ago until 3 Years Forward
CompleteYForward <- function() {
  while (PYR <= FYR) {
    acc <- c(acc, as.character(PYR))
    PYR = PYR + 1
  }
  return(acc)
}

CompleteDaylist <- CompleteYForward()
CompleteDaylist <- as.data.frame(CompleteDaylist)

#Fixed Holidays: Holidays whose dates are fixed no matter what day it ends on
Hol <- c("01-01", "03-26", "06-11", "07-04", "11-11", "12-25")
names <- c("New Years Day", "Prince Kuhio Day", "King Kamehameha Day", "Independence Day", "Veterans Day", "Christmas Day")

myFunction <- function() {
  y1 <- as.numeric(format(FYR, "%Y")) + 1
  y2 <- as.numeric(format(PYR, "%Y"))
  while (y1!= y2) {
    r1 <- paste(y1, Hol, sep = "-")
    acc <- c(acc, r1)
    y1 = y1 - 1
  }
  return(acc)
}

output <- myFunction()


yeardate <- as.factor(output)
hcholidays = data.frame(yeardate, names) 
hcholidays$day <- weekdays(as.Date(yeardate))
hcholidays$dcomb <- paste(hcholidays$yeardate, hcholidays$day, sep = ",")


#Special behavior if a New Year's Day ends up on a Saturday...it can't go backwards to Dec 31st as New Years, so no change with listing where the holiday on Sat or Sun would occur (Sat moves to Fri, Sun moves to Mon)
hcholidays$daycorrector <- ifelse(grepl("-01-01,Saturday", hcholidays$dcomb), "Saturday", ifelse(hcholidays$day == "Saturday", "Friday", ifelse(hcholidays$day == "Sunday", "Monday", NA)))

hcholidays$yeardate <- as.Date(hcholidays$yeardate)

#This generates a new field where the date is adjusted based on the day the holiday lands on. Holidays that land on a Saturday are moved back 1 day to Friday and holidays that land on a Sunday are moved forwards 1 day. The only holiday that does not obey this is New Years Day. Without the origin argument, the output will report the number of days since Jan 1, 1970 instead of a date.
hcholidays$datecorrector <- as.Date(ifelse(is.na(hcholidays$daycorrector), as.Date(hcholidays$yeardate), ifelse(hcholidays$daycorrector == "Friday", (as.Date(hcholidays$yeardate) - 1), ifelse(hcholidays$daycorrector == "Monday", (as.Date(hcholidays$yeardate) + 1), ifelse(hcholidays$daycorrector == "Saturday", as.Date(hcholidays$yeardate), 0)))), origin = "1970-01-01")

#
hcholidays$trueday <- ifelse(is.na(hcholidays$daycorrector), hcholidays$day, hcholidays$daycorrector)


#Variable Holidays: Holidays that are set to be on a certain day after a certain number of weeks

#Martin Luther King
MLKD1 <- as.character(USMLKingsBirthday(as.numeric(PYR3)))
MLKD2 <- as.character(USMLKingsBirthday(as.numeric(PYR2)))
MLKD3 <- as.character(USMLKingsBirthday(as.numeric(PYR1)))
MLKD4 <- as.character(USMLKingsBirthday(as.numeric(CYR)))
MLKD5 <- as.character(USMLKingsBirthday(as.numeric(FYR1)))
MLKD6 <- as.character(USMLKingsBirthday(as.numeric(FYR2)))
MLKD7 <- as.character(USMLKingsBirthday(as.numeric(FYR3)))
NMLKD <- c("Martin Luther King Day")

#El Presidente's Day
PZD1 <- as.character(USPresidentsDay(as.numeric(PYR3)))
PZD2 <- as.character(USPresidentsDay(as.numeric(PYR2)))
PZD3 <- as.character(USPresidentsDay(as.numeric(PYR1)))
PZD4 <- as.character(USPresidentsDay(as.numeric(CYR)))
PZD5 <- as.character(USPresidentsDay(as.numeric(FYR1)))
PZD6 <- as.character(USPresidentsDay(as.numeric(FYR2)))
PZD7 <- as.character(USPresidentsDay(as.numeric(FYR3)))
NPZD <- c("Presidents Day")

#Good Friday Identifier
GFD1 <- as.character(GoodFriday(as.numeric(PYR1)))
GFD2 <- as.character(GoodFriday(as.numeric(PYR2)))
GFD3 <- as.character(GoodFriday(as.numeric(PYR3)))
GFD4 <- as.character(GoodFriday(as.numeric(CYR)))
GFD5 <- as.character(GoodFriday(as.numeric(FYR1)))
GFD6 <- as.character(GoodFriday(as.numeric(FYR2)))
GFD7 <- as.character(GoodFriday(as.numeric(FYR3)))
NGFD <- c("Good Friday")

#Memorial Day
MEMD1 <- as.character(USMemorialDay(as.numeric(PYR1)))
MEMD2 <- as.character(USMemorialDay(as.numeric(PYR2)))
MEMD3 <- as.character(USMemorialDay(as.numeric(PYR3)))
MEMD4 <- as.character(USMemorialDay(as.numeric(CYR)))
MEMD5 <- as.character(USMemorialDay(as.numeric(FYR1)))
MEMD6 <- as.character(USMemorialDay(as.numeric(FYR2)))
MEMD7 <- as.character(USMemorialDay(as.numeric(FYR3)))
NMEMD <- c("Memorial Day")

#Admissions day
BAUG <- "08-01"
AUG1 <- paste((PYR1), BAUG, sep = "-")
AUG2 <- paste((PYR2), BAUG, sep = "-")
AUG3 <- paste((PYR3), BAUG, sep = "-")
AUG4 <- paste((CYR), BAUG, sep = "-")
AUG5 <- paste((FYR1), BAUG, sep = "-")
AUG6 <- paste((FYR2), BAUG, sep = "-")
AUG7 <- paste((FYR3), BAUG, sep = "-")
AMD1 <- as.character(timeNthNdayInMonth(AUG1, nday = 5, nth = 3, format = "%Y-%m-%d"))
AMD2 <- as.character(timeNthNdayInMonth(AUG2, nday = 5, nth = 3, format = "%Y-%m-%d"))
AMD3 <- as.character(timeNthNdayInMonth(AUG3, nday = 5, nth = 3, format = "%Y-%m-%d"))
AMD4 <- as.character(timeNthNdayInMonth(AUG4, nday = 5, nth = 3, format = "%Y-%m-%d"))
AMD5 <- as.character(timeNthNdayInMonth(AUG5, nday = 5, nth = 3, format = "%Y-%m-%d"))
AMD6 <- as.character(timeNthNdayInMonth(AUG6, nday = 5, nth = 3, format = "%Y-%m-%d"))
AMD7 <- as.character(timeNthNdayInMonth(AUG7, nday = 5, nth = 3, format = "%Y-%m-%d"))
NAMD <- c("Admissions Day")

#Labor Day
LBD1 <- as.character(USLaborDay(as.numeric(PYR1)))
LBD2 <- as.character(USLaborDay(as.numeric(PYR2)))
LBD3 <- as.character(USLaborDay(as.numeric(PYR3)))
LBD4 <- as.character(USLaborDay(as.numeric(CYR)))
LBD5 <- as.character(USLaborDay(as.numeric(FYR1)))
LBD6 <- as.character(USLaborDay(as.numeric(FYR2)))
LBD7 <- as.character(USLaborDay(as.numeric(FYR3)))
NLBD <- c("Labor Day")

#Election Day logic...if it is even, it will function, if not, it will return NA to be filtered out
is.even <- function(x) x%%2 == 0
ELD1 <- ifelse(is.even(PYR3), as.character(USElectionDay(PYR3)), NA)
ELD2 <- ifelse(is.even(PYR2), as.character(USElectionDay(PYR2)), NA)
ELD3 <- ifelse(is.even(PYR1), as.character(USElectionDay(PYR1)), NA)
ELD4 <- ifelse(is.even(CYR), as.character(USElectionDay(CYR)), NA)
ELD5 <- ifelse(is.even(FYR1), as.character(USElectionDay(FYR1)), NA)
ELD6 <- ifelse(is.even(FYR2), as.character(USElectionDay(FYR2)), NA)
ELD7 <- ifelse(is.even(FYR3), as.character(USElectionDay(FYR3)), NA)
NELD1 <- ifelse(is.even(PYR3), as.character("General Election Day"), NA)
NELD2 <- ifelse(is.even(PYR2), as.character("General Election Day"), NA)
NELD3 <- ifelse(is.even(PYR1), as.character("General Election Day"), NA)
NELD4 <- ifelse(is.even(CYR), as.character("General Election Day"), NA)
NELD5 <- ifelse(is.even(FYR1), as.character("General Election Day"), NA)
NELD6 <- ifelse(is.even(FYR2), as.character("General Election Day"), NA)
NELD7 <- ifelse(is.even(FYR3), as.character("General Election Day"), NA)

#Thanksgiving Day
TXD1 <- as.character(USThanksgivingDay(as.numeric(PYR1)))
TXD2 <- as.character(USThanksgivingDay(as.numeric(PYR2)))
TXD3 <- as.character(USThanksgivingDay(as.numeric(PYR3)))
TXD4 <- as.character(USThanksgivingDay(as.numeric(CYR)))
TXD5 <- as.character(USThanksgivingDay(as.numeric(FYR1)))
TXD6 <- as.character(USThanksgivingDay(as.numeric(FYR2)))
TXD7 <- as.character(USThanksgivingDay(as.numeric(FYR3)))
NTXD <- c("Thanksgiving Day")

names <- c("Martin Luther King Day", "Presidents Day", "Good Friday", "Memorial Day", "Admissions Day", "Labor Day", "Thanksgiving Day")


#Combining the variable holidays into a dataframe
varholidays <- 0
varholidays$datecorrector <- rbind(MLKD1, MLKD2, MLKD3, MLKD4,  MLKD5, MLKD6, MLKD7, PZD1, PZD2, PZD3, PZD4, PZD5, PZD6, PZD7, GFD1, GFD2, GFD3, GFD4, GFD5, GFD6, GFD7, MEMD1, MEMD2, MEMD3, MEMD4, MEMD5, MEMD6, MEMD7, AMD1, AMD2, AMD3, AMD4, AMD5, AMD6, AMD7, LBD1, LBD2, LBD3, LBD4, LBD5, LBD6, LBD7, ELD1, ELD2, ELD3, ELD4, ELD5, ELD6, ELD7, TXD1, TXD2, TXD3, TXD4, TXD5, TXD6, TXD7)
varholidays <- varholidays[-1]
varholidays$names <- rbind(NMLKD, NMLKD, NMLKD, NMLKD, NMLKD, NMLKD, NMLKD, NPZD, NPZD, NPZD, NPZD, NPZD, NPZD, NPZD, NGFD, NGFD, NGFD, NGFD, NGFD, NGFD, NGFD, NMEMD, NMEMD, NMEMD, NMEMD, NMEMD, NMEMD, NMEMD, NAMD, NAMD, NAMD, NAMD, NAMD, NAMD, NAMD, NLBD, NLBD, NLBD, NLBD, NLBD, NLBD, NLBD, NELD1, NELD2, NELD3, NELD4, NELD5, NELD6, NELD7, NTXD, NTXD, NTXD, NTXD, NTXD, NTXD, NTXD)
varholidays <- as.data.frame(varholidays)
#Assigning day of week to the holiday dates
varholidays$trueday <- weekdays(as.Date(varholidays$datecorrector))
rownames(varholidays) <- c()
#Removing NA rows (generated when it is not an election year)
varholidays <- varholidays[complete.cases(varholidays), ]

#Selecting the useful columns
hcholidays <- select(hcholidays, datecorrector, trueday, names)

#Stacking the Hard Coded and Variable holidays on top of each other
Completeholidays <- rbind(hcholidays, varholidays)
