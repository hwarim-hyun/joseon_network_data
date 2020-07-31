#################################################
#make master table
#################################################
setwd("D:/SNU/CLS/dataScience/extraction")
library("tidyverse")

#load the files
year <- read.csv("YearExtraction.csv", header = T, sep = "|", fileEncoding = "UTF-16", stringsAsFactors = F)
test <-read.csv("TestExtraction.csv", header = T, sep = "|", fileEncoding = "UTF-16", stringsAsFactors = F)

multName <- readxl::read_excel(path = "MultiNameExtraction.xlsx", sheet = "Sheet1", col_names = TRUE)
name <-readxl::read_excel(path = "NameExtraction.xlsx", sheet = "Sheet1", col_names = TRUE)
name2 <-readxl::read_excel(path = "NameExtraction2.xlsx", sheet = "Sheet1", col_names = TRUE)
residence <-readxl::read_excel(path = "ResidenceExtraction.xlsx", sheet = "Sheet1", col_names = TRUE)
choronym <- readxl::read_excel(path = "ChoronymExtraction.xlsx", sheet = "Sheet1", col_names = TRUE)

#load the files 2
family1 <-readxl::read_excel(path = "FamilyExtraction.xlsx", sheet = "Sheet3", col_names = TRUE)
family1_1 <- readxl::read_excel(path = "FamilyExtraction.xlsx", sheet = "Sheet2", col_names = TRUE)
family2 <-readxl::read_excel(path = "FamilyExtraction_new.xlsx", sheet = "Sheet3", col_names = TRUE)

#rename the column of name
name <- rename(name, lastName = 성)
name <- rename(name, name = 성명, ja = 자, ho = 호, siho = 시호, chomyeong = 초명, gaemyeong = 개명)
name <- rename(name, ilmyeong = 일명, saho = 사호, bongho = 봉호, gaeja = 개자, choja = 초자, neungho = 능호, myoho = 묘호, choho = 초호, jaho = 자호)
name <- rename(name, chomyeong2 = 초명2, chomyeong1 = 초명1, bonmyeong = 본명 , gaemyeong2 = 개명2, fileName = 파일명)

#check the number of rows for each column
nrow(filter(name, !is.na(ja))) # 74682
nrow(filter(name, !is.na(ho))) #3531
nrow(filter(name, !is.na(siho))) #1222
nrow(filter(name, !is.na(chomyeong))) #1072
nrow(filter(name, !is.na(gaemyeong))) #935
nrow(filter(name, !is.na(ilmyeong))) #584
nrow(filter(name, !is.na(saho))) #0
nrow(filter(name, !is.na(bongho))) #0
nrow(filter(name, !is.na(gaeja))) #0
nrow(filter(name, !is.na(choja))) #0
nrow(filter(name, !is.na(neungho))) #0
nrow(filter(name, !is.na(myoho))) #0
nrow(filter(name, !is.na(choho))) #0
nrow(filter(name, !is.na(jaho))) #0

#make masterTable
#part 1. combine all names
master <- select(name, FileName, ID, UCI, LastName, Name, Ja, Ho, Siho, Chomyeong, Gaemyeong, Ilmyeong)
master <- rename(master, Chomyung = Chomyeong, Gaemyung = Gaemyeong, Ilmyung = Ilmyeong)
master <- rename(master, fileName = FileName, lastName = LastName, name = Name, ja = Ja, ho = Ho,
                 siho = Siho, chomyung = Chomyung, gaemyung = Gaemyung, ilmyung = Ilmyung)

#check difference between chomyung2 in master vs multName
temp1 <- filter(name2, !is.na(chomyung2)) #2 
temp2 <- filter(multName, !is.na(chomyung2)) #5
#no intersection

#check difference between gaemyung2 in master vs multName
temp1 <- filter(name2, !is.na(gaemyung2)) #1 
temp2 <- filter(multName, !is.na(gaemyung2)) #17
#no intersection

#common column in master & multName: gaemyung, ilmyung, ja, ho, chomyung

#tidy chomyung
#merge chomyung with chomyung1
chomyung <- select(name2, ID, chomyung1) %>%
  filter(!is.na(chomyung1)) %>%
  rename(chomyung = chomyung1)

master2 <- select(master, ID, chomyung)
master2 <- filter(master2, !(ID == "EXM_SA_6JOc_1885_037173"))
chomyung <- rbind(master2, chomyung)
chomyung <- arrange(chomyung, ID)
#combine chomyung2
temp1 <- select(name2, ID, chomyung2) %>% filter(!is.na(chomyung2)) #2 
temp2 <- select(multName, ID, chomyung2) %>% filter(!is.na(chomyung2)) #5
chomyung2 <- rbind(temp1, temp2)

#combine all chomyungs
chomyungs <- select(multName, ID, chomyung3, chomyung4)
chomyung <- left_join(chomyung, chomyung2, by = "ID")
chomyungs <- left_join(chomyung, chomyungs, by = "ID")

#tidy gaemyung
temp1 <- select(name2, ID, gaemyung2) %>% filter(!is.na(gaemyung2)) #1 
temp2 <- select(multName, ID, gaemyung2) %>% filter(!is.na(gaemyung2)) #17
master2 <- select(master, ID, gaemyung)
gaemyung2 <- rbind(temp1, temp2)
gaemyungs <- left_join(master2, gaemyung2, by = "ID")
gaemyungs <- arrange(gaemyungs, ID)

#tidy ilmyung
temp1 <- select(master, ID, ilmyung)
temp2 <- select(multName, ID, ilmyung2, ilmyung3) %>% filter(!is.na(ilmyung2))
ilmyungs <- left_join(temp1, temp2, by = "ID")
ilmyungs <- arrange(ilmyungs, ID)

#tidy ja
temp1 <- select(master, ID, ja)
temp2 <- select(multName, ID, ja2) %>% filter(!is.na(ja2))
jas <- left_join(temp1, temp2, by = "ID") %>% arrange(ID)

#tidy ho
temp1 <- select(master, ID, ho)
temp2 <- select(multName, ID, ho2, ho3) %>% filter(!is.na(ho2))
hos <- left_join(temp1, temp2, by = "ID") %>% arrange(ID)

#combine all names
name3 <- select(name2, ID, saho, bongho, gaeja, choja, neungho, myoho, choho, jaho, bonmyung)
master <- select(master, fileName, ID, UCI, lastName, name, siho)
master <- left_join(master, jas, by = "ID")
master <- left_join(master, hos, by = "ID")
master <- left_join(master, chomyungs, by = "ID")
master <- left_join(master, gaemyungs, by = "ID")
master <- left_join(master, ilmyungs, by = "ID")
master <- left_join(master, name3, by = "ID") %>% arrange(ID)

#combine names with test
master <- left_join(master, test, by = "ID")

#combine master with year
master <- left_join(master, year, by = "ID")

#combine master with choronym
master <- left_join(master, choronym, by = "ID")

#combine master with residence
master <- left_join(master, residence, by = "ID")
master <- arrange(master, ID)

writexl::write_xlsx(master, path = "MasterTable.xlsx", col_names = T, format_headers = T)



####################################################
#identify unique person
####################################################
#동명이인이 얼마나 있을지 살펴보기
uci <- select(master, ID, UCI, name, choronym)
nrow(distinct(uci, UCI)) #90591
nrow(distinct(uci, name)) #79133
nrow(distinct(master, name, choronym)) #88353

#이름 중복이 어떤 식으로 이루어지는지 살펴보기
uci_distinct <- distinct(uci, UCI)
name_choronym_distinct <- distinct(uci, name, choronym)
all_distinct <- distinct(uci, name, choronym, UCI) #91675

#same uci, different xml : 28314 (이름, 본관은 그들끼리 같을 것이라 추정)
uci_distinct2 <- left_join(uci_distinct, uci, by = "UCI") %>% arrange(UCI) %>% group_by(UCI) %>% filter(n() > 1)


#same name&choronym pair, different xml : 31881 (uci는 다를수 있음) 
name_choronym_distinct2 <- left_join(name_choronym_distinct, uci, by = c("name", "choronym")) %>%
  group_by(name, choronym) %>% filter(n()>1)
#same uci, name&choronym pair, different xml : 26378
all_distinct2 <- left_join(all_distinct, uci, by = c("UCI", "name", "choronym")) %>% 
  group_by(UCI, name, choronym) %>% filter(n()>1)

only_uci <- anti_join(uci_distinct2, all_distinct2, by = c("UCI", "name", "choronym", "ID")) #1936
only_name <- anti_join(name_choronym_distinct2, all_distinct2, by = c("UCI", "name", "choronym", "ID")) #5503

#write xslx1
library(openxlsx)
wb = createWorkbook()

addWorksheet(wb, "Sheet1")
addWorksheet(wb, "Sheet2")
addWorksheet(wb, "Sheet3")
addWorksheet(wb, "Sheet4")
addWorksheet(wb, "Sheet5")
addWorksheet(wb, "Sheet6")
addWorksheet(wb, "Sheet7")
addWorksheet(wb, "Sheet8")


writeData(wb, sheet = "Sheet1", x = uci_distinct, colNames = T)
writeData(wb, sheet = "Sheet2", x = name_choronym_distinct, colNames = T)
writeData(wb, x = all_distinct, sheet = "Sheet3", colNames = T)
writeData(wb, x = uci_distinct2, sheet = "Sheet4", colNames = T)
writeData(wb, x = name_choronym_distinct2, sheet = "Sheet5", colNames = T)
writeData(wb, x = all_distinct2, sheet = "Sheet6", colNames = T)
writeData(wb, x = only_uci, sheet = "Sheet7", colNames = T)
writeData(wb, x = only_name, sheet = "Sheet8", colNames = T)

saveWorkbook(wb, "IdentifyUniquePerson2.xlsx")

#########################################
identifyUniquePerson_UCI
#########################################
#same uci more than 2 times
uci_distinct3 <- uci_distinct2 %>% group_by(UCI) %>% filter(n()>2)
#same uci&name, but different choronym (deleted ID for clearification) (at least 2 for each uci&name pair)
uci_distinct4 <- uci_distinct2 %>% select(UCI, name, choronym) %>% group_by(UCI, name) %>% distinct() %>% filter(n()>1)
#for each uci&name in uci_distinct4, show all the cases that one of their choronyms appears in more than one xml file
uci_distinct4_1 <- uci_distinct4 %>% left_join(uci_distinct2, by = c("UCI", "name", "choronym")) %>% group_by(UCI, name) %>%
  filter(n()>2)
uci_distinct5 <- uci_distinct2 %>% select(UCI, name, choronym) %>% group_by(UCI, choronym) %>% distinct() %>% filter(n()>1)
uci_distinct5_1 <- uci_distinct5 %>% left_join(uci_distinct2, by = c("UCI", "name", "choronym")) %>% group_by(UCI, choronym) %>%
  filter(n()>2)

uci_distinct4_2 <- uci_distinct4_1 %>% left_join(master, by = c("UCI", "name", "choronym", "ID"))
uci_distinct5_2 <- uci_distinct5_1 %>% left_join(master, by = c("UCI", "name", "choronym", "ID"))

uci_distinct6 <- readxl::read_excel(path = "IdentifyUniquePerson_verHun.xlsx", sheet = "Sheet10", col_names = TRUE) %>% select(UCI, ID, name, choronym)
uci_distinct6_1 <- uci_distinct6 %>% left_join(master, by =  c("UCI", "name", "choronym", "ID"))

#check whether certain columns can be deleted
nrow(filter(uci_distinct4_2, !is.na(bonmyung))) #0
nrow(filter(uci_distinct4_2, !is.na(jaho))) #0
nrow(filter(uci_distinct4_2, !is.na(choho))) #0
nrow(filter(uci_distinct4_2, !is.na(myoho))) #0
nrow(filter(uci_distinct4_2, !is.na(neungho))) #0
nrow(filter(uci_distinct4_2, !is.na(choja))) #0
nrow(filter(uci_distinct4_2, !is.na(gaeja))) #0
nrow(filter(uci_distinct4_2, !is.na(bongho))) #0
nrow(filter(uci_distinct4_2, !is.na(saho))) #0
nrow(filter(uci_distinct4_2, !is.na(ilmyung3))) #0
nrow(filter(uci_distinct4_2, !is.na(ilmyung2))) #0
nrow(filter(uci_distinct4_2, !is.na(ilmyung))) #1
nrow(filter(uci_distinct4_2, !is.na(gaemyung2))) #0
nrow(filter(uci_distinct4_2, !is.na(gaemyung))) #6
nrow(filter(uci_distinct4_2, !is.na(chomyung4))) #0
nrow(filter(uci_distinct4_2, !is.na(chomyung3))) #0
nrow(filter(uci_distinct4_2, !is.na(chomyung2))) #0
nrow(filter(uci_distinct4_2, !is.na(chomyung))) #1
nrow(filter(uci_distinct4_2, !is.na(ho3))) #0
nrow(filter(uci_distinct4_2, !is.na(ho2))) #0
nrow(filter(uci_distinct4_2, !is.na(ho))) #4
nrow(filter(uci_distinct4_2, !is.na(ja2))) #1
nrow(filter(uci_distinct4_2, !is.na(ja))) #238
nrow(filter(uci_distinct4_2, !is.na(siho))) #3

uci_distinct4_2 <- uci_distinct4_2 %>% select(UCI, name, choronym, ID, fileName, lastName, siho, ja, ja2, ho, chomyung, gaemyung, ilmyung, exmID, exmName, birthyear, deathyear, testyear, Residence)

nrow(filter(uci_distinct5_2, !is.na(bonmyung))) #0
nrow(filter(uci_distinct5_2, !is.na(jaho))) #0
nrow(filter(uci_distinct5_2, !is.na(choho))) #0
nrow(filter(uci_distinct5_2, !is.na(myoho))) #0
nrow(filter(uci_distinct5_2, !is.na(neungho))) #0
nrow(filter(uci_distinct5_2, !is.na(choja))) #0
nrow(filter(uci_distinct5_2, !is.na(gaeja))) #0
nrow(filter(uci_distinct5_2, !is.na(bongho))) #0
nrow(filter(uci_distinct5_2, !is.na(saho))) #0
nrow(filter(uci_distinct5_2, !is.na(ilmyung3))) #0
nrow(filter(uci_distinct5_2, !is.na(ilmyung2))) #0
nrow(filter(uci_distinct5_2, !is.na(ilmyung))) #5
nrow(filter(uci_distinct5_2, !is.na(gaemyung2))) #0
nrow(filter(uci_distinct5_2, !is.na(gaemyung))) #27
nrow(filter(uci_distinct5_2, !is.na(chomyung4))) #0
nrow(filter(uci_distinct5_2, !is.na(chomyung3))) #0
nrow(filter(uci_distinct5_2, !is.na(chomyung2))) #0
nrow(filter(uci_distinct5_2, !is.na(chomyung))) #22
nrow(filter(uci_distinct5_2, !is.na(ho3))) #0
nrow(filter(uci_distinct5_2, !is.na(ho2))) #0
nrow(filter(uci_distinct5_2, !is.na(ho))) #14
nrow(filter(uci_distinct5_2, !is.na(ja2))) #0
nrow(filter(uci_distinct5_2, !is.na(ja))) #199
nrow(filter(uci_distinct5_2, !is.na(siho))) #3

uci_distinct5_2 <- uci_distinct5_2 %>% select(UCI, name, choronym, ID, fileName, lastName, siho, ja, ho, chomyung, gaemyung, ilmyung, exmID, exmName, birthyear, deathyear, testyear, Residence)

nrow(filter(uci_distinct6_1, !is.na(bonmyung))) #0
nrow(filter(uci_distinct6_1, !is.na(jaho))) #0
nrow(filter(uci_distinct6_1, !is.na(choho))) #0
nrow(filter(uci_distinct6_1, !is.na(myoho))) #0
nrow(filter(uci_distinct6_1, !is.na(neungho))) #0
nrow(filter(uci_distinct6_1, !is.na(choja))) #0
nrow(filter(uci_distinct6_1, !is.na(gaeja))) #0
nrow(filter(uci_distinct6_1, !is.na(bongho))) #0
nrow(filter(uci_distinct6_1, !is.na(saho))) #0
nrow(filter(uci_distinct6_1, !is.na(ilmyung3))) #0
nrow(filter(uci_distinct6_1, !is.na(ilmyung2))) #0
nrow(filter(uci_distinct6_1, !is.na(ilmyung))) #1
nrow(filter(uci_distinct6_1, !is.na(gaemyung2))) #0
nrow(filter(uci_distinct6_1, !is.na(gaemyung))) #1
nrow(filter(uci_distinct6_1, !is.na(chomyung4))) #0
nrow(filter(uci_distinct6_1, !is.na(chomyung3))) #0
nrow(filter(uci_distinct6_1, !is.na(chomyung2))) #0
nrow(filter(uci_distinct6_1, !is.na(chomyung))) #3
nrow(filter(uci_distinct6_1, !is.na(ho3))) #0
nrow(filter(uci_distinct6_1, !is.na(ho2))) #2
nrow(filter(uci_distinct6_1, !is.na(ho))) #2
nrow(filter(uci_distinct6_1, !is.na(ja2))) #0
nrow(filter(uci_distinct6_1, !is.na(ja))) #67
nrow(filter(uci_distinct6_1, !is.na(siho))) #0

uci_distinct6_1 <- uci_distinct6_1 %>% select(UCI, name, choronym, ID, fileName, lastName, ja, ho, ho2, chomyung, gaemyung, ilmyung, exmID, exmName, birthyear, deathyear, testyear, Residence)

#write xslx2
library(openxlsx)
wb2 = createWorkbook()

addWorksheet(wb2, "Sheet1")
addWorksheet(wb2, "Sheet2")
addWorksheet(wb2, "Sheet3")
addWorksheet(wb2, "Sheet4")
addWorksheet(wb2, "Sheet5")
addWorksheet(wb2, "Sheet6")
addWorksheet(wb2, "Sheet7")
addWorksheet(wb2, "Sheet8")
addWorksheet(wb2, "Sheet9")

hs <- createStyle(textDecoration = "BOLD")

writeData(wb2, sheet = "Sheet1", x = uci_distinct3, colNames = T, headerStyle = hs)
writeData(wb2, sheet = "Sheet2", x = uci_distinct4, colNames = T, headerStyle = hs)
writeData(wb2, x = uci_distinct4_1, sheet = "Sheet3", colNames = T, headerStyle = hs)
writeData(wb2, x = uci_distinct5, sheet = "Sheet4", colNames = T, headerStyle = hs)
writeData(wb2, x = uci_distinct5_1, sheet = "Sheet5", colNames = T, headerStyle = hs)
writeData(wb2, x = uci_distinct4_2, sheet = "Sheet6", colNames = T, headerStyle = hs)
writeData(wb2, x = uci_distinct5_2, sheet = "Sheet7", colNames = T, headerStyle = hs)
writeData(wb2, x = uci_distinct6, sheet = "Sheet8", colNames = T, headerStyle = hs)
writeData(wb2, x = uci_distinct6_1, sheet = "Sheet9", colNames = T, headerStyle = hs)

saveWorkbook(wb2, "IdentifyUniquePerson_UCI2.xlsx")



######################################################3
#family members : father
#######################################################
father <-readxl::read_excel(path = "FatherExtractionFinal.xlsx", sheet = "Sheet1", col_names = TRUE, skip = 1)
father <- select(father, ID, father_exmid, father_name)
father2 <- readxl::read_excel(path = "MultiNameFather2.xlsx", sheet = "Sheet1", col_names = TRUE) %>% select(-father_exmid)

father <- left_join(father, father2, by = c("ID", "father_name"))
nrow(filter(father, !is.na(father_exmid))) #15597
nrow(filter(father, !is.na(father_name))) #99971

#not available father
father_NA <- father %>% filter(is.na(father_exmid) & is.na(father_name)) #6161

father %>% filter(is.na(father_exmid) & !is.na(father_name)) %>% nrow() #84374

#certain father_1 : sonID, sonName, fatherID, fatherName
just_name <- name %>% select(ID, name)
father_certain <- father %>% filter(!is.na(father_exmid)) %>% select(ID, father_exmid, father_name) %>% 
  left_join(just_name, by = "ID") %>% rename(sonID = ID, sonName = name, fatherID = father_exmid, fatherName = father_name)

#uncertain father_1 : sonID, sonName, fatherID, fatherName
father_uncertain <- father %>% filter(is.na(father_exmid) & !is.na(father_name)) %>% select(ID, father_name) %>%
  left_join(just_name, by = c("father_name" = "name")) %>% rename(sonID = ID.x, fatherName = father_name, fatherID = ID.y) %>%
  left_join(just_name, by = c("sonID" = "ID")) %>% rename(sonName = name) %>% select(sonName, sonID, fatherName, fatherID)

father_uncertain0 <- father_uncertain %>% filter(is.na(fatherID))
father_uncertain1 <- father_uncertain %>% filter(!is.na(fatherID)) %>% group_by(sonID) %>% filter(n()==1)
father_uncertain2 <- father_uncertain %>% filter(!is.na(fatherID)) %>% group_by(sonID) %>% filter(n()>1)
father_uncertain2 %>% distinct(sonID) %>% nrow() #12817

#case1: father candidate exists in only one xml
father_uncertain1_1 <- father_uncertain1 %>% left_join(year, by = c("sonID" = "ID"))
father_uncertain1_1 <- father_uncertain1_1 %>% rename(sonBirth = birthyear, sonDeath = deathyear, sonTest = testyear) %>%
  left_join(year, by = c("fatherID" = "ID")) %>% rename(fatherBirth = birthyear, fatherDeath = deathyear, fatherTest = testyear)

#change 9999 to NA to compare year between son and father
father_uncertain1_1[father_uncertain1_1==9999] <- NA

#filter by year
father_uncertain1_1 <- father_uncertain1_1 %>% mutate(birthDif = sonBirth - fatherBirth,
                                                      deathDif = sonDeath - fatherDeath,
                                                      testDif = sonTest - fatherTest)
father_uncertain1_1 <- father_uncertain1_1 %>% filter((birthDif>0 & birthDif <100) | is.na(birthDif)) %>%
  filter((testDif>0 & testDif <100) | is.na(testDif))  %>% filter(!(!is.na(sonDeath) & !is.na(fatherBirth) & (fatherBirth>sonDeath)))

father_uncertain1_1 %>% filter(is.na(birthDif) & is.na(deathDif) & is.na(testDif)) %>% nrow() #282

#filter by choronym
father_uncertain1_2 <- father_uncertain1_1 %>% select(sonName:fatherID, birthDif, testDif) %>% 
  left_join(choronym, by = c("sonID" = "ID")) %>% rename(sonChoronym = choronym) %>%
  left_join(choronym, by = c("fatherID" = "ID")) %>% rename(fatherChoronym = choronym)

#change 미상(未詳) to NA to compare choronym between son and father
father_uncertain1_2[father_uncertain1_2 == "미상(未詳)"] <- NA
father_uncertain1_2 <- father_uncertain1_2 %>% filter((sonChoronym == fatherChoronym) == T | is.na(sonChoronym == fatherChoronym))

#case2: father candidate exists in more than one xml
father_uncertain2_1 <- father_uncertain2 %>% left_join(year, by = c("sonID" = "ID"))
father_uncertain2_1 <- father_uncertain2_1 %>% rename(sonBirth = birthyear, sonDeath = deathyear, sonTest = testyear) %>%
  left_join(year, by = c("fatherID" = "ID")) %>% rename(fatherBirth = birthyear, fatherDeath = deathyear, fatherTest = testyear)

#change 9999 to NA to compare year between son and father
father_uncertain2_1[father_uncertain2_1==9999] <- NA

#filter by year
father_uncertain2_1 <- father_uncertain2_1 %>% mutate(birthDif = sonBirth - fatherBirth,
                                                      deathDif = sonDeath - fatherDeath,
                                                      testDif = sonTest - fatherTest)
father_uncertain2_1 <- father_uncertain2_1 %>% filter((birthDif>0 & birthDif <100) | is.na(birthDif)) %>%
  filter((testDif>0 & testDif <100) | is.na(testDif)) %>% filter(!(!is.na(sonDeath) & !is.na(fatherBirth) & (fatherBirth>sonDeath)))

father_uncertain2_1 %>% distinct(sonID) %>% nrow() #9411

father_uncertain2_2 <- father_uncertain2_1 %>% select(sonName:fatherID, birthDif, testDif) %>% 
  left_join(choronym, by = c("sonID" = "ID")) %>% rename(sonChoronym = choronym) %>%
  left_join(choronym, by = c("fatherID" = "ID")) %>% rename(fatherChoronym = choronym)

#change 미상(未詳) to NA to compare choronym between son and father
father_uncertain2_2[father_uncertain2_2 == "미상(未詳)"] <- NA
father_uncertain2_2 <- father_uncertain2_2 %>% filter((sonChoronym == fatherChoronym) == T | is.na(sonChoronym == fatherChoronym))

father_uncertain2_2 %>% distinct(sonID) %>% nrow() #6118
#when filtered by year and choronym, only one candidate has left!
father_uncertain2_2_1 <- father_uncertain2_2 %>% group_by(sonID) %>% filter(n()==1)

#when filtered by year and choronym, there are still more than two candidates
father_uncertain2_2_2 <- father_uncertain2_2 %>% group_by(sonID) %>% filter(n()>1)
father_uncertain2_2_2 %>% distinct(sonID) %>% nrow() #3089

#make xlsx file
wb3 <- createWorkbook()

addWorksheet(wb3, "Sheet1")
addWorksheet(wb3, "Sheet2")
addWorksheet(wb3, "Sheet3")
addWorksheet(wb3, "Sheet4")
addWorksheet(wb3, "Sheet5")
addWorksheet(wb3, "Sheet6")
addWorksheet(wb3, "Sheet7")
addWorksheet(wb3, "Sheet8")
addWorksheet(wb3, "Sheet9")
addWorksheet(wb3, "Sheet10")
addWorksheet(wb3, "Sheet11")


writeData(wb3, sheet = "Sheet1", x = father_certain, colNames = T)
writeData(wb3, sheet = "Sheet2", x = father_NA, colNames = T)
writeData(wb3, x = father_uncertain0, sheet = "Sheet3", colNames = T)
writeData(wb3, x = father_uncertain1, sheet = "Sheet4", colNames = T)
writeData(wb3, x = father_uncertain1_1, sheet = "Sheet5", colNames = T)
writeData(wb3, x = father_uncertain1_2, sheet = "Sheet6", colNames = T)
writeData(wb3, x = father_uncertain2, sheet = "Sheet7", colNames = T)
writeData(wb3, x = father_uncertain2_1, sheet = "Sheet8", colNames = T)
writeData(wb3, x = father_uncertain2_2, sheet = "Sheet9", colNames = T)
writeData(wb3, x = father_uncertain2_2_1, sheet = "Sheet10", colNames = T)
writeData(wb3, x = father_uncertain2_2_2, sheet = "Sheet11", colNames = T)

saveWorkbook(wb3, "IdentifyFather.xlsx")

#######################################
#father2
#######################################
chomyung <- select(chomyung, ID, chomyung)

#father name appears in chomyung: 510
father_cho <- father %>% filter(is.na(father_exmid) & !is.na(father_name)) %>% select(ID, father_name) %>%
  left_join(chomyung, by = c("father_name" = "chomyung")) %>% rename(sonID = ID.x, fatherName = father_name, fatherID = ID.y) %>%
  left_join(just_name, by = c("sonID" = "ID")) %>% rename(sonName = name) %>% select(sonName, sonID, fatherName, fatherID) %>%
  filter(!is.na(fatherID))
father_cho$reason <-"초명"

#father name appears in chomyung2: 9
father_cho2 <- father %>% filter(is.na(father_exmid) & !is.na(father_name)) %>% select(ID, father_name) %>%
  left_join(chomyung2, by = c("father_name" = "chomyung2")) %>% rename(sonID = ID.x, fatherName = father_name, fatherID = ID.y) %>%
  left_join(just_name, by = c("sonID" = "ID")) %>% rename(sonName = name) %>% select(sonName, sonID, fatherName, fatherID) %>%
  filter(!is.na(fatherID))
father_cho2$reason <-"초명2"

#father name appears in chomyung3: 0
father_cho3 <- father %>% filter(is.na(father_exmid) & !is.na(father_name)) %>% select(ID, father_name) %>%
  left_join(select(chomyungs, ID, chomyung3), by = c("father_name" = "chomyung3")) %>% rename(sonID = ID.x, fatherName = father_name, fatherID = ID.y) %>%
  left_join(just_name, by = c("sonID" = "ID")) %>% rename(sonName = name) %>% select(sonName, sonID, fatherName, fatherID) %>%
  filter(!is.na(fatherID))

#father name appears in chomyung4: 0
father_cho4 <- father %>% filter(is.na(father_exmid) & !is.na(father_name)) %>% select(ID, father_name) %>%
  left_join(select(chomyungs, ID, chomyung4), by = c("father_name" = "chomyung4")) %>% rename(sonID = ID.x, fatherName = father_name, fatherID = ID.y) %>%
  left_join(just_name, by = c("sonID" = "ID")) %>% rename(sonName = name) %>% select(sonName, sonID, fatherName, fatherID) %>%
  filter(!is.na(fatherID))

#father name appears in gaemyung #545
father_gae <- father %>% filter(is.na(father_exmid) & !is.na(father_name)) %>% select(ID, father_name) %>%
  left_join(select(gaemyungs, ID, gaemyung), by = c("father_name" = "gaemyung")) %>% rename(sonID = ID.x, fatherName = father_name, fatherID = ID.y) %>%
  left_join(just_name, by = c("sonID" = "ID")) %>% rename(sonName = name) %>% select(sonName, sonID, fatherName, fatherID) %>%
  filter(!is.na(fatherID))
father_gae$reason <- "개명"

#father name appears in gaemyung2 #10
father_gae2 <- father %>% filter(is.na(father_exmid) & !is.na(father_name)) %>% select(ID, father_name) %>%
  left_join(select(gaemyungs, ID, gaemyung2), by = c("father_name" = "gaemyung2")) %>% rename(sonID = ID.x, fatherName = father_name, fatherID = ID.y) %>%
  left_join(just_name, by = c("sonID" = "ID")) %>% rename(sonName = name) %>% select(sonName, sonID, fatherName, fatherID) %>%
  filter(!is.na(fatherID))
father_gae2$reason <- "개명2"

#father name appears in ilmyung: 159
father_il <- father %>% filter(is.na(father_exmid) & !is.na(father_name)) %>% select(ID, father_name) %>%
  left_join(select(ilmyungs, ID, ilmyung), by = c("father_name" = "ilmyung")) %>% rename(sonID = ID.x, fatherName = father_name, fatherID = ID.y) %>%
  left_join(just_name, by = c("sonID" = "ID")) %>% rename(sonName = name) %>% select(sonName, sonID, fatherName, fatherID) %>%
  filter(!is.na(fatherID))
father_il$reason <- "일명"

#father name appears in ilmyung2 : 0
father_il2 <- father %>% filter(is.na(father_exmid) & !is.na(father_name)) %>% select(ID, father_name) %>%
  left_join(select(ilmyungs, ID, ilmyung2), by = c("father_name" = "ilmyung2")) %>% rename(sonID = ID.x, fatherName = father_name, fatherID = ID.y) %>%
  left_join(just_name, by = c("sonID" = "ID")) %>% rename(sonName = name) %>% select(sonName, sonID, fatherName, fatherID) %>%
  filter(!is.na(fatherID))
father_il2$reason <- "일명"

#father name appears in ilmyung2 : 0
father_il3 <- father %>% filter(is.na(father_exmid) & !is.na(father_name)) %>% select(ID, father_name) %>%
  left_join(select(ilmyungs, ID, ilmyung3), by = c("father_name" = "ilmyung3")) %>% rename(sonID = ID.x, fatherName = father_name, fatherID = ID.y) %>%
  left_join(just_name, by = c("sonID" = "ID")) %>% rename(sonName = name) %>% select(sonName, sonID, fatherName, fatherID) %>%
  filter(!is.na(fatherID))
father_il2$reason <- "일명"

#rbind
father_chogaeil <- rbind(father_cho, father_cho2, father_gae, father_gae2, father_il) %>% arrange(sonName)

#write xlsx
wb4 <- createWorkbook()
addWorksheet(wb4, "Sheet1")
writeData(wb4, sheet = "Sheet1", x = father_chogaeil, colNames = T, headerStyle = hs)
saveWorkbook(wb4, "IdentifyFather_chomyung,gaemyung,ilmyung.xlsx")

#################################
#find family UCI
#################################
uci <- select(master, ID, UCI)

family1_1 <- left_join(family1, uci, by=c("family_exmid" = "ID"))
family1_1 <- family1_1 %>% rename(family_uci = UCI)
family2_1 <- left_join(family2, uci, by=c("family_exmid" = "ID"))
family2_1 <- family2_1 %>% rename(family_uci = UCI)

#write xlsx
wb5 <- createWorkbook()
addWorksheet(wb5, "Sheet1")
addWorksheet(wb5, "Sheet2")
writeData(wb5, sheet = "Sheet1", x = family1_1, colNames = T, headerStyle = hs)
writeData(wb5, sheet = "Sheet2", x = family2_1, colNames = T, headerStyle = hs)
saveWorkbook(wb5, "FamilyExtraction_UCI.xlsx")

#############################
#check father-son, maternal grandson, and son-in-law
#############################

#use family1(original xml data)
#grandfather also has id
fatherRel_1 <- filter(family1, relation=="부")
MGrandfatherRel <- filter(family1, relation == "외조부")
soninlawRel <- filter(family1, relation == "처부")

has_both <- intersect(select(fatherRel_1, fileID), select(MGrandfatherRel, fileID))
has_both <- has_both %>% left_join(fatherRel_1, by = "fileID")
has_both <- has_both %>% rename(father_name = family_name, father_ID  = family_exmid)
has_both <- has_both %>% left_join(select(MGrandfatherRel, fileID, relation, family_name, family_exmid), by = "fileID")
has_both <- has_both %>% rename(Mgrandfather_name = family_name, Mgrandfather_ID  = family_exmid)

has_all <- intersect(rename(select(has_bothas_both %>% rename(father_name = family_name, father_ID  = family_exmid)h, father_ID), fileID = father_ID), select(soninlawRel, fileID))
has_son_in_law <- TRUE

has_all <- cbind(has_all, has_son_in_law)

has_all <- left_join(has_both, has_all, by = c("father_ID" = "fileID"))
has_all[is.na(has_all)] <- FALSE

#grandfather does not have an id
MgrandfatherRel_2 <- filter(family1_1, relation == "외조부")
soninlawRel_2 <- filter(family1_1, relation == "처부")

has_both2 <- intersect(select(fatherRel_1, fileID), select(MgrandfatherRel_2, fileID))
has_both2 <- has_both2 %>% left_join(fatherRel_1, by = "fileID")
has_both2 <- has_both2 %>% rename(father_name = family_name, father_ID  = family_exmid)
has_both2 <- has_both2 %>% left_join(MgrandfatherRel_2, by = c("fileID", "choronym", "self_name"))
has_both2 <- has_both2 %>% rename(mgrandfather_name = family_name, mgrandfather_exmid = family_exmid)
has_both2 <- has_both2 %>% left_join(select(soninlawRel_2, fileID, relation, family_name, family_exmid), by = c("father_ID" = "fileID"))
has_both2 <- has_both2 %>% rename(mgrandfather_ID = mgrandfather_exmid, 
                                  fatherinlaw_name = family_name, fatherinlaw_ID = family_exmid)
has_both2 <- has_both2 %>% select(fileID, choronym, self_name, father_name, father_ID, 
                                  mgrandfather_name, mgrandfather_ID, fatherinlaw_name, fatherinlaw_ID)
has_both2 <- has_both2 %>% mutate(same_name = (mgrandfather_name == fatherinlaw_name))
has_both2 <- has_both2 %>% mutate(same_ID = (mgrandfather_ID == fatherinlaw_ID))


wb6 <- createWorkbook()
addWorksheet(wb6, "Sheet1")
addWorksheet(wb6, "Sheet2")
writeData(wb6, sheet = "Sheet1", x = has_all, colNames = T, headerStyle = hs)
writeData(wb6, sheet = "Sheet2", x = has_both2, colNames = T, headerStyle = hs)
saveWorkbook(wb6, "HasMgrandfatherButNotSoninlaw2.xlsx")