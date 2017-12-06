# load the data [getting the data]

tr <- read.csv(file.choose())#load the train dataset
te <- read.csv(file.choose())#load the test dataset
givensoloution <- read.csv(file.choose())# given output of test dataset
total <- rbind(tr[,c(1,3:12)],te)

#exploring the data
str(total)
#from this we can know that there are missing  values in age 
# now lets try to explore each coloumn and note down the false in that
summary(total$PassengerId)
summary(is.na(total$PassengerId))
# from this we can know that there are no NA are present
# as the passanger id is just number so we should not worry about this
# next we will look at second coloumn
summary(tr$Survived)
class(tr$Survived)
# survived can take only two values but it is not catgorial but we will convert it
summary(is.na(tr$Survived))
# in this also there are no NA 
summary(total$Pclass)
summary(is.na(total$Pclass))
summary(total$Name)
#from this we can know the names are specified by using family names and then their names
summary(is.na(total$Name))

# ? we will totaly to arrange data using by family names, no of persons aborded from family,and their ids
name1 <- as.character(total$Name)
length(name1)
FamilyNames <- c()
familyn <- c()
unlist_1 <- c()
for(i in 1:1309)
{
  familyn <- strsplit(name1[i],split ="," )
  unlist_1<- unlist(familyn)
  FamilyNames[i] <- unlist_1[1]
  
}
total$FamilyNames <- FamilyNames
##############################################
summary(total$Sex)
summary(is.na(total$Sex))
summary(total$Age)
# *** from this can know   there are 177 NA in it
summary(total$SibSp)
# in tyhis also there are no NA 
summary(total$Parch)
# in this also there are no NA
summary(total$Ticket)
# ? from this we can know that two are more people are getting same  ticket
# ? i am having all the family members are having same ticket
# ? we will totaly to see this in further
# and there are no Na
summary(total$Fare)
# ? here if we look for some cases the fare is zero which is not possible
# ? in future wwe will totaly to find the  correct fare
which(is.na(total$Fare))
na_in_fare <- 1044
summary(total$Cabin)
View(total$Cabin)
summary(is.na(total$Cabin))
# ? if u see this there  are no Na values but there are missing values
class(total$Cabin)
levels(total$Cabin)
# from this we can see there are missing values 
# The first letter of the cabin indicates deck. first class had the top decks (A-E), second class (D-F), and third class (E-G).
# ? train$Embarked also there are missing values 
# finally the what the explorations we find with respect to  missing values
# missing values are in embarked,cabin, age
# wrong values in fare
# by using the graphical  analysis we tell totaly find some relations
# first we will try to find missing values in embark
train <- total
train$Fsize <- train$SibSp + train$Parch + 1
train$FsizeD[train$Fsize == 1] <- 'singleton'
train$FsizeD[train$Fsize < 5 & train$Fsize > 1] <- 'small'
train$FsizeD[train$Fsize > 4] <- 'large'
train$Family <- paste(train$FamilyNames, train$Fsize, sep='_')

class((train$Embarked))
levels(train$Embarked)
levels(train$Embarked)[1] <- NA
summary(train$Embarked)

ind_na_in_embark <- which(is.na(train$Embarked))
#we will consider people under age 14 or less than that considered as childrens
# pclass,sex =female,age  = adults ,fare  by using this factors we can find their embark
#suppose assume that three persons embarked from c per head the fare was 40 so that fare is 120 but in the given format of data they are mentioning the ticket name and ticket fare not the passanger fare
#so if we take the averaage we will get 120 which is not true
# so we will prepare one data frame in thich the passangers ticket and number of persons travelling on that ticket and fare of that ticket so from thatn we can get  the fare for each head
# the another method is from the data we will try to find alone persons traveeling from different ports
View(train)
table_of_tickets <- table(train$Ticket)

length(table_of_tickets)
freq <- tabulate(train$Ticket)
length(freq)
ind <- match(names(table_of_tickets),train$Ticket)
index <- list()
for(i in 1:929)
{
  index[i] <- list(train$PassengerId[train$Ticket == names(table_of_tickets)[i]])
}
sex <- list()
age <- list()
fare <- list()
sibsp <- list()
parch <- list()
familyname <- list()

for(i in 1:929)
{
  sex[i] <- list(train$Sex[train$Ticket == names(table_of_tickets)[i]])
  age[i] <- list(train$Age[train$Ticket == names(table_of_tickets)[i]])
  fare[i] <- list(train$Fare[train$Ticket == names(table_of_tickets)[i]])
  sibsp[i] <- list(train$SibSp[train$Ticket == names(table_of_tickets)[i]])
  parch[i] <- list(train$Parch[train$Ticket == names(table_of_tickets)[i]])
  familyname[i] <- list(train$FamilyNames[train$Ticket == names(table_of_tickets)[i]])
}
ticket_details <- cbind.data.frame(names(table_of_tickets),freq)
View(ticket_details)
names(ticket_details)[1] <- "ticket_num"
names(ticket_details)[2] <- "no_of_persons"
ticket_details$passengers_id <- index
ticket_details$pclass <- train$Pclass[ind]
ticket_details$sex <- sex
ticket_details$age <- age
ticket_details$sibsp <- sibsp
ticket_details$perch <- parch
ticket_details$fare <- train$Fare[ind]
ticket_details$embark <- train$Embarked[ind]
ticket_details$familyname <- familyname
#** NOW AT PRESENT WE CHANGED THE DATA INTO BASED ON TICKET NAME AND NUMBER OF PASSANGERS
####NO_OF_PERSONS_ABORDED
NUM_OF_PERSONS_1 <- ticket_details[ticket_details$no_of_persons == 1,]
NUM_OF_PERSONS_1_BASED_ON_PCLASS1 <- NUM_OF_PERSONS_1[NUM_OF_PERSONS_1$pclass == 1,]
#NUM_OF_PERSONS_1_BASED_ON_PCLASS1 <- na.omit(NUM_OF_PERSONS_1_BASED_ON_PCLASS1)
NUM_OF_PERSONS_1_BASED_ON_PCLASS2 <- NUM_OF_PERSONS_1[NUM_OF_PERSONS_1$pclass == 2,]
NUM_OF_PERSONS_1_BASED_ON_PCLASS3 <- NUM_OF_PERSONS_1[NUM_OF_PERSONS_1$pclass == 3,]
###############################PCLASS1
NUM_OF_PERSONS_1_BASED_ON_PCLASS1_EMBARK_C <- NUM_OF_PERSONS_1_BASED_ON_PCLASS1[NUM_OF_PERSONS_1_BASED_ON_PCLASS1$embark == "C",]
NUM_OF_PERSONS_1_BASED_ON_PCLASS1_EMBARK_Q <- NUM_OF_PERSONS_1_BASED_ON_PCLASS1[NUM_OF_PERSONS_1_BASED_ON_PCLASS1$embark == "Q",]
NUM_OF_PERSONS_1_BASED_ON_PCLASS1_EMBARK_S <- NUM_OF_PERSONS_1_BASED_ON_PCLASS1[NUM_OF_PERSONS_1_BASED_ON_PCLASS1$embark == "S",]
#################################PCLASS2
NUM_OF_PERSONS_1_BASED_ON_PCLASS2_EMBARK_C <- NUM_OF_PERSONS_1_BASED_ON_PCLASS2[NUM_OF_PERSONS_1_BASED_ON_PCLASS2$embark == "C",]
NUM_OF_PERSONS_1_BASED_ON_PCLASS2_EMBARK_Q <- NUM_OF_PERSONS_1_BASED_ON_PCLASS2[NUM_OF_PERSONS_1_BASED_ON_PCLASS2$embark == "Q",]
NUM_OF_PERSONS_1_BASED_ON_PCLASS2_EMBARK_S <- NUM_OF_PERSONS_1_BASED_ON_PCLASS2[NUM_OF_PERSONS_1_BASED_ON_PCLASS2$embark == "S",]
#################################PCLASS3
NUM_OF_PERSONS_1_BASED_ON_PCLASS3_EMBARK_C <- NUM_OF_PERSONS_1_BASED_ON_PCLASS3[NUM_OF_PERSONS_1_BASED_ON_PCLASS3$embark == "C",]
NUM_OF_PERSONS_1_BASED_ON_PCLASS3_EMBARK_Q <- NUM_OF_PERSONS_1_BASED_ON_PCLASS3[NUM_OF_PERSONS_1_BASED_ON_PCLASS3$embark == "Q",]
NUM_OF_PERSONS_1_BASED_ON_PCLASS3_EMBARK_S <- NUM_OF_PERSONS_1_BASED_ON_PCLASS3[NUM_OF_PERSONS_1_BASED_ON_PCLASS3$embark == "S",]
############################################################################################################
#######NUM_OF_PERSONS_ABORDED = 2
NUM_OF_PERSONS_2 <- ticket_details[ticket_details$no_of_persons == 2,]
NUM_OF_PERSONS_2_BASED_ON_PCLASS1 <- NUM_OF_PERSONS_2[NUM_OF_PERSONS_2$pclass == 1,]
#NUM_OF_PERSONS_2_BASED_ON_PCLASS1 <- na.omit(NUM_OF_PERSONS_2_BASED_ON_PCLASS1)
NUM_OF_PERSONS_2_BASED_ON_PCLASS2 <- NUM_OF_PERSONS_2[NUM_OF_PERSONS_2$pclass == 2,]
NUM_OF_PERSONS_2_BASED_ON_PCLASS3 <- NUM_OF_PERSONS_2[NUM_OF_PERSONS_2$pclass == 3,]
###############################PCLASS1
NUM_OF_PERSONS_2_BASED_ON_PCLASS1_EMBARK_C <- NUM_OF_PERSONS_2_BASED_ON_PCLASS1[NUM_OF_PERSONS_2_BASED_ON_PCLASS1$embark == "C",]
NUM_OF_PERSONS_2_BASED_ON_PCLASS1_EMBARK_Q <- NUM_OF_PERSONS_2_BASED_ON_PCLASS1[NUM_OF_PERSONS_2_BASED_ON_PCLASS1$embark == "Q",]
NUM_OF_PERSONS_2_BASED_ON_PCLASS1_EMBARK_S <- NUM_OF_PERSONS_2_BASED_ON_PCLASS1[NUM_OF_PERSONS_2_BASED_ON_PCLASS1$embark == "S",]
#################################PCLASS2
NUM_OF_PERSONS_2_BASED_ON_PCLASS2_EMBARK_C <- NUM_OF_PERSONS_2_BASED_ON_PCLASS2[NUM_OF_PERSONS_2_BASED_ON_PCLASS2$embark == "C",]
NUM_OF_PERSONS_2_BASED_ON_PCLASS2_EMBARK_Q <- NUM_OF_PERSONS_2_BASED_ON_PCLASS2[NUM_OF_PERSONS_2_BASED_ON_PCLASS2$embark == "Q",]
NUM_OF_PERSONS_2_BASED_ON_PCLASS2_EMBARK_S <- NUM_OF_PERSONS_2_BASED_ON_PCLASS2[NUM_OF_PERSONS_2_BASED_ON_PCLASS2$embark == "S",]
#################################PCLASS3
NUM_OF_PERSONS_2_BASED_ON_PCLASS3_EMBARK_C <- NUM_OF_PERSONS_2_BASED_ON_PCLASS3[NUM_OF_PERSONS_2_BASED_ON_PCLASS3$embark == "C",]
NUM_OF_PERSONS_2_BASED_ON_PCLASS3_EMBARK_Q <- NUM_OF_PERSONS_2_BASED_ON_PCLASS3[NUM_OF_PERSONS_2_BASED_ON_PCLASS3$embark == "Q",]
NUM_OF_PERSONS_2_BASED_ON_PCLASS3_EMBARK_S <- NUM_OF_PERSONS_2_BASED_ON_PCLASS3[NUM_OF_PERSONS_2_BASED_ON_PCLASS3$embark == "S",]


####################################################################################
########NUMBER OF PERSONS ABORED = 3
NUM_OF_PERSONS_3 <- ticket_details[ticket_details$no_of_persons == 3,]
NUM_OF_PERSONS_3_BASED_ON_PCLASS1 <- NUM_OF_PERSONS_3[NUM_OF_PERSONS_3$pclass == 1,]
#NUM_OF_PERSONS_3_BASED_ON_PCLASS1 <- na.omit(NUM_OF_PERSONS_3_BASED_ON_PCLASS1)
NUM_OF_PERSONS_3_BASED_ON_PCLASS2 <- NUM_OF_PERSONS_3[NUM_OF_PERSONS_3$pclass == 2,]
NUM_OF_PERSONS_3_BASED_ON_PCLASS3 <- NUM_OF_PERSONS_3[NUM_OF_PERSONS_3$pclass == 3,]
###############################PCLASS1
NUM_OF_PERSONS_3_BASED_ON_PCLASS1_EMBARK_C <- NUM_OF_PERSONS_3_BASED_ON_PCLASS1[NUM_OF_PERSONS_3_BASED_ON_PCLASS1$embark == "C",]
NUM_OF_PERSONS_3_BASED_ON_PCLASS1_EMBARK_Q <- NUM_OF_PERSONS_3_BASED_ON_PCLASS1[NUM_OF_PERSONS_3_BASED_ON_PCLASS1$embark == "Q",]
NUM_OF_PERSONS_3_BASED_ON_PCLASS1_EMBARK_S <- NUM_OF_PERSONS_3_BASED_ON_PCLASS1[NUM_OF_PERSONS_3_BASED_ON_PCLASS1$embark == "S",]
#################################PCLASS2
NUM_OF_PERSONS_3_BASED_ON_PCLASS2_EMBARK_C <- NUM_OF_PERSONS_3_BASED_ON_PCLASS2[NUM_OF_PERSONS_3_BASED_ON_PCLASS2$embark == "C",]
NUM_OF_PERSONS_3_BASED_ON_PCLASS2_EMBARK_Q <- NUM_OF_PERSONS_3_BASED_ON_PCLASS2[NUM_OF_PERSONS_3_BASED_ON_PCLASS2$embark == "Q",]
NUM_OF_PERSONS_3_BASED_ON_PCLASS2_EMBARK_S <- NUM_OF_PERSONS_3_BASED_ON_PCLASS2[NUM_OF_PERSONS_3_BASED_ON_PCLASS2$embark == "S",]
#################################PCLASS3
NUM_OF_PERSONS_3_BASED_ON_PCLASS3_EMBARK_C <- NUM_OF_PERSONS_3_BASED_ON_PCLASS3[NUM_OF_PERSONS_3_BASED_ON_PCLASS3$embark == "C",]
NUM_OF_PERSONS_3_BASED_ON_PCLASS3_EMBARK_Q <- NUM_OF_PERSONS_3_BASED_ON_PCLASS3[NUM_OF_PERSONS_3_BASED_ON_PCLASS3$embark == "Q",]
NUM_OF_PERSONS_3_BASED_ON_PCLASS3_EMBARK_S <- NUM_OF_PERSONS_3_BASED_ON_PCLASS3[NUM_OF_PERSONS_3_BASED_ON_PCLASS3$embark == "S",]

####################################################################################
NUM_OF_PERSONS_4 <- ticket_details[ticket_details$no_of_persons == 4,]
NUM_OF_PERSONS_4_BASED_ON_PCLASS1 <- NUM_OF_PERSONS_4[NUM_OF_PERSONS_4$pclass == 1,]
#NUM_OF_PERSONS_4_BASED_ON_PCLASS1 <- na.omit(NUM_OF_PERSONS_4_BASED_ON_PCLASS1)
NUM_OF_PERSONS_4_BASED_ON_PCLASS2 <- NUM_OF_PERSONS_4[NUM_OF_PERSONS_4$pclass == 2,]
NUM_OF_PERSONS_4_BASED_ON_PCLASS3 <- NUM_OF_PERSONS_4[NUM_OF_PERSONS_4$pclass == 3,]
###############################PCLASS1
NUM_OF_PERSONS_4_BASED_ON_PCLASS1_EMBARK_C <- NUM_OF_PERSONS_4_BASED_ON_PCLASS1[NUM_OF_PERSONS_4_BASED_ON_PCLASS1$embark == "C",]
NUM_OF_PERSONS_4_BASED_ON_PCLASS1_EMBARK_Q <- NUM_OF_PERSONS_4_BASED_ON_PCLASS1[NUM_OF_PERSONS_4_BASED_ON_PCLASS1$embark == "Q",]
NUM_OF_PERSONS_4_BASED_ON_PCLASS1_EMBARK_S <- NUM_OF_PERSONS_4_BASED_ON_PCLASS1[NUM_OF_PERSONS_4_BASED_ON_PCLASS1$embark == "S",]
#################################PCLASS2
NUM_OF_PERSONS_4_BASED_ON_PCLASS2_EMBARK_C <- NUM_OF_PERSONS_4_BASED_ON_PCLASS2[NUM_OF_PERSONS_4_BASED_ON_PCLASS2$embark == "C",]
NUM_OF_PERSONS_4_BASED_ON_PCLASS2_EMBARK_Q <- NUM_OF_PERSONS_4_BASED_ON_PCLASS2[NUM_OF_PERSONS_4_BASED_ON_PCLASS2$embark == "Q",]
NUM_OF_PERSONS_4_BASED_ON_PCLASS2_EMBARK_S <- NUM_OF_PERSONS_4_BASED_ON_PCLASS2[NUM_OF_PERSONS_4_BASED_ON_PCLASS2$embark == "S",]
#################################PCLASS3
NUM_OF_PERSONS_4_BASED_ON_PCLASS3_EMBARK_C <- NUM_OF_PERSONS_4_BASED_ON_PCLASS3[NUM_OF_PERSONS_4_BASED_ON_PCLASS3$embark == "C",]
NUM_OF_PERSONS_4_BASED_ON_PCLASS3_EMBARK_Q <- NUM_OF_PERSONS_4_BASED_ON_PCLASS3[NUM_OF_PERSONS_4_BASED_ON_PCLASS3$embark == "Q",]
NUM_OF_PERSONS_4_BASED_ON_PCLASS3_EMBARK_S <- NUM_OF_PERSONS_4_BASED_ON_PCLASS3[NUM_OF_PERSONS_4_BASED_ON_PCLASS3$embark == "S",]

####################################################################################
NUM_OF_PERSONS_5 <- ticket_details[ticket_details$no_of_persons == 5,]
NUM_OF_PERSONS_5_BASED_ON_PCLASS1 <- NUM_OF_PERSONS_5[NUM_OF_PERSONS_5$pclass == 1,]
#NUM_OF_PERSONS_5_BASED_ON_PCLASS1 <- na.omit(NUM_OF_PERSONS_5_BASED_ON_PCLASS1)
NUM_OF_PERSONS_5_BASED_ON_PCLASS2 <- NUM_OF_PERSONS_5[NUM_OF_PERSONS_5$pclass == 2,]
NUM_OF_PERSONS_5_BASED_ON_PCLASS3 <- NUM_OF_PERSONS_5[NUM_OF_PERSONS_5$pclass == 3,]
###############################PCLASS1
NUM_OF_PERSONS_5_BASED_ON_PCLASS1_EMBARK_C <- NUM_OF_PERSONS_5_BASED_ON_PCLASS1[NUM_OF_PERSONS_5_BASED_ON_PCLASS1$embark == "C",]
NUM_OF_PERSONS_5_BASED_ON_PCLASS1_EMBARK_Q <- NUM_OF_PERSONS_5_BASED_ON_PCLASS1[NUM_OF_PERSONS_5_BASED_ON_PCLASS1$embark == "Q",]
NUM_OF_PERSONS_5_BASED_ON_PCLASS1_EMBARK_S <- NUM_OF_PERSONS_5_BASED_ON_PCLASS1[NUM_OF_PERSONS_5_BASED_ON_PCLASS1$embark == "S",]
#################################PCLASS2
NUM_OF_PERSONS_5_BASED_ON_PCLASS2_EMBARK_C <- NUM_OF_PERSONS_5_BASED_ON_PCLASS2[NUM_OF_PERSONS_5_BASED_ON_PCLASS2$embark == "C",]
NUM_OF_PERSONS_5_BASED_ON_PCLASS2_EMBARK_Q <- NUM_OF_PERSONS_5_BASED_ON_PCLASS2[NUM_OF_PERSONS_5_BASED_ON_PCLASS2$embark == "Q",]
NUM_OF_PERSONS_5_BASED_ON_PCLASS2_EMBARK_S <- NUM_OF_PERSONS_5_BASED_ON_PCLASS2[NUM_OF_PERSONS_5_BASED_ON_PCLASS2$embark == "S",]
#################################PCLASS3
NUM_OF_PERSONS_5_BASED_ON_PCLASS3_EMBARK_C <- NUM_OF_PERSONS_5_BASED_ON_PCLASS3[NUM_OF_PERSONS_5_BASED_ON_PCLASS3$embark == "C",]
NUM_OF_PERSONS_5_BASED_ON_PCLASS3_EMBARK_Q <- NUM_OF_PERSONS_5_BASED_ON_PCLASS3[NUM_OF_PERSONS_5_BASED_ON_PCLASS3$embark == "Q",]
NUM_OF_PERSONS_5_BASED_ON_PCLASS3_EMBARK_S <- NUM_OF_PERSONS_5_BASED_ON_PCLASS3[NUM_OF_PERSONS_5_BASED_ON_PCLASS3$embark == "S",]

####################################################################################
NUM_OF_PERSONS_6 <- ticket_details[ticket_details$no_of_persons == 6,]
NUM_OF_PERSONS_6_BASED_ON_PCLASS1 <- NUM_OF_PERSONS_6[NUM_OF_PERSONS_6$pclass == 1,]
#NUM_OF_PERSONS_6_BASED_ON_PCLASS1 <- na.omit(NUM_OF_PERSONS_6_BASED_ON_PCLASS1)
NUM_OF_PERSONS_6_BASED_ON_PCLASS2 <- NUM_OF_PERSONS_6[NUM_OF_PERSONS_6$pclass == 2,]
NUM_OF_PERSONS_6_BASED_ON_PCLASS3 <- NUM_OF_PERSONS_6[NUM_OF_PERSONS_6$pclass == 3,]
###############################PCLASS1
NUM_OF_PERSONS_6_BASED_ON_PCLASS1_EMBARK_C <- NUM_OF_PERSONS_6_BASED_ON_PCLASS1[NUM_OF_PERSONS_6_BASED_ON_PCLASS1$embark == "C",]
NUM_OF_PERSONS_6_BASED_ON_PCLASS1_EMBARK_Q <- NUM_OF_PERSONS_6_BASED_ON_PCLASS1[NUM_OF_PERSONS_6_BASED_ON_PCLASS1$embark == "Q",]
NUM_OF_PERSONS_6_BASED_ON_PCLASS1_EMBARK_S <- NUM_OF_PERSONS_6_BASED_ON_PCLASS1[NUM_OF_PERSONS_6_BASED_ON_PCLASS1$embark == "S",]
#################################PCLASS2
NUM_OF_PERSONS_6_BASED_ON_PCLASS2_EMBARK_C <- NUM_OF_PERSONS_6_BASED_ON_PCLASS2[NUM_OF_PERSONS_6_BASED_ON_PCLASS2$embark == "C",]
NUM_OF_PERSONS_6_BASED_ON_PCLASS2_EMBARK_Q <- NUM_OF_PERSONS_6_BASED_ON_PCLASS2[NUM_OF_PERSONS_6_BASED_ON_PCLASS2$embark == "Q",]
NUM_OF_PERSONS_6_BASED_ON_PCLASS2_EMBARK_S <- NUM_OF_PERSONS_6_BASED_ON_PCLASS2[NUM_OF_PERSONS_6_BASED_ON_PCLASS2$embark == "S",]
#################################PCLASS3
NUM_OF_PERSONS_6_BASED_ON_PCLASS3_EMBARK_C <- NUM_OF_PERSONS_6_BASED_ON_PCLASS3[NUM_OF_PERSONS_6_BASED_ON_PCLASS3$embark == "C",]
NUM_OF_PERSONS_6_BASED_ON_PCLASS3_EMBARK_Q <- NUM_OF_PERSONS_6_BASED_ON_PCLASS3[NUM_OF_PERSONS_6_BASED_ON_PCLASS3$embark == "Q",]
NUM_OF_PERSONS_6_BASED_ON_PCLASS3_EMBARK_S <- NUM_OF_PERSONS_6_BASED_ON_PCLASS3[NUM_OF_PERSONS_6_BASED_ON_PCLASS3$embark == "S",]

########################################################################################################################
NUM_OF_PERSONS_7 <- ticket_details[ticket_details$no_of_persons == 7,]
NUM_OF_PERSONS_7_BASED_ON_PCLASS1 <- NUM_OF_PERSONS_7[NUM_OF_PERSONS_7$pclass == 1,]
#NUM_OF_PERSONS_7_BASED_ON_PCLASS1 <- na.omit(NUM_OF_PERSONS_7_BASED_ON_PCLASS1)
NUM_OF_PERSONS_7_BASED_ON_PCLASS2 <- NUM_OF_PERSONS_7[NUM_OF_PERSONS_7$pclass == 2,]
NUM_OF_PERSONS_7_BASED_ON_PCLASS3 <- NUM_OF_PERSONS_7[NUM_OF_PERSONS_7$pclass == 3,]
###############################PCLASS1
NUM_OF_PERSONS_7_BASED_ON_PCLASS1_EMBARK_C <- NUM_OF_PERSONS_7_BASED_ON_PCLASS1[NUM_OF_PERSONS_7_BASED_ON_PCLASS1$embark == "C",]
NUM_OF_PERSONS_7_BASED_ON_PCLASS1_EMBARK_Q <- NUM_OF_PERSONS_7_BASED_ON_PCLASS1[NUM_OF_PERSONS_7_BASED_ON_PCLASS1$embark == "Q",]
NUM_OF_PERSONS_7_BASED_ON_PCLASS1_EMBARK_S <- NUM_OF_PERSONS_7_BASED_ON_PCLASS1[NUM_OF_PERSONS_7_BASED_ON_PCLASS1$embark == "S",]
#################################PCLASS2
NUM_OF_PERSONS_7_BASED_ON_PCLASS2_EMBARK_C <- NUM_OF_PERSONS_7_BASED_ON_PCLASS2[NUM_OF_PERSONS_7_BASED_ON_PCLASS2$embark == "C",]
NUM_OF_PERSONS_7_BASED_ON_PCLASS2_EMBARK_Q <- NUM_OF_PERSONS_7_BASED_ON_PCLASS2[NUM_OF_PERSONS_7_BASED_ON_PCLASS2$embark == "Q",]
NUM_OF_PERSONS_7_BASED_ON_PCLASS2_EMBARK_S <- NUM_OF_PERSONS_7_BASED_ON_PCLASS2[NUM_OF_PERSONS_7_BASED_ON_PCLASS2$embark == "S",]
#################################PCLASS3
NUM_OF_PERSONS_7_BASED_ON_PCLASS3_EMBARK_C <- NUM_OF_PERSONS_7_BASED_ON_PCLASS3[NUM_OF_PERSONS_7_BASED_ON_PCLASS3$embark == "C",]
NUM_OF_PERSONS_7_BASED_ON_PCLASS3_EMBARK_Q <- NUM_OF_PERSONS_7_BASED_ON_PCLASS3[NUM_OF_PERSONS_7_BASED_ON_PCLASS3$embark == "Q",]
NUM_OF_PERSONS_7_BASED_ON_PCLASS3_EMBARK_S <- NUM_OF_PERSONS_7_BASED_ON_PCLASS3[NUM_OF_PERSONS_7_BASED_ON_PCLASS3$embark == "S",]

####################################################################################
NUM_OF_PERSONS_8 <- ticket_details[ticket_details$no_of_persons == 8,]
NUM_OF_PERSONS_8_BASED_ON_PCLASS1 <- NUM_OF_PERSONS_8[NUM_OF_PERSONS_8$pclass == 1,]
#NUM_OF_PERSONS_8_BASED_ON_PCLASS1 <- na.omit(NUM_OF_PERSONS_8_BASED_ON_PCLASS1)
NUM_OF_PERSONS_8_BASED_ON_PCLASS2 <- NUM_OF_PERSONS_8[NUM_OF_PERSONS_8$pclass == 2,]
NUM_OF_PERSONS_8_BASED_ON_PCLASS3 <- NUM_OF_PERSONS_8[NUM_OF_PERSONS_8$pclass == 3,]
###############################PCLASS1
NUM_OF_PERSONS_8_BASED_ON_PCLASS1_EMBARK_C <- NUM_OF_PERSONS_8_BASED_ON_PCLASS1[NUM_OF_PERSONS_8_BASED_ON_PCLASS1$embark == "C",]
NUM_OF_PERSONS_8_BASED_ON_PCLASS1_EMBARK_Q <- NUM_OF_PERSONS_8_BASED_ON_PCLASS1[NUM_OF_PERSONS_8_BASED_ON_PCLASS1$embark == "Q",]
NUM_OF_PERSONS_8_BASED_ON_PCLASS1_EMBARK_S <- NUM_OF_PERSONS_8_BASED_ON_PCLASS1[NUM_OF_PERSONS_8_BASED_ON_PCLASS1$embark == "S",]
#################################PCLASS2
NUM_OF_PERSONS_8_BASED_ON_PCLASS2_EMBARK_C <- NUM_OF_PERSONS_8_BASED_ON_PCLASS2[NUM_OF_PERSONS_8_BASED_ON_PCLASS2$embark == "C",]
NUM_OF_PERSONS_8_BASED_ON_PCLASS2_EMBARK_Q <- NUM_OF_PERSONS_8_BASED_ON_PCLASS2[NUM_OF_PERSONS_8_BASED_ON_PCLASS2$embark == "Q",]
NUM_OF_PERSONS_8_BASED_ON_PCLASS2_EMBARK_S <- NUM_OF_PERSONS_8_BASED_ON_PCLASS2[NUM_OF_PERSONS_8_BASED_ON_PCLASS2$embark == "S",]
#################################PCLASS3
NUM_OF_PERSONS_8_BASED_ON_PCLASS3_EMBARK_C <- NUM_OF_PERSONS_8_BASED_ON_PCLASS3[NUM_OF_PERSONS_8_BASED_ON_PCLASS3$embark == "C",]
NUM_OF_PERSONS_8_BASED_ON_PCLASS3_EMBARK_Q <- NUM_OF_PERSONS_8_BASED_ON_PCLASS3[NUM_OF_PERSONS_8_BASED_ON_PCLASS3$embark == "Q",]
NUM_OF_PERSONS_8_BASED_ON_PCLASS3_EMBARK_S <- NUM_OF_PERSONS_8_BASED_ON_PCLASS3[NUM_OF_PERSONS_8_BASED_ON_PCLASS3$embark == "S",]

####################################################################################

NUM_OF_PERSONS_9 <- ticket_details[ticket_details$no_of_persons == 9,]
NUM_OF_PERSONS_9_BASED_ON_PCLASS1 <- NUM_OF_PERSONS_9[NUM_OF_PERSONS_9$pclass == 1,]
#NUM_OF_PERSONS_9_BASED_ON_PCLASS1 <- na.omit(NUM_OF_PERSONS_9_BASED_ON_PCLASS1)
NUM_OF_PERSONS_9_BASED_ON_PCLASS2 <- NUM_OF_PERSONS_9[NUM_OF_PERSONS_9$pclass == 2,]
NUM_OF_PERSONS_9_BASED_ON_PCLASS3 <- NUM_OF_PERSONS_9[NUM_OF_PERSONS_9$pclass == 3,]
###############################PCLASS1
NUM_OF_PERSONS_9_BASED_ON_PCLASS1_EMBARK_C <- NUM_OF_PERSONS_9_BASED_ON_PCLASS1[NUM_OF_PERSONS_9_BASED_ON_PCLASS1$embark == "C",]
NUM_OF_PERSONS_9_BASED_ON_PCLASS1_EMBARK_Q <- NUM_OF_PERSONS_9_BASED_ON_PCLASS1[NUM_OF_PERSONS_9_BASED_ON_PCLASS1$embark == "Q",]
NUM_OF_PERSONS_9_BASED_ON_PCLASS1_EMBARK_S <- NUM_OF_PERSONS_9_BASED_ON_PCLASS1[NUM_OF_PERSONS_9_BASED_ON_PCLASS1$embark == "S",]
#################################PCLASS2
NUM_OF_PERSONS_9_BASED_ON_PCLASS2_EMBARK_C <- NUM_OF_PERSONS_9_BASED_ON_PCLASS2[NUM_OF_PERSONS_9_BASED_ON_PCLASS2$embark == "C",]
NUM_OF_PERSONS_9_BASED_ON_PCLASS2_EMBARK_Q <- NUM_OF_PERSONS_9_BASED_ON_PCLASS2[NUM_OF_PERSONS_9_BASED_ON_PCLASS2$embark == "Q",]
NUM_OF_PERSONS_9_BASED_ON_PCLASS2_EMBARK_S <- NUM_OF_PERSONS_9_BASED_ON_PCLASS2[NUM_OF_PERSONS_9_BASED_ON_PCLASS2$embark == "S",]
#################################PCLASS3
NUM_OF_PERSONS_9_BASED_ON_PCLASS3_EMBARK_C <- NUM_OF_PERSONS_9_BASED_ON_PCLASS3[NUM_OF_PERSONS_9_BASED_ON_PCLASS3$embark == "C",]
NUM_OF_PERSONS_9_BASED_ON_PCLASS3_EMBARK_Q <- NUM_OF_PERSONS_9_BASED_ON_PCLASS3[NUM_OF_PERSONS_9_BASED_ON_PCLASS3$embark == "Q",]
NUM_OF_PERSONS_9_BASED_ON_PCLASS3_EMBARK_S <- NUM_OF_PERSONS_9_BASED_ON_PCLASS3[NUM_OF_PERSONS_9_BASED_ON_PCLASS3$embark == "S",]

####################################################################################
NUM_OF_PERSONS_10 <- ticket_details[ticket_details$no_of_persons == 10,]
NUM_OF_PERSONS_10_BASED_ON_PCLASS1 <- NUM_OF_PERSONS_10[NUM_OF_PERSONS_10$pclass == 1,]
#NUM_OF_PERSONS_10_BASED_ON_PCLASS1 <- na.omit(NUM_OF_PERSONS_10_BASED_ON_PCLASS1)
NUM_OF_PERSONS_10_BASED_ON_PCLASS2 <- NUM_OF_PERSONS_10[NUM_OF_PERSONS_10$pclass == 2,]
NUM_OF_PERSONS_10_BASED_ON_PCLASS3 <- NUM_OF_PERSONS_10[NUM_OF_PERSONS_10$pclass == 3,]
###############################PCLASS1
NUM_OF_PERSONS_10_BASED_ON_PCLASS1_EMBARK_C <- NUM_OF_PERSONS_10_BASED_ON_PCLASS1[NUM_OF_PERSONS_10_BASED_ON_PCLASS1$embark == "C",]
NUM_OF_PERSONS_10_BASED_ON_PCLASS1_EMBARK_Q <- NUM_OF_PERSONS_10_BASED_ON_PCLASS1[NUM_OF_PERSONS_10_BASED_ON_PCLASS1$embark == "Q",]
NUM_OF_PERSONS_10_BASED_ON_PCLASS1_EMBARK_S <- NUM_OF_PERSONS_10_BASED_ON_PCLASS1[NUM_OF_PERSONS_10_BASED_ON_PCLASS1$embark == "S",]
#################################PCLASS2
NUM_OF_PERSONS_10_BASED_ON_PCLASS2_EMBARK_C <- NUM_OF_PERSONS_10_BASED_ON_PCLASS2[NUM_OF_PERSONS_10_BASED_ON_PCLASS2$embark == "C",]
NUM_OF_PERSONS_10_BASED_ON_PCLASS2_EMBARK_Q <- NUM_OF_PERSONS_10_BASED_ON_PCLASS2[NUM_OF_PERSONS_10_BASED_ON_PCLASS2$embark == "Q",]
NUM_OF_PERSONS_10_BASED_ON_PCLASS2_EMBARK_S <- NUM_OF_PERSONS_10_BASED_ON_PCLASS2[NUM_OF_PERSONS_10_BASED_ON_PCLASS2$embark == "S",]
#################################PCLASS3
NUM_OF_PERSONS_10_BASED_ON_PCLASS3_EMBARK_C <- NUM_OF_PERSONS_10_BASED_ON_PCLASS3[NUM_OF_PERSONS_10_BASED_ON_PCLASS3$embark == "C",]
NUM_OF_PERSONS_10_BASED_ON_PCLASS3_EMBARK_Q <- NUM_OF_PERSONS_10_BASED_ON_PCLASS3[NUM_OF_PERSONS_10_BASED_ON_PCLASS3$embark == "Q",]
NUM_OF_PERSONS_10_BASED_ON_PCLASS3_EMBARK_S <- NUM_OF_PERSONS_10_BASED_ON_PCLASS3[NUM_OF_PERSONS_10_BASED_ON_PCLASS3$embark == "S",]

####################################################################################
NUM_OF_PERSONS_11 <- ticket_details[ticket_details$no_of_persons == 11,]
NUM_OF_PERSONS_11_BASED_ON_PCLASS1 <- NUM_OF_PERSONS_11[NUM_OF_PERSONS_11$pclass == 1,]
#NUM_OF_PERSONS_11_BASED_ON_PCLASS1 <- na.omit(NUM_OF_PERSONS_11_BASED_ON_PCLASS1)
NUM_OF_PERSONS_11_BASED_ON_PCLASS2 <- NUM_OF_PERSONS_11[NUM_OF_PERSONS_11$pclass == 2,]
NUM_OF_PERSONS_11_BASED_ON_PCLASS3 <- NUM_OF_PERSONS_11[NUM_OF_PERSONS_11$pclass == 3,]
###############################PCLASS1
NUM_OF_PERSONS_11_BASED_ON_PCLASS1_EMBARK_C <- NUM_OF_PERSONS_11_BASED_ON_PCLASS1[NUM_OF_PERSONS_11_BASED_ON_PCLASS1$embark == "C",]
NUM_OF_PERSONS_11_BASED_ON_PCLASS1_EMBARK_Q <- NUM_OF_PERSONS_11_BASED_ON_PCLASS1[NUM_OF_PERSONS_11_BASED_ON_PCLASS1$embark == "Q",]
NUM_OF_PERSONS_11_BASED_ON_PCLASS1_EMBARK_S <- NUM_OF_PERSONS_11_BASED_ON_PCLASS1[NUM_OF_PERSONS_11_BASED_ON_PCLASS1$embark == "S",]
#################################PCLASS2
NUM_OF_PERSONS_11_BASED_ON_PCLASS2_EMBARK_C <- NUM_OF_PERSONS_11_BASED_ON_PCLASS2[NUM_OF_PERSONS_11_BASED_ON_PCLASS2$embark == "C",]
NUM_OF_PERSONS_11_BASED_ON_PCLASS2_EMBARK_Q <- NUM_OF_PERSONS_11_BASED_ON_PCLASS2[NUM_OF_PERSONS_11_BASED_ON_PCLASS2$embark == "Q",]
NUM_OF_PERSONS_11_BASED_ON_PCLASS2_EMBARK_S <- NUM_OF_PERSONS_11_BASED_ON_PCLASS2[NUM_OF_PERSONS_11_BASED_ON_PCLASS2$embark == "S",]
#################################PCLASS3
NUM_OF_PERSONS_11_BASED_ON_PCLASS3_EMBARK_C <- NUM_OF_PERSONS_11_BASED_ON_PCLASS3[NUM_OF_PERSONS_11_BASED_ON_PCLASS3$embark == "C",]
NUM_OF_PERSONS_11_BASED_ON_PCLASS3_EMBARK_Q <- NUM_OF_PERSONS_11_BASED_ON_PCLASS3[NUM_OF_PERSONS_11_BASED_ON_PCLASS3$embark == "Q",]
NUM_OF_PERSONS_11_BASED_ON_PCLASS3_EMBARK_S <- NUM_OF_PERSONS_11_BASED_ON_PCLASS3[NUM_OF_PERSONS_11_BASED_ON_PCLASS3$embark == "S",]


####################################################################################

get_summary <- function(v)
{ uniqv <- unique(v)
x <- 
  m <- summary(v)
mod <- uniqv[which.max(tabulate(match(v, uniqv)))]
return(list(m,mod))

}
#filling missing values in embark
# by looking at the fare of two persons travelling the we can find the embark
#THE FARE FOR THE MISSING VALUES IN ERMBAEK IS 80
get_summary(NUM_OF_PERSONS_1_BASED_ON_PCLASS1_EMBARK_C)
get_summary(NUM_OF_PERSONS_1_BASED_ON_PCLASS1_EMBARK_Q)
get_summary(NUM_OF_PERSONS_1_BASED_ON_PCLASS1_EMBARK_S)
# FROM THIS WE FOUND THAT THEY EMBARK AT C
train$Embarked[c(62, 830)] <- 'C'
# now we WILL FILL THE MIISING VALUES IN FARES 
# WE FOUND THE MIISING VALUE AT INDEX 1044
#BASED ON WHERE THEY EMBARK AND CLASS WE CAN FIND FARE
get_summary(train[train$Pclass == 3 & train$Embarked == 'S',9])
train$Fare[1044] <- 8.05
# NOW FILLING THE MISSING VALUES AT AGES
library("mice", lib.loc="~/R/win-library/3.4")
mice_mod <- mice(train[, !names(train) %in% c('PassengerId','Name','Ticket','Cabin','Family','Familynames','Survived')], method='rf') 
#seeing the disturbuation
mice_output <- complete(mice_mod)

par(mfrow=c(1,2))
hist(train$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))

train$Age <- mice_output$Age
sum(is.na(train$Age))
#another future 

age_status <- c()
age_status[which(train$Age <15)] <- 0
age_status[which(train$Age >=15)] <- 1
summary(age_status)
train$age_status <- age_status


#another feature no of persons travelling on each ticket
persons_ticket <- c()
for(i in 1:1309)
{
  persons_ticket[i] <- table_of_tickets[names(table_of_tickets) == train$Ticket[i]]
}
train$persons_ticket <- persons_ticket
#############################################
train1 <- train[1:891,c(2,4,5,6,7,9,11,13,14,16,17)]
test<- train[892:1309,c(2,4,5,6,7,9,11,13,14,16,17)]
View(train1)
train1$survived <- tr$Survived
#### first we will see logestic regression classfier
log_model <- glm(survived ~.,family=binomial(link='logit'),data=train1)
summary(log_model)
anova(log_model, test="Chisq")
library(pscl)
pR2(log_model)
fitted.results <- predict(log_model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results !=  givensoloution$Survived)
print(paste('Accuracy',1-misClasificError))
write.csv(fitted.results,file = "log_fitted_values")
accuary_log_classfier <- 1-misClasificError
library(ROCR)
p <- predict(log_model, newdata=test, type="response")
pr <- prediction(p, givensoloution$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

