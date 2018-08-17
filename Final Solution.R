
# set the working directory setwd("C:/Users/Rock/Downloads/data warehouse/case study")

# install.packages("tidyr")
# install.packages("dplyr")
#install.packages("sqldf")

library(tidyr)
library(dplyr)
library(stringr)
library(RMySQL)
library(sqldf)


# reading companies and round2 data
companies<-read.delim("companies.txt", header = TRUE,stringsAsFactors = FALSE)
rounds2<-read.delim("rounds2.csv", header = TRUE,sep=",",stringsAsFactors = FALSE)


# How many unique companies are present in rounds2?

rounds2$company_permalink<-tolower(rounds2$company_permalink)
nrow(distinct(rounds2,company_permalink))


#How many unique companies are present in companies?	

companies$permalink<-tolower(companies$permalink)
nrow(distinct(companies,permalink))


#In the companies data frame, which column can be used as the unique key for each company? 
#Write the name of the column.------- permalink


#Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N

uniq_in_companies<-distinct(companies,permalink)
uniq_in_round2<-distinct(rounds2,company_permalink)
isTRUE(uniq_in_round2 != uniq_in_companies)

#2nd method for finding above

diff<-!(uniq_in_round2 %in% uniq_in_companies)
length(which(diff=="TRUE"))

#Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.
#Name the merged frame master_frame. How many observations are present in master_frame?

master_frame<- merge(x=rounds2,y=companies, by.x = ("company_permalink"),by.y = ("permalink"))


# replacing NA with zeros
#masterframe[which(is.na(masterframe$raised_amount_usd)),"raised_amount_usd"]<-0


#########################CHECKPOINT 2###################################

grp_fund_type<-group_by(master_frame,funding_round_type)
summary_of_funds<-summarise(grp_fund_type,mean(raised_amount_usd,na.rm = TRUE))
names(summary_of_funds)[2]<- "Avg"


#Average funding amount of venture type	
summary_of_funds[summary_of_funds$funding_round_type=="venture",]

#Average funding amount of angel type	 
summary_of_funds[summary_of_funds$funding_round_type=="angel",]

#Average funding amount of seed type	 
summary_of_funds[summary_of_funds$funding_round_type=="seed",]

#Average funding amount of private equity type
summary_of_funds[summary_of_funds$funding_round_type=="private_equity",]

#Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round,
#which investment type is the most suitable for it?

tempvar<-summary_of_funds[(summary_of_funds$funding_round_type %in% c("venture","seed","angel","private_equity")) &
                     (summary_of_funds$Avg>5000000 & summary_of_funds$Avg<15000000),]

tempvar


#########################CHECKPOINT 3 country analysis###################################

master_frame2 = filter(master_frame,funding_round_type=="venture" & country_code!="")

grp_country<- group_by(master_frame2,country_code)
top9<-summarise(grp_country,sum(raised_amount_usd,na.rm=TRUE))
names(top9)[2]<- "Total_Amount"

arrange(top9,desc(Total_Amount))



#########################CHECKPOINT 4 sector analysis-1 ###################################

#Readig mapping file
map_file<-read.csv("mapping.csv",header = TRUE,stringsAsFactors = FALSE)

#converting wide - long format and removing blank items
map_file2<-gather(map_file,main_sector,value,Automotive...Sports:Social..Finance..Analytics..Advertising)
map_file2<-map_file2[!(map_file2$value==0),]
map_file2<-map_file2[,-3]

#mapping primary sector 
master_frame2$primary_sector<-str_split(master_frame2$category_list,"\\|",simplify = TRUE)[,1]

names(map_file2)[1]<- "primary_sector"

#using left join mapping main sector in master DF 
master_frame3<- merge(master_frame2,map_file2,by="primary_sector",all.x = TRUE)


#########################CHECKPOINT 4 sector analysis-2 ###################################



#Create three separate data frames D1, D2 and D3 for each of the three countries containing
#the observations of funding type FT falling within the 5-15 million USD range

D1<-filter(master_frame3,country_code=="USA" & raised_amount_usd>=5000000 & raised_amount_usd<=15000000)
D2<-filter(master_frame3,country_code=="GBR" & raised_amount_usd>=5000000 & raised_amount_usd<=15000000)
D3<-filter(master_frame3,country_code=="IND" & raised_amount_usd>=5000000 & raised_amount_usd<=15000000)

#Total number of Investments (count)

nrow(D1)
nrow(D2)
nrow(D3)

#Total amount of investment (USD)

sum(D1$raised_amount_usd)
sum(D2$raised_amount_usd)
sum(D3$raised_amount_usd)



#The total number (or count) of investments for each main sector in a separate column
#The total amount invested in each main sector in a separate column

#D1 Summary 
D1_grp_by<-group_by(D1,main_sector)
D1_inv_cnt<-summarise(D1_grp_by,length(main_sector))
D1_inv_sum<-summarise(D1_grp_by,sum(raised_amount_usd))
D1_summary<-merge(D1_inv_cnt,D1_inv_sum,by="main_sector")

#D2 Summary 
D2_grp_by<-group_by(D2,main_sector)
D2_inv_cnt<-summarise(D2_grp_by,length(main_sector))
D2_inv_sum<-summarise(D2_grp_by,sum(raised_amount_usd))
D2_summary<-merge(D2_inv_cnt,D1_inv_sum,by="main_sector")

#D3 Summary 
D3_grp_by<-group_by(D3,main_sector)
D3_inv_cnt<-summarise(D3_grp_by,length(main_sector))
D3_inv_sum<-summarise(D3_grp_by,sum(raised_amount_usd))
D3_summary<-merge(D3_inv_cnt,D3_inv_sum,by="main_sector")

# Renaming col names
names(D1_summary)[2]<-"inv_cnt"
names(D1_summary)[3]<-"inv_tot"

names(D2_summary)[2]<-"inv_cnt"
names(D2_summary)[3]<-"inv_tot"

names(D3_summary)[2]<-"inv_cnt"
names(D3_summary)[3]<-"inv_tot"


#geting sumamry of top 
D1_top<-arrange(D1_summary,desc(inv_cnt))
D2_top<-arrange(D2_summary,desc(inv_cnt))
D3_top<-arrange(D3_summary,desc(inv_cnt))

#Find Top/2nd Top  Sector name (no. of investment-wise)

D1_top[1,1]
D2_top[1,1]
D3_top[1,1]

D1_top[2,1]
D2_top[2,1]
D3_top[2,1]

D1_top[3,1]
D2_top[3,1]
D3_top[3,1]


#For the top sector count-wise (point 3), which company received the highest investment?

D1_top_co<-subset(D1,D1$main_sector==D1_top[1,1])
temp_grp1<-group_by(D1_top_co,company_permalink)
D1_top_summ<-summarise(temp_grp1,sum(raised_amount_usd))
x1<-arrange(D1_top_summ,desc(`sum(raised_amount_usd)`))

D2_top_co<-subset(D2,D2$main_sector==D2_top[1,1])
temp_grp2<-group_by(D2_top_co,company_permalink)
D2_top_summ<-summarise(temp_grp2,sum(raised_amount_usd))
x2<-arrange(D2_top_summ,desc(`sum(raised_amount_usd)`))

D3_top_co<-subset(D3,D3$main_sector==D3_top[1,1])
temp_grp3<-group_by(D3_top_co,company_permalink)
D3_top_summ<-summarise(temp_grp3,sum(raised_amount_usd))
x3<-arrange(D3_top_summ,desc(`sum(raised_amount_usd)`))

x1[1,1]
x2[1,1]
x3[1,1]

#For the 2nd top sector count-wise (point 3), which company received the highest investment?

D1_2top_co<-subset(D1,D1$main_sector==D1_top[2,1])
temp_grp21<-group_by(D1_2top_co,company_permalink)
D1_2top_summ<-summarise(temp_grp21,sum(raised_amount_usd))
y1<-arrange(D1_2top_summ,desc(`sum(raised_amount_usd)`))

D2_2top_co<-subset(D2,D2$main_sector==D2_top[2,1])
temp_grp22<-group_by(D2_2top_co,company_permalink)
D2_2top_summ<-summarise(temp_grp22,sum(raised_amount_usd))
y2<-arrange(D2_2top_summ,desc(`sum(raised_amount_usd)`))

D3_2top_co<-subset(D3,D3$main_sector==D3_top[2,1])
temp_grp23<-group_by(D3_2top_co,company_permalink)
D3_2top_summ<-summarise(temp_grp23,sum(raised_amount_usd))
y3<-arrange(D3_2top_summ,desc(`sum(raised_amount_usd)`))

y1[1,1]
y2[1,1]
y3[1,1]



#########################output files for tableau##################

options(scipen = 999)

D1_top$country<-"USA"
D2_top$country<-"GBR"
D3_top$country<-"IND"

tmp1<-rbind(D1_top,D2_top,D3_top)

write.csv(tmp1,file='Top3Sector.csv',row.names = FALSE)

write.csv(master_frame,file='master frame.csv',row.names = FALSE,na="")

write.csv(top9,file='top9.csv',row.names = FALSE)


############end of solution###############

