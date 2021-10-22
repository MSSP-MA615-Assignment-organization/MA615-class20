

library(tidyverse)
library(magrittr)

## load data

strawb <- read.csv("strawberry2021-oct7-c.csv",fileEncoding = "Latin1")


##################################################
## Drop the no-info columns
##################################

## capture column names
cnames <- colnames(strawb)


x <- 1:dim(strawb)[2]

## use this probe to reduce the number of columns

# colsummary <-  sapply(x, names <- function(x){a = nrow(distinct(strawb[x])) 
# return(a)})
# 
# drop_cols <- cnames[which(colsummary == 1)]

##Other way to reduce the columns
## set variable to collect values

T <- NULL

## collect number of unique rows in each column

for(i in x){T <- c(T, dim(unique(strawb[i]))[1])}
  

## use T to select columns to drop
drop_cols <- cnames[which(T == 1)]


## drop strawb
strawb %<>% select(!all_of(drop_cols))



drop_no_info_cols <- function(df){
  cnames = colnames(strawb)
  T = NULL
  for(i in 1:ncol(df)){T <- c(T, nrow(unique(df[i])))}
  drop_cols <- cnames[which(T == 1)]
  return(select(df, !all_of(drop_cols)))
}








#############################################################


strawb %<>% separate(col=Data.Item,
                into = c("Strawberries", "items", "discription", "units"),
                sep = ",",
                fill = "right")

####  explore

distinct(strawb, Strawberries)

distinct(strawb, items)

distinct(strawb, discription)

distinct(strawb, units)
  

################################################

## Separte Domain into 2 columns

strawb %<>%  separate(col=Domain,
                      into = c("dname", "type" ), 
                      sep = ",", 
                      fill = "right")
distinct(strawb, dname)


distinct(strawb, type)


###################################
## make a copy of Domain.Categoy

strawb %<>% 
  mutate(Chemicals = Domain.Category) %>% 
  relocate(Chemicals, .after = Domain.Category) 


## vector of logicals for each row with "CHEM" at 
## the start of strawb$Chemicals

bb <- strawb$Chemicals %>% str_detect("CHEM")

sum(bb)

## index 
ind_C <- (!bb)*(1:dim(strawb)[1])

## 
r1 <- ind_C[ind_C > 0]
r1
## set entries in Chemicals column to " " if they don't start with CHEM

strawb$Chemicals[r1] <- " "

#########################################

## now we need a list of chemicals

strawb %<>% separate(col = Chemicals,
                               into = c("title", "details"),
                               sep = ":",
                               fill = "right")

strawb %<>% mutate(details = str_extract(str_trim(details) ,"[^(].*[^)]") )

strawb %<>% mutate(type = str_trim(type))


distinct(strawb, details)

distinct(strawb, type)

#################################################################

strawb_chem <- strawb %>% filter((type=="FUNGICIDE")|
                                   (type=="HERBICIDE")|
                                   (type=="INSECTICIDE"))



strawb_other <- strawb %>% filter(type=="OTHER")


strawb_na <- strawb %>% filter(is.na(type)==TRUE)

##################################################################

distinct(strawb_chem, details)

distinct(strawb_other, details)

distinct(strawb_na, details)

#############################################

sc1 <- drop_no_info_cols(strawb_chem)





