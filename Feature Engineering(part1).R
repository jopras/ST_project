
# "Classification of the Furniture Description"

#uploading the datasets
total3=read.csv("Final_total.csv", stringsAsFactors = F)
furnit_det=read.csv("Deliveries_Details.csv", stringsAsFactors = F)
library(readxl)
library(sqldf)

#merge the total data with the Deliveries Details
third_join=sqldf("select * from furnit_det a left outer join
                 total3 b on a.order_number=b.orderID 
                 and a.despatch_note=b.despatch_note and a.order_branch=b.order_branch")

third_join_clean=na.omit(third_join)#omiting the NA's

#Tokenization


#call library stringr


library(stringr)
library(dplyr)

# isolate the text column into one dataframe
text_df <- (third_join_clean$product_desc)

Clean_String <- function(string){
  # Lowercase
  temp <- tolower(string)
  #' Remove everything that is not a number or letter (may want to keep more 
  #' stuff in your actual analyses). 
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  # Split it
  temp <- stringr::str_split(temp, " ")[[1]]
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } 
  return(temp)
}

#' function to clean text
Clean_Text_Block <- function(text){
  if(length(text) <= 1){
    # Check to see if there is any text at all with another conditional
    if(length(text) == 0){
      cat("There was no text in this document! \n")
      to_return <- list(num_tokens = 0, unique_tokens = 0, text = " ")
    }else{
      # If there is , and only only one line of text then tokenize it
      clean_text <- Clean_String(text)
      num_tok <- length(clean_text)
      num_uniq <- length(unique(clean_text))
      to_return <- list(num_tokens = num_tok, unique_tokens = num_uniq, text= clean_text)
    }
  }else{
    # Get rid of blank lines
    indexes <- which(text_df == " ")
    if(length(indexes) > 0){
      text <- text_df[-indexes]
    }  
    # Loop through the lines in the text and use the append() function to 
    clean_text <- Clean_String(text[1])
    for(i in 2:length(text_df)){
      # add them to a vector 
      clean_text<- append(clean_text,Clean_String(text[i]))
    }
    # Calculate the number of tokens and unique tokens and return them in a 
    # named list object.
    num_tok <- length(clean_text)
    num_uniq <- length(unique(clean_text))
    to_return <- list(num_tokens = num_tok, unique_tokens = num_uniq, text = clean_text)
  }
  return(to_return)
}

#we are using the function "Clean_Text_Block" in our Description variable 
clean_description<- Clean_Text_Block(text_df)
str(clean_description)

clean_description

clean_description2 <- data.frame(clean_description, stringsAsFactors = F)
colnames(clean_description2)[3]<-"word"

#creating furniture categories
words<-clean_description2$word
sofa_ls=c("sof","rhf","lhf","rh","lh")
chair_ls=c("ch","chr","chairs","swivel","pouffe")
table_ls=c("tbl","tables","tbls")
rec_ls=c("rec","powrec","re","rcln","chaise","chse")
stool_ls=c("footstool","stl","fstl","stl")
bed_ls=c("sofabed","sbed","bedframe","mattress","mtr")
fabricare_ls=("fab")
drawer_ls=c("drw","dr")
cushion_ls=c("sc","scatter","chs","cush","cushions","bolsters","bolster","pillow","pillows")
index=which(words %in% sofa_ls ) 
index_2=which(words %in% chair_ls )
index_3=which(words %in% table_ls )
index_4=which(words %in% rec_ls)
index_5=which(words %in% stool_ls)
index_6=which(words %in% bed_ls )
index_7=which(words %in% fabricare_ls )
index_8=which(words %in% drawer_ls)
index_9=which(words %in% cushion_ls)
words[index]<-"sofa"
words[index_2]<-"chair"
words[index_3]<-"table"
words[index_4]<-"recliner"
words[index_5]<-"stool"  
words[index_6]<-"bed" 
words[index_7]<-"fabricare" 
words[index_8]<-"drawer"
words[index_9]<-"cushion"

# creating special characteristic categories
corner_ls=c("crn","cr","cnr")
storage_ls=c("strg","str","st","stg")
power_ls=c("pow","pw","pwr","elect","electric")
large_ls=c("lrg","lcg","lg")
index_10=which(words %in% corner_ls )
index_11=which(words %in% storage_ls )
index_12=which(words %in% power_ls)
index_13=which(words %in% large_ls)
words[index_10]<-"corner" 
words[index_11]<-"storage" 
words[index_12]<-"power"
words[index_13]<-"large"


#cleaning the dataset by removing stop_words
library(NLP)
library(tidytext)
data(stop_words)
clean_description2 <- clean_description2 %>%
  anti_join(stop_words,clean_description2, by="word")

clean_description2 %>%
  count(word, sort = TRUE)

#ploting words with frequency>250
library(ggplot2)
clean_description2 %>%
  count(word, sort = TRUE) %>%
  filter(n > 1000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


library(foreign)
library(scales)
library(tm)
library(ggplot2)

clean_description_only=clean_description2$word
#exporting clean_description as text file
write.table(clean_description_only,file="/Users/ioannaprassou/Google Drive/Dissertation Project/data/clean_description_only.txt")
# Read the text file 
text <- readLines("/Users/ioannaprassou/Google Drive/Dissertation Project/data/clean_description_only.txt")
# Load the data as a corpus
docs <- Corpus(VectorSource(text))
#Inspect the content of the document
inspect(docs)

#Text transformation
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

#Cleaning the text

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)


#Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)

# Generate the Word cloud

library(RColorBrewer)
library(wordcloud)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 0.65,
          max.words=300, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(3, "Dark2"))
#we count the times the frequency of its word
library(sqldf)
clean_description2=sqldf("select word,count(1)
                         from clean_description2 group by word 
                         having count(1)>=1")

colnames(clean_description2)[2]<-"frequency"
#creating a list with the words that appear more than 100 times in the furniture description
top_words=sqldf("select distinct * from clean_description2 where frequency>=20")

write.table(top_words,file="top_words.csv")


#removing irrelevant for our model words, such as "names" given to furnitures or abbreviations without any meaning

#creating a list with all the irrelevant words & abbreviations

irrelevant_words=c("garrick","care","kit","rs","products","uk","cherish","natural","standard","pb",
                   "pf","pattern","newbury","gl","plain","navona","bol","unit",
                   "sophia","zinc","sprint","sfst","lrg","formal","pw",
                   "pf","fxd","accent","kalamos","choice","designers","newbury",
                   "samson","grammar","skye","rc","aurora","freya","rp","duty","ft","sct",
                   "xe","lydia","falcon","hartley","carmello","pln","plain","lounger",
                   "bowness","check","vision","force","spr","salone","dalmore","ceasar","esquire","pr","caldbeck",
                   "daytona","std","lg","beau","moon","richie","kalamos","fixed","supreme","ex","quest","karisma","half","keeper","na",
                   "active","estate","shine","global","accent","evolution","fs","corinne","ac","astaire","haze","rm","ue","helix","dillon",
                   "eleanor","genesis","shannon","quantum","skyline","linea","toulon","loch","domain","leven","ripple","seat","farrow","arran","dlx",
                   "oakland","liaison","pce","sr","chpr","cure","lh","aspen","el","guide","strg","peyton","bexley","cold","angelic",
                   "exp","rafael","superb","chance","bk","dfs","escape","patch","dice","multi","cr","inception","ut","leyburn","rh",
                   "pioneer","denver","ashby","pearl","er","fxd","medium","rupert","woodlea","fm","stage","ve","top","fling","zuri",
                   "jasper","sch","tranquil","trident","pc","woodland","gower","storm","thorpe","curved","removable","rise","opera","allons","gravity",
                   "rfsta","vettel","stow","fiji","croft","piece","starlet","wing","hogarth","valdez","moray","country","silk","dalton","paradize",
                   "destiny","goulding","cato","kalispera","mn","tilt","liftrise","zenith","emote","maxi","swv","cn","rect","unt","chs",
                   "rhythm","concerto","fx","matt","scat","quentin","rushton","sml","sm","fabric","sts","noarm","grand","deluxe","midi",
                   "fb","diego","pennie","trafalgar","waltz","pl","giovanna","siesta","thrive","elvarado","reign","glow","calvino","cs","merit",
                   "option","pair","blanche","lnger","caps","lustre","padstow","voyage","etoile","metre","mat","oskar","shaped","dazzle","dx",
                   "wyndham","hardy","parade","indulge","opt","oval","rdfsta","banq","dream","valiant","grp","swvl","davenport","latex","floral",
                   "ottoman","patt","anvil","condo","ellie","ertc","sz","king","mc","ortho","relax","viscount","bellini","nexus","tetris",
                   "pckt","pinter","skill","double","focus","octavious","dbl","lusso","margot","oslo","otto","snuggler","lngr","maya","memory",
                   "rachel","krystal","nest","opal","jewel","choice","designers","gloss","london","quatro","react","shout","blink","chk","focal",
                   "ms","butterfly","etls","etrs","lp","sqc","wave","optimum","associate","form","loxley","med","sngl","ssc","zahara")

#creating a dataframe for irrelevant words
irrelevant_words_df<-data.frame(irrelevant_words,stringsAsFactors = F)
colnames(irrelevant_words_df)[1]<-"word"

#creating a list with all furniture categories
furniture_categories=c("sofa","sof","rhf","lhf","rh","lh","chair","ch","chr","chairs","swivel",
                       "pouffe","table","tbl","tables","tbls","bed","sofabed","sbed","bedframe","mattress","mtr","recliner","rec","powrec",
                       "footstool","stool","sfst","stl","fab","fabricare",
                       "drawer","dr","drw","sc","scatter","chs","cush","cushions",
                       "bolster","bolsters","banquette","chaise","chse","other")


#creating a dataframe for furniture_categories
furniture_categories_df<-data.frame(furniture_categories,stringsAsFactors = F)
colnames(furniture_categories_df)[1]<-"word"


#creating a list with all special furniture characteristics
special_characteristics=c("storage","stg","power","pow","pwr","elec","tv",
                          "foam","leather","manual","coffee","lamp","pillowback","corner")

#creating a dataframe for special_characteristics
special_characteristics_df<-data.frame(special_characteristics,stringsAsFactors = F)
colnames(special_characteristics_df)[1]<-"word"


#new clean function
Clean_String_2 <- function(string){
  # Lowercase
  temp <- tolower(string)
  #' Remove everything that is not a number or letter (may want to keep more 
  #' stuff in your actual analyses). 
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } 
  return(temp)
}

#creating a new dataframe from product description
product_description_df=third_join_clean$product_desc
product_description_df<- Clean_String_2(product_description_df)


write.table(product_description_df,file="product_description_df")

#removing irrelevant words
gsub(x = product_description_df , pattern = paste(irrelevant_words, collapse = "|"), replacement="",ignore.case = TRUE)


#tokenize the description
description=product_description_df
word="bookcase"
counter=function(word,description){
  furnt_cat=c()
  for(i in 1:length(description)){
    description_split=strsplit(description[i]," ")[[1]]
    furnt_cat[i]=ifelse(length(description_split[description_split==word])>0,1,0)
  }
  return(furnt_cat)
}


#creating features for the words that exist in the description 1=exists, 0=not exists
description_df=data.frame(description, stringsAsFactors = F)
for (var in 1:length(furniture_categories)){
  description_df[[paste0("flag_", furniture_categories[var])]] <- counter(furniture_categories[var],description_df$description)
}

#categorising features that belong to the same furniture category
description_df$sofas=rowSums(description_df[2:7])
description_df$chairs=rowSums(description_df[8:13])
description_df$tables=rowSums(description_df[14:17])
description_df$beds=rowSums(description_df[18:23])
description_df$recliners=rowSums(description_df[24:26])
description_df$stools=rowSums(description_df[27:30])
description_df$fabrics=rowSums(description_df[31:32])
description_df$drawers=rowSums(description_df[33:35])
description_df$cushions=rowSums(description_df[36:42])
description_df$outdoors=rowSums(description_df[43:45])

colnames(description_df)[46]<-"other"

description_df=description_df[,-2:-45]


#creating a new table that includes the furniture features
furnit_categ=description_df
furnit_categ$sum=rowSums( furnit_categ[,2:12] )

furniture_categories_df2=c("other","sofas","chairs","tables","beds","recliners",
                           "stools","fabrics","drawers","cushions","outdoors")


furniture_categories_df_2<-data.frame(furniture_categories_df2,stringsAsFactors = F)
colnames(furniture_categories_df_2)[1]<-"word"


#checking which word is included in the description 
#& create a new column with the final furniture categories
for(i in 1:nrow(furnit_categ)){
  ind=furnit_categ[i,2:12]>0
  sel=furniture_categories_df_2$word[ind]
  if (length(sel)>0){
    furnit_categ$new[i]=paste0(sel,collapse = "|")
  }else{
    furnit_categ$new[i]="other"#replacing the blank values with "other"
    furnit_categ$other[i]=1
  }
}

library(tm)
docs_2 <- Corpus(VectorSource(furnit_categ$new))
#Inspect the content of the document
inspect(docs_2 )


#Build a term-document matrix
dtm <- TermDocumentMatrix(docs_2)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)


# Generate the Word cloud

library(RColorBrewer)
library(wordcloud)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 0.65,
          max.words=300, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(3, "Dark2"))

library(sqldf)
#checking what other categories exist in others
other_furnt=sqldf("select * from furnit_categ where new='other' ")

description_df=data.frame(description, stringsAsFactors = F)
for (var in 1:length(special_characteristics)){
  description_df[[paste0("flag_", special_characteristics[var])]] <- counter(special_characteristics[var],description_df$description)
}

#create one column for the same meaning of special characteristics
description_df$flag_storage=rowSums(description_df[2:3])
description_df$flag_electric=rowSums(description_df[4:7])

description_df=description_df[,-3]
description_df=description_df[,-3:-6]

#creating a new table that includes the special characteristics
special_char=description_df
special_char$sum=rowSums( special_char[,2:11] )

#Creating new special characteristic features
special_characteristics_df2=c("storage","tv","foam","leather","manual","coffee",
                              "lamp","pillowback","corner","electric")


special_characteristics_df_2<-data.frame(special_characteristics_df2,stringsAsFactors = F)
colnames(special_characteristics_df_2)[1]<-"word"

#checking if which word is included in the description 
#& create a new column with the final special characteristics
for(i in 1:nrow(special_char)){
  ind=special_char[i,2:11]>0
  sel=special_characteristics_df_2$word[ind]
  if (length(sel)>0){
    special_char$new[i]=paste0(sel,collapse="_")
  }else{
    special_char$new[i]="NaN"#replacing the blank values with "NaN"
    
  }
}

docs_3<- Corpus(VectorSource(special_char$new))
#Inspect the content of the document
inspect(docs_3 )


#Build a term-document matrix
dtm <- TermDocumentMatrix(docs_3)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)

# Generate the Word cloud

library(RColorBrewer)
library(wordcloud)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 0.65,
          max.words=300, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(3, "Dark2"))


#aggrogating Details by orderID
orders=unique(Details_df$order_number)
Details_agr=matrix(0,length(orders), length(51:71))
for(i in 1:length(orders)){
  categ_mini=Details_df[which(Details_df$order_number==orders[i]),]
  vector=as.numeric(apply(categ_mini[,51:71],2,sum))
  Details_agr[i,]=vector
}

Details_agr2=data.frame(Details_agr,stringsAsFactors = F)
colnames(Details_agr2)<-colnames(Details_df)[51:71]
Details_agr2$orderID=orders

colnames(Details_agr2)[22]<-"order_number"

#match orders with same ID 

Details_by_order=sqldf("select order_number, despatch_note,Prod_New_Categ,Spec_Chrt,
                       sum(item_weight) item_weight_sum, max(item_weight) item_weight_max, 
                       sum(individual_pieces) individual_pieces_sum,
                       sum(item_volume) item_volume_sum from Details_df
                       group by order_number,despatch_note")

#selecting the distinct orders
All_Details=sqldf("select distinct * from Details_by_order a
                  inner join Details_agr2 b on a.order_number=b.order_number")

All_Details=All_Details[,-30]

#merging the third join with the new features created
All_in_one=sqldf("select * from All_Details a left outer join
                 total3 b on a.order_number=b.orderID
                 and a.despatch_note=b.despatch_note")

All_in_one=All_in_one[,-30:-31]
All_in_one=All_in_one[,-39]

#selecting the distinct values
All_in_one=sqldf("select distinct * from All_in_one")

write.csv(All_in_one,file="Final_All_in_one.csv")

