
##Kevin Johnson
##Assignment 1: Build an RData Data Set


setwd("C:/Users/KJohn/OneDrive/Desktop/Northwestern/MSDS 454 Advanced Modeling Techniques")

###############################
##1. Read in the raw CSV file##
###############################

my.df <- read.csv("spambase_data.csv")
str(my.df)

##############################
##2. Add in the column names##
##############################

#Column names are taken from the spambase_Names file
cols <- c("word_freq_make", "word_freq_address", "word_freq_all", "word_freq_3d", 
          "word_freq_our", "word_freq_over", "word_freq_remove", "word_freq_internet", 
          "word_freq_order", "word_freq_mail", "word_freq_receive", "word_freq_will", 
          "word_freq_people", "word_freq_report", "word_freq_addresses", "word_freq_free",
          "word_freq_business", "word_freq_email", "word_freq_you", "word_freq_credit",
          "word_freq_your", "word_freq_font", "word_freq_000", "word_freq_money", 
          "word_freq_hp", "word_freq_hpl", "word_freq_george", "word_freq_650", 
          "word_freq_lab", "word_freq_labs", "word_freq_telnet", "word_freq_857", 
          "word_freq_data", "word_freq_415", "word_freq_85", "word_freq_technology", 
          "word_freq_1999", "word_freq_parts", "word_freq_pm", "word_freq_direct",
          "word_freq_cs", "word_freq_meeting", "word_freq_original", "word_freq_project",
          "word_freq_re", "word_freq_edu", "word_freq_table", "word_freq_conference", 
          "char_freq_;", "char_freq_(", "char_freq_[", "char_freq_!", "char_freq_$", 
          "char_freq_#", "capital_run_length_average", "capital_run_length_longest", 
          "capital_run_length_total", "spam")
colnames(my.df) <- cols

#Check column names of data frame
names(my.df)

#Check structure of data frame
str(my.df)

##############################################################################
##3. Create a 50/50 train/test split using a uniform random number generator##
##############################################################################

set.seed(123)
u <- runif(n=dim(my.df)[1],min=0,max=1)

#Split train and test sets 50/50
my.df$train <- ifelse(u<0.50,1,0)
my.df$test <- ifelse(u>=0.50,1,0)

#Check dimensions for train and test sets
train_len <- sum(my.df$train)/4600
train_len #The training data is 50.4% of the data set.
test_len <- sum(my.df$test)/4600
test_len #The testing data is 49.6% of the data set.

######################################################
##4. Save the processed data set as an .RData object##
######################################################

saveRDS(my.df, file = "spam_base_clean.RData")

######################################################################
##5. Read the .RData object back into R and check that it is correct##
######################################################################

my.df.clean <- readRDS("spam_base_clean.RData")

#Check structure of new data frame
str(my.df.clean)

#The correct file should be the same as the clean data frame from above
my.df.clean == my.df
