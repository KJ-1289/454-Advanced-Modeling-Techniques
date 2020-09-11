setwd("C:/Users/KJohn/OneDrive/Desktop/Northwestern/MSDS 454 Advanced Modeling Techniques/Assignment 5")

spam.df <- data.frame(readRDS('spam_base_v2.RData'))
str(spam.df)

# Split data and drop non-modeling variables;
drop.list  <- c('u','train','test');
spam.train <- subset(spam.df,train==1)[, !(names(spam.df) %in% drop.list)];
spam.test  <- subset(spam.df,test==1)[, !(names(spam.df) %in% drop.list)];

dim(spam.train)
dim(spam.test)
names(spam.train)

###################################
##Section II: Feature Engineering##
###################################

library(woeBinning)

#Take Top 10 Variables from XGBoost Model
spam.train.top10 <- subset(spam.train, 
                            select=c('char_freq_dollar', 'char_freq_xpoint', 
                                     'word_freq_remove', 'word_freq_hp', 
                                     'capital_run_length_total', 'word_freq_free',
                                     'capital_run_length_longest', 'word_freq_george',
                                     'word_freq_edu', 'word_freq_our', 'spam'))
dim(spam.train.top10)
names(spam.train.top10)

##############
##Variable 1##
##############

woe.char_freq_dollar <- woe.binning(df=spam.train.top10,target.var=c('spam'),
                                    pred.var=c('char_freq_dollar'))

# WOE plot for age bins;
woe.binning.plot(woe.char_freq_dollar)

# Score bins on data frame;
woe.df <- woe.binning.deploy(df=spam.train.top10, binning=woe.char_freq_dollar)
head(woe.df)
table(woe.df$char_freq_dollar.binned)

# See the WOE Binning Table
woe.binning.table(woe.char_freq_dollar)

##############
##Variable 2##
##############

woe.char_freq_xpoint <- woe.binning(df=spam.train.top10,target.var=c('spam'),
                                    pred.var=c('char_freq_xpoint'))

# WOE plot for age bins;
woe.binning.plot(woe.char_freq_xpoint)

# Score bins on data frame;
woe.df <- woe.binning.deploy(df=spam.train.top10, binning=woe.char_freq_xpoint)
head(woe.df)
table(woe.df$char_freq_xpoint.binned)

# See the WOE Binning Table
woe.binning.table(woe.char_freq_xpoint)

##############
##Variable 3##
##############

woe.word_freq_remove <- woe.binning(df=spam.train.top10,target.var=c('spam'),
                                    pred.var=c('word_freq_remove'))

# WOE plot for age bins;
woe.binning.plot(woe.word_freq_remove)

# Score bins on data frame;
woe.df <- woe.binning.deploy(df=spam.train.top10, binning=woe.word_freq_remove)
head(woe.df)
table(woe.df$word_freq_remove.binned)

# See the WOE Binning Table
woe.binning.table(woe.word_freq_remove)

##############
##Variable 4##
##############

woe.word_freq_hp <- woe.binning(df=spam.train.top10,target.var=c('spam'),
                                    pred.var=c('word_freq_hp'))

# WOE plot for age bins;
woe.binning.plot(woe.word_freq_hp)

# Score bins on data frame;
woe.df <- woe.binning.deploy(df=spam.train.top10, binning=woe.word_freq_hp)
head(woe.df)
table(woe.df$word_freq_hp.binned)

# See the WOE Binning Table
woe.binning.table(woe.word_freq_hp)

##############
##Variable 5##
##############

woe.capital_run_length_total <- woe.binning(df=spam.train.top10,target.var=c('spam'),
                                pred.var=c('capital_run_length_total'))

# WOE plot for age bins;
woe.binning.plot(woe.capital_run_length_total)

# Score bins on data frame;
woe.df <- woe.binning.deploy(df=spam.train.top10, binning=woe.capital_run_length_total)
head(woe.df)
table(woe.df$capital_run_length_total.binned)

# See the WOE Binning Table
woe.binning.table(woe.capital_run_length_total)

##############
##Variable 6##
##############

woe.word_freq_free <- woe.binning(df=spam.train.top10,target.var=c('spam'),
                                            pred.var=c('word_freq_free'))

# WOE plot for age bins;
woe.binning.plot(woe.word_freq_free)

# Score bins on data frame;
woe.df <- woe.binning.deploy(df=spam.train.top10, binning=woe.word_freq_free)
head(woe.df)
table(woe.df$word_freq_free.binned)

# See the WOE Binning Table
woe.binning.table(woe.word_freq_free)

##############
##Variable 7##
##############

woe.capital_run_length_longest <- woe.binning(df=spam.train.top10,target.var=c('spam'),
                                  pred.var=c('capital_run_length_longest'))

# WOE plot for age bins;
woe.binning.plot(woe.capital_run_length_longest)

# Score bins on data frame;
woe.df <- woe.binning.deploy(df=spam.train.top10, binning=woe.capital_run_length_longest)
head(woe.df)
table(woe.df$capital_run_length_longest.binned)

# See the WOE Binning Table
woe.binning.table(woe.capital_run_length_longest)

##############
##Variable 8##
##############

woe.word_freq_george <- woe.binning(df=spam.train.top10,target.var=c('spam'),
                                              pred.var=c('word_freq_george'))

# WOE plot for age bins;
woe.binning.plot(woe.word_freq_george)

# Score bins on data frame;
woe.df <- woe.binning.deploy(df=spam.train.top10, binning=woe.word_freq_george)
head(woe.df)
table(woe.df$word_freq_george.binned)

# See the WOE Binning Table
woe.binning.table(woe.word_freq_george)

##############
##Variable 9##
##############

woe.word_freq_edu <- woe.binning(df=spam.train.top10,target.var=c('spam'),
                                    pred.var=c('word_freq_edu'))

# WOE plot for age bins;
woe.binning.plot(woe.word_freq_edu)

# Score bins on data frame;
woe.df <- woe.binning.deploy(df=spam.train.top10, binning=woe.word_freq_edu)
head(woe.df)
table(woe.df$word_freq_edu.binned)

# See the WOE Binning Table
woe.binning.table(woe.word_freq_edu)

###############
##Variable 10##
###############

woe.word_freq_our <- woe.binning(df=spam.train.top10,target.var=c('spam'),
                                 pred.var=c('word_freq_our'))

# WOE plot for age bins;
woe.binning.plot(woe.word_freq_our)

# Score bins on data frame;
woe.df <- woe.binning.deploy(df=spam.train.top10, binning=woe.word_freq_our)
head(woe.df)
table(woe.df$word_freq_our.binned)

# See the WOE Binning Table
woe.binning.table(woe.word_freq_our)

##############
##IV Summary##
##############

Variable <- c("char_freq_xpoint", "word_freq_remove", "char_freq_dollar", 
              "word_freq_george", "word_freq_hp", "capital_run_length_longest",
              "word_freq_free", "word_freq_our", "capital_run_length_total",
              "word_freq_edu")
Information.Value <- c(1.673, 1.647, 1.558, 1.305, 1.280, 1.279, 1.080, 0.853, 0.777,
                       0.196)
iv.df <- data.frame(Variable, Information.Value)
formattable(iv.df)


## Data Frame
binning.var <- woe.binning(df=spam.train.top10,target.var=c('spam'),
                                    pred.var=c('char_freq_dollar', 'char_freq_xpoint', 
                                               'word_freq_remove', 'word_freq_hp', 
                                               'capital_run_length_total', 'word_freq_free',
                                               'capital_run_length_longest', 'word_freq_george',
                                               'word_freq_edu', 'word_freq_our'))

binned.df <- woe.binning.deploy(df=spam.train.top10, binning.var)
binned.df <- subset(binned.df, select = c("char_freq_xpoint.binned", "word_freq_remove.binned",
                                          "char_freq_dollar.binned", "word_freq_george.binned",
                                          "word_freq_hp.binned", "capital_run_length_longest.binned",
                                          "word_freq_free.binned", "word_freq_our.binned",
                                          "capital_run_length_total.binned", "word_freq_edu.binned",
                                          "spam"))
names(binned.df)
dim(binned.df)
