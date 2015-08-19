library(XML)
# Some helper functions
library(tm) 
readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }
# source('/Users/vickyzhang/Documents/MSBA/predictive2/R/textutils.R')
# setwd('/Users/vickyzhang/Documents/MSBA/predictive2/STA380/R')
# author_dirs = Sys.glob('../data/ReutersC50/C50train/*')
# #author_dirs = author_dirs[1:2]
# file_list = NULL
# labels = NULL
# for(author in author_dirs) {
# 	author_name = substring(author, first=29)
# 	files_to_add = Sys.glob(paste0(author, '/*.txt'))
# 	file_list = append(file_list, files_to_add)
# 	labels = append(labels, rep(author_name, length(files_to_add)))
# }
# # Need a more clever regex to get better names here
# all_docs = lapply(file_list, readerPlain) 
# names(all_docs) = file_list
# names(all_docs) = sub('.txt', '', names(all_docs))
# 
# my_corpus = Corpus(VectorSource(all_docs))
# names(my_corpus) = file_list
# # Preprocessing, tokenization, data cleaning
# my_corpus = tm_map(my_corpus, content_transformer(tolower)) # make everything lowercase
# my_corpus = tm_map(my_corpus, content_transformer(removeNumbers)) # remove numbers
# my_corpus = tm_map(my_corpus, content_transformer(removePunctuation)) # remove punctuation
# my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace)) ## remove excess white-space
# my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords("SMART"))
# 
# DTM = DocumentTermMatrix(my_corpus)
# DTM # some basic summary statistics
# class(DTM)  # a special kind of sparse matrix format
# 
# ## You can inspect its entries...
# inspect(DTM[1:10,1:20])
# DTM = removeSparseTerms(DTM, 0.975) # remove those that are 0 in 97.5% of the docs or more
# DTM
# # Now a dense matrix
# X = as.matrix(DTM)
# dim(X)
# # Naive Bayes. only take 45 out of 50 stories for each author because we have to leave out 5 for test.
# smooth_count = 1/nrow(X)
# # using rbind, each row is an author, each col is a word
# prob = foreach(i = 1:50, .combine='rbind') %do% {
#   AP_train = X[(1+50*(i-1)):(50*i),] # be careful about dimensions, always specify both rows and col!
#   w_AP = colSums(AP_train + smooth_count)
#   w_AP = w_AP/sum(w_AP)
# }
# dim(prob)
# 
# ############ test for indexing problem: test on all articles in the training set to see if it predicts correctly, if yes, then it's problem of indexing#######
# predictions = foreach(j=1:2500, .combine='rbind') %do% {
#   x_test = X[j,]
#   
#   logprob = foreach(i = 1:50, .combine='rbind') %do% {
#     sum(x_test*log(prob[i,]))
#   }
#   
#   rownames(logprob) = author_dirs
#   logprob = t(logprob)
#   predict = names(logprob[,logprob == max(logprob)])
# 
# }
# 
# truth = foreach(j=1:50, .combine='c') %do% {
#   one_author = rep(author_dirs[j],50)
# }
# 
# accuracy = foreach(j=1:2500, .combine='c') %do% {
#   if (predictions[j] == truth[j]) {
#     result = TRUE
#   }
#   else {
#     result = FALSE
#   }
#   result
# }
# accuracy[accuracy == TRUE] = 1
# accuracy[accuracy == FALSE] = 0
# 
# sum(accuracy)
# 
# ################## end of test for indexing. conclusion: Yes, there is an indexing problem.#######
# ####### take a test document
# x_test = X[59,]
# # each col is an author
# logprob = foreach(i = 1:50, .combine='rbind') %do% {
#   sum(x_test*log(prob[i,]))
# }
# # get list of authors
# author_dirs = Sys.glob('../data/ReutersC50/C50train/*')
# author_dirs = lapply(author_dirs, function(x){substring(x, first=29)}) 
# # set this list as row names of logprob
# rownames(logprob) = author_dirs
# head(logprob)
# logprob = t(logprob)
# names(logprob[,logprob == max(logprob)])

### get test articles from both test and training directory, do all the pre processing steps
test_dirs = Sys.glob(c('../data/ReutersC50/C50tests/*', '../data/ReutersC50/C50train/*'))
file_list = NULL
labels = NULL
for(author in test_dirs) {
  author_name = substring(author, first=29)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}
# Need a more clever regex to get better names here
all_docs = lapply(file_list, readerPlain) 
names(all_docs) = file_list
names(all_docs) = sub('.txt', '', names(all_docs))

my_corpus = Corpus(VectorSource(all_docs))
names(my_corpus) = file_list

# Preprocessing, tokenization, data cleaning
my_corpus = tm_map(my_corpus, content_transformer(tolower)) # make everything lowercase
my_corpus = tm_map(my_corpus, content_transformer(removeNumbers)) # remove numbers
my_corpus = tm_map(my_corpus, content_transformer(removePunctuation)) # remove punctuation
my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords("SMART"))

DTM = DocumentTermMatrix(my_corpus)
DTM # some basic summary statistics
class(DTM)  # a special kind of sparse matrix format

## You can inspect its entries...
inspect(DTM[1:10,1:20])
DTM = removeSparseTerms(DTM, 0.975) # remove those that are 0 in 97.5% of the docs or more
DTM
# Now a dense matrix
Z = as.matrix(DTM) # Y is test data matrix
dim(Z)

# get prob for training
X = Z[1:2500,]
prob = foreach(i = 1:50, .combine='rbind') %do% {
  AP_train = X[(1+50*(i-1)):(50*i),] # be careful about dimensions, always specify both rows and col!
  w_AP = colSums(AP_train + smooth_count)
  w_AP = w_AP/sum(w_AP)
}
dim(prob)
# 
# # get one test article
# 
# 
# y_test = Y[49,]
# length(y_test)
# dim(Y)
# dim(prob)
# 
# ## we can't get the inner product of y_test and prob now because their
# #dimensions are different. In other words, y_test doesn't have the words unseen
# #in test, prob doesn't have the words unseen in training. So we have to make
# #sure they have all words seen in training OR test. 
# 
# # deal with words never seen before in training set. first, get a list of word never seen before.
# unseen_list = vector()
# for (word in colnames(Y)) {
#   if (word %in% colnames(X)) {
#     
#   }
#   else {
#     unseen_list = append(unseen_list, word)
#   }
# }
# length(unseen_list)
# # next, assign a smoothing factor to all words unseen.
# smooth_count = 1/nrow(Y)
# smooth_list = rep(smooth_count, 50)
# for (word in unseen_list) {
#   prob = cbind(prob, smooth_list)
# }
# head(prob)
# colnames(prob) = c(colnames(X), unseen_list) # successfully let prob have all seen and unseen words
# rownames(prob) = author_dirs
#   # y_test doesn't have the ones seen in X (training) but not seen in Y (test),
#   # so have to add those to it too, and assign smoothing factor to it.
# 
#   seen_not_in_test = vector()
#   for (word in colnames(X)) {
#     if (word %in% colnames(Y)) {
#       
#     }
#     else {
#       seen_not_in_test = append(seen_not_in_test, word)
#     }
#   }
# ### further proof that there is an indexing problem
# colnames(prob)[1:10]
# y_test[1:10]
# # the two lists above look different, the first list doesn't have 'accept', but acutally colnames(prob) has 'accept', just not located in the first 10. so, try this:
# 'accept' %in% colnames(prob)
# # returns TRUE. so it's somewhere else, probably at the end of corpus.
# # have to rearrange sequence of words in y_test so that it matches the corpus
# y_test = data.frame(matrix(unlist(y_test)))
# y_test = y_test[,c(colnames(prob))]
# same_words = intersect(rownames(y_test), colnames(prob))
# library(gdata)
# reorder(X=y_test, new.order=c(colnames(prob)))



# have to remember the dot before combine argument, otherwise... it won't run
cols = colnames(prob)
Y = Z[2501:5000,]
predictions = foreach(j=1:2500, .combine='rbind') %do% {
  y_test = Y[j,]
  print(j)
  
  y_test = c(y_test, rep(smooth_count, length(seen_not_in_test)))
  names(y_test) = c(colnames(Y), seen_not_in_test) # succesfully let y_test include words seen in training
  # but not in test. Now y_test and log(prob) can do inner multiplication!!
  
  # for each article in test set, get the inner products, get the predicted author, put into a list
  
  
  logprob = foreach(i = 1:50, .combine='rbind') %do% {
    sum(y_test*log(prob[i,]))
  }
  
  # set the list of authors as row names of logprob
  rownames(logprob) = author_dirs
  logprob = t(logprob)
  # get the predicted author
  y_predict = names(logprob[,logprob == max(logprob)])
}

# Was my prediction correct? What's the truth?
truth = foreach(j=1:50, .combine='c') %do% {
  one_author = rep(author_dirs[j],50)
}
accuracy = foreach(j=1:2500, .combine='c') %do% {
  if (predictions[j] == truth[j]) {
    result = TRUE
  }
  else {
    result = FALSE
  }
  result
}
sum(accuracy) # numbers predicted corrrectly
sum(accuracy) / 2500 # prediction correct rate

```
So it doesn't work very well. Try PCA.

```{r}
# each row is the PCA of an author
pca_all_author = foreach(j=1:50, .combine='rbind') %do% {

corpus = X[j:(j+49),] # don't forget the bracket around j+49
corpus = corpus/rowSums(corpus)

# add unseen words to training data, assign smooth factors to them
smooth_list = rep(smooth_count, 50)
for (word in unseen_list) {
  corpus = cbind(corpus, smooth_list)
}
colnames(corpus) = c(colnames(X), unseen_list) # successfully let corpus have all seen and unseen words

# all prepared. run PCA!
pca_author = prcomp(corpus, scale=FALSE)
pca_author = pca_author$rotation[order(abs(pca_author$rotation[,1]),decreasing=TRUE),1]
}
pca_all_author[,(1541-152):1541] = 0.00000001
rownames(pca_all_author) = author_dirs

prediction_pca = foreach(j=1:2500, .combine='rbind') %do% {
  y_test = Y[j,]
  print(j)
  
  y_test = c(y_test, rep(smooth_count, length(seen_not_in_test)))
  names(y_test) = c(colnames(Y), seen_not_in_test) # succesfully let y_test include words seen in training
  # but not in test. Now y_test and log(prob) can do inner multiplication!!
  
  # for each article in test set, get the inner products, get the predicted author, put into a list
  logprob = foreach(i = 1:50, .combine='rbind') %do% {
    product = y_test*log(pca_all_author[i,])
    product[product == -Inf] = 0.0000001
    sum(product, na.rm = TRUE)
  }
  
  # set the list of authors as row names of logprob
  rownames(logprob) = author_dirs
  logprob = t(logprob)
  # get the predicted author
  y_predict = names(logprob[,logprob == max(logprob)])
}

correct = 0
accuracy_pca = foreach(j=1:2500, .combine='c') %do% {
  if (prediction_pca[j] == truth[j]) {
    result = TRUE
  }
  else {
    result = FALSE
  }
  result
}
accuracy[accuracy == TRUE] = 1
accuracy[accuracy == FALSE] = 0

sum(accuracy_pca)