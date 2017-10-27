# groupingKeywords
A function to review keyword / phrase similarities and assign cluster groupings

df='keywords' - the name of the dataframe containing a 'Keyword' column
weights=c(0,5,0,2) - the weightings to apply to identical words or bigrams in two keyword strings. 4 values, the first is a weighting to assign to a single word match, the second is a weighting to assign to no matches, the third is a weighting to assign to a single bigram match, the second is a weighting to assign to no bigram matches. The weightings are added to the adist() measure prior to tree cutting. i.e. the higher the weighting the more liekly you are going to exclude similar words for a chosen chop value. 
chop=6 - the cutree value for hclust()
chunksize=10 - the number of chunks to disect the dataframe into to allow calculation of large keyword vectors. 
