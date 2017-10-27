# daniel Hough

create_labels <- function(df_label,label_title,weights=c(0,2,0,1),chop=6,plot_hc=FALSE){
  ### function to apply labels to keywords based on similar wording/phrase ###
  analysis_table <- get(paste0(df_label))
  analysis_table$Keyword <- as.character(analysis_table$Keyword)
  pb <- txtProgressBar(min = 0, max = length(analysis_table$Keyword), style = 3)
  # adist application
  C <- adist(analysis_table$Keyword)
  rownames(C) <- analysis_table$Keyword
  
  # word match
  B <- matrix(nrow=length(analysis_table$Keyword),ncol=length(analysis_table$Keyword))
  vec_for_mat <- c()
  for(i in 1:length(analysis_table$Keyword)){
    length <- vapply(strsplit(analysis_table$Keyword[i], "\\W+"), length, integer(1))
    terms <- c()
    for(k in 1:length){
      terms <- c(terms,word(analysis_table$Keyword[i],start=k,end=k))
    }
    terms <- terms[!is.na(terms)]
    row_1 <- sapply(analysis_table$Keyword,function(x){ifelse(any(sapply(terms,grepl,x,ignore.case=TRUE)),1,0)})
    vec_for_mat <- c(vec_for_mat,row_1)
    setTxtProgressBar(pb,i)
  }
  B <- matrix(vec_for_mat,nrow=length(analysis_table$Keyword),ncol=length(analysis_table$Keyword))
  
  rownames(B) <- analysis_table$Keyword
  B[is.na(B)] <- 0
  B[B==0] <- as.numeric(weights[2])
  B[B==1] <- as.numeric(weights[1])
  
  pd <- txtProgressBar(min = 0, max = length(analysis_table$Keyword), style = 3)
  
  # bigram match
  D <- matrix(nrow=length(analysis_table$Keyword),ncol=length(analysis_table$Keyword))
  vec_for_mat2 <- c()
  for(i in 1:length(analysis_table$Keyword)){
    length <- vapply(strsplit(analysis_table$Keyword[i], "\\W+"), length, integer(1))
    terms <- c()
    for(k in 1:(length-1)){
      terms <- c(terms,word(analysis_table$Keyword[i],start=k,end=k+1))
    }
    terms <- terms[!is.na(terms)]
    row_1 <- sapply(analysis_table$Keyword,function(x){ifelse(any(sapply(terms,grepl,x,ignore.case=TRUE)),1,0)})
    vec_for_mat <- c(vec_for_mat,row_1)
    setTxtProgressBar(pd,i)
  }
  D <- matrix(vec_for_mat,nrow=length(analysis_table$Keyword),ncol=length(analysis_table$Keyword))
  
  rownames(D) <- analysis_table$Keyword
  D[is.na(D)] <- 0
  D[D==0] <- as.numeric(weights[4])
  D[D==1] <- as.numeric(weights[3])
  
  # combine matrix
  A <- B + C + D
  
  hc <- hclust(as.dist(A))
  if(plot_hc==TRUE){
  plot(hc)
  }
  df <- data.frame(analysis_table$Keyword,cutree(hc,h=chop))
  colnames(df) <- c("Keyword","Label")
  df$Label <- as.character(df$Label)
  df$Keyword <- as.character(df$Keyword)
  analysis_table <- analysis_table %>% left_join(unique(df),by="Keyword") %>% mutate(Label = paste0(label_title,'_',Label))
  assign(paste0(df_label),analysis_table,envir = parent.frame())
}

apply_cl_chunks <- function(df,weights=c(0,2,0,1),chop=6,chunksize=1){
  keywords <- get(paste0(df))
  n <- chunksize
  k <- round(nrow(keywords)/n)
  part <- keywords[1:(k),]
  environment(create_labels) <- environment()
  create_labels('part','subset',weights=weights,chop=chop)
  part_first <- part[match(unique(part$Label), part$Label),]
  
  for(i in 2:(n-1)){
    next_part <- bind_rows(keywords[(k*(i-1)+1):(k*i),],part_first %>% select(-Label))
    environment(create_labels) <- environment()
    create_labels('next_part','subset',weights=weights,chop=chop)
    part_first <- part_first %>% left_join(select(next_part,Keyword,New_Label=Label),by="Keyword") %>% unique()
    part <- part %>% left_join(select(part_first,Label,New_Label),by="Label")
    part$Label <- part$New_Label
    part <- part %>% select(-New_Label)
    next_part <- bind_rows(part,next_part) %>% unique()
    part <- next_part
    part_first <- part[match(unique(part$Label), part$Label),]
  }
  next_part <- bind_rows(keywords[(k*(n-1)+1):nrow(keywords),],part_first %>% select(-Label))
  environment(create_labels) <- environment()
  create_labels('next_part','subset',weights=weights,chop=chop,plot_hc = TRUE)
  part_first <- part_first %>% left_join(select(next_part,Keyword,New_Label=Label),by="Keyword")
  part <- part %>% left_join(select(part_first,Label,New_Label),by="Label")
  part$Label <- part$New_Label
  part <- part %>% select(-New_Label)
  next_part <- bind_rows(part,next_part) %>% unique()
  part <- next_part
  part_first <- part[match(unique(part$Label), part$Label),]
  assign(df,part,envir = .GlobalEnv)
}

apply_cl_chunks(df='keywords',weights=c(0,5,0,2),chop=6,chunksize=10)

