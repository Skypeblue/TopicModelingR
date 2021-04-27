library(gutenbergr)
library(tm)
library(SnowballC)
library(wordcloud)
library(dplyr)
library(stringr)
library(textstem)

set.seed(831)

df_title = data.frame(gutenberg_works()$title)

sum(is.na(df_title))

df_title = na.omit(df_title)
colnames(df_title) = "title"
df_title = filter(df_title,str_count(title,"\\w+")>7)
dim(df_title)

corp_title = Corpus(VectorSource(df_title$title))

inspect(corp_title[[1]])

mywords= c("volume","complete","vol")

oldw = getOption("warn")
options(warn=-1)
clean = 
function(corp){
corp %>% 
        tm_map(content_transformer(tolower)) %>% 
        tm_map(content_transformer(function(x) str_replace_all(x,"([\r\n—])|('s)"," "))) %>%        
        tm_map(removeWords,c(mywords,stopwords("en"))) %>%
        tm_map(removePunctuation) %>% 
        tm_map(content_transformer(lemmatize_strings)) %>%
        tm_map(stripWhitespace)
}
corp_title = clean(corp_title)
options(warn=oldw)

inspect(corp_title[[1]])

dtm = DocumentTermMatrix(corp_title)
inspect(dtm)

k=1000
rand = sample(seq_len(length(title)),size=floor(.5*length(title)))
test = dtm[rand,1:k]
inspect(test)

freq = data.frame(sort(colSums(as.matrix(test)),decreasing=TRUE))
dim(freq)
head(freq)
wordcloud(rownames(freq),freq[,1],max.words=20)

library(topicmodels)
ind = unique(dtm$i)
dtm=dtm[ind,] 

library(ggplot2)

find_best_nb = 
function(nb_topic){
        mod = LDA(dtm,k=nb_topic,method="Gibbs",
        control=list(alpha=0.5, iter=1000, seed=12345, thin=1))
        c(nb_topic,perplexity(mod,dtm))
}

graph = sapply(2:12,find_best_nb)
graph = data.frame(t(graph))

ggplot(data=graph,aes(x=X1,y=X2))+
                geom_line()+
                geom_point()+
                labs(title="Évolution de la perplexité selon le nombre de topic",
                x="Nombre de topic",
                y="Perplexité")+
                theme_minimal()

library(ldatuning)

res = FindTopicsNumber(
        dtm,
        topics = 2:20,
        metrics = c("CaoJuan2009","Arun2010"),
        method = "Gibbs",
        control = list(seed = 12345),
        mc.cores = 2L,
        verbose = TRUE)

FindTopicsNumber_plot(res)

nb_term = 12
lda_8 = LDA(dtm,8,method="Gibbs")

terms(lda_8,nb_term)

library(tidytext)

top = 
function(mod,nb_term){        
beta_mat = tidy(mod,matrix="beta")
top_word = beta_mat %>%
           group_by(topic) %>%
           top_n(nb_term,beta) %>%
           ungroup() %>%
           arrange(topic,-beta)
top_word %>%
        mutate(term = reorder_within(term, beta, topic)) %>%
        ggplot(aes(beta, term, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        scale_y_reordered()
}

top(lda_8,nb_term)
posterior(lda_8)$terms[1,]
posterior(lda_8)$topics[1,]

lda_20 = LDA(dtm,20,method="Gibbs")

top(lda_20,nb_term)
posterior(lda_20)$terms[1,]
posterior(lda_20)$topics[1,]

library(quanteda)

corp_qtd = corpus(corp_title)
n_gram = tokens_ngrams(quanteda::tokens(corp_qtd),n=1:3)
dtm_qtd = dfm(n_gram)
nfeat(dtm_qtd)

library(stm)
cor = readCorpus(dtm,type="slam")
out = prepDocuments(documents=cor$documents,vocab=cor$vocab)

stm.search = searchK(documents = out$documents,
                      vocab = out$vocab,
                      K = 10:30,
                      init.type = "Spectral")
plot(stm.search)

mod_stm = stm(documents = out$documents,vocab=out$vocab,K=13,init.type="Spectral",seed=831)
plot(mod_stm,n=4,text.cex=.8)

#matrice des probabilités a posteriori des documents selon les topic
mod_stm$theta[1,]



