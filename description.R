library(text2vec)
library(data.table)
library(magrittr)
library(stopwords)
library(SnowballC)
#library(qlcMatrix)

## Description based filtering
description <- na.omit(clean[,c("id","description")])
View(description)



## generate tfidf matrix
sim_description <- function(test){
  
  ## removing numerals
  prep_fun <- function(x) {gsub("\\d+", "", tolower(x))}
  ## removing stemming
  stem_tokenizer =function(x) {
    lapply(word_tokenizer(x), SnowballC::wordStem, language="en")
  }
  ## generating word tokens
  it_test <- itoken(test$description,  preprocessor = prep_fun, tokenizer = stem_tokenizer, ids = test$id, progressbar = FALSE)
  stop_words <- word_tokenizer("a, NA, about, above, across, after, again, against, all, almost, alone, along, already, also, although, always, am, among, an, and, another, any, anybody, anyone, anything, anywhere, are, area, areas, aren't, around, as, ask, asked, asking, asks, at, away, b, back, backed, backing, backs, be, became, because, become, becomes, been, before, began, behind, being, beings, below, best, better, between, big, both, but, by, c, came, can, cannot, can't, case, cases, certain, certainly, clear, clearly, come, could, couldn't, d, did, didn't, differ, different, differently, do, does, doesn't, doing, done, don't, down, downed, downing, downs, during, e, each, early, either, end, ended, ending, ends, enough, even, evenly, ever, every, everybody, everyone, everything, everywhere, f, face, faces, fact, facts, far, felt, few, find, finds, first, for, four, from, full, fully, further, furthered, furthering, furthers, g, gave, general, generally, get, gets, give, given, gives, go, going, good, goods, got, great, greater, greatest, group, grouped, grouping, groups, h, had, hadn't, has, hasn't, have, haven't, having, he, he'd, he'll, her, here, here's, hers, herself, he's, high, higher, highest, him, himself, his, how, however, how's, i, i'd, if, i'll, i'm, important, in, interest, interested, interesting, interests, into, is, isn't, it, its, it's, itself, i've, j, just, k, keep, keeps, kind, knew, know, known, knows, l, large, largely, last, later, latest, least, less, let, lets, let's, like, likely, long, longer, longest, m, made, make, making, man, many, may, me, member, members, men, might, more, most, mostly, mr, mrs, much, must, mustn't, my, myself, n, necessary, need, needed, needing, needs, never, new, newer, newest, next, no, nobody, non, noone, nor, not, nothing, now, nowhere, number, numbers, o, of, off, often, old, older, oldest, on, once, one, only, open, opened, opening, opens, or, order, ordered, ordering, orders, other, others, ought, our, ours, ourselves, out, over, own, p, part, parted, parting, parts, per, perhaps, place, places, point, pointed, pointing, points, possible, present, presented, presenting, presents, problem, problems, put, puts, q, quite, r, rather, really, right, room, rooms, s, said, same, saw, say, says, second, seconds, see, seem, seemed, seeming, seems, sees, several, shall, shan't, she, she'd, she'll, she's, should, shouldn't, show, showed, showing, shows, side, sides, since, small, smaller, smallest, so, some, somebody, someone, something, somewhere, state, states, still, such, sure, t, take, taken, than, that, that's, the, their, theirs, them, themselves, then, there, therefore, there's, these, they, they'd, they'll, they're, they've, thing, things, think, thinks, this, those, though, thought, thoughts, three, through, thus, to, today, together, too, took, toward, turn, turned, turning, turns, two, u, under, until, up, upon, us, use, used, uses, v, very, w, want, wanted, wanting, wants, was, wasn't, way, ways, we, we'd, well, we'll, wells, went, were, we're, weren't, we've, what, what's, when, when's, where, where's, whether, which, while, who, whole, whom, who's, whose, why, why's, will, with, within, without, won't, work, worked, working, works, would, wouldn't, x, y, year, years, yes, yet, you, you'd, you'll, young, younger, youngest, your, you're, yours, yourself, yourselves, you've, z")
  ## maybe change n-gram to words (remove it)
  ## creating vocabulary
  vocab <- create_vocabulary(it_test, ngram = c(1L, 2L), stopwords = prep_fun(c(stopwords('en'),stop_words[[1]])))
  #vocab <- prune_vocabulary(vocab, term_count_min = 10, doc_proportion_max = 0.5)
  bigram_vectorizer <- vocab_vectorizer(vocab)
  ## the dtm vector
  dtm_test <- create_dtm(it_test, bigram_vectorizer)
  #dtm_l1_norm = normalize(dtm, "l1")
  
  # define tfidf model
  tfidf = TfIdf$new()
  # fit model to test data and transform test data with fitted model
  dtm_test_tfidf = fit_transform(dtm_test, tfidf)
  
  # tfidf modified by fit_transform() call!
  # apply pre-tested tf-idf transformation to test data
  #dtm_test_tfidf = create_dtm(it_test, vectorizer)
  #dtm_test_tfidf = transform(dtm_test_tfidf, tfidf)

  
  ## Calculating cosine similarity
  fast_row_normalize <- function(m){
    d <- Diagonal(x=1/sqrt(rowSums(m^2)))
    return(t(crossprod(m, d)))
  }
  dtm_test_tfidf_norm <- fast_row_normalize(dtm_test_tfidf)
  sim <- tcrossprod(dtm_test_tfidf_norm)
  
  return(sim)
}

get_recommendation <- function(title=list(), num=5, expansion=FALSE){
  ## title must be a list
  if(is.list(title)==TRUE & length(title)>=1){
    
    test<- na.omit(clean_30[,c("id","type","name","description")])[1:5000,]
    if(expansion==FALSE){
      test<- test[test$type=="boardgame",]
    }
    sim <- sim_description(test)          # similarity matrix of data
    #test$sim_id <- as.numeric(sim@Dimnames[[1]])
    id.title <- sapply(title, function(x) {which(test$name==x)})
    x <- sort(colMeans(sim[id.title,-id.title]),decreasing = TRUE)[1:num]
    id.top <- as.numeric(names(x))
    return(test$name[sapply(id.top, function(x) which(test$id==x))])
  } 
  else{
    print("input must be nonempty list!")
  }
}
num <- 10
title <-list("Catan","Wacky Wacky West","Ra")
get_recommendation(title, 10, expansion = FALSE)

