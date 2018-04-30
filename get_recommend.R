## generate tfidf matrix
sim_text <- function(test){
  
  ## removing numerals
  prep_fun <- function(x) {gsub("\\d+", "", tolower(x))}
  ## removing stemming
  stem_tokenizer =function(x) {
    lapply(word_tokenizer(x), SnowballC::wordStem, language="en")
  }
  ## generating word tokens
  it_test <- itoken(test$text,  preprocessor = prep_fun, tokenizer = stem_tokenizer, ids = test$id, progressbar = FALSE)
  stop_words <- word_tokenizer("a, NA, about, above, across, after, again, against, all, almost, alone, along, already, also, although, always, am, among, an, and, another, any, anybody, anyone, anything, anywhere, are, area, areas, aren't, around, as, ask, asked, asking, asks, at, away, b, back, backed, backing, backs, be, became, because, become, becomes, been, before, began, behind, being, beings, below, best, better, between, big, both, but, by, c, came, can, cannot, can't, case, cases, certain, certainly, clear, clearly, come, could, couldn't, d, did, didn't, differ, different, differently, do, does, doesn't, doing, done, don't, down, downed, downing, downs, during, e, each, early, either, end, ended, ending, ends, enough, even, evenly, ever, every, everybody, everyone, everything, everywhere, f, face, faces, fact, facts, far, felt, few, find, finds, first, for, four, from, full, fully, further, furthered, furthering, furthers, g, gave, general, generally, get, gets, give, given, gives, go, going, good, goods, got, great, greater, greatest, group, grouped, grouping, groups, h, had, hadn't, has, hasn't, have, haven't, having, he, he'd, he'll, her, here, here's, hers, herself, he's, high, higher, highest, him, himself, his, how, however, how's, i, i'd, if, i'll, i'm, important, in, interest, interested, interesting, interests, into, is, isn't, it, its, it's, itself, i've, j, just, k, keep, keeps, kind, knew, know, known, knows, l, large, largely, last, later, latest, least, less, let, lets, let's, like, likely, long, longer, longest, m, made, make, making, man, many, may, me, member, members, men, might, more, most, mostly, mr, mrs, much, must, mustn't, my, myself, n, necessary, need, needed, needing, needs, never, new, newer, newest, next, no, nobody, non, noone, nor, not, nothing, now, nowhere, number, numbers, o, of, off, often, old, older, oldest, on, once, one, only, open, opened, opening, opens, or, order, ordered, ordering, orders, other, others, ought, our, ours, ourselves, out, over, own, p, part, parted, parting, parts, per, perhaps, place, places, point, pointed, pointing, points, possible, present, presented, presenting, presents, problem, problems, put, puts, q, quite, r, rather, really, right, room, rooms, s, said, same, saw, say, says, second, seconds, see, seem, seemed, seeming, seems, sees, several, shall, shan't, she, she'd, she'll, she's, should, shouldn't, show, showed, showing, shows, side, sides, since, small, smaller, smallest, so, some, somebody, someone, something, somewhere, state, states, still, such, sure, t, take, taken, than, that, that's, the, their, theirs, them, themselves, then, there, therefore, there's, these, they, they'd, they'll, they're, they've, thing, things, think, thinks, this, those, though, thought, thoughts, three, through, thus, to, today, together, too, took, toward, turn, turned, turning, turns, two, u, under, until, up, upon, us, use, used, uses, v, very, w, want, wanted, wanting, wants, was, wasn't, way, ways, we, we'd, well, we'll, wells, went, were, we're, weren't, we've, what, what's, when, when's, where, where's, whether, which, while, who, whole, whom, who's, whose, why, why's, will, with, within, without, won't, work, worked, working, works, would, wouldn't, x, y, year, years, yes, yet, you, you'd, you'll, young, younger, youngest, your, you're, yours, yourself, yourselves, you've, z")
  ## maybe change n-gram to words (remove it)
  ## creating vocabulary
  vocab <- create_vocabulary(it_test, ngram = c(1L, 2L), stopwords = prep_fun(c(stopwords('en'),stop_words[[1]])))
  vocab <- prune_vocabulary(vocab, term_count_min = 1, term_count_max = 13, doc_proportion_max = 0.6)
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

get_recommend <- function(title=list(), num=10, meta=FALSE, expansion=FALSE, rating=TRUE){
  ## title must be a list
  if(is.list(title)==TRUE & length(title)>=1){
    
    if(expansion==FALSE){
      data<- data[data$type=="boardgame",]
    }
    
    index.title <- sapply(title, function(x) {which(data$name==x)})
    id.title <- data$id[index.title]
    if(meta==TRUE){
      clust.title <- unique(data$clust_cat_mec[index.title])
      print(clust_characteristic_cat_mec[clust.title])
      data_clust <- data[data$clust_cat_mec %in% clust.title,]
    } else{
      clust.title <- unique(data$clust_all[index.title])
      print(clust_characteristic_all[clust.title])
      data_clust <- data[data$clust_all %in% clust.title,]
    }

    

    data_clust$text <- paste(data_clust$description, gsub("-","",gsub(" ","",gsub("[.]","",paste(data_clust$artist, data_clust$designer,sep = ",")))))
    sim <- sim_text(data_clust)
    index.clust <- sapply(title, function(x) {which(data_clust$name==x)})
    sim_pruned <- sim[index.clust,-index.clust]
    
    if(rating==TRUE){
      weight <- data_clust[-index.clust,]$average
    } else{
      weight <- rep(1,ncol(sim_pruned))
    }
    
    if(length(title)==1) {
      sim_weighted <- sim_pruned*weight
      x <- sort(sim_weighted)[1:num]
    } else {
      sim_weighted <- t(apply(sim_pruned, 1, function(x) return(x*weight)))
      x <- sort(colMeans(sim_weighted),decreasing = TRUE)[1:num]
    }
    
    
    id.top <- as.numeric(names(x))
    index.recommend <- sapply(id.top, function(x) which(data_clust$id==x))
    cluster.recommend <- data_clust$clust_all[index.recommend]
    names.recommend <- data_clust$name[index.recommend]
    index.combined <- c(index.clust, index.recommend)
    
    return(list(index.recommend=index.recommend, cluster.recommend = cluster.recommend, names.recommend = names.recommend, sim.recommend = x))
  } 
  else{
    print("input must be nonempty list!")
  }
}

boardgame_final <- read_csv("~/Boardgames/boardgame_final.csv")
clust_characteristic_cat_mec <- read_csv("~/Boardgames/df_cat_mec.csv")
clust_characteristic_cat_mec <- as.character(clust_characteristic_cat_mec$characteristic)
data <- boardgame_final

title <-list("Catan","Fossil","Ra","Tikal")

title <- list("7 Wonders","Scythe","The Resistance","Karmaka","Splendor","Dead of Winter: A Crossroads Game")
OUT_recommend <- get_recommend(title= title, num=40, meta = TRUE, expansion=FALSE, rating=TRUE)
names.recommend <- OUT_recommend$names.recommend
index.recommend <- OUT_recommend$index.recommend
sim.recommend <- OUT_recommend$sim.recommend
cluster.recommend <- OUT_recommend$cluster.recommend

plot_recommend <- function(OUT_recommend){
  
}
