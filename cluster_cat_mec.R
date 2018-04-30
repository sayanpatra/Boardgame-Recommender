library(text2vec)
library(data.table)
library(magrittr)
library(stopwords)
library(SnowballC)
#library("qlcMatrix")
library(tsne)
library(Rtsne)
#library(factoextra)

# DTM matrix
dtm_cat_mec <- function(test){
  ## removing commas from the string and keep the space between words intact
  tok_fun <- function(x) strsplit(x,",")
  ## generating word tokens
  it_test <- itoken(test$cat_mec, tokenizer = tok_fun, ids = test$id, progressbar = FALSE)
  stop_words <- word_tokenizer("a, NA, about, above, across, after, again, against, all, almost, alone, along, already, also, although, always, am, among, an, and, another, any, anybody, anyone, anything, anywhere, are, area, areas, aren't, around, as, ask, asked, asking, asks, at, away, b, back, backed, backing, backs, be, became, because, become, becomes, been, before, began, behind, being, beings, below, best, better, between, big, both, but, by, c, came, can, cannot, can't, case, cases, certain, certainly, clear, clearly, come, could, couldn't, d, did, didn't, differ, different, differently, do, does, doesn't, doing, done, don't, down, downed, downing, downs, during, e, each, early, either, end, ended, ending, ends, enough, even, evenly, ever, every, everybody, everyone, everything, everywhere, f, face, faces, fact, facts, far, felt, few, find, finds, first, for, four, from, full, fully, further, furthered, furthering, furthers, g, gave, general, generally, get, gets, give, given, gives, go, going, good, goods, got, great, greater, greatest, group, grouped, grouping, groups, h, had, hadn't, has, hasn't, have, haven't, having, he, he'd, he'll, her, here, here's, hers, herself, he's, high, higher, highest, him, himself, his, how, however, how's, i, i'd, if, i'll, i'm, important, in, interest, interested, interesting, interests, into, is, isn't, it, its, it's, itself, i've, j, just, k, keep, keeps, kind, knew, know, known, knows, l, large, largely, last, later, latest, least, less, let, lets, let's, like, likely, long, longer, longest, m, made, make, making, man, many, may, me, member, members, men, might, more, most, mostly, mr, mrs, much, must, mustn't, my, myself, n, necessary, need, needed, needing, needs, never, new, newer, newest, next, no, nobody, non, noone, nor, not, nothing, now, nowhere, number, numbers, o, of, off, often, old, older, oldest, on, once, one, only, open, opened, opening, opens, or, order, ordered, ordering, orders, other, others, ought, our, ours, ourselves, out, over, own, p, part, parted, parting, parts, per, perhaps, place, places, point, pointed, pointing, points, possible, present, presented, presenting, presents, problem, problems, put, puts, q, quite, r, rather, really, right, room, rooms, s, said, same, saw, say, says, second, seconds, see, seem, seemed, seeming, seems, sees, several, shall, shan't, she, she'd, she'll, she's, should, shouldn't, show, showed, showing, shows, side, sides, since, small, smaller, smallest, so, some, somebody, someone, something, somewhere, state, states, still, such, sure, t, take, taken, than, that, that's, the, their, theirs, them, themselves, then, there, therefore, there's, these, they, they'd, they'll, they're, they've, thing, things, think, thinks, this, those, though, thought, thoughts, three, through, thus, to, today, together, too, took, toward, turn, turned, turning, turns, two, u, under, until, up, upon, us, use, used, uses, v, very, w, want, wanted, wanting, wants, was, wasn't, way, ways, we, we'd, well, we'll, wells, went, were, we're, weren't, we've, what, what's, when, when's, where, where's, whether, which, while, who, whole, whom, who's, whose, why, why's, will, with, within, without, won't, work, worked, working, works, would, wouldn't, x, y, year, years, yes, yet, you, you'd, you'll, young, younger, youngest, your, you're, yours, yourself, yourselves, you've, z")
  ## creating vocabulary
  vocab <- create_vocabulary(it_test, stopwords = c(stopwords('en'),stop_words[[1]]))
  #vocab <- prune_vocabulary(vocab, term_count_min = 10, doc_proportion_max = 0.5)
  word_vectorizer <- vocab_vectorizer(vocab)
  ## the dtm vector
  dtm_test <- create_dtm(it_test, word_vectorizer)
  dtm_test[which(dtm_test!=0 & dtm_test!=1)]=1 #removing duplicates of cat_mec
  return(dtm_test)
}

# clustering function
cluster <- function(data_to_cluster, perplex =20, nclust = 30){
  ## clustering using t-sne
  tsne_bgg = Rtsne(as.matrix(data_to_cluster),dims=2, check_duplicates = FALSE, perplexity=perplex,verbose=TRUE,max_iter = 5000,theta = 0,pca = TRUE)
  #tsne_bgg = Rtsne(gower_dist, is_distance=TRUE, dims=2, check_duplicates = FALSE,perplexity=20,verbose=TRUE,max_iter = 5000,theta = 0,pca = TRUE)
  d = dist(tsne_bgg$Y)
  clusters <- hclust(d,method = 'ward.D2')
  #nclust <- fviz_nbclust(tsne_bgg$Y, hcut, method = "gap_stat", k.max = 50, nboot = 50,verbose = TRUE, barfill = "steelblue", barcolor = "steelblue",
  #             linecolor = "steelblue")
  
  clusterCut <- cutree(clusters, nclust)
  ## Plot the clustered data
  palette(rainbow(nclust))
  plot(tsne_bgg$Y, t='n', main="Board Games Map", xlab = "dim 1", ylab = "dim 2")
  text(tsne_bgg$Y, labels=test[,"id"],cex = 1, col = clusterCut)
  return(clusterCut)
}

cluster_cat_mec_process <- function(test_cat_mec, nclust = 50){
  binary <- dtm_cat_mec(test_cat_mec[,c("id","cat_mec")])      # DTM matrix of factors and cate
  nclust <- nclust
  ind.clust <- cluster(as.matrix(binary), perplex = 10, nclust = nclust)
  #tsne_bgg = Rtsne(gower_dist, is_distance=TRUE, dims=2, check_duplicates = FALSE,perplexity=20,verbose=TRUE,max_iter = 5000,theta = 0,pca = TRUE)
  
  clust_characteristic <- character(nclust)
  for (i in 1:nclust){
    clust_characteristic[i] <- toString(names(sort(colMeans(as.matrix(binary[which(ind.clust == i),])), decreasing=TRUE)[1:7]))
  }
  print(clust_characteristic)
  
  return(list(ind.clust = ind.clust, clust_characteristic = clust_characteristic))
}

test_cat_mec <- na.omit(clean[,c("id","name","cat_mec")])
OUT_cat_mec <- cluster_cat_mec_process(test_cat_mec, nclust = 50)
test_cat_mec$clust_cat_mec <- OUT_cat_mec$ind.clust
clust_characteristic_cat_mec <- OUT_cat_mec$clust_characteristic

num <- 5
title <-list("Catan","Fossil","Ra","Tikal")
id.title <- sapply(title, function(x) {which(test_cat_mec$name==x)})
clust.title <- unique(test_clust_mec$clust_cat_mec[id.title])
test_clust <- test_cat_mec[test_cat_mec$clust_cat_mec %in% clust.title,]

