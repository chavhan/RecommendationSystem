## Recommend system on book dataset
library(recommenderlab)

book_data <- book[,c(-1)]   ## Eliminating sn.n  
head(book_data)   
str(book_data)
dim(book_data)
names(book_data)
names(book_data)[names(book_data) == 'User.ID'] <- 'uid'         ## rephrase column name 
names(book_data)[names(book_data) == 'Book.Rating'] <- 'rating'
names(book_data)[names(book_data) == 'Book.Title'] <- 'title'

book_data_realRating <- as(book_data,'realRatingMatrix')  ## converting df to realRating matrix



## observing book_data_realRatingMatrix
hasRating(book_data_realRating)
head(colCounts(book_data_realRating))
head(rowCounts(book_data_realRating))
dimnames(book_data_realRating)
nratings(book_data_realRating)  ## total ratings 9993

image(book_data_realRating)
hist(colCounts(book_data_realRating))
hist(rowCounts(book_data_realRating))
head(as(book_data_realRating[2000],'list'))[[1]]


book_recomm_model1 <- Recommender(book_data_realRating, method="POPULAR")
book_recomm_model2 <- Recommender(book_data_realRating, method="RANDOM")
book_recomm_model3 <- Recommender(book_data_realRating, method="UBCF")
##book_recomm_model4 <- Recommender(book_data_realRating, method="IBCF")

recommended_items1 <- predict(book_recomm_model3, book_data_realRating[1], n=5)
as(recommended_items1, "list")

eval_sets <- evaluationScheme(data = book_data_realRating,method='split',
                              train = 0.8, given =1 , goodRating = 7
)

models <- list(
  IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
  IBCF_cor = list(name = "IBCF", param = list(method = "pearson")),
  #UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),
  #UBCF_cor = list(name = "UBCF", param = list(method = "pearson")),
  RANDOM = list(name = "RANDOM"),
  POPULAR = list(name = "POPULAR")
)
eval_results <- evaluate(x = eval_sets, method = models, n = 1:19)
avg(eval_results)

plot(eval_results, annotate=TRUE,legend = "topleft")
plot(eval_results, "prec/rec", annotate=TRUE,legend = "bottomright")
ev <- evaluate(eval_sets, models, type="ratings")
avg(ev)
plot(ev,legend= "left")
