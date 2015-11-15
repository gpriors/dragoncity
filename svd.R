reduce <- function(A,dim) {
    #Calculates the SVD
    sing <- svd(A)

    #Approximate each result of SVD with the given dimension
    u<-as.matrix(sing$u[, 1:dim])
    v<-as.matrix(sing$v[, 1:dim])
    d<-as.matrix(diag(sing$d)[1:dim, 1:dim])

    #Create the new approximated matrix
    return(u%*%d%*%t(v))
}

.svd <- svd(X)
names(X.svd)

plot(reduce(X,1))

pca <- prcomp(dragoncity_hackathon_train[,vars],
                 center = TRUE,
                 scale. = TRUE)
predictions <- predict(pca, newdata=dragoncity_hackathon_train)
ROCRpred = prediction(predictions, dragoncity_hackathon_train$churn)
auc <- as.numeric(performance(ROCRpred, "auc")@y.values)
