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

X.svd <- svd(X)
names(X.svd)

plot(reduce(X.svd,1))
