#determining the number of NA responses to the LiveNearby Q
LocalNAs <-sum(is.na(CUVAsem$LiveNearby))
LocalNAs
length(CUVAsem$LiveNearby)

#This bit of code bins ReEntry respondents. The code isn't working just yet.
#Let R be the reentry question and let DK be the don't know response
#A is people who didn't answer R (i.e., R = NA) and answered DK = 99 (i.e., they didn't know and they answered so)
#B is people who skipped the Q (i.e., R = NA and DK = NA)
#C is people who answered R but also weren't sure (R = # & DK=99)
A <- NULL
B <- NULL
C <- NULL

for (x in 1:length(CUVAsem$ID)){
 if (is.na(CUVAsem$ReEnter[x]) & is.numeric(CUVAsem$DKReEnter[x])){
   A <- append(A, CUVAsem$ID[x])
 }
  else if (is.na(CUVAsem$ReEnter[x]) & is.na(CUVAsem$DKReEnter[x])){
    B <- append(B, CUVAsem$ID[x])
  }
  else if (is.numeric(CUVAsem$ReEnter[x]) & is.numeric(CUVAsem$DKReEnter[x])){
    C <- append(C, CUVAsem$ID[x])
  }
  }
# #Another way that we could verify this question is to work on expectations of what
# we think is reasonable based on their trip characteristics
# For example, if they lodge outside of the park and they visited the park for 3 days, then we
# would expect reentry to be 3


#determining the number of NA responses to the ReEnter
ReEnterNAs <-sum(is.na(CUVAsem$ReEnter))
ReEnterNAs

#Example of the apply function
## Compute row and column sums for a matrix:
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]
apply(x, 2, mean, trim = .2)
col.sums <- apply(x, 2, sum)
row.sums <- apply(x, 1, sum)
rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))



