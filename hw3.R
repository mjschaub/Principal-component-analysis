setwd('D:/CS498/HW3 - PCA/')
#got from piazza post on ways to get data into R format

rm(list=ls()) #delete the environment

labels <- read.table("cifar-10-batches-bin/batches.meta.txt")
images.rgb <- list()
images.lab <- list()
num.images = 10000 # Set to 10000 to retrieve all images per file to memory

# Cycle through all 5 binary files
for (f in 1:5) {
  to.read <- file(paste("cifar-10-batches-bin/data_batch_", f, ".bin", sep=""), "rb")
  for(i in 1:num.images) {
    l <- readBin(to.read, integer(), size=1, n=1, endian="big")
    r <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    g <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    b <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    index <- num.images * (f-1) + i
    images.rgb[[index]] = data.frame(r, g, b)
    images.lab[[index]] = l+1
  }
  close(to.read)
  remove(l,r,g,b,f,i,index, to.read)
}

drawImage <- function(index) {
  # Testing the parsing: Convert each color layer into a matrix,
  # combine into an rgb object, and display as a plot
  img <- images.rgb[[index]]
  img.r.mat <- matrix(img$r, ncol=32, byrow = TRUE)
  img.g.mat <- matrix(img$g, ncol=32, byrow = TRUE)
  img.b.mat <- matrix(img$b, ncol=32, byrow = TRUE)
  img.col.mat <- rgb(img.r.mat, img.g.mat, img.b.mat, maxColorValue = 255)
  dim(img.col.mat) <- dim(img.r.mat)
  
  # Plot and output label
  library(grid)
  grid.raster(img.col.mat, interpolate=FALSE)
  
  # clean up
  remove(img, img.r.mat, img.g.mat, img.b.mat, img.col.mat)
  
  labels[[1]][images.lab[[index]]]
}

drawImage(sample(1:(num.images*5), size=1))


#twenty_comp_images = images.rgb #[1:20] #for 20 pca
#images.rgb is a 1d list of 50k images
#images.rgb[[1]]$r to get r,g,b component of images
#images.rgb[[index]] is a dataframe with dimension 1024x3 for each 1024 pixels in rgb


#reformat data to work well with PCA
images_split_by_class <- array(NA, dim = c(10, 5000, 3072)) #each class needs a matrix of num_imagesx3072 
num_images_by_class <- array(NA, dim=c(10))
for(idx in 1:50000)
{
  curr_class <- images.lab[[idx]]
  rgb_image <- array(NA,dim=c(3072))
  rgb_image[1:1024] <- images.rgb[[idx]]$r
  rgb_image[1025:2048] <- images.rgb[[idx]]$g
  rgb_image[2049:3072] <- images.rgb[[idx]]$b
  if(is.na(num_images_by_class[curr_class]))
    num_images_by_class[curr_class] <- 1
  images_split_by_class[curr_class,num_images_by_class[curr_class],] <- rgb_image
  num_images_by_class[curr_class] <- num_images_by_class[curr_class]+1
  
}


#then take the top 20 eigenvalues that result from this
#error of this is all the eigenvalues that you don't take

#prin_components <- c()
#prin_comp <- prcomp(images_split_by_class[1,,])
#prin_comp$rotation[,1:20]
#prin_comp$center

#calculating principle components of each class
for(x in 1:10)
{
  prin_components <- c(prin_components,prcomp(images_split_by_class[x,,])) #rank.=20 #do the prcomp on each category for all images in it
}

#creating twenty principal components matrix for each of the categories
twenty_comp_images <- array(NA,dim=c(10,3072,20))
curr_class <- 12
curr_class_idx <- 1
while(curr_class < 58)
{
  twenty_comp_images[curr_class_idx,,] <- prin_components[[curr_class]][,1:20]
  curr_class <- curr_class+5
  curr_class_idx <- curr_class_idx+1
}
#output of the first class' principal component
twenty_comp_images[1,,]



#mean images from PCA
mean_images <- array(NA,dim=c(10,1,3072))
curr_class <- 13
curr_class_idx <- 1
while(curr_class < 59)
{
  mean_images[curr_class_idx,,] <- prin_components[[curr_class]]
  curr_class <- curr_class+5
  curr_class_idx <- curr_class_idx+1
}
#example mean image of the first class
mean_images[1,,]

#display mean images
disp_img_matrix = rgb(mean_images[1,1,1:1024],mean_images[1,1,1025:2048],mean_images[1,1,2049:3072],maxColorValue=255)
dim(disp_img_matrix) = c(32,32)
grid.raster(t(disp_img_matrix), interpolate=FALSE)



#error
error_sums <- array(NA,dim=c(10))
curr_class <- 11
curr_class_idx <- 1
while(curr_class < 58)
{
  error_sums[curr_class_idx] <- Reduce("+",prin_components[[curr_class]][20:3072])
  curr_class <- curr_class+5
  curr_class_idx <- curr_class_idx+1
}
error_sums

plot(labels[[1]], error_sums,col="blue", xlab = "categories", ylab = "error magnitude", asp = 1, axes = TRUE,main = "Error for PCA") 
text(x, y, ,pos=1)


#Part 2
dist_matrix <- array(NA,dim=c(10,10))
for(x in 1:10)
{
  for(y in x:10)
  {
    temp_dist <- (mean_images[x,,] - mean_images[y,,])^2
  
    dist_matrix[x,y] <- Reduce("+",temp_dist)
    dist_matrix[y,x] <- dist_matrix[x,y]
  }
}
dist_matrix #10x10 matrix of differences in mean images


as.table(dist_matrix)
#cmdscale method for principal coordinate analysis
MDS_result <- cmdscale(d=dist_matrix,k=2)
x <- MDS_result[,1]
y <- MDS_result[,2] 
# note asp = 1, to ensure Euclidean distances are represented correctly
plot(x, y,col="red", xlab = "", ylab = "", asp = 1, axes = TRUE,main = "cmdscale(dist_matrix of classes part 2)")
text(x, y, labels[[1]],pos=1)

#part 3
#1/2(E(A|B) + E(B|A))
#E(X|Y) is to be the average error of representing all the images of class X using the mean of X
# and the first 20 principle components of class Y

dist_matrix_part_3 <- array(NA,dim=c(10,10))
#pg 75 algo:
# mean(x) + summation for all 20 principle components of Y 

for(x in 1:10)
{
  for(y in x:10)
  {
    #E(X|Y)
    xi_minus_mean = images_split_by_class[x,,] - mean_images[x,,]
    summation_twenty_comp <- array(0,dim=c(1,3072))
    #intergral part
    for(s in 1:20)
    {
      summation_twenty_comp <- summation_twenty_comp + t(twenty_comp_images[y,,s]) * xi_minus_mean[s,] * twenty_comp_images[y,,s]
    }
    low_dim_reconstruction_of_X_using_Y <- t(mean_images[x,,] + summation_twenty_comp)
    
    #E(Y|X)
    xi_minus_mean_two = images_split_by_class[y,,] - mean_images[y,,]
    summation_twenty_comp_two <- array(0,dim=c(1,3072))
    #integral part
    for(s in 1:20)
    {
      summation_twenty_comp_two <- summation_twenty_comp_two + t(twenty_comp_images[x,,s]) * xi_minus_mean_two[s,] * twenty_comp_images[x,,s]
    }
    low_dim_reconstruction_of_Y_using_X <- t(mean_images[y,,] + summation_twenty_comp_two)
    
    temp_dist <- (mean_images[x,,] - low_dim_reconstruction_of_X_using_Y)^2 #E(X|Y)
    temp_dist_two <- (mean_images[y,,] - low_dim_reconstruction_of_Y_using_X)^2 #E(Y|X)
    
    dist_matrix_part_3[x,y] <- 1/2*(Reduce("+",temp_dist)+Reduce("+",temp_dist_two)) # 1/2(E(A|B) + E(B|A))
    dist_matrix_part_3[y,x] <- dist_matrix_part_3[x,y]
  }
}
dist_matrix_part_3

#output as a table
as.table(dist_matrix_part_3)

MDS_result <- cmdscale(d=dist_matrix_part_3,k=2)
x <- MDS_result[,1]
y <- MDS_result[,2] 
## note asp = 1, to ensure Euclidean distances are represented correctly
plot(x, y,col="red", xlab = "", ylab = "", asp = 1, axes = TRUE,main = "cmdscale(dist_matrix of classes for part 3)") 
text(x, y, labels[[1]],pos=1)


