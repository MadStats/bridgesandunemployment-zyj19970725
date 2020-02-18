# my favorite function generates a standard normal variable and calls it x.
source("scripts/functions/myFavoriteFunction.R")   

# save it in the data folder.
save(x, file = "data/x.RData")

#plot it
pdf(file= "output/plots/x.pdf"); plot(x,0); dev.off()

#manipulate it and put it output/data:
y = x+1; save(y, file = "output/data/y.RData")