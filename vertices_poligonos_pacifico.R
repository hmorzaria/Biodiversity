#C:\Users\Gaby Cruz\Desktop\SIG\vertices_pacific_9km.txt
setwd("C:/Users/Gaby Cruz/Desktop/SIG")
file=read.csv("vertices_pacific_9km.txt");
#4long #5lat
1273665/5
datos =matrix(1:1273665, ncol=1)
#1273665  #482795
contador=0
for (i in 1:1273665)
{
  coords=paste(file[contador*5+1,4],file[contador*5+1,5],file[contador*5+2,4],file[contador*5+2,5],file[contador*5+3,4],file[contador*5+3,5],file[contador*5+4,4],file[contador*5+4,5],file[contador*5+5,4],file[contador*5+5,5], sep=",")
  datos[i]=paste("POLYGON((",coords,"))", sep="")
  contador=contador+1
  print(contador/1273665)
  
}
write.csv(datos,"vertices_pacific.csv")