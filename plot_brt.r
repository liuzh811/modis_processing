load("D:\\dataset\\MTBSdata\\EM_revision\\Data2_0516\\finalanalysis.RData")

library(gbm)
library(dismo)

Variable.list1 = c("Gras", "PCDF", "InDF", "PJW", "PIPO", "Salp", "Mixd", "Hdwd", "CaCr","DeSc", "MMsh" , "Sgbr" ,"Shsp",
					"D2WUI","D2Rd","RiverD","DEM", "Slop",
					"P90Ao","PPAo","PWAo", "PP2GAo",  
					"T90Mn","T90Ao","TPAo", "TWAo", "TP2GAo",
					"W90Mn","H90Mn","H90Ao","HPAo","LOshp" ,"Status","Varregion")

Variable.list2 = c("Gras", "PCDF", "InDF", "PJW", "PIPO", "Salp", "Mixd", "Hdwd", "CaCr","DeSc", "MMsh" , "Sgbr" ,"Shsp",
					"D2WUI","D2Rd","RiverD","DEM", "Slop",
					"P90Ao","PPAo","PWAo", "PAo","PP2GAo", 
					"T90Mn","T90Ao","TPAo", "TWAo", "TAo","TP2GAo",
					"WMx","WMn","H90Ao","HAo", "HPAo","HWAo", "LOshp" ,"Calacre_ha","FSH","FSH_PRO")
			

Variable.list1.c = c(rep("lightgreen", 13), rep("lightcoral", 2), rep("skyblue", 3), rep("yellow3", 13), "lightcoral", "lightcoral" , "lightcoral")
Variable.list1.c = cbind(Variable.list1, Variable.list1.c)

Variable.list2.c = c(rep("lightgreen", 13), rep("lightcoral", 2), rep("skyblue", 3), rep("yellow3", 17), "lightcoral", "lightcoral" , "lightcoral", "lightcoral")
Variable.list2.c = cbind(Variable.list2, Variable.list2.c)

png(file = "D:\\dataset\\MTBSdata\\EM_revision\\Data2_0516\\VariInfluence2_0625.png", width = 2000, height = 2000, units = "px", res = 300)
par(mfrow=c(3,1),mar=c(0,3,1,0),oma=c(0,3,0,0))




Fire.1 = read.csv("Fire.1.csv")
Fire.1 = Fire.1[,-1]
Fire.1 = as.data.frame(Fire.1)
Fire.1$LOshp = as.factor(Fire.1$LOshp)
Variable.list1.exclu = c("H90Mn","T90Mn","HPAo")
Fire.1 = Fire.1[ , -which(names(Fire.1) %in% Variable.list1.exclu)]
#make a prediction dataset
preds.dat.occ = c()
for(i in 1:ncol(Fire.1)){preds.dat.occ = cbind(preds.dat.occ, rep(mean(Fire.1[,i], na.rm = TRUE), nrow(Fire.1)))}
preds.dat.occ = data.frame(preds.dat.occ)
colnames(preds.dat.occ) <- colnames(Fire.1)
preds.dat.occ$LOshp = Fire.1$LOshp

pred.list = list()
for (i in 1:5){
idx = which(colnames(Fire.1) == a$var[i])
preds.dat.occ1 = preds.dat.occ
preds.dat.occ1[,idx] = Fire.1[,idx]
preds = predict.gbm(gbm1, preds.dat.occ1, n.trees = gbm1$gbm.call$best.trees, type="response")
pred.list[[i]] <- data.frame(preds.dat.occ1[,idx], preds)
}

#for fire size and severity
Fire1Var.1 = read.csv("Fire1Var.1.csv")
Fire1Var.1 = as.data.frame(Fire1Var.1)
Fire1Var.1$LOshp = factor(Fire1Var.1$LOshp)
Fire1Var.1 = Fire1Var.1[,-1]
Fire1Var.1$Calacre_ha = log(Fire1Var.1$Calacre_ha)
Variable.list2.exclu = c("T90Mn","H90Ao","HPAo","HWAo")
Fire1Var.1 = Fire1Var.1[ , -which(names(Fire1Var.1) %in% Variable.list2.exclu)]

preds.dat = c()
for(i in 1:ncol(Fire1Var.1)){preds.dat = cbind(preds.dat, rep(mean(Fire1Var.1[,i], na.rm = TRUE), nrow(Fire1Var.1)))}
preds.dat = data.frame(preds.dat)
colnames(preds.dat) <- colnames(Fire1Var.1)
preds.dat$LOshp = Fire1Var.1$LOshp


pred.list.fs = list()
for (i in 1:6){
idx = which(colnames(Fire1Var.1) == b$var[i])
preds.dat1 = preds.dat
preds.dat1[,idx] = Fire1Var.1[,idx]
preds = predict.gbm(gbm.fs, preds.dat1, n.trees = gbm.fs$gbm.call$best.trees, type="response")
pred.list.fs[[i]] <- data.frame(preds.dat1[,idx], preds)
}

pred.list.per = list()
for (i in 1:5){
idx = which(colnames(Fire1Var.1) == b2$var[i])
preds.dat1 = preds.dat
preds.dat1[,idx] = Fire1Var.1[,idx]
preds = predict.gbm(gbm.per, preds.dat1, n.trees = gbm.per$gbm.call$best.trees, type="response")
pred.list.per[[i]] <- data.frame(preds.dat1[,idx], preds)
}

############################################################
### plot starts here
#plot barplot
png(file = "D:\\dataset\\MTBSdata\\EM_revision\\Data2_072815\\VariInfluence3_072815.png", width = 2500, height = 1500, units = "px", res = 300)

par(mfrow=c(2,2),mar=c(0,3,1,0),oma=c(0,3,0,0))
a.col = merge(a, Variable.list1.c, by.x = "var", by.y = "Variable.list1")
rownames(a.col) = a.col$var
a.col = a.col[rownames(a), ]
a.col = as.character(a.col[,3])
mar=c(3,0,0,0)
barplot(height=a$rel.inf[1:5],
		horiz=FALSE,xlab="",
		#xlim = c(, 15),
		ylim = c(-4.9, 15),
		cex.axis=1.5,
		cex.lab=1.5,
		col = a.col)
#for(i in 1:nrow(a)){text(0.5+1.2*(i-1),-2.5, labels=a$var[i],cex=1.5, srt=45)}
for(i in 1:5){text(0.5+1.2*(i-1),-2.5, labels=a$var[i],cex=1.5, srt=45)}
text(0.5+1.2*(i-1),14,labels="(a)",cex=2)
text(0.7,13,labels="17.3",cex=1.5, srt=90)

xposition = c(0.08, 0.16, 0.25, 0.34, 0.42)
#plot margial effects
for (i in 1:5){
par(fig = c(xposition[i]-0.05, xposition[i]+0.05, 0.65, 0.77), new = TRUE)
mar=c(0,0,0,0)
if (i == 3){
plot(pred.list[[i]][,1], pred.list[[i]][,2],col = a.col[i],xaxt='n',yaxt='n',xlim = c(0,0.2),axes=FALSE)
lines(lowess(pred.list[[i]][,1], pred.list[[i]][,2]), lwd = 2, xlim = c(0,0.2),col = "red")
} else {
plot(pred.list[[i]][,1], pred.list[[i]][,2],  col = a.col[i],xaxt='n',yaxt='n',axes=FALSE)
lines(lowess(pred.list[[i]][,1], pred.list[[i]][,2]), lwd = 2, col = "red")
}
}

####plot fire size

par(fig = c(0.5, 1, 0.5, 1), new = TRUE)

b.col = merge(b, Variable.list2.c, by.x = "var", by.y = "Variable.list2")
rownames(b.col) = b.col$var
b.col = b.col[rownames(b), ]
b.col = as.character(b.col[,3])
mar=c(3,0,0,0)
barplot(height=b$rel.inf[1:6],horiz=FALSE,xlab="",
		#xlim = c(, 15),
		ylim = c(-4.9, 15),
		cex.axis=1.5,
		cex.lab=1.5,
		col = b.col)
#for(i in 1:nrow(b)){text(0.5+1.2*(i-1),-2.5, labels=b$var[i],cex=1.5, srt=45)}
for(i in 1:6){text(0.5+1.2*(i-1),-2.5, labels=b$var[i],cex=1.5, srt=45)}
text(0.5+1.2*(i-1),14,labels="(b)",cex=2)

xposition = c(0.07, 0.14, 0.212, 0.287, 0.36, 0.435)+0.5
#plot margial effects
for (i in 1:6){
par(fig = c(xposition[i]-0.05, xposition[i]+0.05, 0.65, 0.77), new = TRUE)
mar=c(0,0,0,0)
if(i == 2 | i == 6){
plot(pred.list.fs[[i]][,1], pred.list.fs[[i]][,2],  col = b.col[i],xaxt='n',yaxt='n',xlim = c(0.1,1), axes=FALSE)
lines(lowess(pred.list.fs[[i]][,1], pred.list.fs[[i]][,2]), lwd = 2, ,xlim = c(0.1,1),col = "red")
}else if (i == 3 | i == 5){
plot(pred.list.fs[[i]][,1], pred.list.fs[[i]][,2],  col = b.col[i],xaxt='n',yaxt='n',xlim = c(0,0.2), axes=FALSE)
lines(lowess(pred.list.fs[[i]][,1], pred.list.fs[[i]][,2]), lwd = 2, ,xlim = c(0,0.2),col = "red")
} else {
plot(pred.list.fs[[i]][,1], pred.list.fs[[i]][,2],  col = b.col[i],xaxt='n',yaxt='n', axes=FALSE)
lines(lowess(pred.list.fs[[i]][,1], pred.list.fs[[i]][,2]), lwd = 2,,col = "red")
}

}

#plot fire severity
par(fig = c(0, 0.5, 0, 0.5), new = TRUE)
b2.col = merge(b2, Variable.list2.c, by.x = "var", by.y = "Variable.list2")
rownames(b2.col) = b2.col$var
b2.col = b2.col[rownames(b2), ]
b2.col = as.character(b2.col[,3])
mar=c(3,0,0,0)
barplot(height=b2$rel.inf[1:5],horiz=FALSE,xlab="",
		#xlim = c(, 15),
		ylim = c(-4.9, 15),
		cex.axis=1.5,
		cex.lab=1.5,
		col = b2.col)
#for(i in 1:nrow(b2)){text(0.5+1.2*(i-1),-2.5, labels=b2$var[i],cex=1.5, srt=45)}
for(i in 1:5){text(0.5+1.2*(i-1),-2.5, labels=b2$var[i],cex=1.5, srt=45)}
text(0.5+1.2*(i-1),14,labels="(c)",cex=2)
text(0.7,13,labels="42.3",cex=1.5, srt=90)

xposition = c(0.08, 0.16, 0.25, 0.34, 0.42)
#plot margial effects
for (i in 1:5){
par(fig = c(xposition[i]-0.05, xposition[i]+0.05, 0.13, 0.25), new = TRUE)
mar=c(0,0,0,0)
if(i == 2){
plot(pred.list.per[[i]][,1], pred.list.per[[i]][,2],  col = b2.col[i],xaxt='n',yaxt='n',xlim = c(0,0.2), axes=FALSE)
lines(lowess(pred.list.per[[i]][,1], pred.list.per[[i]][,2]), lwd = 2,xlim = c(0,0.2), col = "red")

} else if (i == 3)
{
plot(pred.list.per[[i]][,1], pred.list.per[[i]][,2],  col = b2.col[i],xaxt='n',yaxt='n',xlim = c(0,20),axes=FALSE)
lines(lowess(pred.list.per[[i]][,1], pred.list.per[[i]][,2]), lwd = 2,xlim = c(0,20), col = "red")
} else {


plot(pred.list.per[[i]][,1], pred.list.per[[i]][,2],  col = b2.col[i],xaxt='n',yaxt='n',axes=FALSE)
lines(lowess(pred.list.per[[i]][,1], pred.list.per[[i]][,2]), lwd = 2, col = "red")
}
}

par(fig = c(0.5, 1, 0, 0.5), new = TRUE)
plot.new()

#add legend
legend(0.05,0.75, 
       legend = c("Vegetation", "Human", "Topography","Short-term climate" ), 
       fill = c("lightgreen", "lightcoral", "skyblue", "yellow3"), cex = 1.5, , bty = "n")

mtext("Relative importance (%)", side = 2, cex = 1.5,outer=TRUE,padj = -0.8,adj = 0.5) #http://stat.ethz.ch/R-manual/R-devel/library/graphics/html/mtext.html

dev.off()
