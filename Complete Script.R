# Dimension reduction and statistical analysis of PsyCoP data
# Authour:      Marius Stephan
# Created:      27.03.2020
# Last edited:  13.01.2021

#####################################
# Setup
#####################################

# Load packages
library(ade4)
library(RColorBrewer)
library(pheatmap)
library(car)
library(energy)

# Clear Workspace
rm(list=ls())

# Load user function library
source("%LIBRARYPATH%/graph.functions v24.R")

# Load data
load("%WHEREEVERYOURFILEIS%/MetaCompound.RData")
rm(data.ID,MetaBattery)

####################################
# Filter and preprocess
####################################

# Remove dead mouse
MetaCompound <- filter(MetaCompound, Animal != "900200000066341")

# Collapse factors to one
MetaCompound$Group <- as.character(paste0(MetaCompound$Genotype,".",MetaCompound$Environmental))

#Isolate variables from identifiers
d <- MetaCompound[5:length(MetaCompound)]

# Remove outliers manually
d$Rotations[d$Rotations == 0] <- NA
d$Alternations[d$Alternations > 75] <- NA

####################################
# ANOVA procedure
####################################

# Fit linear model
d.lm <- lm(sprintf("cbind(%s) ~ Genotype * Environmental", toString(colnames(d)[1:(length(d)-1)])),separate(d,Group,c("Genotype","Environmental")))

# Test for normality
res <- resid(d.lm)
sres <- scale(res,center=F)
res_norm <- rnorm(n=1000000,mean=0,sd=1)
energy::mvnorm.etest(sres,R = 1000)

#Density plot
plot(density(sres),lwd=2)
lines(density(res_norm),col="red",lwd=2,lty=2)

# QQ plot
qqplot(sres,res_norm)
abline(coef=c(0,1),col="red")

# MANOVA + Univariate ANOVAs
a <- Manova(d.lm,test.statistic = "Wilks",univariate=T,multivariate=T,style="long",by="response")
s <- select(as.data.frame(summary(a, univariate=TRUE, multivariate = FALSE)$univaov), -SS)

colnames(s) <- c("variable","term","df","F","p")

s$annot <- sapply(s$p,graph.helper.sigAnnot)
s$p_adj <- p.adjust(s$p,method="fdr")
s$annot_adj <- sapply(s$p_adj,graph.helper.sigAnnot)
#write.table(s,file="uniaov.tsv",quote = F,row.names = F,col.names = T,sep="\t")

####################################
# Boxplots
####################################

# Create reference for statistics label
ref <- data.frame(full=c("Genotype","Environmental","Genotype:Environmental"),short=c("G","E","GxE"))
at <- left_join(s,ref,by=c('term'='full'))
at$size <- 10
at$size[at$annotation == "n.s."] <- 8 
at$size[at$annotation == "p<0.1"] <- 7

at$size.adj <- 10
at$size.adj[at$annot_adj == "n.s."] <- 8 
at$size.adj[at$annot_adj == "p<0.1"] <- 7

# Read Look-up table for y-labels
lutPath <- "%LOOKUPTABLEPATH/Label LUT.txt"
if(file.exists(lutPath)) lut <- as_tibble(read.delim(lutPath,stringsAsFactors=F))

# Create plot grouping table
groupy <- graph.grouping.wizard(unique(d$Group)[order(unique(d$Group))],labels = c("tg hc", "tg sd", "wt hc", "wt sd"),order = c(3,4,1,2))


# Output boxplots to PDF file 
# pdf("Boxplots Tcf4 ANOVA p_adj test.pdf",height = 5, width = 5)
for (i in 5:(length(d)-1)){
  # for (i in 5:5){ # For debugging
  y <- colnames(d)[i]
  ifelse(y %in% lut$var, {ylab <- lut$lab[which(lut$var == y)] ; tlab <- lut$title[which(lut$var == y)]}, {ylab <- y ; tlab <- ""} )
  y.max <- max(unlist(d[y])[!is.na(d[y])])
  
  j <- i-4
  print(graph.box(d,dep.var=y,comparison.list = list(),custom.group.tbl = groupy,custom.colours = c(hsv(0,0,0.71,1),hsv(0,0,0.24,1),hsv(0.55,0.48,1,1),hsv(0.66,0.88,1,1)),y.label=ylab,graph.title = tlab)+
          geom_text(data=s,mapping=aes(x=2.5,y=1.2*y.max),
                    label = paste0("G: ", filter(at, term=="Genotype",variable==y)$annot_adj," | ",
                                   "E: ",filter(at, term=="Environmental",variable==y)$annot_adj," | ",
                                   "GxE: ", filter(at, term=="Genotype:Environmental",variable==y)$annot_adj),
                    size=8,show.legend = F, hjust="center")
  )
}
# dev.off()

####################################
# Data processing for dimension reduction
####################################

# Replace variable names with abbreviations from look-up table (LUT)
lut <- read.delim("%LOOKUPTABLEPATH%/Abb_LUT.txt",stringsAsFactors = F)

for (i in 1 :(length(d)-1)){
  colnames(d)[colnames(d) == lut$Var[i]] <- lut$Lab[i]
}

# Estimate missing values with NIPALS
df <- as.data.frame(d)
nip <- nipals(df[-length(df)],rec=T,niter = 1000, nf=19)#$rec
n <- nip$rec

# Scale data and export intermediate results
n.scaled <- scale(n)
d.n <- cbind(Animal=MetaCompound$Animal,as.data.frame(n))
d.n.scaled <- cbind(Animal=MetaCompound$Animal,as.data.frame(n.scaled))
d.scaled <- cbind(Animal=MetaCompound$Animal,as.data.frame(nip$tab))
# write.table(arrange(d.scaled,Animal),"scaled.tsv",quote = F,row.names = F,col.names = T,sep="\t",dec = ",")
# write.table(arrange(d.n,Animal),"NIPAL_reconstituted.tsv",quote = F,row.names = F,col.names = T,sep="\t",dec = ",")
# write.table(arrange(d.n.scaled,Animal),"NIPAL_scaled.tsv",quote = F,row.names = F,col.names = T,sep="\t",dec = ",")

d.scaled <- cbind(Group=MetaCompound$Group,d.scaled[2:length(d.scaled)])
d.t <- gather(as_tibble(d.scaled),key="variable",value = "value",-Group)
d.t %>% group_by(Group,variable) %>% summarise(mean=mean(value[!is.na(value)])) -> d.sum
d.w <- spread(d.sum,key="variable",value="mean")
# write.table(d.w,"mean.tsv",quote = F,row.names = F,col.names = T,sep="\t",dec = ",")

d.n.scaled <- cbind(Group=MetaCompound$Group,d.n.scaled[2:length(d.n.scaled)])
d.t <- gather(as_tibble(d.n.scaled),key="variable",value = "value",-Group)
d.t %>% group_by(Group,variable) %>% summarise(mean=mean(value[!is.na(value)])) -> d.sum
d.w <- spread(d.sum,key="variable",value="mean")
# write.table(d.w,"NIPALS_mean.tsv",quote = F,row.names = F,col.names = T,sep="\t",dec = ",")

# Add group variable to supplemented data
n$Group <- unlist(df[length(df)])

####################################
# Dimension plots from PCA
####################################

gp <- groupedPCA(n,naManagement = "omit")
print(graph.biplot(gp,plot.vectors = F, x.limits = c(-4.5,5), custom.colours = c(hsv(0.55,0.48,1,1),hsv(0.66,0.88,1,1),hsv(0,0,0.71,1),hsv(0,0,0.24,1)),graph.title = "", alpha = 1))
print(graph.biplot(gp,plot.points= F, rdoc = T, x.limits = c(-4.5,5),  custom.colours = c(hsv(0.55,0.48,1,1),hsv(0.66,0.88,1,1),hsv(0,0,0.71,1),hsv(0,0,0.24,1)),graph.title = "", alpha = 1))

 u8####################################
# Dimension plots from Canonical Discriminant Analysis (CDA)
####################################

data.lm <- lm(sprintf("cbind(%s) ~ Group", toString(colnames(n)[-length(n)])),n)
data.cd <- candisc::candisc(data.lm)
print(graph.biplot(data.cd,plot.points= T,  plot.vectors= F, custom.colours = c(hsv(0.55,0.48,1,1),hsv(0.66,0.88,1,1),hsv(0,0,0.71,1),hsv(0,0,0.24,1)),graph.title = "", alpha = 1))
print(graph.biplot(data.cd,rdoc=T,plot.points= F,  plot.vectors= T, custom.colours = c(hsv(0.55,0.48,1,1),hsv(0.66,0.88,1,1),hsv(0,0,0.71,1),hsv(0,0,0.24,1)),graph.title = "", alpha = 1))

####################################
# Heatmap from Canonical Discriminant Analysis (CDA)
####################################

# Calculate CDA for each term of the two-factorial model
ns <- separate(n,Group,c("Genotype","Environment"))
formula2way <- sprintf("cbind(%s) ~ Genotype * Environment", toString(colnames(n)[-length(n)]))
data.lm <- lm(formula2way,ns)
data.cd.g <- candisc::candisc(data.lm, term="Genotype")
data.cd.e <- candisc::candisc(data.lm, term="Environment")
data.cd.gxe <- candisc::candisc(data.lm, term="Genotype:Environment")

# Put them togeth in one object
data.cd2 <- list(scores=data.cd.e$scores,structure=data.cd.e$structure,pct=data.cd.e$pct[1])
data.cd2$scores$Group <- paste0(data.cd2$scores$Genotype,".",data.cd2$scores$Environment)
data.cd2$scores <- data.cd2$scores[-c(1,2)]
data.cd2$scores <- cbind(data.cd2$scores[2],data.cd2$scores[1])
data.cd2$scores$Can2 <- unlist(data.cd.g$scores$Can1)
data.cd2$scores$Can3 <- unlist(data.cd.g$scores$Can1)
data.cd2$pct[2] <- data.cd.g$pct[1]
data.cd2$pct[3] <- -data.cd.gxe$pct[1]
data.cd2$structure <- cbind(data.cd2$structure,Can2=unlist(data.cd.g$structure),Can3=-unlist(data.cd.gxe$structure))
colnames(data.cd2$structure) <- c("Can1","Can2","Can3")
class(data.cd2) <- "candisc"

# Isolate Structure
d.m <- data.cd2$structure
d.j <- as_tibble(d.m)
d.j$Var <- row.names(d.m)

# Define order of plotting as well as gaps (for grouping) based on RDoC
rdoc <- as_tibble(data.frame(Var=c("Alt","RvL","SrL","Cue","Con","PpiBs","Ppi70","Ppi75","Ppi80","ScP","PcP","Rot","Ctr","FrBs","TmIm","Act","Noc","Chc","MnSp"),stringsAsFactors = F))
gapsrow <- c(5,9,11,15)

# Convert dataset to matrix
d.j <- left_join(rdoc,d.j,by="Var")
d.m <- as.matrix(d.j[2:4])
#d.m <- -1*as.matrix(d.j[2:4])
rownames(d.m) <- unlist(d.j$Var)

# Predefine plotting parameters
breakNo <- 200
cutoff <- 0.20
colBias <- 0.8
colnames(d.m) <- c("E","G","GxE")

# Define colour breaks
breaksList <- seq(floor(min(d.m)*10)/10, ceiling(max(d.m)*10)/10, length.out = breakNo)
n_white <- sum(abs(breaksList)<cutoff)
colBreaks <- (ceiling(length(seq(0, breakNo, by = 1)) - n_white)/2)

# Define colour palette based on breaks
color_spec.1 <- colorRampPalette(c("#2D004B","#968CBD","#FFFFFF"),bias=colBias)(colBreaks)
color_spec.2 <- colorRampPalette(c("#FFFFFF","#EF9F3F","#7F3B08"),bias=colBias)(colBreaks)
color_spec <- c(color_spec.1,rep("#FFFFFF",n_white),color_spec.2)

# Plot heatmap

pheatmap(d.m[,c(2,1,3)],#[,-3],
         #main = paste(title),
         fontsize = 16,
         fontsize_col = 12,
         lwd=2,
         show_rownames = T,
         cluster_rows = F,
         cluster_cols = F,
         gaps_row = gapsrow,
         color = color_spec,
         breaks = breaksList)
