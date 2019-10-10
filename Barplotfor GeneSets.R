###########################################################################
# This is how to make a plot with 2 y-axes and legends with ggplot2 in R
#  2019-10-10
###########################################################################

# 2 y-axis different scales
# one bar plot and one point 
rm(list=ls())
dat1 <- data.frame(GO_Term = factor(c("positive regulation of intracellular protein transport","response to oxidative stress",
                                      "regulation of transcription regulatory region DNA binding","calcium ion binding",
                                      "calcium ion transmembrane transporter activity",
                                      "JAK-STAT cascade ","regulation of I-kappaB kinase/NF-kappaB signaling","fertilization")),
                   Significant_Genes = c(5,7,3,12,3,4,4,6), Total_Genes = c(27,65,12,135,16,19,27,42),
                   log10pvalue = c(2.07,1.44,1.75,2.0,1.42,1.93,1.41,1.83))

# pasting two columns together and separated by ;
dat2 <- within(dat1, Tot_Pval <- Total_Genes)
head(dat2)

# Inserting paranthesis around all elements of a column in a dataframe in R
# Do not unlist and list for several times in R
dat2[,5] <- paste0("(", format(unlist(dat2[,5])),")")
dat2

# Removing the white space  
dat2$Tot_Pval <- gsub(" ","" ,dat2$Tot_Pval, fixed = TRUE)
dat2

# Finally pasting two columns together
dat3 <- within(dat2,GO_Term_TGenes <- paste(GO_Term,Tot_Pval,sep=" ")) 
dat3


##################################################################################################
# making a ggplot with 2-y axes on a single plot in R
# When you want to bold the expression, make bold earlier in the expression 
################################################################################################
# Here, we are bolding the axes and titles 

library(ggplot2)
p <- ggplot(dat3) + 
  geom_point(mapping = aes(x = GO_Term_TGenes, y = log10pvalue * 14/2.07,fill="-log10pvalue"))  +
  geom_bar(mapping = aes(x = GO_Term_TGenes, y = Significant_Genes, color ="Significant Genes"), stat = "identity", fill= "red",width=0.5) +
  scale_y_continuous(name = expression(bold("Significant Genes")),sec.axis = sec_axis(~ . * 2.07/14,name='-log10pvalue'),limits = c(0, 14), breaks = seq(0,14,by=2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.text = element_text(size = 8, face = "bold"), # modify legend appearance
        legend.title = element_blank(), 
        legend.background = element_rect(fill = F),
        legend.position = c(0.7,0.6),legend.justification = c("right","top"),legend.direction = "vertical",
        legend.key.size = unit(0.4, "cm"), legend.key.height = unit(0.001,"cm"), legend.spacing.y = unit(0.001,"cm"))


##################################################################################################################
# Flipping the plot and making st the numbers
p <- p + coord_flip() + theme(axis.text.x = element_text(angle = 360, hjust = 1))
p

###########################################################################################

# Removing title of y-axis and bolding text and title 
p + theme(axis.title.y = element_blank())
p <- p + theme(axis.text.y = element_text(size=11,face="bold",color = "black"),axis.text.x = element_text(size=11,face="bold",color = "black")) + theme(axis.title.y = element_blank()) 
p <- p + theme(axis.title.x = element_text(size=12,face="bold")) 
p <- p + theme(axis.title.x = element_text(size=12,face="bold"))
p

#################################################################
# Exporting tiff image in R
###############################################################
Plots_grids <- p + theme(
  panel.background = element_rect(fill = "lightblue",
                                  colour = "lightblue",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)

tiff("Final_GO_Genesets_Oct8.tiff", width = 10, height = 5, units = 'in', res = 300)
plot(Plots_grids)
dev.off()
