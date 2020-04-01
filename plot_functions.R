setwd('~/Downloads')
library('gplots')
library('ggplot2')

country2reactions =read.csv('country2reactions.csv', stringsAsFactors = F)
head(country2reactions[1:5,1:5])
rownames(country2reactions) = country2reactions$X
country2reactions$X = NULL

most_freq_responses = read.csv('max_20_reactions.csv')
most_countries = read.csv('country2counts_max20.csv')

country2reactions = country2reactions/rowSums(country2reactions)
country2reactions = country2reactions[,colnames(country2reactions) %in% most_freq_responses$X]
country2reactions = country2reactions[rownames(country2reactions) %in% most_countries$X,]


which(rowSums(country2reactions) == 0)

crp <- colorRampPalette(c("blue","red","orange","yellow"))(100)
heatmap(as.matrix(country2reactions),dendrogram='none',trace='none', col=crp)
#dev.off()

noreactions2countries = read.csv('no_reactions2country.csv')
kruskal.test(noreactions2countries$no_reactions, noreactions2countries$Country)


#ggplot(noreactions2countries,aes(x=Country))+
#  geom_bar(stat='count')

nodrugs2countries = read.csv('no_drugs2country.csv')
kruskal.test(nodrugs2countries$no_drugs, nodrugs2countries$Country)




cor.test(noreactions2countries$no_reactions, nodrugs2countries$no_drug, method = 'spearman')

cor_line = rep(cor(noreactions2countries$no_reactions, nodrugs2countries$no_drug, method = 'spearman'),dim(df_nodrugs_noreactions)[1])
df_nodrugs_noreactions = data.frame(no_drugs = nodrugs2countries$no_drugs, no_reactions = noreactions2countries$no_reactions, spearman_cor = cor_line)
ggplot(df_nodrugs_noreactions,aes(x=no_drugs,y=no_reactions))+
  geom_point()+
  labs(x='No of drugs', y='No of reactions')
ggsave('~/Documents/no.drugs_no.responses.pdf')


