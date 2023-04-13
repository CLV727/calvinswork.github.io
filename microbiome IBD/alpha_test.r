#qiime diversity core-metrics-phylogenetic   --i-phylogeny rooted-tree.qza   --i-table table.qza   --p-sampling-depth 10000  --m-metadata-file sample.tsv   --output-dir core-metrics-results
library(tidyverse)
library(qiime2R)
setwd("C:/Users/dingw/OneDrive/Desktop/mydata")

metadata<-read_q2metadata("sample.txt")
shannon<-read_qza("shannon_vector.qza")

###
set.seed(1234)
erDF = estimate_richness(sub.group, split = TRUE)
erDF =data.frame(erDF[,c("Observed","Shannon", "Simpson") ],sample_data(sub.group))

shannon<-shannon$data %>% rownames_to_column("SampleID") # this moves the sample names to a new column that matches the metadata and allows them to be merged
gplots::venn(list(metadata=metadata$SampleID, shannon=shannon$SampleID))
metadata<-
  metadata %>% 
  left_join(shannon)


metadata %>%
  filter(!is.na(shannon_entropy)) %>%
  ggplot(aes(x=group, y=shannon_entropy, color=group)) +
  stat_summary(geom="errorbar", fun.data=mean_se, width=0) +
  stat_summary(geom="line", fun.data=mean_se) +
  stat_summary(geom="point", fun.data=mean_se) +
  xlab("Days") +
  ylab("Shannon Diversity") +
  theme_q2r() + # try other themes like theme_bw() or theme_classic()
  scale_color_viridis_d(name="Body Site") # use different color scale which is color blind friendly
ggsave("Shannon_by_time.pdf", height=3, width=4, device="pdf") # save 


metadata %>%
  filter(!is.na(shannon_entropy)) %>%
  ggplot(aes(x=group, y=shannon_entropy, fill=group)) +
  stat_summary(geom="bar", fun.data=mean_se, color="black") + #here black is the outline for the bars
  geom_jitter(shape=21, width=0.2, height=0) +
  coord_cartesian(ylim=c(2,7)) + # adjust y-axis
 # facet_grid(~group) + # create a panel for each body site
  xlab("Group") +
  ylab("Shannon Diversity") +
  theme_q2r() +
  scale_fill_manual(values=c("cornflowerblue","indianred")) + #specify custom colors
  theme(legend.position="none") #remove the legend as it isn't needed
ggsave("Shannon_by_abx.pdf", height=3, width=4, device="pdf") #



#
library(tidyverse)
library(qiime2R)

metadata<-read_q2metadata("sample.txt")
uwunifrac<-read_qza("unweighted_unifrac_pcoa_results.qza")
shannon<-read_qza("shannon_vector.qza")$data %>% rownames_to_column("SampleID") 

uwunifrac$data$Vectors %>%
  select(SampleID, PC1, PC2) %>%
  left_join(metadata) %>%
  left_join(shannon) %>%
  ggplot(aes(x=PC1, y=PC2, color=group, shape=group, size=shannon_entropy)) +
  geom_point(alpha=0.5) + #alpha controls transparency and helps when points are overlapping
  theme_q2r() +
  scale_shape_manual(values=c(16,1), name="Group") + #see http://www.sthda.com/sthda/RDoc/figure/graphs/r-plot-pch-symbols-points-in-r.png for numeric shape codes
  scale_size_continuous(name="Shannon Diversity") +
  scale_color_discrete(name="group")

ggsave("PCoA.pdf", height=4, width=5, device="pdf") # save a PDF 3 inches by 4 inches
##

library(tidyverse)
library(qiime2R)

metadata<-read_q2metadata("sample.txt")
SVs<-read_qza("table.qza")$data
taxonomy<-read_qza("taxonomy.qza")$data

SVs<-apply(SVs, 2, function(x) x/sum(x)*100) #convert to percent

SVsToPlot<-  
  data.frame(MeanAbundance=rowMeans(SVs)) %>% #find the average abundance of a SV
  rownames_to_column("Feature.ID") %>%
  arrange(desc(MeanAbundance)) %>%
  top_n(30, MeanAbundance) %>%
  pull(Feature.ID) #extract only the names from the table

SVs %>%
  as.data.frame() %>%
  rownames_to_column("Feature.ID") %>%
  gather(-Feature.ID, key="SampleID", value="Abundance") %>%
  mutate(Feature.ID=if_else(Feature.ID %in% SVsToPlot,  Feature.ID, "Remainder")) %>% #flag features to be collapsed
  group_by(SampleID, Feature.ID) %>%
  summarize(Abundance=sum(Abundance)) %>%
  left_join(metadata) %>%
  mutate(NormAbundance=log10(Abundance+0.01)) %>% # do a log10 transformation after adding a 0.01% pseudocount. Could also add 1 read before transformation to percent
  left_join(taxonomy) %>%
  mutate(Feature=paste(Feature.ID, Taxon)) %>%
  mutate(Feature=gsub("[kpcofgs]__", "", Feature)) %>% # trim out leading text from taxonomy string
  ggplot(aes(x=SampleID, y=Feature, fill=NormAbundance)) +
  geom_tile() +
  facet_grid(~group, scales="free_x") +
  theme_q2r() +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_fill_viridis_c(name="log10(% Abundance)")

ggsave("heatmap.pdf", height=4, width=11, device="pdf") # save a PDF 3 inches
