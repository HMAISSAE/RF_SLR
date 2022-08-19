wd = getwd()
DF= read.csv2(".\\input\\SLR_searchselection_140822.csv", sep=";")

library(grid)
library(ggplot2)
library(tidyverse)
library(scales)
library(highcharter)
library(inspectdf)
library(venneuler)
source("./sunburstfunct.R")
names(DF)

DF1 = DF %>% select(category,cited_190622_googlescholar,Source, Publication.Year, Name,
                    ant.post.hoc, agnostic.specific,
                    Objective,Objectiveabr,
                    Pb,Pbabr, Input, Output,
                    Methodology, Methodologyabr,
                    Code.tool, 
                    Title, Publication.Title, Key)

# ----------------------------------------------------------------------------
  # Bibliographic analysis>>paper count
barfill <- "#4271AE"
barlines <- "#1F3552"
DF_publication_count= DF1 %>%
  select(Publication.Year)%>%
  group_by(Publication.Year) %>%
  summarise(n = n())
ggpublication_count= ggplot(data = DF_publication_count, aes(x = Publication.Year, y=n))+ 
  labs(title="", x="Publication year", y="Papers Count")+
  geom_line(color="blue")+
  # geom_smooth(method = lm)+
  # theme_bw()+
  scale_x_continuous(breaks = seq(2007,2021, by = 1))+
  scale_y_continuous(breaks = seq(0,15, by = 2))+
  theme(axis.title.y=element_text(face="bold", size =14, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x=element_text(face="bold", size =14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        legend.position = "top",legend.title = element_blank(),
        legend.text =element_text(size=7),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        axis.line = element_line(size=1, colour = "Gray"),
        panel.grid.major.x =  element_blank(),
        panel.grid.major.y =  element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        # text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 12, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y=element_text(colour="black", size = 12))

ggpublication_count

# ----------------------------------------------------------------------------
  # Bibliographic analysis>>Citation count
DFcitation= DF1 %>%select(cited_190622_googlescholar,category,Publication.Year)
ggcitation= ggplot(data = DFcitation, aes(x = Publication.Year, y=cited_190622_googlescholar, color=category, shape=category))+ 
  labs(title="", x="Publication year", y="Citations Count")+
  geom_point(position=position_jitter(h=0.1,w=0), size = 3)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                limits=c(10^0,10^6))+
  scale_x_continuous(breaks = seq(2007,2021, by = 1))+
  theme(axis.title.y=element_text(face="bold", size =14, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x=element_text(face="bold", size =14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        legend.position = "top",legend.title = element_blank(),
        legend.text =element_text(size=14),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        axis.line = element_line(size=1, colour = "Gray"),
        panel.grid.major.x =  element_blank(),
        panel.grid.major.y =  element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        # text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 12, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y=element_text(colour="black", size = 12))
ggcitation

# ----------------------------------------------------------------------------
# Literature classification>>summary
DF1summary = DF1 %>% 
  select(ant.post.hoc, agnostic.specific, Objective, Pb, Input, Output, Methodology, Code.tool) %>% 
  inspect_cat()
DF1summary %>% show_plot()

# ----------------------------------------------------------------------------
# Literature classification>>objective
DFobjective = DF1 %>% select(Name,Objective)
DFobjective$GlobalOverview=0
DFobjective$LocalExplanation=0
DFobjective$PatternDiscovery=0
GO_ids= which(grepl("Global overview",DFobjective$Objective))
LE_ids= which(grepl("Local explanation",DFobjective$Objective))
PD_ids= which(grepl("Pattern discovery",DFobjective$Objective))
DFobjective$GlobalOverview[GO_ids]=1
DFobjective$LocalExplanation[LE_ids]=1
DFobjective$PatternDiscovery[PD_ids]=1

DFvenn= DFobjective %>% select(Name, GlobalOverview, LocalExplanation, PatternDiscovery)
vennSet <- DFvenn %>%
  gather(Objective_class, binary,2:ncol(DFvenn)) %>% # take all binary mappings and convert to be a the set indicator
  filter(binary == 1) %>% # only include set matches
  select(Name, Objective_class) %>% # only include ID and set category
  mutate(Objective_class = factor(Objective_class)) # set the rules column as a factor

venplot=venneuler(data.frame(vennSet))
plot(venplot,quantities = TRUE)
DF1summury_obj=DF1summary[[5]][[6]]
DF1summury_obj$prop=100*round(DF1summury_obj$prop,2)
# library(ggVennDiagram)
# ggVennDiagram(list("Global Overview" = GO_ids, "Local Explanation" = LE_ids, "Pattern Discovery" = PD_ids))

# ----------------------------------------------------------------------------
# Literature classification>>inputs-outputs-issue
dataForSankey <- DF1 %>% select( Input, Pb, Output)
dataForSankey$Input[grepl("Mixte",dataForSankey$Input)] <- "Continous/categorical"

hchart(data_to_sankey(dataForSankey), "sankey", name = " Pb and Output based Input")%>% 
  hc_plotOptions(series = list(dataLabels = list( style = list(fontSize = "0px"))))
hchart(data_to_sankey(dataForSankey), "sankey", name = " Pb and Output based Input")    

DF1summury_inputs=DF1summary[[5]][[4]]
DF1summury_inputs$prop=100*round(DF1summury_inputs$prop,2)  

DF1summury_ouputs=DF1summary[[5]][[7]]
DF1summury_ouputs$prop=100*round(DF1summury_ouputs$prop,2)

DF1summury_issue=DF1summary[[5]][[8]]
DF1summury_issue$prop=100*round(DF1summury_issue$prop,2)

# ----------------------------------------------------------------------------
# Literature classification>>Methodology
unique(DF1$Methodology)
# levl_methd= c("Rule Extraction","Features oriented","Template/Web interface","Sample similarity-based","Features oriented + Sample similarity-based","Size reduction")
DFobjvsmethod=DF1 %>%
  select(Objective,Methodology)%>%
  # mutate(method = factor(Methodology, levels=levl_methd))%>%
  group_by(Objective,Methodology) %>%
  summarise(count = n()) 
sum(DFobjvsmethod$count)
DFobjvsmethod= DFobjvsmethod %>% mutate (percent= 100*count/sum(DFobjvsmethod$count))
DFobjvsmethod$Objective=as.factor(DFobjvsmethod$Objective)
DFobjvsmethod$Methodology=as.factor(DFobjvsmethod$Methodology)
str(DFobjvsmethod$Objective)
# levels(DFobjvsmethod$Methodology)[2] <- gsub(" ", "\n", levels(DFobjvsmethod$Methodology))
levels(DFobjvsmethod$Methodology)[2] <- "Features oriented + \nSample similarity-based"
levels(DFobjvsmethod$Objective)[4] <- "Global overview + Pattern discovery \n+ Local explanation"
mycols01 <- c("salmon2","#EFC000FF","darkseagreen","#0073C2FF","coral4","#868686FF")

ggplot(DFobjvsmethod, aes(x = reorder(Methodology, desc(-count)), y = percent, fill = Objective)) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Methodology", y = "percent", fill = "Objective") +
  theme_bw()+
  scale_y_continuous(breaks=c(0, 10, 20, 30, 40),labels = scales::percent(c(0, 0.1, 0.2, 0.3,0.4)))+ 
  scale_fill_manual(values=mycols01)+
  coord_flip()+
  theme(axis.title.y=element_text(face="bold", size =15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x=element_text(face="bold", size =15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        legend.text =element_text(size=13),
        axis.line = element_line(size=1, colour = "gray"),
        # panel.grid.major.x =  element_blank(),
        panel.grid.major.y =  element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        # text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 13),
        axis.text.y=element_text(colour="black", size = 13))
# ggplot(DFobjvsmethod) +
#   geom_bar(aes(y=count, x=reorder(Methodology, desc(-count)), fill=Objective), 
#            stat="identity", position="dodge" ) +
#   guides(fill = guide_legend(reverse = FALSE)) +
#   theme_bw()+
#   scale_fill_manual(values=mycols01)+
#   coord_flip()+
#   labs(title="", x="Methodology", y="count") 
# 
# ggplot(DFobjvsmethod) +
#   geom_bar(aes(y=count, x=reorder(Methodology, desc(-count))), 
#            stat="identity", position="dodge", fill ="blue" ) +
#   guides(fill = guide_legend(reverse = FALSE)) +
#   theme_bw()+
#   coord_flip()+
#   labs(title="", x="Methodology", y="count")
# ----------------------------------------------------------------------------
# Literature classification>>code implementation
DFYC= DF1  %>% select(Publication.Year, Name, Code.tool) %>% filter(Code.tool != "Not mentioned")
DFYC$Periode=DFYC$Publication.Year
DFYC[which(DFYC$Periode %in% c(2007,2008,2009)),"Periode"]="2007-2009"
DFYC[which(DFYC$Periode %in% c(2010,2011,2012)),"Periode"]="2010-2012"
DFYC[which(DFYC$Periode %in% c(2013,2014,2015)),"Periode"]="2013-2015"
DFYC[which(DFYC$Periode %in% c(2016,2017,2018)),"Periode"]="2016-2018"
DFYC[which(DFYC$Periode %in% c(2019,2020,2021)),"Periode"]="2019-2021"
DFYC$Code.tool = as.factor(DFYC$Code.tool)
DFYC$Code.tool=factor(DFYC$Code.tool, levels = rev(levels(DFYC$Code.tool)))
mycols <- c("#0073C2FF","#EFC000FF", "#868686FF")
ggcode1=ggplot(DFYC, aes(x=Periode, group = Code.tool, fill = Code.tool))+ 
  labs(title="", x="Publication year", y="Count") +
  geom_bar(position = "dodge")+
  # geom_histogram(aes(y =..density..),
  #                bins=15,
  #                color="black",
  #                fill="dodgerblue") +
  # scale_x_continuous(breaks = seq(2007,2021, by = 1))+
  scale_y_continuous(breaks = seq(0,20, by = 2))+
  theme_bw()+
  
  # coord_flip()+
  # scale_fill_brewer(palette="Dark2")+
  # scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#39B600"))+
  # scale_fill_manual(values=c("#868686FF", "#EFC000FF", "#0073C2FF", "#CD534CFF"))+
  scale_fill_manual(values=mycols)+
  # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  theme(axis.title.y=element_text(face="bold", size =14, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x=element_text(face="bold", size =14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        legend.position = "top",legend.title = element_blank(),
        legend.text =element_text(size=10),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        axis.line = element_line(size=1, colour = "gray"),
        panel.grid.major.x =  element_blank(),
        panel.grid.major.y =  element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        # text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12))

print(ggcode1)

mycols <- c("#0073C2FF","#EFC000FF", "#868686FF")
DFYCtable=DFYC %>% 
  group_by(Code.tool) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  mutate (pos.lab=cumsum(prop) - 0.5*prop) %>%
  mutate(labels= paste0(100*round(prop,2)," %")) 

ggcode2=ggplot(DFYCtable, aes(x = "", y = prop, fill = Code.tool)) +
  geom_col() +
  coord_polar(theta = "y")+
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  
  scale_fill_manual(values = mycols) +
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         panel.background = element_blank())
ggcode2

# ----------------------------------------------------------------------------
# Literature classification>>sunburst
DFnames= DF1 %>% select(Objective,Methodology,Pb, Name) %>% count(Objective,Methodology, Pb,Name, sort= TRUE)
DFsunburst <-DFnames
DFsuncol <- c("objective","Methodology", "Pb", "Name" )

DFsunburst<-as.data.table(DFsunburst) 
# setcolorder(DFsunburst, c(DFsuncol, "n")) 
sunburstDF <- as.sunburstDF(DFsunburst, valueCol = "n")

# Sunburst
plot_ly(data = sunburstDF, ids = ~ids, labels= ~labels, text=~labels, textinfo='label+percent parent', parents = ~parents, values= ~values, type='sunburst', branchvalues = 'total')
fig00=plot_ly(data = sunburstDF, ids = ~ids, labels= ~labels, text=~labels, textinfo='label', parents = ~parents, values= ~values, type='sunburst', branchvalues = 'total')
orca(fig00, "name_classification.svg")


DFnames= DF1 %>% select(Objectiveabr,Methodologyabr,Pbabr, Name) %>% count(Objectiveabr,Methodologyabr, Pbabr,Name, sort= TRUE)
DFsunburst <-DFnames
DFsuncol <- c("objectiveabr","Methodologyabr", "Pbabr", "Name" )

DFsunburst<-as.data.table(DFsunburst) 
# setcolorder(DFsunburst, c(DFsuncol, "n")) 
sunburstDF <- as.sunburstDF(DFsunburst, valueCol = "n")



# Sunburst
plot_ly(data = sunburstDF, ids = ~ids, labels= ~labels, text=~labels, textinfo='label+percent parent', parents = ~parents, values= ~values, type='sunburst', branchvalues = 'total')
# fig0=plot_ly(data = sunburstDF, ids = ~ids, labels= ~labels, text=~labels, textinfo='label+percent parent', parents = ~parents, values= ~values, type='sunburst', branchvalues = 'total')
fig0=plot_ly(data = sunburstDF, ids = ~ids, labels= ~labels, text=~labels, textinfo='label', parents = ~parents, values= ~values, type='sunburst', branchvalues = 'total')

plot_ly(data = sunburstDF, ids = ~ids, labels= ~labels, text=~labels, textinfo="", parents = ~parents, values= ~values, type='sunburst', branchvalues = 'total')
if (!require("processx")) install.packages("processx")
fig <- fig0 %>% add_surface()
orca(fig0, "name_plot.svg")


