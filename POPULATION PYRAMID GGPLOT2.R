###########################################################################################
###                                                                                     ###
### CODE FOR THE CREATION OF OVERLAPPED AND COMPOSED POPULATION PYRAMIDS USING GGPLOT2  ###
### Author: JUAN GALEANO                                                                ###  
### Contact: jgaleano@ced.uab.es, Centre d'Estudis Demogràfics, Barcelona, Spain        ###
###                                                                                     ###
###########################################################################################

# For this example I'm using data by age, sex and place of birth (Spanish-born vs Foreign-born) 
# from some particular census tracts (those with more than 50% foreign-born residents) from the
# municipalities of Barcelona, Santa Coloma de Gramanet and Badalona in 2014.

library(ggplot2)
library(scales)
library(plyr)
# OVERLAPPED POPULATION PYRAMID:

load(url('http://gedemced.uab.cat/images/POP_ENC_AMB_2014.Rdata')) #read data from server

# The data set is by 5 columns (Edad (Age), REGION_NAC2 (Region of birth), sexo (sex), n and nREL)
# As we want to create and overlapped pyramid we need to divide population in each age, sex and grup 
# (REGION_NAC2) by the total population in each grup (ESP or EXT). 
# In the case of males we multiplied the results by -1. This process is reflected in the column nREL.

head(POP_ENC_AMB_2014)

# Once we have the data as we want we can create the overlapped pyramid:

OVERLLAPED_PYRAMID <- ggplot(POP_ENC_AMB_2014, aes(x=edad, y=nREL, fill=sexo))+
  geom_bar(stat="identity", size=.3, colour="black", position="identity")+
  coord_flip()+
  scale_y_continuous(limits=c(-2.5,2.5),
                     breaks = c(-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2, 2.5), 
                     labels = paste0(as.character(c(seq(2.5, 0, -0.5), seq(0.5, 2.5, 0.5))), "%"))+ 
  scale_x_continuous(breaks=seq(0,110,5)) +
  scale_fill_manual(values = c("#00BFC4", "#7CAE00","#F8766D", "#C77CFF"))+
  # we genereate and ad hoc legend
  annotate("text", x = 107.5, y = -0.4, label = "Hombres",size=13)+
  annotate("text", x = 107.5, y = 0.36, label = "Mujeres",size=13)+
  annotate("rect", xmin = 101, xmax = 104, ymin = -2, ymax =-1.8, alpha=1, fill="#00BFC4")+
  annotate("text", x = 102.5, y = -1.53, label = "España",size=11)+
  annotate("rect", xmin = 91, xmax = 94, ymin = -2, ymax =-1.8, alpha=1, fill="#7CAE00")+
  annotate("text", x = 92.5,  y = -1.40, label = "Extranjeros",size=11)+ 
  annotate("rect", xmin = 101, xmax = 104, ymin = 1.8, ymax =2, alpha=1, fill="#F8766D")+
  annotate("text", x = 102.5, y = 1.53, label = "España",size=11)+
  annotate("rect", xmin = 91, xmax = 94, ymin = 1.8, ymax =2, alpha=1, fill="#C77CFF")+
  annotate("text", x = 92.5,  y = 1.40, label = "Extranjeros",size=11)+ 
  #
  theme(plot.title = element_text(lineheight=1.6, size=20, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size = 35),
        legend.position="none",
        legend.background = element_rect(fill="#FFFFFF"),
        axis.title.x = element_blank(),
        axis.text.x  = element_text(angle = 0,vjust=0.5, size=35,colour="black"),
        axis.title.y = element_text(angle = 90,vjust=0.5, size=35,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=35,colour="black"),
        plot.background = element_rect(fill = "#FFFFFF"))+ylab("")+xlab("Edad")

plot(OVERLLAPED_PYRAMID)

# we save the plot as a png file using the function ggsave

setwd("C:/Users/ced/Desktop/")
ggsave("OVERLLAPED_PYRAMID.png", scale = 3, dpi = 300)

##################################################################################################

# COMPOSED POPULATION PYRAMID:

load(url('http://gedemced.uab.cat/images/POP_ENC_AMB_2014GRUPS.Rdata')) #read data from server

#in this case we will plot a composed by region of birth population pyramid. Our pupulation grups are:
# Spanish-born (1), Latinamerican-born (2), Western European-born (3), Eastern European-born(4), 
# African-born (5), Asian-born (6) and other (0)

COMPOSED_PYRAMID <- ggplot(POP_ENC_AMB_2014GRUPS, aes(x=edad, y=n, fill=sexo))+
  geom_bar(data = POP_ENC_AMB_2014GRUPS[(POP_ENC_AMB_2014GRUPS$sexo=="Mujeres\nEspaña"|
                                         POP_ENC_AMB_2014GRUPS$sexo=="Mujeres\nLatinoamerica"|
                                         POP_ENC_AMB_2014GRUPS$sexo=="Mujeres\nEuropa_Occ."|
                                         POP_ENC_AMB_2014GRUPS$sexo=="Mujeres\nEuropa_Or."|
                                         POP_ENC_AMB_2014GRUPS$sexo=="Mujeres\nÁfrica"|
                                         POP_ENC_AMB_2014GRUPS$sexo=="Mujeres\nAsia"|
                                         POP_ENC_AMB_2014GRUPS$sexo=="Mujeres\nOtros"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = edad, y = n,fill = sexo), alpha = 1)+
  geom_bar(data = POP_ENC_AMB_2014GRUPS[(POP_ENC_AMB_2014GRUPS$sexo=="Hombres\nEspaña"|
                                           POP_ENC_AMB_2014GRUPS$sexo=="Hombres\nLatinoamerica"|
                                           POP_ENC_AMB_2014GRUPS$sexo=="Hombres\nEuropa_Occ."|
                                           POP_ENC_AMB_2014GRUPS$sexo=="Hombres\nEuropa_Or."|
                                           POP_ENC_AMB_2014GRUPS$sexo=="Hombres\nÁfrica"|
                                           POP_ENC_AMB_2014GRUPS$sexo=="Hombres\nAsia"|
                                           POP_ENC_AMB_2014GRUPS$sexo=="Hombres\nOtros"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = edad, y = n,fill = sexo), alpha = 1)+
  coord_flip()+
  scale_y_continuous(limits=c(-750,750),
                     breaks = c(-750, -500, -250,0,250,500,750), 
                     labels = paste0(as.character(c(seq(750, 0, -250), seq(250, 750, 250))), "")) + 
  scale_x_continuous(breaks=seq(0,110,5)) +
  scale_fill_manual(values = c("#00FF00","#FE2EC8","#F4FA58","#0000FF",
                               "#2ECCFA","#FF0000","#A4A4A4",
                               "#00FF00","#FE2EC8","#F4FA58","#0000FF",
                               "#2ECCFA","#FF0000","#A4A4A4"))+
  # we genereate and ad hoc legend
  annotate("text", x = 107.5, y = -120, label = "Hombres",size=13)+
  annotate("text", x = 107.5, y = 108, label = "Mujeres",size=13)+
  annotate("rect", xmin = 106, xmax = 109, ymin = 400, ymax =450, alpha=1, fill="#F4FA58")+
  annotate("text", x = 107.5, y = 526, label = "España",size=11)+
  annotate("rect", xmin = 96, xmax = 99, ymin = 400, ymax =450, alpha=1, fill="#FF0000")+
  annotate("text", x = 97.5, y = 583, label = "Latinoamérica",size=11)+
  annotate("rect", xmin = 86, xmax = 89, ymin = 400, ymax =450, alpha=1, fill="#0000FF")+
  annotate("text", x = 87.5, y = 625, label = "Europa Occidental",size=11)+
  annotate("rect", xmin = 76, xmax = 79, ymin = 400, ymax =450, alpha=1, fill="#2ECCFA")+
  annotate("text", x = 77.5, y = 600, label = "Europa Oriental",size=11)+
  annotate("rect", xmin = 66, xmax = 69, ymin = 400, ymax =450, alpha=1, fill="#00FF00")+
  annotate("text", x = 67.5, y = 515, label = "África",size=11)+
  annotate("rect", xmin = 56, xmax = 59, ymin = 400, ymax =450, alpha=1, fill="#FE2EC8")+
  annotate("text", x = 57.5, y = 500, label = "Asia",size=11)+
  #
  theme(plot.title = element_text(lineheight=1.6, size=20, face="bold"),
        legend.title = element_blank(),
        legend.text = element_text(colour="black", size = 35),
        legend.position="none",
        legend.background = element_rect(fill="#FFFFFF"),
        axis.title.x = element_text(angle = 0,vjust=0.5, size=30,colour="black"),
        axis.text.x  = element_text(angle = 0,vjust=0.5, size=35,colour="black"),
        axis.title.y = element_text(angle = 90,vjust=0.5, size=30,colour="black"),
        axis.text.y  = element_text( vjust=0.5, size=35,colour="black"),
        plot.background = element_rect(fill = "#FFFFFF"))+ylab("")+xlab("Edad")

plot(COMPOSED_PYRAMID)
# we save the plot as a png file using the function ggsave

setwd("C:/Users/ced/Desktop/")
ggsave("COMPOSED_PYRAMID.png", scale = 3, dpi = 300)
