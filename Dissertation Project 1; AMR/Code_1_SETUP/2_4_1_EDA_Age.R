if (!exists('t.trakage')){
  source('Code_1_SETUP/2_3_1_EDA_TRAK.R')
}


#### Age and DDD

ageddd <- ggplot(data.cc) +
  aes(x = l_DDDp1kOBD, y = AvgAge, color = Class_Gr) +
  stat_smooth(method = 'lm', fullrange = TRUE) +
  guides(color = guide_legend(title = 'Class Group', 
                              nrow = 1, 
                              title.theme = element_text(size = 14), 
                              label.theme = element_text(size = 14), 
                              keywidth = 1.2, 
                              keyheight = 1.2)) +
  labs(x = 'Log DDD/1000 OBD', y = 'Average Age', 
       title = 'Average Age vs Log DDD by Class') +
  theme(axis.text.x  = element_text(size = 14), 
        axis.text.y  = element_text(size = 14), 
        axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        plot.title = element_text(size = 18))


##============================================================================##
####                            SAVE PLOTS                                  ####
##============================================================================##

ggsave('Figures/2_4_1_EDA_Age.png', plot = ageddd, width = 10, height = 6)









##============================================================================##
####                            COMMENTS                                    ####
##============================================================================##

summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'A']))$r.squared
summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'B']))$r.squared
summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'C']))$r.squared
summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'D']))$r.squared
summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'E']))$r.squared
summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'F']))$r.squared
summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'G']))$r.squared


summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'A']))$coef[2,1]
summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'B']))$coef[2,1]
summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'C']))$coef[2,1]
summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'D']))$coef[2,1]
summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'E']))$coef[2,1]
summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'F']))$coef[2,1]
summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'G']))$coef[2,1]


summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'A']))
summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'B']))
summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'C']))
summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'D']))
summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'E']))
summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'F']))
summary(lm(AvgAge ~ l_DDDp1kOBD, data = data.cc[Class_Gr == 'G']))





