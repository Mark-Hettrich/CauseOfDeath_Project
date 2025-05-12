file_path <- here("Bivariate Analysis von Xinyu Meng", "data causes of death", "15 causes of death.csv")
df.15.causes <- read.csv(file_path)
top5_deaths <- df.15.causes[1:4,]

#`15 Leading Causes of Death`                          `15 Leading Causes of Death Code`  Deaths Population `Crude Rate`
#<chr>                                                 <chr>                               <dbl>      <dbl>        <dbl>
1 #Diseases of heart (I00-I09,I11,I13,I20-I51)          GR113-054                         4090792 1984987277        206. 
2 #Malignant neoplasms (C00-C97)                        GR113-019                         3628161 1984987277        183. 
3 #Accidents (unintentional injuries) (V01-X59,Y85-Y86) GR113-112                         1215794 1984987277         61.2
4 #COVID-19 (U07.1)                                     GR113-137                         1004208 1984987277         50.6
5 #Cerebrovascular diseases (I60-I69)                   GR113-070                          949001 1984987277         47.8