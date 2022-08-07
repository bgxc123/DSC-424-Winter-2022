library(corrplot)
library(psych) #has a much better scatterplot function
library(dplyr)
df = read.csv("NFLData.csv")

str(df)
summary(df)

df_nums = df[,c(-1,-3,-7:-10,-12:-13,-20,-25)]
head(df_nums)

df_nums = df_nums %>% relocate(pick, .before = combineYear)
head(df_nums)
cor.data = cor(df_nums)
corrplot(cor.data)

pairs.panels(df_nums)
