library("tidyverse") 
library("stringr")    
library("dplyr")      
library("ggplot2") 

#������ ������ �� ����� 
eddy = read_csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
eddy = eddy[-1,]
#��������� �������� ������ ����������
eddy = eddy %>% mutate_if(is.character, factor)
#�������� ����� �������
names(eddypro) = names(eddypro) %>% 
  str_replace_all("[!]", "_emph_") %>% 
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_div_") %>%
  str_replace_all("[%]", "_perc_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "L_")

glimpse(eddy)

#��������� �������
eddy = drop_na(eddy)
eddy = filter(eddy, DOY >= 59 & DOY < 151)
eddy = filter(eddy, daytime==FALSE)
eddy_numeric = eddy[,sapply(eddy,is.numeric) ]
eddy_non_numeric = eddy[,!sapply(eddy,is.numeric) ]

#������ ������� ��� �������
cor_td = cor(drop_na(eddy_numeric)) %>% as.data.frame %>% select(h2o_flux)
#������� ����� ���������� (�����) � ������������� ������������ ������ 0.1
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude; vars
#� ���� �������:
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep="")); formula
#�������� �������:
row_numbers = 1:length(eddy$date)
teach = sample(row_numbers, floor(length(eddy$date)*.7))
test = row_numbers[-teach]
#����������
teaching_tbl_unq = eddy[teach,]
testing_tbl_unq = eddy[test,]


#������� ������ �������� ���������
model1 = lm(formula, eddy = eddy);model1

summary(model1)
anova(model1)
plot(model1)

#������� 2-� ������ 
formula = as.formula(paste("h2O_flux~", "(", paste(vars,collapse = "+"), ")^2", sep="", collapse = NULL));formula
#������� ������ �������� ���������
model2 = lm(formula, data = data);model2



summary(model2)
anova(model2)
plot(model2)

