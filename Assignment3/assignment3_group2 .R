# Question 1-a,b with sample size 50 and 150
pnorm(75,80,25/sqrt(50))
pnorm(75,80,25/sqrt(150))

# Question 1-d  95% confidence interval for a sample mean of 72 (Sample size 50)
zalfa <- qnorm(0.975,0,1)
error <- (25/sqrt(50)) * zalfa
72 + c(-error, error)

# Question 2-a  descriptive statistics to summarize the data.
professional_df <- read.csv("Professional.csv", header = TRUE)
summary(professional_df)

# Question 2-b 95% confidence intervals for the mean age and household income
ageMean <- mean(professional_df$Age)
thalfa <- qt(0.975,df=length(professional_df$Age)-1)
error <- sd(professional_df$Age)/sqrt(length(professional_df$Age))*thalfa
ageMean +c (-error,+error)

householdMean <- mean(professional_df$Household.Income)
thalfa <- qt(0.975,df=length(professional_df$Household.Income)-1)
error <- sd(professional_df$Household.Income)/sqrt(length(professional_df$Household.Income))*thalfa
householdMean +c (-error,+error)

# Question 2-c 95% confidence intervals for the proportion of subscribers who have broad- band and children
broadbandMeanProp <- sum(professional_df$Broadband.Access == "Yes")/length(professional_df$Broadband.Access)
broadbandMeanProp
zhalfa <- qnorm(0.975,0,1)
error <- sqrt(broadbandMeanProp * (1-broadbandMeanProp)/length(professional_df$Broadband.Access))*zhalfa
broadbandMeanProp + c(-error,+error)          

childrenMeanProp <- sum(professional_df$Have.Children == "Yes")/ length(professional_df$Have.Children)
childrenMeanProp
zhalfa <- qnorm(0.975,0,1)
error <- sqrt(childrenMeanProp * (1-childrenMeanProp)/length(professional_df$Have.Children))*zhalfa
childrenMeanProp + c(-error,+error)

# Question 2-d good advertising outlet for online brokers
prop.table(table(professional_df$Broadband.Access))
professional_withbroadband_df <- professional_df[professional_df$Broadband.Access == "Yes", ]
prop.table(table(professional_withbroadband_df$Real.Estate.Purchases))

# Question 2-e good place to advertise for companies selling educational
# software and computer games for young children
children_df <- professional_df[professional_df$Have.Children == 'Yes', ]
mean(children_df$Age)

# Question 3 
# Load data
gulf_info_df <- read.csv("GulfProp.csv" , header = TRUE)

# Ignoring null values
gulf_info_df <- na.omit(gulf_info_df)

# Splitting into separate dataframes
gulf_view_df <- gulf_info_df[gulf_info_df$Type == "Gulf View",]
no_gulf_view_df <- gulf_info_df[gulf_info_df$Type == "No Gulf View",]

# Question 3-a,b: descriptive statistics to summarize Gulf View and No Gulf View condominiums
summary(gulf_view_df)
summary(no_gulf_view_df)

# Question 3-d: 95% confidence interval estimate of the population mean sales price and 
# population mean number of days to sell for Gulf View condominiums.
mean(gulf_view_df$Sale.Price)
thalfa <- qt(0.975,df=length(gulf_view_df$Sale.Price)-1)
error <- sd(gulf_view_df$Sale.Price)/sqrt(length(gulf_view_df$Sale.Price)) * thalfa
sd(gulf_view_df$Sale.Price)
mean(gulf_view_df$Sale.Price) + c(-error,error)

mean(gulf_view_df$Days.to.Sell)
thalfa <- qt(0.975,df=length(gulf_view_df$Days.to.Sell)-1)
error <- sd(gulf_view_df$Days.to.Sell)/sqrt(length(gulf_view_df$Days.to.Sell)) * thalfa
mean(gulf_view_df$Days.to.Sell) + c(-error,error)
c(-error,error)


# Question 3-e: 95% confidence interval estimate of the population mean sales price and 
# population mean number of days to sell for No Gulf View condominiums.
mean(no_gulf_view_df$Sale.Price)
thalfa <- qt(0.975,df=length(no_gulf_view_df$Sale.Price)-1)
error <- sd(no_gulf_view_df$Sale.Price)/sqrt(length(no_gulf_view_df$Sale.Price)) * thalfa
mean(no_gulf_view_df$Sale.Price) + c(-error,error)

mean(no_gulf_view_df$Days.to.Sell)
thalfa <- qt(0.975,df=length(no_gulf_view_df$Days.to.Sell)-1)
error <- sd(no_gulf_view_df$Days.to.Sell)/sqrt(length(no_gulf_view_df$Days.to.Sell)) * thalfa
mean(no_gulf_view_df$Days.to.Sell) + c(-error,error)

#Question 3-f:finding the sample size with given margin
zhalfa <- qnorm(0.975,0,1)
zhalfa
error <- 40000
zhalfa^2*(sd(gulf_view_df$Sale.Price)*1000)^2/error^2
sd(gulf_view_df$Sale.Price)

zhalfa <- qnorm(0.975,0,1)
error <- 15000
zhalfa^2*(sd(no_gulf_view_df$Sale.Price)*1000)^2/error^2
sd(no_gulf_view_df$Sale.Price)

# Question 3-g: estimate of the final selling price and number of days

mean_listprice <- mean(gulf_view_df$List.Price)
mean_sale_price <- mean(gulf_view_df$Sale.Price)
mean_decrease_percent <- ((mean_listprice-mean_sale_price)/mean_listprice)*100
new_sale_price <- 589000 - ((mean_decrease_percent * 589000)/100)
new_sale_price

mean_listprice <- mean(no_gulf_view_df$List.Price)
mean_sale_price <- mean(no_gulf_view_df$Sale.Price)
mean_decrease_percent <- ((mean_listprice-mean_sale_price)/mean_listprice)*100
mean_decrease_percent
new_sale_price <- 285000 - ((mean_decrease_percent * 285000)/100)
new_sale_price

mean(gulf_view_df$Days.to.Sell)
mean(no_gulf_view_df$Days.to.Sell)















