if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, hrbrthemes,
               scales, plotly, gganimate, cobalt, ivpack, stargazer, haven, ggthemes,
               gifski, magick, rdrobust, here, modelsummary, rddensity,
               MatchIt, cobalt, lfe, fixest, dotwhisker, kableExtra, knitr)

# Cleaning and arranging datasets ------------------------------------------------------
final.hcris.v1996 <- HCRIS_Data_v1996
final.hcris.v2010 <- HCRIS_Data_v2010

## create missing variables for columns introduced in v2010 of hcris forms
final.hcris.v1996 = final.hcris.v1996 %>%
  mutate(hvbp_payment=NA, hrrp_payment=NA)

## combine v1996 and v2010 hcris forms, and sort by provider_number/year
final.hcris=rbind(final.hcris.v1996,final.hcris.v2010) %>%
  mutate(fy_end=mdy(fy_end),fy_start=mdy(fy_start),
         date_processed=mdy(date_processed),date_created=mdy(date_created),
         tot_discounts=abs(tot_discounts), hrrp_payment=abs(hrrp_payment)) %>%
  mutate(fyear=year(fy_end)) %>%
  arrange(provider_number,fyear) 

## count of hospitals/provider_number by year
hospcountyear <- final.hcris %>% group_by(fyear) %>% count()

final.hcris =
  final.hcris %>% 
  add_count(provider_number, fyear, name="total_reports")

## create running total of reports
final.hcris =
  final.hcris %>% 
  group_by(provider_number, fyear) %>%
  mutate(report_number=row_number())

## identify hospitals with only one report per fiscal year 
## this will be the first set of hospitals in the final dataset
unique.hcris1 =
  final.hcris %>%
  filter(total_reports==1) %>%
  select(-report, -total_reports, -report_number, -npi, -status) %>%
  mutate(source='unique reports')


## identify hospitals with multiple reports per fiscal year
duplicate.hcris = 
  final.hcris %>%
  filter(total_reports>1) %>%
  mutate(time_diff=fy_end-fy_start)

## calculate elapsed time between fy start and fy end for hospitals with multiple reports
duplicate.hcris = 
  duplicate.hcris %>% 
  group_by(provider_number, fyear) %>%
  mutate(total_days=sum(time_diff))

## if the elapsed time within a fy sums to around 365, then just take the total of the two
## this will be the second set of hospitals in the final dataset
unique.hcris2 = 
  duplicate.hcris %>%
  filter(total_days<370) %>%
  group_by(provider_number, fyear) %>%
  mutate(hrrp_payment=if_else(is.na(hrrp_payment),0,hrrp_payment),
         hvbp_payment=if_else(is.na(hvbp_payment),0,hvbp_payment)) %>%
  summarize(beds=max(beds), tot_charges=sum(tot_charges), tot_discounts=sum(tot_discounts),
            tot_operating_exp=sum(tot_operating_exp), ip_charges=sum(ip_charges),
            icu_charges=sum(icu_charges), ancillary_charges=sum(ancillary_charges),
            tot_discharges=sum(tot_discharges), mcare_discharges=sum(mcare_discharges),
            mcaid_discharges=sum(mcaid_discharges), tot_mcare_payment=sum(tot_mcare_payment),
            secondary_mcare_payment=sum(secondary_mcare_payment), hvbp_payment=sum(hvbp_payment),
            hrrp_payment=sum(hrrp_payment), fy_start=min(fy_start), fy_end=max(fy_end),
            date_processed=max(date_processed), date_created=min(date_created), 
            street=first(street), city=first(city), state=first(state),
            zip=first(zip), county=first(county)) %>%
  mutate(source='total for year')

## identify hospitals with more than one report and with elapsed time exceeding 370 days
duplicate.hcris2 =
  duplicate.hcris %>%
  filter(total_days>=370) %>%
  mutate(max_days=max(time_diff), max_date=max(fy_end))

## identify hospitals with one report (out of multiple) that appears to cover the full year
## this will be the third set of hospitals in the final dataset
unique.hcris3 =
  duplicate.hcris2 %>%
  filter(max_days==time_diff, time_diff>360, max_date==fy_end) %>%
  select(-report, -total_reports, -report_number, -npi, -status, -max_days, -time_diff, -total_days, -max_date) %>%
  mutate(source='primary report')

## identify remaining hospitals (those with more than one report that cover more than one full year and that do
##   not appear to have one report that takes up the full year)
## these hospitals appear to have changed their fiscal years
duplicate.hcris3=anti_join(duplicate.hcris2, unique.hcris3, by=c("provider_number", "fyear"))
duplicate.hcris3 =
  duplicate.hcris3 %>%
  mutate(total_days=as.integer(total_days), time_diff=as.integer(time_diff)) %>%
  mutate_at(c("tot_charges","tot_discounts", "tot_operating_exp", "ip_charges",
              "icu_charges", "ancillary_charges", "tot_discharges", "mcare_discharges",
              "mcaid_discharges", "tot_mcare_payment", "secondary_mcare_payment",
              "hvbp_payment", "hrrp_payment"),list(~ .*(time_diff/total_days)))

## form weighted average of values for each fiscal year
unique.hcris4 = 
  duplicate.hcris3 %>%
  group_by(provider_number, fyear) %>%
  mutate(hrrp_payment=if_else(is.na(hrrp_payment),0,hrrp_payment),
         hvbp_payment=if_else(is.na(hvbp_payment),0,hvbp_payment)) %>%
  summarize(beds=max(beds), tot_charges=sum(tot_charges), tot_discounts=sum(tot_discounts),
            tot_operating_exp=sum(tot_operating_exp), ip_charges=sum(ip_charges),
            icu_charges=sum(icu_charges), ancillary_charges=sum(ancillary_charges),
            tot_discharges=sum(tot_discharges), mcare_discharges=sum(mcare_discharges),
            mcaid_discharges=sum(mcaid_discharges), tot_mcare_payment=sum(tot_mcare_payment),
            secondary_mcare_payment=sum(secondary_mcare_payment), hvbp_payment=sum(hvbp_payment),
            hrrp_payment=sum(hrrp_payment), fy_start=min(fy_start), fy_end=max(fy_end),
            date_processed=max(date_processed), date_created=min(date_created), 
            street=first(street), city=first(city), state=first(state),
            zip=first(zip), county=first(county)) %>%
  mutate(source='weighted_average')



# Save + create final HCRIS data ---------------------------------------------------------

final.hcris.data=rbind(unique.hcris1, unique.hcris2, unique.hcris3, unique.hcris4)
final.hcris.data =
  final.hcris.data %>%
  # rename(year=fyear) %>%
  arrange(provider_number, year)

write_rds(final.hcris.data,'HCRIS_Data.rds')
write_csv(final.hcris.data,'HCRIS_Data.csv')

# Adding Profit Status Data  ---------------------------------------------------------

final.hcris.data$provider_number <- as.numeric(final.hcris.data$provider_number)

ProfitIncluded <- final.hcris.data %>% 
  left_join(profit_status_by_provider_number, by.x="provider_number", by.y="provider_number", all.x=TRUE, na.rm=TRUE)

profit_status_by_provider_number$provider_number <- as.numeric(profit_status_by_provider_number$provider_number)



# Creation of Final Dataset
FINALDATA <- ProfitIncluded %>% 
  filter(!is.na(Profit))

FINALDATA <- FINALDATA %>% 
 mutate(ProfitStatus = ifelse(Profit == 1, "ForProfit", "NonProfit"))  

write.csv(FINALDATA, "FINALDATA.csv")
write_rds(FINALDATA,'FINALDATA.rds')


# Data Exploration::TOTCHARGES  ---------------------------------------------------------

## Creation of Dataset
forprofitFINAL <- FINALDATA %>%
  filter(Profit == 1)%>%
  select(year, tot_charges, ProfitStatus) %>% 
  group_by(year) %>%
  summarize(meanTotCharges = mean(tot_charges, na.rm =TRUE)) %>% 
  mutate(ProfitStatus = "For-Profit")

nonprofitFINAL <- FINALDATA %>%
  filter(Profit == 0)%>%
  select(year, tot_charges, ProfitStatus) %>% 
  group_by(year) %>%
  summarize(meanTotCharges = mean(tot_charges, na.rm =TRUE)) %>% 
  mutate(ProfitStatus = "Non-Profit") 


profit_TotCharges <- rbind(forprofitFINAL, nonprofitFINAL)

## Graphing
profitvnonprofit_totcharges <- profit_TotCharges %>%
  group_by(year) %>%
  ggplot( aes(x=year, y = meanTotCharges,  fill=ProfitStatus, color=ProfitStatus)) +
  geom_line() +
  theme_bw() +
  xlab("Year") + ylab(" Total Charges ($)")

profitvnonprofit_totcharges
# Data Exploration::Total Operating Expenditure  ---------------------------------------------------------

## Creation of Data
OperatingExpProfit <- FINALDATA %>%
  filter(Profit == 1)%>%
  select(year, tot_operating_exp, ProfitStatus) %>% 
  group_by(year) %>%
  summarize(meanOperatingExp = mean((tot_operating_exp), na.rm =TRUE)) %>% 
  mutate(ProfitStatus = "For-Profit")


OperatingExpNonProfit <- FINALDATA %>%
  filter(Profit == 0)%>%
  select(year, tot_operating_exp, ProfitStatus) %>% 
  group_by(year) %>%
  summarize(meanOperatingExp = mean((tot_operating_exp), na.rm =TRUE)) %>% 
  mutate(ProfitStatus = "Non-Profit")

profit_operatingexp <- rbind(OperatingExpProfit, OperatingExpNonProfit)

## Graphing
profitvnonprofit_operatingexp <- profit_operatingexp %>%
  group_by(year) %>%
  ggplot( aes(x=year, y = meanOperatingExp,  fill=ProfitStatus, color=ProfitStatus)) +
  geom_line() +
  theme_bw() +
  xlab("Year") + ylab(" Total Operating Expenditure ($)")

  
profitvnonprofit_operatingexp

# Data Exploration:: Overall Price Analysis  ---------------------------------------------------------
## Data Creation
PriceData <- FINALDATA %>% 
  mutate( discount_factor = 1-tot_discounts/tot_charges,
          price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment,
          price_denom = tot_discharges - mcare_discharges,
          price = price_num/price_denom) %>% 
  filter(price_denom>10, !is.na(price_denom), 
         price_num>0, !is.na(price_num))

## Graphing
PriceDataYearGraph <- PriceData %>% 
  ggplot(aes(x=as.factor(year), y=price, fill = ProfitStatus)) + 
  geom_boxplot() +
  labs(
    x="Year",
    y="Price ($)",
  ) + scale_y_continuous(labels=comma, limits = c(0, 60000)) +
  theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust=1))
PriceDataYearGraph

# Matching and Weighting Analysis  ---------------------------------------------------------

lp.vars2 <- PriceData %>% 
  ungroup() %>% 
  dplyr::select(beds, ip_charges, tot_mcare_payment, price, tot_operating_exp, Profit)
  
lp.vars <- PriceData %>% 
  ungroup() %>% 
  dplyr::select(beds, ip_charges, tot_mcare_payment, price, tot_operating_exp, Profit) %>% 
  rename("In Patient Charges" = ip_charges, 
         "Price" = price, 
         "Total Medicare Payment" = tot_mcare_payment, 
         "Total Operating Expenses" = tot_operating_exp, 
         )

lp.covs <- lp.vars %>% 
  dplyr::select(-c("Profit", 
                                 "beds"))

## Mahalonobis nearest neighbor matching
m.nn.md <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$Profit,
                           X=lp.covs,
                           M=1,
                           Weight=2,
                           estimand="ATE") 
summary(m.nn.md)


v.name=data.frame(new=c("In Patient Charges"," Total MCare Payment", "Price",
                        " Total Operating Expenditure"))

### Creation of Love Plot
FinalLovePlot <- love.plot(bal.tab(m.nn.md, covs = lp.covs, treat = lp.vars$Profit), 
          threshold=0.1, 
          var.names=v.name,
          grid=FALSE, sample.names=c("Unmatched", "Matched"),
          position="top", shapes=c("circle","triangle"),
          colors=c("black","blue")) + 
  theme_bw()
FinalLovePlot


## Propensity score nearest neighbor matching for Common Support Analysis

logit.model <- glm(Profit ~ ip_charges + tot_mcare_payment +
                     tot_operating_exp, family=binomial, data=lp.vars2)
ps <- fitted(logit.model)
m.nn.ps <- Matching::Match(Y=lp.vars$price,
                           Tr=lp.vars$Profit,
                           X=ps,
                           M=1,
                           estimand="ATE")

CommonSupportTest <- ggplot(lp.vars2, aes(x=ps)) + geom_histogram() + 
  facet_wrap(~ Profit, ncol=1) +
  theme_bw()
CommonSupportTest

# save work   ---------------------------------------------------------

save.image("470FinalProject.Rdata")