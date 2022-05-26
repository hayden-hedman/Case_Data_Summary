#title: "infectious_disease_case_summary"
#author: "Hayden Hedman"
#date: "2021-10-21"

## FIRST CHECKS IF PACKAGE PACMAN IS INSTALLED
install.packages(setdiff("pacman", rownames(installed.packages()))) 
## PACKAGE PACMAN LOADS AND CHECKS IF EACH PACKAGE NEEDS INSTALLED
pacman::p_load(dplyr,tidyr, tidyverse, zoo, scales, ggpubr, xlsx)

## SET WORKING DIRECTORY WHERE THE .XLSX LINE LIST IS LOCATED
setwd(choose.dir())

## LOAD DATA
#df2 <- read_excel("Data entry from sites_Ft. Dix.xlsx")
#df2 <- read_excel("Data entry from sites_Ft. Bliss.xlsx")

df <- read.xlsx("Dummy_Data_Case_Data_Summary.xlsx",sheetIndex=1, colNames = TRUE,detectDates=F)
#####################eb###########################################################
## SET SITE POPULATION FACTOR
P = 9999 # IMAGINATION LAND
PF = 100000/P
###############################################################################
## COMPILE MULTIPLE PATHOGENS (1-6)
p1 <- df[,c("Testing.Date","Pathogen.1","Case.Status.1")]
colnames(p1)[2]<- "Pathogen"
colnames(p1)[3]<- "case_status"

p2 <- df[,c("Testing.Date","Pathogen.2","Case.Status.2")]
colnames(p2)[2]<- "Pathogen"
colnames(p2)[3]<- "case_status"

p3 <- df[,c("Testing.Date","Pathogen.3","Case.Status.3")]
colnames(p3)[2]<- "Pathogen"
colnames(p3)[3]<- "case_status"

p4 <- df[,c("Testing.Date","Pathogen.4","Case.Status.4")]
colnames(p4)[2]<- "Pathogen"
colnames(p4)[3]<- "case_status"

p5 <- df[,c("Testing.Date","Pathogen.5","Case.Status.5")]
colnames(p5)[2]<- "Pathogen"
colnames(p5)[3]<- "case_status"

p6 <- df[,c("Testing.Date","Pathogen.6","Case.Status.6")]
colnames(p6)[2]<- "Pathogen"
colnames(p6)[3]<- "case_status"
## COMPILED PATHOGEN DATAFRAME
df2 <- rbind(p1,p2,p3,p4,p5,p6)
colnames(df2)[1]<-"test_date"
###############################################################################
## STRATIFY DATA OF INTEREST
df2 <- subset(df2, case_status == "Positive" | case_status == "Suspect active TB" | case_status == "LTBI")
#df2$test_date <- as.Date(format(as.Date(df2$test_date, "%m/%d/%Y"), "%Y-%m-%d"))
## PROXY COUNT 
df2$count=1
###############################################################################
## SUMMARIZE BY TEST DATE
daily_summ <- data.frame(group_by(df2, test_date, Pathogen) %>% 
                        summarize(count = sum(count=="1")))

## STRATIFY DATASET BY Pathogen OF INTEREST
daily_summ2 <- subset(daily_summ, !is.na(test_date))

## MISSING DATES
missing_dates <- subset(df2, is.na(test_date) & Case.Status=="Positive")
##write.csv(missing_dates, "missing_dates_picket.csv", row.names=F)
######################################################################################
## CREATE DUMMY_PATHOGEN_df2
dummy_df2 <- head(daily_summ2, 1)
dummy_df2$test_date <-  Sys.Date()
dummy_df2$Pathogen <- "CTHULU"
######################################################################################
## COMBINE DUMMY df2 WITH DAILY SUMM
daily_summ3 <- rbind(daily_summ2, dummy_df2)
######################################################################################
## WORKING SUBSET FUNCTION!!!
daily_summ4 <- complete(daily_summ3, Pathogen, test_date)
daily_summ4 <- data.frame(daily_summ4)
daily_summ4 <- subset(daily_summ4, Pathogen!= "CTHULU")

daily_summ4[is.na(daily_summ4)] <- 0
######################################################################################
## CUMULATIVE SUMMARY METRICS BY PATHOGEN
cum_df2 <- with(daily_summ4,
            by(daily_summ4, Pathogen,
               cumulative_mean <- function(x) {
                 output_data <- x %>%
                   arrange(test_date) %>%
                   mutate(rolling_7d_avg =zoo::rollapplyr(count, 7, mean, partial = TRUE)) %>%
                   mutate(rolling_21d_avg =zoo::rollapplyr(count, 21, mean, partial = TRUE)) %>%
                   mutate(rolling_7d_sum =zoo::rollapplyr(count, 7, sum, partial = TRUE)) 
                 
                 output_data$rolling_7d_avg <- signif(output_data$rolling_7d_avg, digits=2)
                 output_data$rolling_21d_avg <- signif(output_data$rolling_21d_avg, digits=2)
                 output_data[is.na(output_data)] <- 0
                 output_data$Status = "Neutral"
                 output_data$Status[which(output_data$rolling_7d_avg > output_data$rolling_21d_avg)]<- "Increasing"
                 output_data$Status[which(output_data$rolling_21d_avg > output_data$rolling_7d_avg)]<- "Decreasing"
                 output_data$rolling_7d_avg = signif(output_data$rolling_7d_avg*PF, digits=2)
                 output_data$rolling_21d_avg = signif(output_data$rolling_21d_avg*PF,digits=2)
                 print(output_data)
               }))
######################################################################################
## PREP DATA FOR TABLE OUTPUT 
rec_cum <- do.call(rbind.data.frame, cum_df2)
rec_cum <- subset(rec_cum, test_date==Sys.Date())

## SUMMARIZE TOTAL DATA
no_missing_date <- subset(df2, !is.na(test_date))
total_summ <- data.frame(group_by(no_missing_date, Pathogen) %>% 
                           summarize(cum_sum = sum(count=="1"),
                                     min_date = min(test_date)))
rec_cum$cum_sum <- total_summ$cum_sum
rec_cum$cum_date <- total_summ$min_date
rec_cum$cum_date <- format(as.Date(rec_cum$cum_date, "%Y-%m-%d"), "%m/%d/%y")
rec_cum$test_date <- format(as.Date(rec_cum$test_date, "%Y-%m-%d"), "%m/%d/%y")


location_name = head(df$Location.Where.Case.Identified,1)
rec_cum$Location = location_name
rec_cum$priority= "No"

## ADD CATEGORIZATION FOR HIGH PRIORITY Pathogen
rec_cum$priority[which(rec_cum$Pathogen=="Malaria" & rec_cum$diff_prop >= 150)] <- "Yes"
rec_cum$priority[which(rec_cum$Pathogen=="Meningitis" & rec_cum$diff_prop >= 150)] <- "Yes"
rec_cum$priority[which(rec_cum$Pathogen=="Meningitis" & rec_cum$rolling_7d_sum >= 5)] <- "Yes"
rec_cum$priority[which(rec_cum$Pathogen=="Cholera" & rec_cum$rolling_7d_sum > 0)] <- "Yes"
rec_cum$priority[which(rec_cum$Pathogen=="Measles" & rec_cum$rolling_7d_sum > 0)] <- "Yes"
rec_cum$priority[which(rec_cum$Pathogen=="Acute Flaccid Paralysis" & rec_cum$rolling_7d_sum > 0)] <- "Yes"
rec_cum$priority[which(rec_cum$Pathogen=="Polio" & rec_cum$rolling_7d_sum > 0)] <- "Yes"
rec_cum$priority[which(rec_cum$Pathogen=="Yellow Fever" & rec_cum$rolling_7d_sum > 0)] <- "Yes"
rec_cum$priority[which(rec_cum$Pathogen=="Viral Haemorrhagic Fevers" & rec_cum$rolling_7d_sum > 0)] <- "Yes"

rec_cum2 <- rec_cum[,c("Pathogen","priority","rolling_21d_avg","rolling_7d_avg","Status","cum_sum","cum_date")] 
## CLEAN UP VARIABLE NAMES
rec_cum3 <- rec_cum2
colnames(rec_cum3)[1]<- "Pathogen"
colnames(rec_cum3)[2]<- "Public Health Concern"
colnames(rec_cum3)[3]<- "21-day Average"
colnames(rec_cum3)[4]<- "7-day Average"
colnames(rec_cum3)[5]<- "Status"
colnames(rec_cum3)[6]<- "Cumulative Count"
colnames(rec_cum3)[7]<- "First Detected"
###########################################################################################
## CONVERT LISTS OF df2'S TO DATA FRAME  
cum_df22 <- do.call(rbind.data.frame, cum_df2)

## GGPLOT: COUNT AND 7-DAY AVG
date_limit = Sys.Date() - 43
sys_date = format(Sys.Date(), "%m/%d/%y")

non_covid <- subset(cum_df22, test_date > date_limit & Pathogen!="COVID-19")

fn_lineplot1<- paste(location_name,"-",sys_date)

non_covid_line_plot <- ggplot(non_covid, aes(x=test_date, y=rolling_7d_avg)) + 
        geom_line(size=1, aes(color=Pathogen))+
        geom_point(size=2, aes(color=Pathogen))+
        ylab("7-day moving average of cases per 100,000") + 
         xlab("Date") +
        ggtitle(fn_lineplot1)+
         scale_x_date(date_breaks = "1 week", date_labels = "%m/%d")+
         #facet_wrap(~Pathogen,scales='free')+
         scale_y_continuous(breaks= pretty_breaks())+
         theme(text = element_text(size = 12),
               plot.title = element_text(size = 12, face = "bold"),
               panel.background = element_blank(),
               panel.border = element_rect(colour = "black", fill=NA, size=3),
               axis.title.x = element_blank(),
               axis.line = element_line(size = 1, colour = "black"))
##############################################################################################
## COVID-19 LINE PLOT
covid <- subset(cum_df22, test_date > date_limit & Pathogen=="COVID-19")
## ASSIGN TITLE OF PLOT
fn_lineplot2<- paste(location_name,"-","COVID-19")

covid_line_plot <- ggplot(covid, aes(x=test_date, y=rolling_7d_avg)) + 
  geom_line(size=1)+
  geom_point(size=2)+
  ylab("7-day moving average of new cases per 100,000") + 
  xlab("Date") +
  ggtitle("COVID-19")+
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d")+
  #facet_wrap(~Pathogen,scales='free')+
  scale_y_continuous(breaks= pretty_breaks())+
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=3),
        axis.title.x = element_blank(),
        axis.line = element_line(size = 1, colour = "black"))
##############################################################################################
## CREATE TABLE 
summary_table <- ggtexttable(rec_cum3, rows = NULL, 
                        theme = ttheme("mOrange", base_size=4))
## TABLE CAPTION TEXT
text <- paste("Concern: based on UNHRC",
              "Cumulative 21- and 7-day averages are per 100,000",
              "COVID cases were identified through a combination of screening all Afghan guests upon arrival, serial COVID screening (XX% of guests per week), testing symptomatic guests who present to the clinic, and serial testing of family members isolating with confirmed COVID cases. Consequently, incidence rates of COVID cases per 100,000 guests at Safe Havens should not be interpreted the same as community rates throughout the United States.",
              sep = "; ")
text.p <- ggparagraph(text = text, size = 7, color = "black")
##############################################################################################
integer_breaks <- function(x)
  seq(floor(min(x)), ceiling(max(x)))

non_covid_barplot <- ggplot(non_covid, aes(x=test_date, y=count)) + 
         geom_bar(stat="identity", color="black", fill="lightblue") +
         ylab("Case Count") + 
         xlab("Date") +
        ggtitle("Other Pathogens")+
         facet_wrap(~Pathogen)+
         scale_x_date(date_breaks = "1 week", date_labels = "%m/%d")+
         scale_y_continuous(breaks = integer_breaks)+
         theme(text = element_text(size = 12),
               plot.title = element_text(size = 20, face = "bold"),
               panel.background = element_blank(),
               axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
               strip.background =element_rect(fill="black"),
               strip.text = element_text(colour = 'white'),
               panel.border = element_rect(colour = "black", fill=NA, size=3),
               axis.title.x = element_blank(),
               axis.line = element_line(size = 1, colour = "black"))
############################################################################
covid_barplot <- ggplot(covid, aes(x=test_date, y=count)) + 
  geom_bar(stat="identity", color="black", fill="lightblue") +
  ylab("Case Count") + 
  xlab("Date") +
  ggtitle("COVID-19")+
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d")+
  scale_y_continuous(breaks = seq(0, max(covid$count), by = 4))+
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold"),
        panel.background = element_blank(),
        strip.background =element_rect(fill="black"),
        strip.text = element_text(colour = 'white'),
        panel.border = element_rect(colour = "black", fill=NA, size=3),
        axis.title.x = element_blank(),
        axis.line = element_line(size = 1, colour = "black"))
##############################################################################################
## COMPILE 1ST PAGE PLOTS
pg1_plots <- ggarrange(non_covid_line_plot, summary_table,text.p,
                        ncol = 1, nrow = 3,
                        heights = c(1, 0.8,0.2))

fn_report<- paste("Summary_Report_",location_name,"_",sys_date, ".pdf", sep = "")
fn_report <- gsub(" ", "_", fn_report)
fn_report <- gsub("/", "_", fn_report)



ggexport(pg1_plots,non_covid_barplot,filename = fn_report)


ggexport(pg1_plots,covid_line_plot,covid_barplot,non_covid_barplot,filename = fn_report)







