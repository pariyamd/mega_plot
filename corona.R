library(tidyverse)
library(lubridate)
devtools::install_github("an-bui/calecopal")
library(calecopal)
library(colorspace)
library(scales)
library(stringr)
library(ggpubr)
theme_set(theme_pubr())
my_palette <- cal_palette(name = "kelp1")
more_pal <- colorRampPalette(my_palette)(10)

corona<-read.csv("~/Documents/hesaba/coron/corona.csv")
iran_data<-corona%>%filter(Country=="Iran (Islamic Republic of)")


df <- iran_data %>% 
  mutate(weekday = wday(Date_reported, label = T, week_start = 7), # can put week_start = 1 to start week on Monday
         month = month(Date_reported, label = T),
         date = yday(Date_reported),
         week = epiweek(Date_reported))

df$week[df$month=="Dec" & df$week ==1] = 53 

dfPlot <- df %>% 
  group_by(month) %>% 
  mutate(monthweek = 1 + week - min(week))



cal_plt<-dfPlot %>%
  ggplot(aes(weekday,-week, fill = New_cases)) +
  geom_tile(colour = "lightgrey")  + 
  geom_text(aes(label = New_cases), size = 4, color = "white") +
  theme( 
        legend.text = element_text(color="white"),
        legend.title = element_text(color="white"),
        legend.position = "top",
        legend.key.width = unit(3, "cm"),
        legend.background =element_rect(fill="#2a2d30"),
        axis.line = element_line(color=NA),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color="white"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="#2a2d30"),
        legend.title.align = 0.5,
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 15,color="white"),
        panel.border = element_rect(colour = "#2a2d30", fill=NA, size=1),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold",color = "white",
                                  margin = margin(2,0,0.5,0, unit = "cm"))) +
  scale_fill_gradientn(colours = more_pal,
                       values = scales::rescale(c(1, 0.05, 0, -0.05, -1)), 
                       name = "Values",
                       guide = guide_colorbar(title.position = "top", 
                                              direction = "horizontal")) +
  facet_wrap(~month, nrow = 4, ncol = 3, scales = "free") +
  labs(title = "Daily New Cases")+
  theme(plot.background = element_rect(fill = "#2a2d30",colour = "#2a2d30"))
cal_plt

library(waterfalls)

waterfall_data<-dfPlot%>%
  group_by(month)%>%
  summarise(month_cases=sum(New_deaths))%>%
  mutate(diff=month_cases-lag(month_cases))%>%
  select(month,diff)
waterfall_data$diff[waterfall_data$month=="Jan"]=0

w_plt<-waterfall(waterfall_data,
                 put_rect_text_outside_when_value_below=10,
          values = waterfall_data$diff,
          fill_colours=colorRampPalette(my_palette)(10),
          rect_border=NA,
          fill_by_sign = FALSE,
          rect_text_size=1.5,
          draw_axis.x = "none",
          total_rect_text_color = "white")+
  labs(title="Monthly Deaths")+
  theme(plot.background = element_rect(fill = "#2a2d30",colour = "#2a2d30"),
        panel.background = element_rect(fill="#2a2d30",colour = "#2a2d30"),
        axis.text = element_text(color="white"),
        axis.line = element_line(color="darkgrey"),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold",color = "white",
                                  margin = margin(2,0,0.5,0, unit = "cm")))
w_plt
maximum_new_cases<-df%>%ungroup()%>%

    filter(New_cases==max(New_cases))%>%select(Date_reported,New_cases)

mncd<-ggplot(as.data.frame(table(maximum_new_cases)), aes(x=New_cases,y=Date_reported)) +
  geom_tile(fill=more_pal[1])+
  geom_text(aes(x=New_cases,y=Date_reported,
                label=str_c("Most new cases \n in one day \n",New_cases,"\n",Date_reported)),
            size=4,
            colour="white")+
  theme( plot.background = element_rect(fill = "#2a2d30",colour = "#2a2d30"),
        panel.background = element_rect(fill="#2a2d30",colour = "#2a2d30"),
    axis.line=element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    )

maximum_new_deaths<-df%>%ungroup()%>%
  filter(New_deaths==max(New_deaths))%>%select(Date_reported,New_deaths)
mndd<-ggplot(as.data.frame(table(maximum_new_deaths)), aes(x=New_deaths,y=Date_reported)) +
  geom_tile(fill=more_pal[2])+
  geom_text(aes(x=New_deaths,y=Date_reported,
                label=str_c("Most new deaths \n in one day \n",New_deaths,"\n",Date_reported)),
            size=4,
            colour="white")+
  theme(plot.background = element_rect(fill = "#2a2d30",colour = "#2a2d30"),
        panel.background = element_rect(fill="#2a2d30",colour = "#2a2d30"),
    axis.line=element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

cum_cases<-df%>%ungroup()%>%
  filter(Cumulative_cases==last(Cumulative_cases))%>%select(Date_reported,Cumulative_cases)
ccd<-ggplot(as.data.frame(table(cum_cases)), aes(x=Cumulative_cases,y=Date_reported)) +
  geom_tile(fill=more_pal[5])+
  geom_text(aes(x=Cumulative_cases,y=Date_reported,
                label=str_c("Total Cases\nUntil ",Date_reported,"\n",Cumulative_cases)),
            size=4,
            colour="white")+
  theme(plot.background = element_rect(fill = "#2a2d30",colour = "#2a2d30"),
        panel.background = element_rect(fill="#2a2d30",colour = "#2a2d30"),
    axis.line=element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

cum_deaths<-df%>%ungroup()%>%
  filter(Cumulative_deaths==last(Cumulative_deaths))%>%select(Date_reported,Cumulative_deaths)
cdd<-ggplot(as.data.frame(table(cum_deaths)), aes(x=Cumulative_deaths,y=Date_reported)) +
  geom_tile(fill=more_pal[7])+
  geom_text(aes(x=Cumulative_deaths,y=Date_reported,
                label=str_c("Total Deaths\nUntil ",Date_reported,"\n",Cumulative_deaths)),
            size=4,
            colour="white")+
  theme(plot.background = element_rect(fill = "#2a2d30",colour = "#2a2d30"),
        panel.background = element_rect(fill="#2a2d30",colour = "#2a2d30"),
    axis.line=element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


stats<-ggarrange(mncd,mndd,ccd,cdd,nrow = 1, ncol = 4)+bgcolor("#2a2d30")+ border(color="#2a2d30")
stats
lower<-ggarrange(w_plt,stats,nrow = 2, ncol = 1,widths= c(1, 1),heights = c(1,0.3))+bgcolor("green")+ border(color="#2a2d30")
lower
tot<-ggarrange(lower,cal_plt,
          nrow = 1, ncol = 2,widths = c(0.75, 1.25),heights = c(0.25,1))
tot<-tot+bgcolor("#2a2d30")+ border(color="#2a2d30")

plt<-annotate_figure(tot,
                top = text_grob("Covid-19 in Iran",
                                color = "white",
                                face = "bold",
                                size = 30),
                bottom = text_grob("Auhtor:  \n Pariya Mehrbod  ", color = "lightgrey",
                                   hjust = 1, x = 1, face = "italic", size = 15))+bgcolor("#2a2d30")+ border(color="#2a2d30")

plt+theme(plot.margin = margin(c(1, 0, 0, 0), unit="cm"),
          plot.background = element_rect(fill = "#2a2d30"))

