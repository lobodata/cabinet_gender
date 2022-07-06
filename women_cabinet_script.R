library(tidyverse)
library(data.table)
library(mnis)

#Get the list of all government roles from 1950.
all_government_roles <- pdpr::fetch_mps_government_roles(from_date = "1950-06-10", to_date = "2022-07-05", while_mp = TRUE)

all_both_houses <- mnis_all_members(
  house = "all",
  party = NULL,
  tidy = TRUE,
  tidy_style = "snake_case"
)

cabinet_roles <- all_government_roles %>%
  filter(grepl("Home Secretary",position_name) |
           grepl("Secretary of State",position_name) |
           grepl("Chancellor of the Exchequer", position_name) |
           grepl("Foreign Secretary", position_name) |
           grepl("Attorney General", position_name) |
           grepl("Prime Minister, First Lord of the Treasury", position_name)
  ) %>%
  filter(!grepl("Attorney General's Office",position_name))

genders <- all_both_houses %>% select(member_id,house_start_date,gender) %>% rename(mnis_id = member_id)
cabinet_roles <- left_join(cabinet_roles,genders, by = c("mnis_id"))

mps <- all_both_houses %>% filter(house_membership != "NULL") 

g1 <- mps %>% filter(grepl("c\\(", house_membership)) %>% filter(grepl("XML",house_membership)) %>% unnest(cols = c(house_membership)) %>%
  mutate(end_date = ifelse(grepl("list", as.character(EndDate)),NA,as.character(EndDate)),
         end_date = as.Date(end_date, format = "%Y-%m-%d"),
         start_date = as.Date(StartDate,format = "%Y-%m-%d")) %>%
  select(House,start_date,end_date,display_as,gender)

g2 <- mps %>% filter(grepl("c\\(", house_membership)) %>% filter(!grepl("XML",house_membership)) %>% unnest(cols = c(house_membership)) %>%
  mutate(end_date = ifelse(grepl("list", as.character(EndDate)),NA,as.character(EndDate)),
         end_date = as.Date(end_date, format = "%Y-%m-%d"),
         start_date = as.Date(StartDate,format = "%Y-%m-%d")) %>%
  select(House,start_date,end_date,display_as,gender)

g3 <- mps %>% filter(!grepl("c\\(", house_membership)) %>% unnest_wider(house_membership) %>%
  mutate(end_date = ifelse(grepl("list", as.character(EndDate)),NA,as.character(EndDate)),
         end_date = as.Date(end_date, format = "%Y-%m-%d"),
         start_date = as.Date(StartDate,format = "%Y-%m-%d")) %>%
  select(House,start_date,end_date,display_as,gender)

mps <- bind_rows(g1,g2,g3) %>%
  mutate(end_date = fifelse(is.na(end_date ),as.Date("2050-12-30", format = "%Y-%m-%d"),end_date )) 


dates <- data.frame(dates = seq(from = as.Date("1965-01-31"), to = as.Date("2022-07-05"), by = "months")) %>% mutate(dates = as.Date(dates, format = "%Y-%m-%d"))

gender_final <- data.frame()
house_gender <- data.frame()
experience_final <- data.frame()
numbers_final <- data.frame()

for (i in as.list(dates$dates)) {
  
  print(i)
  
  temp <- cabinet_roles %>%
    group_by(position_name) %>%
    arrange(government_incumbency_start_date) %>%
    mutate(lagged_start = lead(government_incumbency_start_date),
           adjusted_end = fifelse(is.na(government_incumbency_end_date) & !is.na(lagged_start), lagged_start,government_incumbency_end_date), #This just means only one person can hold the position at any one time. There are some dupes, e.g. "Minister of State (Department for Business, Energy and Industrial Strategy)"
           adjusted_end = fifelse(is.na(adjusted_end),as.Date("2050-12-30", format = "%Y-%m-%d"),adjusted_end)) %>%
    filter(government_incumbency_start_date < i) %>%
    filter(adjusted_end > i) %>%
    distinct(display_name, .keep_all = T) #just in case there are dupes for whatever reason.
  
  
  gender_split <- temp %>% ungroup() %>% group_by(gender) %>% tally() %>% mutate(date = i)
  
  experience_split <- temp %>% ungroup() %>% mutate(experience = as.Date(i,format = "%Y-%m-%d") - as.Date(house_start_date,format = "%Y-%m-%d"),
                                                    experience = as.numeric(experience / 365.25,units = "days")) %>% 
    
    summarise(average_experience = mean(experience)) %>% mutate(date = i)
  
  temp_mps <- mps %>% filter(House == "Commons") %>% 
    filter(start_date < i) %>%
    filter(end_date > i) 
  
  temp_mps_gender <- temp_mps %>%
    group_by(gender) %>%
    tally() %>%
    mutate(date = i)
  
  number_mps <- nrow(temp_mps)
  number_cabinet <- nrow(temp)
  numbers <- data.frame(mps = c(number_mps), cabinet = c(number_cabinet), date = c(i))
  
  gender_final <- bind_rows(gender_final,gender_split)
  house_gender <- bind_rows(house_gender, temp_mps_gender)
  experience_final <- bind_rows(experience_final,experience_split)
  numbers_final <- bind_rows(numbers_final,numbers)
  
  
}

numbers_final <- numbers_final %>%
  filter(date != "1974-03-03") %>% #Wierd number of cabinet positions
  filter(date != "1997-05-01") %>% #Wierd number of cabinet positions
  filter(date != "2019-12-01") %>% #Wierd number of MPs
  filter(date != "2017-05-31") %>% #Wierd number of MPs
  filter(date != "2015-03-31") #Wierd number of MPs

ggplot(data = numbers_final, aes(x = date, y = cabinet)) + geom_line() 

gender_final <- gender_final %>% complete(.,date,gender,fill = list(n = 0)) %>%
  ungroup() %>% group_by(date) %>%
  mutate(share =  n / sum(n)) %>%
  filter(date != "1974-03-03") %>% #Wierd number of cabinet positions
  filter(date != "1997-05-01") %>% #Wierd number of cabinet positions
  filter(date != "2019-12-01") %>% #Wierd number of MPs
  filter(date != "2017-05-31") %>% #Wierd number of MPs
  filter(date != "2015-03-31") #Wierd number of MPs


house_gender <- house_gender%>% complete(.,date,gender,fill = list(n = 0)) %>%
  ungroup() %>% group_by(date) %>%
  mutate(share =  n / sum(n)) %>%
  filter(date != "1974-03-03") %>% #Wierd number of cabinet positions
  filter(date != "1997-05-01") %>% #Wierd number of cabinet positions
  filter(date != "2019-12-01") %>% #Wierd number of MPs
  filter(date != "2017-05-31") %>% #Wierd number of MPs
  filter(date != "2015-03-31") #Wierd number of MPs

female <- gender_final %>% filter(gender == "F") 
house_female <- house_gender %>% filter(gender == "F")
total_female <- left_join(female,house_female, by = c("date","gender")) %>% select(-c(n.x,n.y)) %>%
  mutate(amount = share.x - share.y,
         excess = ifelse(amount > 0, amount + share.y,NA),
         deficit = ifelse(amount < 0, amount + share.y, NA)) %>%
  filter(date > "1979-12-31")

ggplot() + geom_line(data = total_female, aes(x = date, y = share.x), colour = "darkgray",size = 0.75) + 
  geom_ribbon(data = total_female, aes(x = date, ymax = share.y, ymin = excess), fill = "green",alpha = 0.05) + 
  geom_ribbon(data = total_female, aes(x = date, ymax = deficit, ymin = share.y), fill = "darkblue",alpha = 0.05) + 
  geom_line(data = total_female, aes(x = date, y = share.y), colour = "black", size = 1) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.5)) +
  labs(title = " Women account for 35% of MPs but only 25% of the cabinet.",
       subtitle = " Black line shows proportion of MPs which are women. \n Grey line shows the proportion of cabinet members that are women.  \n Shared area is green when women account for a greater share of cabinet members than MPs \n",
       caption = " \n  Produced by @DataLobo. Data sourced from UK Parliament's data platform") +
  theme(
    #text = element_text(family = "mulish-2"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text = element_text(size=12),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line( size=.1, color="black" ),
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    plot.title = element_text(hjust = 0,size=12),
    plot.caption = element_text(hjust = 0,size=9),
    plot.subtitle = element_text(hjust = 0,size=9),
    legend.position = "none")