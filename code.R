library("rwhatsapp") # حزمة خاصة لقراءة ومعالجة بيانات المحادثات على الواتس
library("ggplot2") # حزمة لتمثيل البيانات 
library("dplyr") # حزمة لمعالجة البيانات
library("forcats") # حزمة للتعامل مع البيانات الغير كمية 
library("lubridate") # حزمة للتعامل مع بيانات الوقت والتاريخ
library("tidyr") # حزمة لمعالجة البيانات 
library("tidytext") # حزمة لتحليل البيانات النصية
library("magrittr") # حزمة لصنع الـ pipes
library("stringr") # حزمة للتعامل مع البيانات النصية
library("stopwords") # حزمة تحويل على قائمة بالمفردات التي لا معنى لها

df <- rwa_read("D:/100 hours of ML/whatsapp analysis/WhatsApp Chat with AOU.txt", verbose = TRUE)

#This step is to annonomize source to protect privacy 
df1 <- df %>% select(-source)
str( df1 %>%
       mutate(
       max.level = 1,
       vec.len=4))

# Feature Engineering 
df1 <- df1 %>% 
  mutate(day = date(time)) %>% 
  mutate(day_num = yday(time)) %>% 
  mutate(weekdays = weekdays(time) ) %>% 
  mutate(hour = hour(time)) %>% 
  mutate(month = month(time, label = TRUE)) %>%
  mutate(year = year(time)) 

## Group Activity 
df1 %>% 
  count(day) %>% 
  ggplot(aes(x = day, y= n)) +
  geom_bar(stat = "identity") +
  ylab("# messages") + xlab("Date") +
  ggtitle("Group Activity since 2019") + theme_classic()

# Summarized data for number of messeges a day
df_summarized <-  df1 %>% 
  filter(year != "NA") %>% 
  mutate( year = as.factor(year)) %>% 
  group_by(year) %>% 
  count(day_num)


g1 <- df_summarized %>% 
  ggplot(aes(x = day_num, y= n,group = year, color = year, alpha =year)) +
  geom_path(stat = "identity") +
  labs(y ="# messages" , x = "# Day of 365",alpha = "Year", color = "Year") +
  scale_alpha_manual(values = c(rep(1, 1),1)) +
  ggtitle("Group Activity since 2019") +
  ggtitle("# Messeges spikes in 2020") +
  theme_classic()

g1
#scale_alpha_manual(values = c(rep(0.2,4), 10)) +

df_usr_summarized <- df1 %>%
  filter(author != "NA") %>% 
  mutate(author = fct_rev(fct_infreq(fct_lump_min(author, min = 20)))) %>% 
  count(author)


df_usr_mean<- df1 %>%
  filter(author != "NA") %>% 
  filter(!str_detect(text, c("media omitted",
                             "image omitted",
                             "audio omitted",
                             "video omitted",
                             "sticker omitted"))) %>% 
  mutate(author = fct_rev(fct_infreq(fct_lump_min(author, min = 20)))) %>% 
  mutate(msg_length = nchar(text)) %>% 
  group_by(author) %>% 
  summarise("avr_msg_lngth" = round(mean(msg_length),0), .groups = "keep") %>%
  right_join(df_usr_summarized,by = "author")


g2 <- df_usr_summarized %>%
  ggplot(aes(x = reorder(author, n), y = n, fill = ifelse(n>mean(n), "amber", "lightblue"))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(mapping = aes(y = n, label = n), size = 2.5, nudge_y = 620) +
  scale_y_discrete(expand = c(0,3000)) +
  scale_x_discrete(expand = c(0,0))+
  coord_flip() + 
  labs(title ="Most Active Members in term of # of Messages",
       subtitle = paste0("Above average is colored in red "),
       x ="", y = "") +
  theme_classic() + 
  theme(axis.text.y = element_text(size = 9),
        axis.line = element_blank(), 
        axis.ticks = element_blank())
g2 +  
  geom_col(data = df_usr_mean,
           mapping = aes(group = author,x = reorder(author,n), y =avr_msg_lngth*60),
           alpha = 0.5,
           fill = "gray",
           show.legend = FALSE) 

df1 %>% 
  unnest(emoji) %>%
  count(emoji, sort = TRUE) %>%
  top_n(n = 14, n) %>% 
  
  ggplot(aes(x = reorder(emoji, n), y = n, fill = emoji)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Most often used emojis", x = "", y ="") +
  scale_y_discrete(expand = c(0,0))+
  theme_classic()+
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_text(size = 15), 
        axis.line = element_blank())

top_5 <- df1 %>%
  mutate(day = date(time)) %>%
  filter(author != "NA") %>% 
  mutate(author = fct_rev(fct_infreq(fct_lump_min(author, min = 20)))) %>% 
  count(author) %>% arrange(desc(n)) %>% top_n(5) %>% select(author) 

df1 %>% filter(author %in% top_5$author) %>% 
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 6, n) %>% 
  
  ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_discrete(limits = cumsum(c(0,rep(500,5))), expand = c(0, 100))+
  coord_flip() + 
  labs(title ="Most often used emojis", x = "", y = "") +
  facet_wrap(~author, ncol = 2, scales = "free_y")+
  theme_classic() +
  theme(axis.text.y = element_text(size = 10), 
        axis.ticks = element_blank(), 
        axis.line.y = element_blank())

to_remove <- c(stopwords(language = "ar", source = "misc"),stopwords(language = "en"), "media", "omitted",  "image",  "sticker",  "video",  "audio", "الله",
               "يا", "بس", "مو","الي", "ها", "أبو", "https", "ويش", "انت", "لو", "يعني", 
               "message", "deleted", "أن","أنا", "شي","هدي","هذي","عليك", "twitter.com", 
               "علشان", "ليش", "زي","لك", "احنا", "يقول", "حق", "ترى", "الحين", "انا", 
               "هل", "طيب","الي", "حد", "status","d8","s","بو", "دا", "وين", "d9", "دي","يبغى", 
               "أحد", "إذا", "نص", "صار", "بالله", "عنده", "أو","فيك", "بعدين", "شوي", "منهو", 
               "ليي", "اللي","أبو", "متا","هاذي","حتا" , "مادري","تبغا","هذولا","يبغا","مب",
               "يبغي","خي","انزين","ويشي","ماادري","ماشاءالله","شاءالله","كده","شدي","هههه",
               "ههه","والا","ياللا","كيدا","واللهي","اليهم","ويه","اله",
               "25d8","25d9")

df1 %>%  
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  count(author, word, sort = TRUE) %>%
  bind_tf_idf(term = word, document = author, n = n) %>%
  group_by(author) %>%
  top_n(n = 6, tf_idf) %>%
  ggplot(aes(x = reorder_within(word,n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  theme_classic() +
  facet_wrap(~author, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Important words using tf–idf by author")
