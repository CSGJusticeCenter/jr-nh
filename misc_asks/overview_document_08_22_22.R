# load packages
library(dplyr)
library(ggplot2)
library(showtext)

# In Cheshire County, 84 percent of people in jail who received a mental health assessment in 2019 met the criteria for alcohol
# and/or drug use disorder or dependence,
# and 64 percent had co-occurring mental illness and substance use disorders
pcs <- 590
nonpcs <- 1687-590
df <- as.data.frame(c(pcs, nonpcs))
df$type <- c("Protective\nCustody\nHolds", "Other Bookings")
df <- df %>% select(type, value = 'c(pcs, nonpcs)')

df <- df %>%
  arrange(desc(type)) %>%
  mutate(prop = value / sum(df$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(df, aes(x="", y=prop, fill=type)) +
  geom_bar(stat="identity", width=10, size = 3, color = "white") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label =  paste(type, "\n", round(prop,0), "%", sep = "")),
            color = "black", size=6, nudge_x = 0.2, nudge_y = 1.5) +
  scale_fill_brewer(palette="Set1") + scale_fill_manual(values=c("#d3d3d3", "#E69F00"))

# And an analysis conducted by the Sullivan County Department of Corrections found a 76 percent
# overlap between people in the county jail and people served by the community mental health center
# venn diagram made in canva
# https://www.canva.com/design/DAFKFEZpvw0/iIRoEM8o8GLV1wWHjv01JA/edit?utm_content=DAFKFEZpvw0&utm_campaign=designshare&utm_medium=link2&utm_source=sharebutton

# bar plot
# Between 2010 and 2019, the capacity of the New Hampshire Hospital, the state-operated psychiatric facility, decreased from 252 beds to 158
beds2010 <- 252
beds2019 <- 158
df <- as.data.frame(c(beds2010, beds2019))
df$year <- c("2010", "2019")
df <- df %>% select(year, value = 'c(beds2010, beds2019)')

ggplot(data=df, aes(x=year, y=value)) +
  geom_bar(stat="identity", fill="steelblue", width = .75)+
  theme_void()

# In Cheshire County, 84 percent of people in jail who received a mental health assessment in 2019
# met the criteria for alcohol and/or drug use disorder or dependence, and 64 percent had co-occurring mental illness and substance use disorders
# install.packages("devtools")
# devtools::install_github("joelkuiper/personograph")

data <- list(first=0.84, second=0.16)
personograph(data, colors=list(first="#E69F00", second="#d3d3d3"), n.icons=100)


data <- list(first=0.64, second=0.36)
personograph(data, colors=list(first="#8ECAFC", second="#d3d3d3"), n.icons=100)
