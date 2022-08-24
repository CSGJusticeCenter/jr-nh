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
