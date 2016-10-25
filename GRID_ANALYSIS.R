#------------------------------------------------------------------------------
# GRID_ANALYSIS
# Here we're going to compare rates of surface temperature increase across
# different parts of North America. To do this, we'll construct a grid
# data frame and group stations that fall within discrete grids into one mean 
# value. Then, a heat map will be constructed to show rates of warming relative
# to the baseline. Different grid-sizes will be used.

#------------------------------------------------------------------------------
require(dplyr)

mround <- function(x,base){ 
  base*round(x/base) 
}

getRate <- function(year, mt){
  df = data.frame(cbind(year,mt)) %>% arrange(year)
  end = ifelse(nrow(filter(df, year> 2000)) > 0,
               mean(filter(df, year > 2000)$mt, na.rm=TRUE),
               df$mt[length(df)])
  start = ifelse(nrow(filter(df, year< 1960)) > 0,
               mean(filter(df, year < 1960)$mt, na.rm=TRUE),
               df$mt[1])
  return((end - start)/(length(df$year)))
}

rd = transform(rd, m_temp =  (VALUE1 + VALUE2 + VALUE3 + VALUE4 + VALUE5 + 
                 VALUE6 + VALUE7 + VALUE8 + VALUE9 + VALUE10 + VALUE11 + VALUE12)/1200)

d2  <-  rd[c('YEAR','m_temp','lat','long')]
box_size = 2
d2 = transform(rd, latbox = mround(lat,box_size), longbox=mround(long,box_size))

d3 = group_by(d2, lat, long) %>% summarise(rate = getRate(YEAR, m_temp))

boxes = group_by(d2, latbox,longbox) %>% summarise(rate =  getRate(YEAR, m_temp))
                                               
m = map_data("world2", c("usa", "Canada"))

ggplot(boxes, aes(x=longbox, y=latbox, fill=rate)) + geom_tile()+
  geom_polygon(data=m,aes(x=long -360, y=lat, group=group), colour="black", fill="white", alpha=0) +
  scale_fill_gradient(low = "royalblue", high = "red") +
  theme_bw() + xlim(-180,-50) +
  ylim(15,85) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Gridded HeatMap by Rate of Warming
Per Year in Degrees Celsius")

ggplot(d3, aes(x=long, y=lat, color=rate)) + geom_point(size=6, alpha=.8)  +
  geom_polygon(data=m,aes(x=long -360, y=lat, group=group), colour="black", fill="white", alpha=0) +
  scale_color_gradient(low = "white",high = "red", guide = "legend") +
  theme_bw() + xlim(-180,-50) + 
  ylim(15,85) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Disc HeatMap by Rate of Warming
Per Year in Degrees Celsius")
