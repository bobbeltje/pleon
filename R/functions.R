
# https://github.com/coolbutuseless/hershey

`%>%` <- magrittr::`%>%`

dat <- data.table::fread('data/data.csv')
dat$stroke <- as.character(dat$stroke)  # to suppress warnings later on
dat[, c('x', 'y') := .(x /22, y/32)]

ax <- list(title="", zeroline=FALSE, showline=FALSE, showticklabels=FALSE, showgrid=FALSE)
ax <- list(title='', zeroline=F)
xaxis <- yaxis <- ax
xaxis$range <- c(0, 15)
yaxis$range <- c(-5.5, -0.5)

make_line <- function(chars, line){
  l <- lapply(chars, function(chr) dat[font == 'cursive' & char == chr])
  for (i in seq_along(l)){
    l[[i]][, 'x' := x + i]
    l[[i]][, 'y' := y - line]
    l[[i]][, 'stroke' := paste(stroke, i, line)]
  }
  data.table::rbindlist(l)
}

pleon <- function(...){
  mc <- match.call(expand.dots = F)
  x <- unlist(mc$...)
  char_list <- strsplit(x, '')
  l <- mapply(make_line, char_list, seq_along(char_list), SIMPLIFY = F)
  d <- data.table::rbindlist(l)

  plotly::plot_ly(d, x=~x, y=~y, type='scatter', mode='lines', color=~stroke) %>%
    plotly::layout(showlegend=F, xaxis=xaxis, yaxis=yaxis, margin=list(t=0, b=0, r=0, l=0)) %>%
    plotly::config(displayModeBar=F)
}

