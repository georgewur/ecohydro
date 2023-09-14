library(plotly)

#save(df.results,file = "results.dat")
load(file = "results.dat")

fig2 = df.results %>%
  plot_ly(
    x = ~Location,
    y = ~F0,
    #frame = ~as.Date(as.character(df.results$Date), "%Y%m%d"),
    frame = ~Date,
    type = 'scatter',
    mode = 'lines',
    name = "bare soil"
    #showlegend = F
  )
fig2 = fig2 %>%
  add_lines(
    x = ~Location,
    y = ~F1,
    name = "veg. 1"
  )

fig2 = fig2 %>%
  add_lines(
    x = ~Location,
    y = ~F2,
    name = "veg. 2"
  )

fig2 = fig2 %>%
  add_lines(
    x = ~Location,
    y = ~F3,
    name = "veg. 3"
  )

fig2 = fig2 %>%
  add_lines(
    x = ~Location,
    y = ~F4,
    name = "veg. 4"
  )

fig2 = fig2 %>%
  add_lines(
    x = ~Location,
    y = ~F5,
    name = "veg. 5"
  )


fig2 = fig2 %>%
  add_lines(
    x = ~Location,
    y = ~F6,
    name = "veg. 6"
  )



fig2 = fig2 %>%
  animation_opts(
    frame = 10,
    transition = 10
  )

fig2
