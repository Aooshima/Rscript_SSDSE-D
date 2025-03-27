library(tidyverse)
library(readxl)
library(patchwork)
library(shiny)
library(shinydashboard)

rm(list = ls())

df <- read_excel("@ssdse-d-dat.xlsx")

# nest_byで種目・性別ごとにネスト
df1 <- df %>% 
  select(category, item, prefecture, gender, rate) %>% 
  nest_by(category, item, gender)

# categnameを定義
categname <- df1$category %>% unique()

# 棒グラフ作成用関数 男女で色分け
bar_func <- function(dat, itm, gend){
  fillcol <- if_else(gend == "男", "skyblue", "pink")
  gg <- dat %>% 
    ggplot(aes(x = fct_reorder(prefecture, rate), y = rate)) +
    geom_bar(stat = "identity", fill = fillcol) + 
    geom_hline(yintercept = 0, color = "darkgrey") +
    labs(title = str_c(gend, "---", itm),
         x = "", y = "") +
    coord_flip() +
    theme_bw() +
    theme(
      plot.title = element_text(size = 16),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16)
    )
  return(gg)
}

# グラフ取り出し用関数
pullfig <- function(categoryname, gend = "男", top_n = 5){
  df2_topn <- df1 %>% 
    filter(category == categoryname, gender == gend) %>% 
    mutate(topn = list(slice_max(data, rate, n = top_n, with_ties = FALSE)))
  
  df3_fig <- df2_topn %>% 
    mutate(
      fig = list(bar_func(dat = topn, itm = item, gend = gender))
    )
  
  outfig <- df3_fig %>% 
    pull(fig) %>% 
    wrap_plots(ncol = 4) +
    plot_annotation(
      title = categoryname,
      subtitle = "x軸：活動した人の割合(％)",
      caption = "x軸：活動した人の割合(％)",
      theme = theme(text = element_text(size = 24)))
  return(outfig)
}

# 動的にグラフの高さを変える仕様を追加
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "SSDSE-D"),
  dashboardSidebar(
    radioButtons("radio", label = h4("カテゴリー"), choices = categname, selected = "スポーツ"),
    hr(),
    radioButtons("gender", label = h4("性別"), choices = c("女", "男"), selected = "男"),
    hr(),
    sliderInput("top_n", label = h4("トップn"), min = 5, max = 47, value = 5, step = 1),
    hr(),
    sliderInput("height", label = h4("グラフ高さ(px)"), min = 300, max = 4000, value = 1500, step = 100)
  ),
  dashboardBody(
    fluidRow(
      column(12, uiOutput("plot_ui"))
    )
  )
)

server <- function(input, output, session){
  output$patchplot <- renderPlot({
    pullfig(categoryname = input$radio, gend = input$gender, top_n = input$top_n)
  })
  
  output$plot_ui <- renderUI({
    plotOutput("patchplot", height = str_c(input$height, "px"))
  })
}

shinyApp(ui = ui, server = server)

