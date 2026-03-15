library(shiny)
library(tidyverse)
library(lubridate) 
library(janitor)  
library(skimr)
library(scales)
library(forcats)

# 2. 读取数据
df <- readRDS("df.rds")
transactions <- readRDS("transactions.rds")
tx_summary <- readRDS("tx_summary.rds")

# 3. UI 界面部分 (负责网页长什么样)
# ==========================================
ui <- navbarPage(
  title = "客户与交易数据探索(EDA)大屏", # 网页标题
  
  # 第一个标签页：客户画像
  tabPanel("客户画像分析",
           fluidPage(
             h2("核心客户群特征"),
             hr(),
             fluidRow(
               column(6, plotOutput("age_plot")),      # 左边放年龄图
               column(6, plotOutput("income_plot"))    # 右边放收入图
             )
           )
  ),
  
  # 第二个标签页：业务指标
  tabPanel("满意度与流失风险",
           fluidPage(
             h2("流失率核心驱动因素"),
             hr(),
             fluidRow(
               column(6, plotOutput("churn_scatter")), # 左边散点图
               column(6, plotOutput("churn_box"))      # 右边箱线图
             )
           )
  ),
  
  # 第三个标签页：交易时序
  tabPanel("交易趋势监控",
           fluidPage(
             h2("平台交易活跃度"),
             hr(),
             fluidRow(
               column(12, plotOutput("monthly_trend")) # 占据整行的折线图
             )
           )
  )
)

# ==========================================
# 4. Server 逻辑部分
# ==========================================
server <- function(input, output, session) {
  
  # 渲染：年龄分布图
  output$age_plot <- renderPlot({
    ggplot(df, aes(x = age)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#4C72B0", color = "white", alpha = 0.8) +
      geom_density(color = "#C44E52", size = 1) +
      labs(title = "客户年龄分布特征", x = "年龄", y = "密度") +
      theme_minimal(base_size = 14)
  })
  
  # 渲染：收入与教育交叉图
  output$income_plot <- renderPlot({
    ggplot(df, aes(x = education_level, fill = income_bracket)) +
      geom_bar(position = "fill", color = "black", alpha = 0.8) +
      scale_y_continuous(labels = scales::percent_format()) + 
      scale_fill_brewer(palette = "Blues") + 
      labs(title = "不同教育水平的收入占比", x = "教育水平", y = "比例", fill = "收入阶层") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # 渲染：满意度与流失风险散点图
  output$churn_scatter <- renderPlot({
    ggplot(df, aes(x = satisfaction_score, y = churn_probability)) +
      geom_point(alpha = 0.4, color = "#55A868") +
      geom_smooth(method = "lm", color = "red", se = TRUE) + 
      labs(title = "满意度 vs 流失风险", x = "满意度评分", y = "流失概率") +
      theme_minimal(base_size = 14)
  })
  
  # 渲染：客户群组流失箱线图
  output$churn_box <- renderPlot({
    ggplot(df, aes(x = customer_segment, y = churn_probability, fill = customer_segment)) +
      geom_boxplot(alpha = 0.7) +
      labs(title = "各客户层级的流失风险对比", x = "客户层级", y = "流失概率") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  # 渲染：月度趋势折线图
  output$monthly_trend <- renderPlot({
    # 先在内部进行数据按月汇总
    monthly_trend_data <- transactions %>%
      mutate(
        amount = as.numeric(amount),
        month = floor_date(date, "month")
      ) %>% 
      group_by(month) %>%
      summarise(total_amount = sum(amount, na.rm = TRUE))
    
    # 画图
    ggplot(monthly_trend_data, aes(x = month, y = total_amount)) +
      geom_line(color = "#2b8cbe", size = 1.2) +
      geom_point(color = "#045a8d", size = 3) +
      scale_y_continuous(labels = scales::comma_format()) + 
      labs(title = "平台月度总交易金额走势", x = "时间 (月份)", y = "总交易金额") +
      theme_minimal(base_size = 14)
  })
}

# ==========================================
# 5. 启动 App
# ==========================================
shinyApp(ui = ui, server = server)