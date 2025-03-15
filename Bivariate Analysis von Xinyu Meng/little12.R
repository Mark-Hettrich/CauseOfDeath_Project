# 加载必要的库
library(ggplot2)
library(dplyr)

# 读取数据
df <- read.csv("Cleaned_Data.csv", stringsAsFactors = FALSE)

# 确保 Crude Rate 为数值类型
df$Crude.Rate <- as.numeric(df$Crude.Rate)

# 选择出现最多的前10种疾病
top_causes <- df %>%
  count(Cause.of.death, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(Cause.of.death)

# 过滤数据，只保留前10种疾病
df_filtered <- df %>% filter(Cause.of.death %in% top_causes)

# 绘制箱线图
ggplot(df_filtered, aes(x = reorder(Cause.of.death, Crude.Rate, median), y = Crude.Rate)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  coord_flip() +  # 旋转坐标轴，横向显示
  labs(title = "死亡率 vs. 疾病类别（前10种疾病）",
       x = "疾病类别 (Cause of Death)",
       y = "死亡率 (Crude Rate)") +
  theme_minimal()
