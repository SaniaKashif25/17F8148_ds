library(tidyverse)
library(janitor)
library(glue)
library(skimr)
library(corrplot)
library(GGally)
library(caret)
library(randomForest)
library(scales)
library(RColorBrewer)

# Read data
WaterPotability <- read_csv("/Users/hp/Desktop/Fall 2021/Data Science 7C/Assignment Sol/17F8148_Assessment-II_Solution_7C/Water_Quality_Analysis/water_potability.csv") 
# Clean names and Convert the target Var to factor
WaterPotability <- WaterPotability %>% 
  mutate(Potability = as.factor(Potability)) %>%
  clean_names() %>% glimpse()
# Summary
summary(WaterPotability)

# Missing data
WaterPotability %>%
  summarise_all(~ sum(is.na(.)))

#### Missing data rate

WaterPotability %>%  skim() %>%
  filter(n_missing != 0) %>%
  as_tibble() %>%
  select(skim_variable, n_missing, complete_rate) %>%
  mutate(missing_rate = round(abs(complete_rate - 1) * 100, 1)) %>%
  ggplot(aes(
    x = fct_reorder(skim_variable, n_missing),
    y = missing_rate,
    fill = skim_variable,
    label = paste0(missing_rate, "%")
  )) +
  geom_col(width = .6) +
  geom_text(
    size = 4.5,
    hjust = 1.2,
    vjust = .25,
    col = "white"
  ) +
  coord_flip() + theme(aspect.ratio = .4) +
  theme(
    legend.position = "none"
  ) +
  scale_y_continuous(label = label_percent(scale = 1)) +
  scale_fill_manual(values = c("#25C5DA",
                               "#25A69A",
                               "#66BA6A")) +
  labs(
    title = "Missing Data rate",
    subtitle = "Plot, Missing Data distribution",
    caption = "Data source: Kaggle.com, Water Quality",
    x = NULL,
    y = NULL
  )

# Missing data rate VS Target Variable

WaterPotability %>% group_by(potability) %>%  skim() %>%
  filter(n_missing != 0) %>%
  as_tibble() %>%
  select(skim_variable, n_missing, complete_rate, potability) %>%
  mutate(missing_rate = round(abs(complete_rate - 1) * 100, 1)) %>%
  ggplot(aes(
    x = fct_reorder(skim_variable, n_missing),
    y = missing_rate,
    fill = skim_variable,
    label = paste0(missing_rate, "%")
  )) +
  geom_col(width = .6) +
  geom_text(
    size = 4,
    hjust = 1.2,
    vjust = 0.25,
    col = "white"
  ) +
  coord_flip() +
  facet_wrap(vars(potability)) +
  theme(aspect.ratio = .7) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill="#94246D"),
    strip.text = element_text(color = "white", face = "bold", size = 12)
  ) +
  scale_y_continuous(label = label_percent(scale = 1)) +
  scale_fill_manual(values = c("#D41C64",
                               "#4B6FB5",
                               "#6C3996")) +
  labs(
    title = "Missing rate VS Target Variable",
    subtitle = "Plot, Missing Data distribution VS Target Variable",
    caption = "Data source: Kaggle.com, Water Quality",
    x = NULL,
    y = NULL
  )

# Replace Missing data with mean

WaterPotability <- WaterPotability %>% 
  group_by(potability) %>%
  mutate(across(where(is.numeric), ~if_else(is.na(.), mean(., na.rm = T), as.numeric(.)))) %>% ungroup()

### Target Variable (potability)

WaterPotability %>%
  select(potability) %>%
  count(potability) %>% mutate(percent = paste0(round(n / sum(n) * 100), "%"), 2) %>%
  ggplot(aes(
    x = potability,
    y = n,
    label = percent,
    fill = potability
  )) +
  geom_col() +
  geom_text(vjust = -0.2, color = "#7C4EA8") +
  scale_fill_manual(values = c("#EF1A25", "#0099D5")) +
  labs(
    title = "Potability distribution",
    subtitle = "Plot, Column Plot, Potability distribution",
    caption = "Data source: Kaggle.com, Water Quality",
    x = NULL,
    y = NULL,
    fill = NULL
  )

#### Box Plot and Jitter

WaterPotability %>%
  pivot_longer(cols = -potability, names_to = "feature") %>%
  ggplot(aes(x = feature, y = value)) +
  geom_jitter(aes(y = value, col = potability), alpha = 0.1) +
  geom_boxplot(aes(fill = potability)) +
  facet_wrap(vars(feature), ncol = 3, scales = "free") +
  scale_color_manual(values = c("#E4652E", "#0E8A41")) +
  scale_fill_manual(values = c("#E4652E", "#0E8A41")) +
  theme(
    legend.position = "right",
    strip.background = element_rect(fill = "#0B2D5B"),
    strip.text = element_text(color = "white", face = "bold", size = 8)
  ) +
  labs(
    title = "Detect Outliers With Boxplot",
    subtitle = "Plot, Box and Jitter Plot",
    caption = "Data source: Kaggle.com, Water Quality",
    x = NULL,
    y = NULL,
    fill = NULL,
    color = NULL
  )

#### Histogram
WaterPotability %>%
  pivot_longer(cols = -potability, names_to = "feature") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, aes(fill = feature)) +
  facet_wrap(vars(feature, potability), ncol = 4, scales = "free") +
  scale_fill_brewer(palette = "Paired") +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "#1F5736"),
    strip.text = element_text(color = "white", face = "bold", size = 8)
  ) +
  labs(
    title = "Detect Outliers With Histogram",
    subtitle = "Plot, Histogram",
    caption = "Data source: Kaggle.com, Water Quality",
    x = NULL,
    y = NULL
  )

# Correlation
# Corrplot

corrplot(
  cor(WaterPotability[, -10]),
  type = "lower",
  method = "circle",
  number.cex = .9,
  order = "alphabet",
  tl.col = "#00796B",
  tl.srt = 25,
  col = brewer.pal(n = 9, name = "Purples"),
  title  = "\nCorolation Plot of Water Potability Data "
)

# ggpairs

ggpairs(
  WaterPotability,
  aes(color = potability),
  columns = 1:9,
  lower = list(continuous = wrap(
    "smooth",
    alpha = 0.2,
    size = 0.5,
    color = "#DE942E"
  )),
  diag = list(continuous = "barDiag"),
  upper = list(continuous = wrap("cor", size = 4))
) +
  scale_color_manual(values = c("#1F5736", "#E94046")) +
  scale_fill_manual(values = c("#1F5736", "#E94046")) +
  theme(
    axis.text = element_text(size = 8),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "white"),
    strip.background.x = element_rect(colour = "black"),
    strip.background.y = element_rect(colour = "black"),
    strip.text = element_text(color = "black", face = "bold", size = 8)
  ) +
  labs(
    title = "Pair plot by Potability Var",
    subtitle = "Pair Plot, scatter plot, Histogram and Correlation coefficient",
    caption = "Data source: Kaggle.com, Water Quality",
    x = NULL,
    y = NULL
  )
# Caret Random Forest Model

#### Data Split, 80 % Traning Data

set.seed(31967)

TrainIndex <-
  createDataPartition(WaterPotability$potability, p = 0.8, list = FALSE)

TrainingSet <- WaterPotability[TrainIndex, ]
TestSet <- WaterPotability[-TrainIndex, ]

summary(TrainingSet)

summary(TestSet)

#### Best mtry




xdf <- TrainingSet %>% select(-potability)
ydf <- TrainingSet %>% select(potability)

set.seed(31967)

BestMtry <-
  tuneRF(
    xdf,
    ydf$potability,
    stepFactor = 1.5,
    improve = 1e-6,
    ntree = 1000, 
    plot = F
  )



BestMtry %>% as_tibble() %>% 
  ggplot(aes(x = mtry, y = OOBError)) +
  geom_line(col = "steelblue", size = 1.5)+
  geom_point(col = "orange", size = 3)+
  labs(
    title = "Best Mtry",
    caption = "Data source: Kaggle.com, Water Quality",
    x = "Mtry",
    y = "OOB Error"
  )


#### Train Random Forest Model


set.seed(31967)
control <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 7)

set.seed(31967)
RfFinal <- train(
  potability ~ .,
  data = TrainingSet,
  method = "rf",
  metric = "Accuracy",
  tuneGrid = expand.grid(.mtry = 4),
  trControl = control,
  ntree = 1000
)


plot(RfFinal$finalModel)

#### Important variables
VarsImp <- varImp(RfFinal, scale = FALSE)

VarsImp$importance %>% 
  rownames_to_column(var = "Variable") %>% 
  as_tibble() %>% 
  ggplot(aes(x = fct_reorder(Variable,Overall), y = Overall)) +
  geom_col(fill = "#1F5736", size = 1) +
  coord_flip()+
  labs(
    title = "Variables importance",
    subtitle = "Column Plot, Variables importance, RfFinal mpdel",
    caption = "Data source: Kaggle.com, Water Quality",
    x = NULL,
    y = NULL
  )

#### Prediction

set.seed(31967)

PredRf <- predict(RfFinal,TestSet, type = "raw")


confusionMatrix(data = PredRf, reference = TestSet$potability, positive = "1")












library(tidyverse)
library(janitor)
library(glue)
library(skimr)
library(corrplot)
library(GGally)
library(caret)
library(randomForest)
library(scales)
library(RColorBrewer)

# Read data
WaterPotability <- read_csv("/Users/hp/Desktop/Fall 2021/Data Science 7C/Assignment Sol/17F8148_Assessment-II_Solution_7C/Water_Quality_Analysis/water_potability.csv") 
# Clean names and Convert the target Var to factor
WaterPotability <- WaterPotability %>% 
  mutate(Potability = as.factor(Potability)) %>%
  clean_names() %>% glimpse()
# Summary
summary(WaterPotability)

# Missing data
WaterPotability %>%
  summarise_all(~ sum(is.na(.)))

#### Missing data rate

WaterPotability %>%  skim() %>%
  filter(n_missing != 0) %>%
  as_tibble() %>%
  select(skim_variable, n_missing, complete_rate) %>%
  mutate(missing_rate = round(abs(complete_rate - 1) * 100, 1)) %>%
  ggplot(aes(
    x = fct_reorder(skim_variable, n_missing),
    y = missing_rate,
    fill = skim_variable,
    label = paste0(missing_rate, "%")
  )) +
  geom_col(width = .6) +
  geom_text(
    size = 4.5,
    hjust = 1.2,
    vjust = .25,
    col = "white"
  ) +
  coord_flip() + theme(aspect.ratio = .4) +
  theme(
    legend.position = "none"
  ) +
  scale_y_continuous(label = label_percent(scale = 1)) +
  scale_fill_manual(values = c("#25C5DA",
                               "#25A69A",
                               "#66BA6A")) +
  labs(
    title = "Missing Data rate",
    subtitle = "Plot, Missing Data distribution",
    caption = "Data source: Kaggle.com, Water Quality",
    x = NULL,
    y = NULL
  )

# Missing data rate VS Target Variable

WaterPotability %>% group_by(potability) %>%  skim() %>%
  filter(n_missing != 0) %>%
  as_tibble() %>%
  select(skim_variable, n_missing, complete_rate, potability) %>%
  mutate(missing_rate = round(abs(complete_rate - 1) * 100, 1)) %>%
  ggplot(aes(
    x = fct_reorder(skim_variable, n_missing),
    y = missing_rate,
    fill = skim_variable,
    label = paste0(missing_rate, "%")
  )) +
  geom_col(width = .6) +
  geom_text(
    size = 4,
    hjust = 1.2,
    vjust = 0.25,
    col = "white"
  ) +
  coord_flip() +
  facet_wrap(vars(potability)) +
  theme(aspect.ratio = .7) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill="#94246D"),
    strip.text = element_text(color = "white", face = "bold", size = 12)
  ) +
  scale_y_continuous(label = label_percent(scale = 1)) +
  scale_fill_manual(values = c("#D41C64",
                               "#4B6FB5",
                               "#6C3996")) +
  labs(
    title = "Missing rate VS Target Variable",
    subtitle = "Plot, Missing Data distribution VS Target Variable",
    caption = "Data source: Kaggle.com, Water Quality",
    x = NULL,
    y = NULL
  )

# Replace Missing data with mean

WaterPotability <- WaterPotability %>% 
  group_by(potability) %>%
  mutate(across(where(is.numeric), ~if_else(is.na(.), mean(., na.rm = T), as.numeric(.)))) %>% ungroup()

### Target Variable (potability)

WaterPotability %>%
  select(potability) %>%
  count(potability) %>% mutate(percent = paste0(round(n / sum(n) * 100), "%"), 2) %>%
  ggplot(aes(
    x = potability,
    y = n,
    label = percent,
    fill = potability
  )) +
  geom_col() +
  geom_text(vjust = -0.2, color = "#7C4EA8") +
  scale_fill_manual(values = c("#EF1A25", "#0099D5")) +
  labs(
    title = "Potability distribution",
    subtitle = "Plot, Column Plot, Potability distribution",
    caption = "Data source: Kaggle.com, Water Quality",
    x = NULL,
    y = NULL,
    fill = NULL
  )

#### Box Plot and Jitter

WaterPotability %>%
  pivot_longer(cols = -potability, names_to = "feature") %>%
  ggplot(aes(x = feature, y = value)) +
  geom_jitter(aes(y = value, col = potability), alpha = 0.1) +
  geom_boxplot(aes(fill = potability)) +
  facet_wrap(vars(feature), ncol = 3, scales = "free") +
  scale_color_manual(values = c("#E4652E", "#0E8A41")) +
  scale_fill_manual(values = c("#E4652E", "#0E8A41")) +
  theme(
    legend.position = "right",
    strip.background = element_rect(fill = "#0B2D5B"),
    strip.text = element_text(color = "white", face = "bold", size = 8)
  ) +
  labs(
    title = "Detect Outliers With Boxplot",
    subtitle = "Plot, Box and Jitter Plot",
    caption = "Data source: Kaggle.com, Water Quality",
    x = NULL,
    y = NULL,
    fill = NULL,
    color = NULL
  )

#### Histogram
WaterPotability %>%
  pivot_longer(cols = -potability, names_to = "feature") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, aes(fill = feature)) +
  facet_wrap(vars(feature, potability), ncol = 4, scales = "free") +
  scale_fill_brewer(palette = "Paired") +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "#1F5736"),
    strip.text = element_text(color = "white", face = "bold", size = 8)
  ) +
  labs(
    title = "Detect Outliers With Histogram",
    subtitle = "Plot, Histogram",
    caption = "Data source: Kaggle.com, Water Quality",
    x = NULL,
    y = NULL
  )

# Correlation
# Corrplot

corrplot(
  cor(WaterPotability[, -10]),
  type = "lower",
  method = "circle",
  number.cex = .9,
  order = "alphabet",
  tl.col = "#00796B",
  tl.srt = 25,
  col = brewer.pal(n = 9, name = "Purples"),
  title  = "\nCorolation Plot of Water Potability Data "
)

# ggpairs

ggpairs(
  WaterPotability,
  aes(color = potability),
  columns = 1:9,
  lower = list(continuous = wrap(
    "smooth",
    alpha = 0.2,
    size = 0.5,
    color = "#DE942E"
  )),
  diag = list(continuous = "barDiag"),
  upper = list(continuous = wrap("cor", size = 4))
) +
  scale_color_manual(values = c("#1F5736", "#E94046")) +
  scale_fill_manual(values = c("#1F5736", "#E94046")) +
  theme(
    axis.text = element_text(size = 8),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "white"),
    strip.background.x = element_rect(colour = "black"),
    strip.background.y = element_rect(colour = "black"),
    strip.text = element_text(color = "black", face = "bold", size = 8)
  ) +
  labs(
    title = "Pair plot by Potability Var",
    subtitle = "Pair Plot, scatter plot, Histogram and Correlation coefficient",
    caption = "Data source: Kaggle.com, Water Quality",
    x = NULL,
    y = NULL
  )
# Caret Random Forest Model

#### Data Split, 80 % Traning Data

set.seed(31967)

TrainIndex <-
  createDataPartition(WaterPotability$potability, p = 0.8, list = FALSE)

TrainingSet <- WaterPotability[TrainIndex, ]
TestSet <- WaterPotability[-TrainIndex, ]

summary(TrainingSet)

summary(TestSet)

#### Best mtry




xdf <- TrainingSet %>% select(-potability)
ydf <- TrainingSet %>% select(potability)

set.seed(31967)

BestMtry <-
  tuneRF(
    xdf,
    ydf$potability,
    stepFactor = 1.5,
    improve = 1e-6,
    ntree = 1000, 
    plot = F
  )



BestMtry %>% as_tibble() %>% 
  ggplot(aes(x = mtry, y = OOBError)) +
  geom_line(col = "steelblue", size = 1.5)+
  geom_point(col = "orange", size = 3)+
  labs(
    title = "Best Mtry",
    caption = "Data source: Kaggle.com, Water Quality",
    x = "Mtry",
    y = "OOB Error"
  )


#### Train Random Forest Model


set.seed(31967)
control <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 7)

set.seed(31967)
RfFinal <- train(
  potability ~ .,
  data = TrainingSet,
  method = "rf",
  metric = "Accuracy",
  tuneGrid = expand.grid(.mtry = 4),
  trControl = control,
  ntree = 1000
)


plot(RfFinal$finalModel)

#### Important variables
VarsImp <- varImp(RfFinal, scale = FALSE)

VarsImp$importance %>% 
  rownames_to_column(var = "Variable") %>% 
  as_tibble() %>% 
  ggplot(aes(x = fct_reorder(Variable,Overall), y = Overall)) +
  geom_col(fill = "#1F5736", size = 1) +
  coord_flip()+
  labs(
    title = "Variables importance",
    subtitle = "Column Plot, Variables importance, RfFinal mpdel",
    caption = "Data source: Kaggle.com, Water Quality",
    x = NULL,
    y = NULL
  )

#### Prediction

set.seed(31967)

PredRf <- predict(RfFinal,TestSet, type = "raw")


confusionMatrix(data = PredRf, reference = TestSet$potability, positive = "1")












