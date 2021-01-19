# Regresi Linear Sederhana

# Panggil Data
donor_data <- read_csv("E:/DATA DRIVEN/2020/LATIHAN R/DonorSampleDataML.csv")
#
glimpse(donor_data)
#
# Melihat hubungan variabel antara AGE dan TotalGiving
with(donor_data, plot(AGE, TotalGiving))
#
# Membuat model
giving_model <- lm(TotalGiving ~AGE,
                   data = donor_data)
abline(giving_model, col = "red")
# Menguji model
summary(giving_model)
################################################################################

# Regresi Linear Berganda

# Panggil Data
donor_data <- read_csv("E:/DATA DRIVEN/2020/LATIHAN R/DonorSampleDataML.csv")
#
glimpse(donor_data)
#
pred_vars <- c('MARITAL_STATUS', 
               'GENDER', 
               'ALUMNUS_IND', 
               'PARENT_IND',
               'HAS_INVOLVEMENT_IND', 
               'DEGREE_LEVEL',
               'PREF_ADDRESS_TYPE', 
               'EMAIL_PRESENT_IND')

donor_data <- select(donor_data,
                     pred_vars,
                     AGE,
                     PrevFYGiving,
                     PrevFY1Giving,
                     PrevFY2Giving,
                     PrevFY3Giving,
                     PrevFY4Giving,
                     CurrFYGiving)
glimpse(donor_data)
#
# convert atribut/feature to factor
donor_data <- mutate_at(donor_data, 
                        .vars = pred_vars,
                        .funs = as.factor)
glimpse(donor_data)
#
# Membuat model Regresi Linear Berganda
giving_mlg_model <- lm(CurrFYGiving ~ .,
                       data = donor_data)
#
# Melihat performa model
summary(giving_mlg_model)

################################################################################

# Regresi Logistik

# Panggil Data
donor_data <- read_csv("E:/DATA DRIVEN/2020/LATIHAN R/DonorSampleDataML.csv")
#
glimpse(donor_data)
#
pred_vars <- c('MARITAL_STATUS', 
               'GENDER', 
               'ALUMNUS_IND', 
               'PARENT_IND',
               'HAS_INVOLVEMENT_IND', 
               'DEGREE_LEVEL',
               'PREF_ADDRESS_TYPE', 
               'EMAIL_PRESENT_IND',
               'DONOR_IND')
head(pred_vars)
donor_data <- select(donor_data,
                     pred_vars,
                     AGE)
#
head(donor_data)
# convert atribut/feature to factor
donor_data <- mutate_at(donor_data, 
                        .vars = pred_vars,
                        .funs = as.factor)
# Bagi 70% : 30% - Data Training:Data Uji
dd_index <- sample(2, 
                   nrow(donor_data), 
                   replace = TRUE, 
                   prob = c(0.7, 0.3))
dd_trainset <- donor_data[dd_index == 1, ]
dd_testset <- donor_data[dd_index == 2, ]
#
dim(dd_trainset)
dim(dd_testset)
#
# Membuat model regresi logistik
giving_lr_model <- glm(DONOR_IND ~ ., 
                       data   = dd_trainset, 
                       family = "binomial")
#
summary(giving_lr_model)
#
# Membuat prediksi
predictions <- predict(giving_lr_model, 
                       newdata = dd_testset, 
                       type    = "response")
preds <- as.factor(ifelse(predictions > 0.5, 
                          "Y", 
                          "N"))
#
install.packages("caret")
library(caret)
head(preds, n=10)
# Melihat matrik konfusi
confusionMatrix(table(preds, dd_testset$DONOR_IND), 
                positive = "Y")
################################################################################