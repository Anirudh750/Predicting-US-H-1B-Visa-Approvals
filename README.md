# Predicting-US-H-1B-Visa-Approvals

**Dataset Description:**

This dataset is taken from Kaggle source where the dataset was prepared by Abishek Anbarasan. It consists of 654361 rows and 52 columns which includes the data related to the total number of Visa Applications submitted for the year 2018. This data includes details about each visa application case which is uniquely identified by a Case Number assigned to each application. In order to understand the decision and duration taken for a decision is specified using Case Submission Date and Case Decision Dates. The status of each application is classified as Certified, Withdrawn and Denied based on factors like Visa Type for which they applied, Employer Country, Wages, Duration of the employment. This case status shall be converted into the binary response variable for further modelling purposes. Some other factors also include whether a particular applicant used an agent in order to file the H1B case. Also, there exist records about whether the applicant has previously applied for any kind of Visa types. The information about an applicant being a Full-Time employee or working on Contract basis is provided. For every job type there exists a SOC\_Code and SOC\_Name which is Standard Occupation Classification, authorized and approved by the Government which explicitly classifies the Job Role and Job Specifications which also play a major role while picking for visas. The wages received by an applicant and the unit of pay details like Annual, biweekly or hourly pay detail also add up to be a significant factor while considering for H-1B status. Few H1B visas are filed as dependents, hence this information also adds up to be a factor for a specific visa type to get picked. Other generic information about the Employer company, its location and contact details, Agent contact details and work location related details. Clearly, as per the objective of the project, the newly modified Case Status column will be the dependent variable for which the models are trained to predict the Status of an Application based on the above-mentioned factors.

**Data Pre-processing:**

Data preprocessing is an essential stage before modelling is performed on the data. After EDA and visualizations, certain correlations between the variables, few important values are identified. As a part of data pre-processing, we performed the Data Filtration, Handling Missing Data, Handling Near Zero Variance, Feature Engineering, One Hot Encoding and Resampling for the chosen dataset. After the dataset is processed, the set of variables are fed to the predictive models for further prediction of the visa status.

- **Data Filtration:** As per the objective of the project, we wanted to predict the Case status for the Visa type H-1B and employer country as the USA. Hence, as a part of data filtration, we filtered the columns whose Employer country is the USA and Visa Type was H-1B. Later, we removed these columns as the needed information from them is extracted. Also, after understanding the importance and significance of each column, we dropped a few columns which contain genetic information like Postal codes, address, Employee business DBA, agent address and contact information. These columns do not contribute towards acting as predicting variables. Certain uninformative columns have been dropped because of the larger number of null values present in them so that other important information can be retained.
- **Handling Missing Data:** One of the major steps while performing data preprocessing is identifying and handling the missing data. After data filtration and removal of unimportant columns, we checked for NA values (Null values) in the dataset. We could see that not many null values are present. Hence, we omitted the null values since they were not creating a huge difference to the output. Below figure depicts the number of missing values the dataset has before and after omitting the NA values.

- **Handling Near Zero Variance:** From the concept of Near-zero variance, there are certain columns present in the dataset, which gives a constant value throughout. For example, they only predict the output belonging to one class throughout. Such columns are considered to be outliers and these uninformative columns should be removed in order to improve the predictive power of the model.Handling near-zero variance variables is important as they could lead to misleading and biased outputs of a model. In our dataset, we have found 4 such predictors TOTAL\_WORKERS, NEW\_CONCURRENT\_EMP, FULL\_TIME\_POSITION, WILLFUL\_VIOLATOR. The below shows the near-zero variance columns for the chosen dataset.

- **Feature Engineering:** As a part of feature engineering, we have done scaling, refactoring, introduced calculated columns, added new columns, formatted the uncleaned data columns, etc. New Case Status and Employment duration are two new columns which are added to the existing dataset. New case status is the response variable which is of binary type, where the Certified values are treated as 1 and the other statuses are treated as 0. Employment duration is the calculated column, which is calculated using the applicant&#39;s employment start and end dates.

The dates present in the dataset used different conventions in different columns and rows, hence for the better understanding of the model, we converted the date into a single format and picked up only the months for each applicant. This data is fed to the model as one of the predictor variables. Wage levels are present for the different duration for different applicants like yearly, bi-weekly, weekly, hourly etc. Hence all these values have been scaled to one wage rate which is wage per annum using the if-Else functions. Columns like Job\_title, Employer name has many levels, hence these columns have to be refactored. We have considered the top 50 levels based on the frequency of the job titles and employer company names. The rest values are categorized as &quot;Others&quot; for better modelling purposes.

- **One Hot Encoding:** It is known that one hot encoding is a process where categorical variables are converted into a form which is given to the machine learning model for better prediction. Here, we chose one hot encoding over the label encoding because in label encoding the assigned numeric value to the label could be treated as order or hierarchy in the data which is not the correct value of the column. Hence in one-hot encoding, the available categorical variables have split into columns based on the labels or unique entities present in the column. These newly formed columns are binary columns with 1 for existence and 0 for nonexistence values. We had 11 categorical variables in our dataset on which we have performed One Hot Encoding which resulted in a total of 306 variables. Below figure represents the newly formed columns after one-hot encoding.

- **Resampling:** In Classification of datasets, the algorithms cannot run efficiently if the response variable is imbalanced as they do not get enough cases of the lower class to properly predict. And due to the unequal distribution of classes, the algorithms tend to be biased towards the majority class. So, it is desirable to attain a balanced dataset if not an equal number of classes. So, we have implemented oversampling on our training data to balance the classes. Where the algorithm works on replicating the minority class and balances the data using the **ROSE** package in R.

Also, since the dataset is huge and consists of over 6 lakh records, the models would take a lot of time to run and their speed and performance decreases. Hence, we have taken a subset of the dataset with around 10,000 records randomly so that the chosen data is not biased and the model is trained on all classes of the output with better accuracy. Also, Cross Validation is performed for all the selected models to avoid overfitting and multicollinearity. traincontrol() function is used to perform cross-validation where the method is set to cv and number parameter is set to 5 which represents the number of folds for which cross-validation should take place. [CITATION Pra16 \l 1033]

**Data Modelling**

After model resampling and cross-validation, we split the data set into train and test data for modelling the data and also understanding the accuracies and prediction of the model. Since the model has to predict a classification output, whether an applicant will be granted H-1B or not, we have chosen 4 models and trained them for the 80% of the dataset and verified the performances using the 20% of the dataset. Comparison of the accuracies, ROC curves and the AUC values generated by each model, their confusion matrix values are given below.

**Understanding the Confusion Matrix**

To attain good performance of a model and to improve its prediction power, we need to understand whether we need to reduce the False Positives or False Negatives for a model. Here, for example, an Applicant is denoted by X. From the definition of True Positives: Prediction is applicant X got the visa and Actually also visa was granted to X. Similarly, False Negatives imply that Prediction is applicant X did not receive the visa and Actually Visa was not granted to X. Hence, we understand that TruePositives and True Negatives are preferred scenarios. False Positives indicate that Prediction is applicant X will get the visa but actually, he did not get the Visa. False Negatives indicate that Prediction is applicant X will not get the visa but Actually, X was granted a visa. Hence, False Positives are more dangerous to our model compared to False Negatives. Our aim is to decrease the False Positives, with the cost of increasing False Negatives. Hence, we look at the specificity and recall values to maintain the balance between the FN and FPs.

**Logistic Regression:**

As the data that has been incorporated deals with a classification problem i.e. whether the application will be approved or denied. Logistic regression is practically good at solving these kinds of problems. Hence, a logistic regression model has been performed. A classic logistic regression model has been built with 306 predictors as One-Hot encoding was performed and the response variable as case status. In this model, 85% of accuracy was obtained on the original data and 61% of accuracy was obtained for the resampled data. The accuracy has been decreased when the prediction was run on the resampled data. Confusion matrix has shown the sensitivity of 0.181 and specificity of 0.944 with the imbalanced data.

**Random Forest:**

In a quest to get a better prediction accuracy random forest has been implemented which is a classification model. It was performed on both the data sets i.e. actual and resampled. Confusion matrix has shown the sensitivity of 0.189 and specificity of 1.000 with the imbalanced data.

Whereas with the resampled data sensitivity of 0.481 and specificity of 0.934 was obtained. With the original data, an accuracy of 90% is obtained with zero false negatives &amp; 197 false positives. The accuracy of 88% is achieved with the resampled data. False negatives are increased but the false positives are decreased to 126.

**Support Vector Machine**

By the concept of Support Vector Machine, it works by mapping the data to a high dimensional feature space so that data can be categorized. A separator between the categories is forced by the algorithm so that the data are transformed in such a way that the separator could be drawn as a hyperplane. Support vectors are data points that are closer to the hyperplane and influence the position and orientation of the hyperplane. Using these support vectors, we can maximize the classifier margin. Support vectors are the data points which help in building our model. The **&quot;e1071&quot;** package provides the SVM algorithm **svm()** function where the model is built using the 80 training data. Based on the trial and error method, we have chosen the kernel to &quot;radial&quot; and the Cost parameter as 1 and the gamma value to be 2. The decision values parameter is set to True to control the binary classifiers. The confusion matrix threshold is set as 0.5.

In order to understand which data was providing us better accuracies by reducing the False Positives, for the imbalanced data we can see that the Specificity is 1 which is an ideal scenario and also False-negative value is zero. For the Over Sampled Data, we could see that specificity is close to 1. So, for both the data we could see that the False positives are reduced with an accuracy of 89% provided by the imbalanced data. From the below ROC curves and the respective Area under Curves (AUC) between imbalanced and oversampled data are seen. We could see that the AUC value was 62.1% with not much difference between the two data&#39;s and the ROC curve depicts the prediction power and better accuracy.

**XG Boost**

XGBoost is a scalable and accurate gradient boosting machine. This model is built solely to develop computational speed and model performance. The xgboost package provides us with the xgboost() model for which a new train and test data sets are required. Here we would store the label that the model has to predict in a separate variable. And the training input is taken in terms of the matrix. Here the objective parameter is set to binary: logistic since it has to train a binary classifier. The nrounds parameter is set to 250 so that the data is passed for 250 times. The class depth value is given as 5 indicating the tree maximum depth. Nthread parameter indicates the number of rounds the processor turns, here our model takes the nthread value as 6. The threshold for confusion matrix for prediction values is set to 0.5 and results in the below.

**Model Comparison**

From the below accuracies, we can say that all the model performances are efficient in terms of accuracies. Hence, we also consider the AUC values while choosing the best model for the given dataset, as the AUC values compare the model&#39;s prediction power using various threshold values. Hence from the below list of values, we can interpret that tree-based models outperform logistic and SVM.

We can understand the significant variables to predict the visa case status for an applicant. We could see that prevailing wage, duration of employment, job roles and the status decision outcomes which are released in March month, worksite location, etc are the most significant factors responsible for predicting the approval of the visa.

**Conclusion**

Working towards this project, we learned the importance of Data cleaning and preprocessing. Understanding that data preprocessing step plays a major role in the data and also on the model output predicted. Implementing methods like one-hot encoding, cross-validation and resampling of the data for better performance of the model and understanding the changes it had made to the output. By analyzing the columns, and deciding between which columns are significant and which columns are uninformative makes an impact on the model. This is a very important stage while identifying significant predictors. Choosing the sample of data so that the model can be trained on unbiased data to provide an unbiased model. Based on the kind of output the model has to generate and the independent variables we had, to identify the significant model was an important stage. By trial and error method, the various parameters of the model are tuned and model training is done. We also implemented the ROC and AUC values and analyzed the importance of these concepts. Since the project deals with a real-life problem, we could predict that factors like an applicant&#39;s wage, the employer company, whether the applicant is a full-time employee or not, the work location etc are few of the significant factors for predicting whether an applicant will be granted with the H-1B visa or not.
