# Customer-Churn

##Introduction      
Every company wants to increase its revenue and profitability. To do that, while they acquire new customers, they also want to make sure that the existing ones stay with them for a long term. Also, its strategically important to know beforehand whether a set of customers are planning to stop using their services (especially recurring ones like internet, cable, phone etc.). To do that, every company or business creates and tracks customer metrics which are then used to predict their likelihood of churn. In this project, we will use the customer data of a telecom sector company to predict the probability of churn for each of the customer.    

##Attributes:   
•	state: the state the user lives in   
•	account length: the number of days the user has this account   
•	area code: the code of the area the user lives in   
•	phone number: the phone number of the user    
•	international plan: true if the user has the international plan, otherwise false   
•	voice mail plan: true if the user has the voice mail plan, otherwise false   
•	number vmail messages: the number of voice mail messages the user has sent   
•	total day minutes: total number of minutes the user has been in calls during the day   
•	total day calls: total number of calls the user has done during the day    
•	total day charge: total amount of money the user was charged by the Telecom company for calls during the day    
•	total eve minutes: total number of minutes the user has been in calls during the evening    
•	total eve calls: total number of calls the user has done during the evening    
•	total eve charge: total amount of money the user was charged by the Telecom company for calls during the evening    
•	total night minutes: total number of minutes the user has been in calls during the night    
•	total night calls: total number of calls the user has done during the night     
•	total night charge: total amount of money the user was charged by the Telecom company for calls during the night    
•	total intl minutes: total number of minutes the user has been in international calls     
•	total intl calls: total number of international calls the user has done    
•	total intl charge: total amount of money the user was charged by the Telecom company for international calls     
•	customer service calls: number of customer service calls the user has done     
•	churn: true if the user terminated the contract, otherwise false    

##ML algorithms used:      
•	Logistic Regression      
•	Decision trees      
•	C5.0     
•	Xgboost Tree    

Caret package in R is a comprehensive framework for building machine learning models.Using the caret package the above mentioned algorithms were trained on the dataset and compared with respect to accuracy and kappa statistic to determine the best model.       
Refer to Customer churn.md for detailed analysis.     

##Conclusion:      
C5.0 model is selected as final prediction model as it has the highest accuracy and kappa statistic as well as the least standard deviation across cross-validation sample indicating low variance.      
