Background:
==========
Data originated from an experiments where activities from 30 volunteers where measured using the accelerometers and gyrometers from the Samsung Galaxy S smartphone. 
Volunteers performed 5 type of activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) during which the data was collected.
Subsequently, the data was pre-processed and labeled w.r.t the different activities.

A full description is available at the site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

The original data used for this project can be found at:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

This original data has been tidied up further for analysis.

Data description:
================
The resulting "tidy_data" datasets contains the following variables:
- set: factor variable indicating the split into training and test set
	training
	test

- subject: integer indicating the participant number [1 to 30]
	
- activity: code (integer) corresponding to the different activities
	1 WALKING
	2 WALKING_UPSTAIRS
	3 WALKING_DOWNSTAIRS
	4 SITTING
	5 STANDING
	6 LAYING

- activity name: factor variable with the actual activity names
	walking
	walking_upstairs
	walking_downstairs
	sitting
	standing
	laying
	
66 continuous variables corresponding to selected features from the original accelerometer and gyroscope data (see explanation further below).
- tBodyAcc_mean_X          
- tBodyAcc_mean_Y           
- tBodyAcc_mean_Z           
- tBodyAcc_std_X            
- tBodyAcc_std_Y           
- tBodyAcc_std_Z           
- tGravityAcc_mean_X        
- tGravityAcc_mean_Y        
- tGravityAcc_mean_Z        
- tGravityAcc_std_X         
- tGravityAcc_std_Y        
- tGravityAcc_std_Z         
- tBodyAccJerk_mean_X       
- tBodyAccJerk_mean_Y       
- tBodyAccJerk_mean_Z       
- tBodyAccJerk_std_X       
- tBodyAccJerk_std_Y       
- tBodyAccJerk_std_Z        
- tBodyGyro_mean_X          
- tBodyGyro_mean_Y          
- tBodyGyro_mean_Z         
- tBodyGyro_std_X          
- tBodyGyro_std_Y           
- tBodyGyro_std_Z           
- tBodyGyroJerk_mean_X      
- tBodyGyroJerk_mean_Y     
- tBodyGyroJerk_mean_Z      
- tBodyGyroJerk_std_X       
- tBodyGyroJerk_std_Y       
- tBodyGyroJerk_std_Z       
- tBodyAccMag_mean         
- tBodyAccMag_std           
- tGravityAccMag_mean       
- tGravityAccMag_std        
- tBodyAccJerkMag_mean      
- tBodyAccJerkMag_std      
- tBodyGyroMag_mean         
- tBodyGyroMag_std          
- tBodyGyroJerkMag_mean     
- tBodyGyroJerkMag_std      
- fBodyAcc_mean_X          
- fBodyAcc_mean_Y           
- fBodyAcc_mean_Z           
- fBodyAcc_std_X            
- fBodyAcc_std_Y            
- fBodyAcc_std_Z           
- fBodyAccJerk_mean_X       
- fBodyAccJerk_mean_Y       
- fBodyAccJerk_mean_Z       
- fBodyAccJerk_std_X        
- fBodyAccJerk_std_Y       
- fBodyAccJerk_std_Z        
- fBodyGyro_mean_X          
- fBodyGyro_mean_Y          
- fBodyGyro_mean_Z          
- fBodyGyro_std_X          
- fBodyGyro_std_Y           
- fBodyGyro_std_Z           
- fBodyAccMag_mean          
- fBodyAccMag_std           
- fBodyBodyAccJerkMag_mean 
- fBodyBodyAccJerkMag_std   
- fBodyBodyGyroMag_mean     
- fBodyBodyGyroMag_std      
- fBodyBodyGyroJerkMag_mean 
- fBodyBodyGyroJerkMag_std

Description of the selected features:
====================================
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. 
These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. 
Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. 
Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter 
with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). 
Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. 
(Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

The mean and standard deviation calculated from these features are included in the tidy dataset, coded as follows:

mean: Mean value
std: Standard deviation
