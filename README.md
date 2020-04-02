# QA'QC code for GL4 buoy sensors 
The buoy is deploed in GL4 as a part of Niwot Ridge Long Term Ecological Research Program (https://nwt.lternet.edu/data)

The buoy is instramented with 8 invidual RBR sensors that measure temperature, 3 PME sensors that measure dissloved oxygen, 1 PME sensor that uses a Li-COR quantum sensor to measure photosynthetically active radiation, and 1 PME C7 sensor to opically measure chlorophyll concentration. 

# Workflow:
All scripts read in raw data from the Niwot LTER Server so initial file paths for inputDir and outputDir need to be modified for each user. For file output don't forget to move final files (pending datamanager approval) into final folder in server.

# Explaination of scripts:
  All original scripts from 2018 are labeled with "_Summer18"
  
  All current scripts in which ongoing sensor QA'QC can take place are labeled with "_agg"
  
# File naming:
All raw data files are named with the sensor measurement (DO, PAR, C7, Temp), the sensor serial number, the deployment date range (07_03_08_21_2018, means July 3th 2018 to August 21st 2018) and deployment depth in meter. 

QA'Qced scripts are named as deployment season and sensor type. So Summer19_PME_DO.csv has all of the QA'QC ed data from the 3 sensors aggregated from all deployments up until summer 2019. 

