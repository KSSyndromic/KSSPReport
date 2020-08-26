
## Interpreting `biosensequality` Reports"


## Output Structure

If you run the `write_reports` or `write_reports_local` function with `nexamples` set to anything larger than 0, there are three types of .xlsx files that will be written:  

* State_Summary.xlsx, which contains summaries for counts and percentages of null and invalid entries, as well as the lag between patient visits and record arrivals to the BioSense Platform. All of these are at the facility level, but statewide counts and percents are reported, as well.  
* *_Summary.xlsx (where the asterisk represents a facility name), which contains the same information from the State_Summary.xlsx workbook; however, it only includes information from that one facility. These are meant to be able to send directly to facilities as a summary for how they themselves are doing.  
* *_Examples.xlsx (where the asterisk represents a facility name), which contains examples of messages that are considered invalid or null. These are meant to be able to send directly to facilities as a way to locate which records are invalid and why they are invalid.  

I will discuss each of these three documents in turn.  

## State Summary

This document has three different tabs: required nulls, optional nulls, and invalids. Required and optional refers to if these fields are required to be reported or optional.  

The exact specifics of what is needed will likely vary by jurisdiction; for this reason, I made sure to annotate the code to make it clear what I'm doing at each point of the functions.  

### Required and Optional Nulls

We used the [PHIN messaging guide](https://www.cdc.gov/nssp/documents/guides/syndrsurvmessagguide2_messagingguide_phn.pdf) from the CDC to determine what was required or optional, but also relied on our own needs at Kansas Department of Health and Environment.  

Here is an example of what the nulls tabs look like:  
<img src="https://github.com/SophiaLC/biosensequality/blob/master/Interpretation/State_Summary_Req_Null.PNG" alt="hi" class="inline"/>

The first two columns show the facility's name and ID number, with the Feed Name and Sending Application for this facility showing next to them. Column 5 (Measures) points out what type of measure is being used: the raw count of nulls? Or the percentage that are null? Columns 6 and on are the fields that are being checked. For instance, the column named Chief_Complaint_Text shows the counts and pertanges of missing chief complaints for each of the facility. Rows 1 and 2 shaded in blue in the headline show the state counts and percents, respectively. So 742 is total NULL counts for Chief_Complaint_Text across the state, which takes about 4% of the visits. This is the setup for both the required and optional null tabs. The first three rows and first five columns are frozen to make it easier to navigate the page.  

### Invalids

Invalids were determined using the [PHIN VADS value sets](https://phinvads.cdc.gov/vads/ViewView.action?name=Syndromic%20Surveillance), but also sometimes relied on our own judgment. For instance, we considered certain fields like age and temperature to be invalid if age and temperature units are missing. Age of 1 year is also invalid since children under 2 years should send their ages in months. The precise details of these can be found by looking at the documentation for the functions ending with  `_invalid`.  


The layout is the same as the null tabs, except now it it displays when fields are invalid. You may see that there are some gaps: This is to be expected. I exclude all nulls (that is, nulls are *not* considered invalid, since there is already a function that measures nulls), so if *all* examples of this field was null, then there is nothing to count! This means a denominator of zero for the percentages; a blank is just put here.  


## Facility Summary

The function will return one .xlsx file for each facility that is in the data you pulled. This will have eleven tabs: facility information and timeliness report, required nulls, optional nulls, invalids, batch information, race and ethnicity, patient location,
other patient information (insurance company, patient class, age group, etc), facility type and diagnosis code, chief complaint text, and validity of date time format.  

### Facility Information

Here is an example of what the facility information tab looks like, with information about Stormont-Vail:  

<img src="https://github.com/SophiaLC/biosensequality/blob/master/Interpretation/Facility_Summary_FacInfo.PNG" alt="hi" class="inline"/> 

The rows each list different summary information about the facility. Column 1 will list the HL7 segments that this information is taken from (the guide for these HL7 segments are found at [the CDC's NSSP data dictionary](https://www.cdc.gov/nssp/biosense/docs/NSSP-Data-Dictionary.xlsx)), while Column 2 lists the field name, and Column 3 lists the value. The dates for patient visit time represented in this file are 2018-08-10 to 2018-08-17. During these 8 days, there are 1296 visits in total and 162 visit per day on average.

### Timeliness Report
For each facility, we calculate the average time, in hours, between (a) when the patient visited the emergency department and (b) when the first message was recorded, and (c) when the first message was sent, and (d) when the first record for this visit arrived to the NSSP BioSense Platform for each visit. 

Here is an example of the timeliness report for the earliest non NA chief complaint, the earliest non NA diagnosis code, and different types of trigger events.
<img src="https://github.com/SophiaLC/biosensequality/blob/master/Interpretation/Facility_Summary_Timeliness.PNG" alt="hi" class="inline"/>


### Nulls and Invalids

Here is an example of what each of the nulls and invalid tabs will look like (I used invalid as an example here): 

<img src="https://github.com/SophiaLC/biosensequality/blob/master/Interpretation/Facility_Summary_Invalid.PNG" alt="hi" class="inline"/>

The HL7 segment that these fields are taken from is in Column 1, Field checked is in Column 2, that specific facility's counts and percents are in Columns 3 and 4, and the statewide percent is in Column 5. For example, there are 39 invalid inputs (3.01%) for Age_Reported while the state-average percentage is 15.43%.

### Batch Information
The first part of the output gives the average number of messages per batch. The second part shows the number of batches per day, and the time, in hours, between the batches.

<img src="https://github.com/SophiaLC/biosensequality/blob/master/Interpretation/Facility_Summary_Batch.PNG" alt="hi" class="inline"/>

### Race and Ethnicity
Here is the example of count and percentage for race and ethnicity. 

<img src="https://github.com/SophiaLC/biosensequality/blob/master/Interpretation/Facility_Summary_Race.PNG" alt="hi" class="inline"/>


### Others
The facility summary output also provide patient locations (country, state, city, county), other patient information such as insurance company, patient class, age group, trigger event, smoking status, and discharge disposition.

## Facility Examples

If you set `nexamples` as greater than zero, you will also get an .xlsx file for every single facility. This file has two tabs: invalids and nulls. I did not post a screenshot here, as there is generally a lot of visit-specific information in the columns it returns.  

For the invalids tab, Column 1 contains the patient-visit ID number (C_BioSense_ID), Column 2 contains the field that is invalid, and Column 3 contains the entry that is invalid. If you want to know *why* it is invalid, the documentation for all of the `*_invalid` functions talk about what specifically is valid or invalid. Again, we use the PHIN VADS value sets and what we wanted for KDHE as determinants of what is invalid. This is likely to change by jurisdiction; I sacrificed some speed in the code for readability, so it should be doable to edit the code as need be for your department's specific needs.  

The rest of the columns gives you information needed to find the record and visit in question, in case the facility needs to track down what happened where in the data pipeline.  

The nulls tab is almost the same. Columns 1 and 2 are the visit ID and null field, respectively; the rest of the columns are the same columns that give you information needed to find the record and visit in question.  

Any questions about interpretability or reports of bugs or other issues can be addressed to sophiacrossen@gmail.com. 
<br>
<br>
