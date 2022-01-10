**Overview**

The Basic Officers Course (BOC) Military Occupational Specialty (MOS) Assignment Tool - BMAT, is an optimization tool provided to The Basic School, USMC, to assign Marines to jobs through minimizing cost.

**Features** 

Using VBA in Excel, we build an assignment tool to assign Marines to MOSs based on specified constraints. The use of the Hungarian Method algorithm is applied to enable us to exceed the decision variable capacity of Excel. BMAT constructs cost values to each Marine's job preferences, evaluates job availability, and assigns Marines to jobs through minimizing the cost function. Under the most complex scenarios (300 Marines to 29 different jobs with similar preferences), the tool is able to provide a solution in under 3 minutes, with an average solution time of 40 seconds under normal conditions. The tool contains an interactive dashboard with point-click operation, and outputs an Excel file which enables users to identify key takeaways from the assignment model. 

**Impact**

Currently under consideratoin at The Basic School -  the tool is able to reduce annual man-hours from 70 to 12. In addition, the tool automates the MOS assignment metrics enabling decision makers the ability to highlight specific changes as required.

**File Overview**

1. *BMAT.pdf*: Overview flyer.
2. *MOS_Interface_V4.3.xlsm*: Excel document containing BMAT and associated interface. 
3. *MOS_Results 30-11-2021.xlsm*: model results based on user created data.
