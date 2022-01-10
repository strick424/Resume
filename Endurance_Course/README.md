**Overview**

The United States Naval Academy does not have an accurate endurance course scoring matrix which mirrors the USMC's Officer Candidate Course endurance course scoring table. This project constructs an accurate and unbiased scoring matrix to align with other fleet endurance course scoring systems.

**Features** 

Using Python, proper statistical tests (Shapiro-Wilk, Kruskal-Wallis, ANOVA, F-test, and Levene) are applied to historical endurance course completion times to construct a scoring system capable of handling a variety of scores across males and females. The scoring algorithm utilizes percentiles to create point-scales for providing accurate scores to the user. The scoring matrix is evaluated on recent endurance course scoring data to confirm accuracy. 

**Impact**

The first extensive analysis on endurance course completion times to establish an unbiased scoring system for USNA. The scoring system is currently being utilized to provide performance feedback for those who participate in the USNA endurance course.

**File Overview**

1. *USNA_Endurance_Course_Scoring_Analysis.pdf*: final report submitted to senior Marine Corps leadership at USNA.
2. *USNA_Endurance_Course_Analysis.ipynb*: Python notebook containing all calculations and algorithms. 
3. *Historic_Times.csv*: endurance course completion time data used for analysis.
