**Overview**

Competency to objective mapping and ranking using NLP: implementation of text mining procedures to conduct similarity comparison and exact term matching. The algorithm is able to map 4,368 objectives to 1,979 compentencies in under 2 minutes.

**Features**

Utilizing the data provided, a sorted list of mapped course objectives relating to individual competencies is achieved. The use of various text analysis methods (jaccard distance and cosine similarity) to find the closest set of objectives to each competency is used. In addition, the algorithm utilizes the given search terms provided by the client to ensure the objectives are matched to each compentency correctly. The output of the project is a CSV file containing the objectives matched to each compentency along with the rank, compentency number, course and objective number, and similarity score. 

**Example**

Given the following compentency, we iterate through the given objectives to find the objectives that are most similar to the competency's text.

*Competency*: Understand different types of contracts.

*Competency Search Terms*: contract type, types of contracts, fixed price, fixed-price, cost-reimbursement, time-and-materials, labor-hour

*Objectives Matched*: 
1. Differentiate between different types of contracts. (Similarity Score: 0.667)
2. Explain different contracting methods and contract types. (Similarity Score: 0.577)
3. Identify the types of contracts, price structures, and incentives used to manage risk and motivate contractor performance. (Similarity Score: 0.258)
4. Given a scenario, recommend the appropriate contract type and price structure to incentivize the contractor to meet program objectives. (Similarity Score: 0.183)
