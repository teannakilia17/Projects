# SMM638 Network Analysis

### 📌 ** Individual Project Improving Efficiency of R&D Projects at Silicon**
#### Problem Statement:
Silicon, a global semiconductor company, wants to improve the efficiency of its R&D projects in microprocessors. The task is to identify how knowledge exchange among team members influences project success, duration, and innovation levels. The analysis involves three datasets: team affiliations, project outcomes, and a knowledge exchange network.

#### Steps Taken:
1. **Data Preprocessing**:
   - Cleaned and merged datasets for team affiliation, project outcomes, and the knowledge exchange network.
   - Calculated network metrics (e.g., average degree, betweenness centrality, clustering coefficient) using NetworkX for employees based on their knowledge-sharing ties.

2. **Statistical Analysis**:
   - Merged network metrics with project outcome data.
   - Conducted regression analysis to identify the relationship between network metrics and project outcomes like success, duration, and novelty.

3. **Results**:
   - Found that **average degree** and **betweenness centrality** significantly influenced project success and duration.
   - Proposed that increasing average team degree and managing betweenness centrality could optimize project success and reduce durations.

4. **Business Recommendations**:
   - Increase team network density by facilitating in-person interactions.
   - Adjust office seating plans to enhance internal collaboration.
---
### 📌 ** Group Project Analyzing Music Genre Preferences at Deezer**
#### Problem Statement:
Deezer seeks to improve its music recommendation system by understanding the relationships between music genres and user preferences. The task is to analyze the "like" relationships between users and genres, identify genre similarities, and understand the influence of social ties on genre preferences.

#### Steps Taken:
1. **Data Preprocessing**:
   - Created a bipartite graph linking users to genres based on their "likes."
   - Projected the graph to generate a **genre-genre** network and applied network analysis techniques.

2. **Network Analysis**:
   - Used the **Louvain algorithm** to detect communities in the genre network, identifying groups of related genres.
   - Analyzed **centrality metrics** (degree, betweenness) to assess the importance of genres in the network and their role in user preferences.

3. **Results**:
   - Discovered that mainstream genres like **Pop** and **Dance** had the strongest overlap in terms of listeners.
   - Identified key genres like **Pop** and **Rock** as having the highest centrality and bridging roles in user engagement.

4. **Business Recommendations**:
   - Deezer should use central genres like **Pop** and **Rock** as anchors for user engagement in the recommendation system.
   - Focus on genres with high centrality for talent scouting and strategic artist collaborations.

---
## Files Included
- **Data**: Contains cleaned data files used for analysis.
- **Code**: Python scripts and Jupyter notebooks that perform the data analysis.

---
