# 📊 Revenue Management & Pricing – Problem Sets  

This repository contains individual problem sets completed as part of the **Revenue Management & Pricing** module in my Master's in Business Analytics at Bayes Business School.  

The problem sets focus on **pricing optimization, revenue maximization, and strategic decision-making** using data-driven models and analytical techniques.  

---

## 📂 Problem Sets  

### 📌 **Hotel & Airline Revenue Optimization**  

**🔍 Overview**  
This problem set focuses on **revenue optimization strategies for a hotel and an airline network**, applying pricing policies and dynamic programming concepts.  

**📝 Questions**  
1. **Hotel Room Pricing Strategy**  
   - A hotel in Cotswolds offers both regular and discounted early reservations.  
   - The task involves optimizing the number of rooms reserved for each category to maximize revenue.  
   - Uses **Poisson demand modeling** to compute expected revenue.  

2. **Airline Network Optimization**  
   - An airline operating flights from **Dublin → London → Edinburgh** needs an optimal seat allocation strategy.  
   - Requires understanding of **value functions** and **dynamic programming** to modify the airline's pricing model.  

3. **Real-World Application of Pricing Models**  
   - Identify a **real-world pricing problem** (e.g., a London-based service) and propose a pricing optimization model.  

---

### 📌 **Congestion Pricing & Strategic Pricing Decisions**  

**🔍 Overview**  
This problem set examines **urban congestion pricing** and its impact on revenue and emissions. It also extends the **strategic pricing framework** introduced in Problem Set 1.  

**📝 Questions**  
1. **London Congestion Charge Pricing**  
   - Analyze survey data to determine the optimal congestion charge price for peak and non-peak periods.  
   - Use demand elasticity and emissions modeling to balance **revenue maximization vs. environmental impact**.  
   - Explore price optimization under different policy constraints, including self-sustainability for public transport funding.  

2. **Custom Pricing Optimization Case Study**  
   - Identify a new **pricing problem in a real-world setting** and develop an optimization approach.  
   - Can extend the setting from Problem Set 1 or explore a new business scenario.  

--- 
### 📌 ** Santander Cycle **  

This project applies revenue management principles to optimize the pricing and operational efficiency of Santander Cycles, a bike-sharing scheme in London. Given challenges such as competition from Lime and high redistribution costs, our goal was to develop dynamic pricing strategies to improve revenue and bike utilization.

## Group Component
### Task Description
As a group, we acted as revenue management consultants tasked with analyzing and improving the revenue model of a local business. We chose Santander Cycles and aimed to:
- Develop pricing strategies to increase revenue while maintaining affordability.
- Implement demand-based incentives to redistribute bike availability across stations.
- Provide data-driven recommendations based on simulation models.

### Steps Taken
1. **Problem Identification**: We outlined key challenges, including declining usage, inelastic demand during peak hours, and costly bike redistribution.
2. **Data Collection & Assumptions**: We based our analysis on Transport for London (TfL) data and structured demand into peak and non-peak segments.
3. **Methodology**:
   - **Single Pricing Model**: Determined an optimal flat rate for all trips.
   - **Tiered Pricing Model**: Implemented separate peak and non-peak pricing to better capture willingness-to-pay.
   - **Dynamic Redistribution Incentives**: Introduced a tax on high-demand stations and discounts on low-demand stations to balance bike distribution.
4. **Optimization**:
   - Used the COBYLA algorithm for non-linear optimization.
   - Applied Poisson and Normal distributions to model demand and willingness-to-pay.

**Results & Findings**:
   - **Single Price Model**: £2.47 per ride, generating £190,993 in revenue.
   - **Tiered Pricing Model**: £2.49 (peak) and £1.26 (non-peak), increasing revenue by 26.51%.
   - **Redistribution Strategy**: 35.97% tax on high-demand stations, 7.34% discount on low-demand stations, improving utilization by 8.17%.

**Recommendations**:
   - Implement dynamic pricing to capture peak-period demand.
   - Apply targeted incentives to balance bike availability.
   - Continuously monitor demand for adaptive pricing adjustments.





