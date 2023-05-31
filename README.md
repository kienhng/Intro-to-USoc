# Intro-to-USoc
Getting to know the Understanding Society Database. Based on online course provided by Essex University.
# Notes on Understanding Society dataset

> These information are collected mainly from the course ******[Introduction to Understanding Society using R](https://open.essex.ac.uk/course/view.php?id=221)** provided by Essex University.
> 

## 1. Key materials

Introduction to the study: https://www.understandingsociety.ac.uk/about/about-the-study

FAQs on the UK Data Service website: https://beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000053#!/faqs

- **Introduction slides:**
    - [**Overview](https://open.essex.ac.uk/pluginfile.php/221375/mod_resource/content/27/StudyOverview.pdf) (specific data files description from Slides 80 to 82; naming convention in Slides 92 - 93)**
    - [**Data Access](https://open.essex.ac.uk/pluginfile.php/221379/mod_resource/content/9/DataAccess.pdf) (names of datasets, descriptions and serial numbers in Slides 3, 4 and 5)**
    - [**Walkthrough the USoc website**](https://open.essex.ac.uk/pluginfile.php/221383/mod_resource/content/13/WebsiteWalkThrough.pdf)
    - [**Longterm Content Plan](https://www.understandingsociety.ac.uk/sites/default/files/downloads/general/long-term-content-plan.pdf) (showing the frequency of datasets from Wave 1 in 2009 to Wave 16 in 2020)**
    - [**Geographical Identifiers**](https://www.understandingsociety.ac.uk/documentation/linked-data/geographical-identifiers)
- **Identiying individuals and households:**
    - Using **`pidp`**: Unique person identifier to link across waves and across files within the same wave. This identifier is constant, unique within and across waves.
    - Using **`w-pidp`**: Household identifier to identify members of the same households within one wave. Not constant, so *cannot be used to link households across waves* (no concept of longitudinal household)
- **Data Structure (Graphs only)**

![Untitled](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/440d5081-0121-4924-b138-226b296428ff/Untitled.png)

![Untitled](https://s3-us-west-2.amazonaws.com/secure.notion-static.com/03ebe243-f974-444f-8c5e-877a70aafa00/Untitled.png)

## 2. Some important notes

- USoc has subsample for Ethinic minorities (**Ethnic Minority Boost sample and an Immigrant and Ethnic Minority Boost sample).** The Boost sample is to improve the analysis of under-represented groups of people by oversampling such groups.
- The USoc made up of 2 big datasets: **BHPS** (from 1991 to 2008) and **UKHLS** (from 2009 until now). Key changes in the datasets timeline are:
    
    <aside>
    🗓️ 1991 Interviews (Started as **BHPS**) **⇒** 2001 Interviews **⇒** 2009 Interviews (Changed to **UKHLS**) **⇒** 2015 interviews
    
    </aside>
    
    Each annual interview is referred to as a **wave**. There are are 31 waves now.
    
    Using the dataset for analysis during a long period of time, therefore, should find a way to incorporate data from all **waves.** 
    
- After the first surveys (original sample), other surveyees in next waves are all connected to the original sample in some ways

## **Other Longtitunal Datasets**

- https://closer.ac.uk/explore-the-studies/
