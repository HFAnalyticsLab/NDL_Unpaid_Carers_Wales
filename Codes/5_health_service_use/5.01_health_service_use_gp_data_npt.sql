-------------------------------------------------------------
/* NPT GP Interactions: 1:1 Match */
-------------------------------------------------------------

/* STEPS:
 * 1. Get WLGP (GP event cleansed) events information for thsoe with an interaction in the year prior to being identified as an unpaid carer
 * - 1.1 Select main variables from deduplicated table (PEJE_MATCHED_COHORT_NPT_2)
 * - 1.2 Left join to WLGP
 * - 1.3 ... where event was in year prior to being identified as a carer
 * - 1.4 ... and WLGP ALF_STS_CD in 1,4,39
 * - 1.5 pull in WLGP event details (date based on distinct alf and date)
 * 2.Join deduplicated carer table back to above to those without an interaction in the year prior to being identified
 * 
 * Note: In GP data each read code gets a new line even for the same date/interaction. As we are not interested
 * in the read codes we are dealing with it just by looking a unique EVENT_DTs rather than by rows 
 * 
 */

DROP TABLE SAILW1429V.GBH_MATCHEDCOHORT_GP_NPT_STEP1 ;
DROP TABLE SAILW1429V.GBH_MATCHEDCOHORT_GP_NPT ;

-- Create step 1 table
CREATE TABLE SAILW1429V.GBH_MATCHEDCOHORT_GP_NPT_STEP1 (
ALF_PE BIGINT, 
SEX INTEGER, 
AGE INTEGER,
WOB DATE,
WIMD INTEGER,
RUC VARCHAR (50),
LA_NAME VARCHAR (30), 
FIRST_IDENTIFIED_DATE DATE, 
IDENTIFIEDBY VARCHAR (10), 
TOTAL_NUM_ASSESSMENTS INTEGER,
EVENT_DT DATE);

-- Insert values to the above table
INSERT INTO SAILW1429V.GBH_MATCHEDCOHORT_GP_NPT_STEP1 
-- 1. Get info for those with an interaction in the year prior to being identified as an unpaid carer 
-- 1.1 Select main variables from deduplicated table (PEJE_MATCHED_COHORT_NPT_2)
SELECT
	DISTINCT N.ALF_PE,
	N.SEX,
	N.AGE_INDEXDATE, 
	N.WOB,
	N.WIMD, 
	N.RUC11_DESC,
	'Neath Port Talbot' AS LA_NAME,
	N.FIRST_IDENTIFIED_DATE,
	N.FIRST_IDENTIFIED_BY,
	N.TOTAL_NUM_ASSESSMENTS,
-- 1.5 Pull in GP event details 
	GP.EVENT_DT 
FROM SAILW1429V.PEJE_MATCHED_COHORT_NPT_2 N
-- 1.2 left to join to GP
LEFT JOIN SAIL1429V.WLGP_GP_EVENT_CLEANSED_20220301 GP ON
	N. ALF_PE = GP.ALF_PE 
WHERE 
	-- 1.3 ... where event was in year prior to being identified as being a carer	
	GP.EVENT_DT BETWEEN (N.FIRST_IDENTIFIED_DATE - 1 YEAR) AND (N.FIRST_IDENTIFIED_DATE - 1 DAY)	
	-- 1.4 ... and GP ALF_STS_CD in 1,4,39
	AND GP.ALF_STS_CD IN(1,4,39); 

-- check for many rows per alf and event date 
SELECT ALF_PE, EVENT_DT, COUNT(*)
FROM SAILW1429V.GBH_MATCHEDCOHORT_GP_NPT_STEP1
GROUP BY ALF_PE, EVENT_DT
HAVING COUNT(*) >1; 


CREATE TABLE SAILW1429V.GBH_MATCHEDCOHORT_GP_NPT (
	ALF_PE BIGINT,
	SEX INTEGER,
	SEX_DESC VARCHAR(10),
	AGE INTEGER,
	WOB DATE,
	WIMD INTEGER,
	WIMD_DESC VARCHAR(50),
	RUC_DESC VARCHAR(50),
	LA_NAME VARCHAR(100), 
	FIRST_IDENTIFIED_DATE DATE,
	FIRST_IDENTIFIED_BY VARCHAR(10),
	TOTAL_NUM_ASSESSMENTS INTEGER,
	CARER_FLAG INTEGER, 
	EVENT_DT DATE);

-- in the first table we created a df using the matched cohort with interactions, 
-- and now in the second table we adding back in the rest of the matched cohort that don't have any interaction
INSERT INTO SAILW1429V.GBH_MATCHEDCOHORT_GP_NPT
SELECT DISTINCT *
FROM(
	SELECT
		DISTINCT N.ALF_PE,
		N.SEX,
		N.SEX_DESC,
		N.AGE_INDEXDATE,
		N.WOB,
		N.WIMD,
		N.WIMD_DESC,
		N.RUC11_DESC,
		'Neath Port Talbot' AS LA_NAME,
		N.FIRST_IDENTIFIED_DATE,
		N.FIRST_IDENTIFIED_BY,
		N.TOTAL_NUM_ASSESSMENTS,
		N.TREATED,
		B.EVENT_DT
	FROM SAILW1429V.PEJE_MATCHED_COHORT_NPT_2 N
	-- 2.Join deduplicated carer table to WLGP info for those with an interaction in the year prior to being identified as a carer from step 1
	LEFT JOIN (SELECT * 
		FROM SAILW1429V.GBH_MATCHEDCOHORT_GP_NPT_STEP1) B
		ON N.ALF_PE = B.ALF_PE AND N.FIRST_IDENTIFIED_DATE = B.FIRST_IDENTIFIED_DATE); 



--CHECK COUNTS

-- Row count
SELECT 
	COUNT(*)
FROM 
	SAILW1429V.GBH_MATCHEDCOHORT_GP_NPT; 
-- Carer count
SELECT 
	COUNT(UNIQUE(ALF_PE))
FROM 
	SAILW1429V.GBH_MATCHEDCOHORT_GP_NPT; 
-- checking how many carers vs non-carers, that 1:1 match is true
SELECT COUNT(DISTINCT ALF_PE) total,  CARER_FLAG 
FROM SAILW1429V.GBH_MATCHEDCOHORT_GP_NPT 
GROUP BY CARER_FLAG; 


-- check how many individuals do not have an GP interaction
SELECT COUNT(*) 
FROM SAILW1429V.GBH_MATCHEDCOHORT_GP_NPT
WHERE EVENT_DT IS NULL; 

-- check how many of those without a GP interaction by carer status
SELECT count(DISTINCT ALF_PE) total,  CARER_FLAG 
FROM SAILW1429V.GBH_MATCHEDCOHORT_GP_NPT
WHERE EVENT_DT IS NULL
GROUP BY CARER_FLAG; 

-- check that multiple rows are not from multiple index dates
SELECT 	COUNT(*) 
FROM (
SELECT ALF_PE, FIRST_IDENTIFIED_DATE 
FROM SAILW1429V.GBH_MATCHEDCOHORT_GP_NPT
GROUP BY ALF_PE, FIRST_IDENTIFIED_DATE 
); -

-- counting how many unique interaction dates 
SELECT 	COUNT(*)
FROM  (
SELECT ALF_PE, EVENT_DT 
FROM SAILW1429V.GBH_MATCHEDCOHORT_GP_NPT
GROUP BY ALF_PE, EVENT_DT 
); 

-- view individuals with multiple interactions (unique days as events)
SELECT  ALF_PE,	EVENT_DT, COUNT(*)
FROM  SAILW1429V.GBH_MATCHEDCOHORT_GP_NPT
GROUP BY ALF_PE, EVENT_DT 
HAVING COUNT(*) > 1; 
	


