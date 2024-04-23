-------------------------------------------------------------
/* DENBIGHSHIRE ED */
-------------------------------------------------------------

/* STEPS:
 * 1. Get ED info for those with an attendance in the year prior to being identified as a carer
 * 	- 1.1. Select main variables from deduplicated table (PEJE_MATCHED_COHORT_DENBIGHSHIRE_2)
 * 	- 1.2. Left join to ED 
 * 	- 1.3. ...where event was in year prior to being identified as a carer
 * 	- 1.4. ...and ED ALF_STS_CD in 1, 4, 39
 * 	- 1.5. Pull in ED event details (admin_arr_dt, admin_end_dt)
 * 2. Join deduplicated carer table to ED info for those with an attendance in the year prior to being identified as a carer from step 1

 */

DROP TABLE SAILW1429V.PEJE_MATCHEDCOHORT_ED_DENBIGHSHIRE_2_STEP1;

CREATE TABLE SAILW1429V.PEJE_MATCHEDCOHORT_ED_DENBIGHSHIRE_2_STEP1
(ALF_PE BIGINT
, FIRST_IDENTIFIED_DATE DATE
, ADMIN_ARR_DT DATE
, ADMIN_END_DT DATE
);

INSERT INTO SAILW1429V.PEJE_MATCHEDCOHORT_ED_DENBIGHSHIRE_2_STEP1
-- 1. Get ED info for those with an attendance in the year prior to being identified as a carer
	-- 1.1. Select main variables from deduplicated table (PEJE_DENBIGHSHIRE_GP_LA_STEP4)
SELECT
	DISTINCT J.ALF_PE,
	J.FIRST_IDENTIFIED_DATE,	
	-- 1.5. Pull in ED details
	ED.ADMIN_ARR_DT,
	ED.ADMIN_END_DT 
FROM SAILW1429V.PEJE_MATCHED_COHORT_DENBIGHSHIRE_2 J
-- 1.2. Left join to ED 
LEFT JOIN (SELECT * FROM SAIL1429V.EDDS_EDDS_20220601) ED ON J.ALF_PE = ED.ALF_PE
WHERE
	-- 1.3. ...where event was in year prior to being identified as a carer
	ED.ADMIN_ARR_DT BETWEEN (J.FIRST_IDENTIFIED_DATE - 1 YEAR) AND (J.FIRST_IDENTIFIED_DATE - 1 DAY)
	-- 1.4. ...and ED ALF_STS_CD in 1, 4, 39
	AND ED.ALF_STS_CD IN(1, 4, 39); 



DROP TABLE SAILW1429V.PEJE_MATCHEDCOHORT_ED_DENBIGHSHIRE_2;

CREATE TABLE SAILW1429V.PEJE_MATCHEDCOHORT_ED_DENBIGHSHIRE_2 
(ALF_PE BIGINT,
SEX INTEGER,
SEX_DESC VARCHAR(10),
AGE INTEGER,
WOB DATE,
ETHNICITY_DESC VARCHAR(7), 
LSOA2011_CD VARCHAR(15),
WIMD INTEGER,
RUC VARCHAR(47),
LA_NAME VARCHAR(50),
CARER_FLAG INTEGER,
FIRST_IDENTIFIED_DATE DATE,
FIRST_IDENTIFIED_BY VARCHAR(5),
TOTAL_NUM_ASSESSMENTS INTEGER,
ADMIN_ARR_DT DATE,
ADMIN_END_DT DATE);


INSERT INTO SAILW1429V.PEJE_MATCHEDCOHORT_ED_DENBIGHSHIRE_2
SELECT DISTINCT 
J.ALF_PE,
J.SEX,
J.SEX_DESC, 
J.AGE_INDEXDATE AS AGE,
J.WOB,
J.ETHNICITY_DESC, 
J.LSOA2011_CD,
J.WIMD,
J.RUC11_DESC,
'DENBIGHSHIRE' AS LA_NAME,
J.TREATED ,
J.FIRST_IDENTIFIED_DATE,
J.FIRST_IDENTIFIED_BY,
0 TOTAL_NUM_ASSESSMENTS,
B.ADMIN_ARR_DT,
B.ADMIN_END_DT
FROM SAILW1429V.PEJE_MATCHED_COHORT_DENBIGHSHIRE_2 J
LEFT JOIN ( SELECT * FROM SAILW1429V.PEJE_MATCHEDCOHORT_ED_DENBIGHSHIRE_2_STEP1 ) B 
ON J.ALF_PE = B.ALF_PE AND J.FIRST_IDENTIFIED_DATE = B.FIRST_IDENTIFIED_DATE; 


-- Check counts
-- Row count
SELECT 
	COUNT(*)
FROM 
	SAILW1429V.PEJE_MATCHEDCOHORT_ED_DENBIGHSHIRE_2; 
-- Carer count
SELECT 
	COUNT(UNIQUE(ALF_PE))
FROM 
	SAILW1429V.PEJE_MATCHEDCOHORT_ED_DENBIGHSHIRE_2; 
	
-- Check multiple rows per carer are due to multiple ed events, not multiple identification dates
SELECT 
	COUNT(*)
FROM 
	(
		SELECT 
			ALF_PE,
			FIRST_IDENTIFIED_DATE 
		FROM 
			SAILW1429V.PEJE_MATCHEDCOHORT_ED_DENBIGHSHIRE_2
		GROUP BY 
			ALF_PE,
			FIRST_IDENTIFIED_DATE 
		);
	
-- Check multiple rows per carer are due to multiple ed events
SELECT 
	COUNT(*)
FROM 
	(
		SELECT 
			ALF_PE,
			ADMIN_ARR_DT 
		FROM 
			SAILW1429V.PEJE_MATCHEDCOHORT_ED_DENBIGHSHIRE_2
		GROUP BY 
			ALF_PE,
			ADMIN_ARR_DT 
		); 
	
