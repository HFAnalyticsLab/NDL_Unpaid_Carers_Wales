/* Cleaning steps
 * STEP 1. Full join GP and LA Unpaid carers from existing tables
 * a. Create first identified by flag 
 * b. Select first identified date
 * 
 * STEP 2. Select required columns from GP unpaid carers
 * a. Select those who were first identified in the GP
 * b. Get baseline characteristics 
 * c. Add RUC to GP cohort (left join)
 * d. Join a and b (left join)
 * 
 * STEP 3. Select required columns from LA unpaid carers
 * a. Select those who were first identified in the GP
 * b. Get baseline characteristics 
 * c. Join a and b  (left join)
 * 
 * STEP 4. Union key columns from Steps 2 AND 3 
 * - join with first identified date
 * - Union GP and LA key columns  * 
 * 
 */

-- count total unique individuals and total number of rows in each table
SELECT count (DISTINCT CARER_ALF_PE) FROM SAILW1429V.LB_LA_CARER_NPT;
SELECT count (DISTINCT ALF_PE) FROM SAILW1429V.LB_GPCARER_NPT;

SELECT COUNT(*) FROM SAILW1429V.LB_LA_CARER_NPT;
SELECT count (*) FROM SAILW1429V.LB_GPCARER_NPT;


-- STEP 1. Full join GP and LA Unpaid carers from existing tables
DROP TABLE SAILW1429V.PEJE_GP_LA_NPT_STEP1;

CREATE TABLE SAILW1429V.PEJE_GP_LA_NPT_STEP1
(
Main_ALF_PE BIGINT,
GP_ALF_PE BIGINT,
GP_IDENTIFIED_DATE DATE,
LA_ALF_PE BIGINT,
LA_IDENTIFIED_DATE DATE,
FIRST_IDENTIFIED_BY VARCHAR(3),
FIRST_IDENTIFIED_DATE DATE);

INSERT INTO SAILW1429V.PEJE_GP_LA_NPT_STEP1
SELECT DISTINCT 
CASE 
WHEN gp.EVENT_DT < la.FIRST_CARER_ASSMT OR (la.FIRST_CARER_ASSMT IS NULL AND gp.EVENT_DT IS NOT NULL)  THEN gp.ALF_PE
WHEN gp.EVENT_DT > la.FIRST_CARER_ASSMT OR (la.FIRST_CARER_ASSMT IS NOT NULL AND gp.EVENT_DT IS NULL) THEN la.ALF_PE
WHEN gp.EVENT_DT = la.FIRST_CARER_ASSMT AND (la.FIRST_CARER_ASSMT IS NOT NULL AND gp.EVENT_DT IS NOT NULL) THEN gp.ALF_PE
ELSE NULL
END AS Main_ALF_PE
, gp.ALF_PE AS gp_ALF
, gp.EVENT_DT AS GP_Identified_DT
, la.ALF_PE AS la_ALF
, la.FIRST_CARER_ASSMT AS la_FIRST_CARER_ASSMT_DT 
-- a. Create first identified by flag
, CASE 
WHEN gp.EVENT_DT < la.FIRST_CARER_ASSMT OR (la.FIRST_CARER_ASSMT IS NULL AND gp.EVENT_DT IS NOT NULL)  THEN 'GP'
WHEN gp.EVENT_DT > la.FIRST_CARER_ASSMT OR (la.FIRST_CARER_ASSMT IS NOT NULL AND gp.EVENT_DT IS NULL) THEN 'LA'
WHEN gp.EVENT_DT = la.FIRST_CARER_ASSMT AND (la.FIRST_CARER_ASSMT IS NOT NULL AND gp.EVENT_DT IS NOT NULL) THEN 'EQ'
ELSE NULL
END AS First_Identified_By
--  b. Select first identified date
, CASE 
WHEN gp.EVENT_DT < la.FIRST_CARER_ASSMT OR (la.FIRST_CARER_ASSMT IS NULL AND gp.EVENT_DT IS NOT NULL)  THEN gp.EVENT_DT 
WHEN gp.EVENT_DT > la.FIRST_CARER_ASSMT OR (la.FIRST_CARER_ASSMT IS NOT NULL AND gp.EVENT_DT IS NULL) THEN la.FIRST_CARER_ASSMT 
WHEN gp.EVENT_DT = la.FIRST_CARER_ASSMT AND (la.FIRST_CARER_ASSMT IS NOT NULL AND gp.EVENT_DT IS NOT NULL) THEN gp.EVENT_DT 
ELSE NULL
END AS First_Identified_DT
FROM SAILW1429V.LB_GPCARER_NPT gp
FULL OUTER JOIN 
(SELECT CARER_ALF_PE AS ALF_PE, FIRST_CARER_ASSMT FROM SAILW1429V.LB_LA_CARER_NPT ) la 
ON gp.ALF_PE = la.ALF_PE; 


-- verifying counts
SELECT count(*) total, First_Identified_By FROM SAILW1429V.PEJE_GP_LA_NPT_STEP1
GROUP BY First_Identified_By;


SELECT count(DISTINCT Main_ALF_PE) total FROM SAILW1429V.PEJE_GP_LA_NPT_STEP1;

-- eyeball no duplicates
SELECT * FROM (
SELECT count( Main_ALF_PE) total, Main_ALF_PE FROM SAILW1429V.PEJE_GP_LA_NPT_STEP1 
GROUP BY Main_ALF_PE
) WHERE total >1


-- STEP 2. Select required columns from GP unpaid carers
DROP TABLE SAILW1429V.PEJE_GP_LA_NPT_STEP2;

CREATE TABLE SAILW1429V.PEJE_GP_LA_NPT_STEP2
(
ALF_PE BIGINT
, SEX INTEGER
, AGE INTEGER
, AGE_STUDYSTART INTEGER
, WOB DATE
, FIRST_CARER_ASSMT DATE
, LATEST_CARER_ASSMT DATE
, TOTAL_NUM_ASSESSMENTS INTEGER
, DEATH_DT DATE
, LSOA2011_CD VARCHAR(20)
, WIMD INTEGER
, RUC11CD VARCHAR(3)
, RUC11_DESC VARCHAR(30)
, LA_NAME VARCHAR(100)
, IdentifiedBy VARCHAR(10)
);



INSERT INTO SAILW1429V.PEJE_GP_LA_NPT_STEP2
WITH cte_gp_keycols AS 
(
	-- b. Get baseline characteristics
	SELECT ALF_PE, SEX, AGE, YEARS_BETWEEN('2017-07-01', WOB) AGE_STUDYSTART, WOB, EVENT_DT AS FIRST_CARER_ASSMT, NULL AS LATEST_CARER_ASSMT
	, NULL AS TOTAL_NUM_ASSESSMENTS, DEATH_DT, LSOA2011_CD, WIMD_2019_QUINTILE AS WIMD, RUC11CD, gp.RUC11 AS RUC11_DESC, LA_NAME 
	, 'GP' AS IdentifiedBy
	FROM  SAILW1429V.LB_GPCARER_NPT gp
	LEFT JOIN (
	-- c. Add RUC to GP cohort (left join)
		SELECT DISTINCT LSOA11CD, RUC11CD, RUC11
		FROM SAILREFRV.RURAL_URBAN_CLASS_2011_OF_LLSOAREAS_IN_ENG_AND_WAL
		) r	ON gp.LSOA2011_CD = r.LSOA11CD
	
),
cte_gp_carers AS 
(
	-- a. Select those who were first identified in the GP
	SELECT Main_ALF_PE FROM SAILW1429V.PEJE_GP_LA_NPT_STEP1
	WHERE First_Identified_By = 'GP'
)
-- c. Join a and b  (left join)
SELECT g.* FROM cte_gp_carers s
LEFT JOIN (SELECT * FROM cte_gp_keycols )g 
ON s.Main_ALF_PE = g.ALF_PE; 




-- eyeball no duplicates
SELECT * FROM (
SELECT count( ALF_PE) total, ALF_PE FROM SAILW1429V.PEJE_GP_LA_NPT_STEP2 
GROUP BY ALF_PE
) WHERE total >1



-- STEP 3. Select required columns from LA unpaid carers
DROP TABLE SAILW1429V.PEJE_GP_LA_NPT_STEP3;

CREATE TABLE SAILW1429V.PEJE_GP_LA_NPT_STEP3
(
ALF_PE BIGINT
, SEX INTEGER
, AGE INTEGER
, AGE_STUDYSTART INTEGER
, WOB DATE
, FIRST_CARER_ASSMT DATE
, LATEST_CARER_ASSMT DATE
, TOTAL_NUM_ASSESSMENTS INTEGER
, DEATH_DT DATE
, LSOA2011_CD VARCHAR(20)
, WIMD INTEGER
, RUC11CD VARCHAR(3)
, RUC11_DESC VARCHAR(30)
, LA_NAME VARCHAR(100)
, IdentifiedBy VARCHAR(10)
);

INSERT INTO SAILW1429V.PEJE_GP_LA_NPT_STEP3
WITH cte_la_keycols AS (
	-- b. Get baseline characteristics
	SELECT DISTINCT CARER_ALF_PE AS ALF_PE, SEX_CARER AS SEX, AGE_CARER AS AGE
	, YEARS_BETWEEN('2017-07-01', WOB_CARER) AGE_STUDYSTART
	, WOB_CARER AS WOB
	, FIRST_CARER_ASSMT, LATEST_CARER_ASSMT
	, TOTAL_NUM_ASSESSMENTS, DOD_CARER AS DEATH_DT
	, LSOA2011_CD_CARER
	, WIMD_CARER AS WIMD, RUC_CARER AS RUC, RUC_DESC_CARER AS RUC11_DESC, LSOA_DESC_CARER AS LA_NAME 
	, 'LA' AS IdentifiedBy
	FROM SAILW1429V.LB_LA_CARER_NPT 
),
cte_la_carers AS 
(
	-- a. Select thos who were first identified in the GP
	SELECT Main_ALF_PE FROM SAILW1429V.PEJE_GP_LA_NPT_STEP1
	WHERE First_Identified_By = 'LA'
)
-- c. Join a and b  (left join)
SELECT l.* FROM cte_la_carers s
LEFT JOIN (SELECT * FROM cte_la_keycols ) l
ON s.Main_ALF_PE = l.ALF_PE; 

SELECT COUNT(*) FROM SAILW1429V.PEJE_GP_LA_NPT_STEP3; 

-- eyeball no duplicates
SELECT * FROM (
SELECT count( ALF_PE) total, ALF_PE FROM SAILW1429V.PEJE_GP_LA_NPT_STEP3
GROUP BY ALF_PE
) WHERE total >1



-- STEP 4. Union key columns from Steps 2 AND 3 
DROP TABLE SAILW1429V.PEJE_DEDUP_GP_LA_NPT;

CREATE TABLE SAILW1429V.PEJE_DEDUP_GP_LA_NPT
(
ALF_PE BIGINT
, SEX INTEGER
, AGE INTEGER
, AGE_STUDYSTART INTEGER
, WOB DATE
, FIRST_CARER_ASSMT DATE
, LATEST_CARER_ASSMT DATE
, TOTAL_NUM_ASSESSMENTS INTEGER
, DEATH_DT DATE
, LSOA2011_CD VARCHAR(20)
, WIMD INTEGER
, RUC11CD VARCHAR(3)
, RUC11_DESC VARCHAR(30)
, LA_NAME VARCHAR(100)
, IdentifiedBy VARCHAR(10)
, FIRST_IDENTIFIED_DATE DATE
);

INSERT INTO SAILW1429V.PEJE_DEDUP_GP_LA_NPT
-- - join with first identified date
SELECT u.*, s1.FIRST_IDENTIFIED_DATE FROM SAILW1429V.PEJE_GP_LA_NPT_STEP1 s1
LEFT JOIN (
	-- - Union GP and LA key columns 
	SELECT * FROM SAILW1429V.PEJE_GP_LA_NPT_STEP2
	UNION ALL
	SELECT * FROM SAILW1429V.PEJE_GP_LA_NPT_STEP3
) u 
ON s1.Main_ALF_PE = u.ALF_PE; 

-- check that one row per ALF
SELECT count(DISTINCT ALF_PE) FROM SAILW1429V.PEJE_DEDUP_GP_LA_NPT; 

-- drop interim tables
DROP TABLE SAILW1429V.PEJE_GP_LA_NPT_STEP1;
DROP TABLE SAILW1429V.PEJE_GP_LA_NPT_STEP2;
DROP TABLE SAILW1429V.PEJE_GP_LA_NPT_STEP3;
