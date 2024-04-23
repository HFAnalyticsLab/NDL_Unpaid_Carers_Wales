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
SELECT count (DISTINCT ALF_PE) FROM SAILW1429V.LB_LA_CARER_SWANSEA; 
SELECT count (DISTINCT ALF_PE) FROM SAILW1429V.LB_SWANSEA_CARERS; 

DROP TABLE SAILW1429V.PEJE_GP_LA_SWA_STEP1;
DROP TABLE SAILW1429V.PEJE_GP_LA_SWA_STEP2;
DROP TABLE SAILW1429V.PEJE_GP_LA_SWA_STEP3;
DROP TABLE SAILW1429V.PEJE_DEDUP_GP_LA_SWANSEA;



-- STEP 1. Full join GP and LA Unpaid carers from existing tables
DROP TABLE SAILW1429V.PEJE_GP_LA_SWA_STEP1;

CREATE TABLE SAILW1429V.PEJE_GP_LA_SWA_STEP1
(
Main_ALF_PE BIGINT,
GP_ALF_PE BIGINT,
GP_IDENTIFIED_DATE DATE,
LA_ALF_PE BIGINT,
LA_IDENTIFIED_DATE DATE,
FIRST_IDENTIFIED_BY VARCHAR(3),
FIRST_IDENTIFIED_DATE DATE);

INSERT INTO SAILW1429V.PEJE_GP_LA_SWA_STEP1
SELECT 
CASE 
WHEN gp.EVENT_DT < la.FIRST_ASSESS OR (la.FIRST_ASSESS IS NULL AND gp.EVENT_DT IS NOT NULL)  THEN gp.ALF_PE
WHEN gp.EVENT_DT > la.FIRST_ASSESS OR (la.FIRST_ASSESS IS NOT NULL AND gp.EVENT_DT IS NULL) THEN la.ALF_PE
WHEN gp.EVENT_DT = la.FIRST_ASSESS AND (la.FIRST_ASSESS IS NOT NULL AND gp.EVENT_DT IS NOT NULL) THEN gp.ALF_PE
ELSE NULL
END AS Main_ALF_PE
, gp.ALF_PE AS gp_ALF
, gp.EVENT_DT AS GP_Identified_DT
, la.ALF_PE AS la_ALF
, la.FIRST_ASSESS AS la_First_Assess_DT 
-- a. Create first identified by flag
, CASE 
WHEN gp.EVENT_DT < la.FIRST_ASSESS OR (la.FIRST_ASSESS IS NULL AND gp.EVENT_DT IS NOT NULL)  THEN 'GP'
WHEN gp.EVENT_DT > la.FIRST_ASSESS OR (la.FIRST_ASSESS IS NOT NULL AND gp.EVENT_DT IS NULL) THEN 'LA'
WHEN gp.EVENT_DT = la.FIRST_ASSESS AND (la.FIRST_ASSESS IS NOT NULL AND gp.EVENT_DT IS NOT NULL) THEN 'EQ'
ELSE NULL
END AS First_Identified_By
--  b. Select first identified date
, CASE 
WHEN gp.EVENT_DT < la.FIRST_ASSESS OR (la.FIRST_ASSESS IS NULL AND gp.EVENT_DT IS NOT NULL)  THEN gp.EVENT_DT 
WHEN gp.EVENT_DT > la.FIRST_ASSESS OR (la.FIRST_ASSESS IS NOT NULL AND gp.EVENT_DT IS NULL) THEN la.FIRST_ASSESS 
WHEN gp.EVENT_DT = la.FIRST_ASSESS AND (la.FIRST_ASSESS IS NOT NULL AND gp.EVENT_DT IS NOT NULL) THEN gp.EVENT_DT 
ELSE NULL
END AS First_Identified_DT
FROM SAILW1429V.LB_GPCARER_SWANSEA gp
FULL OUTER JOIN 
(SELECT ALF_PE, FIRST_ASSESS FROM SAILW1429V.LB_LA_CARER_SWANSEA ) la 
ON gp.ALF_PE = la.ALF_PE; 


SELECT count(*) total, First_Identified_By FROM SAILW1429V.PEJE_GP_LA_SWA_STEP1
GROUP BY First_Identified_By; 

SELECT count(DISTINCT Main_ALF_PE) total, First_Identified_By FROM SAILW1429V.PEJE_GP_LA_SWA_STEP1
GROUP BY First_Identified_By; 


-- STEP 2. Select required columns from GP unpaid carers
DROP TABLE SAILW1429V.PEJE_GP_LA_SWA_STEP2;

CREATE TABLE SAILW1429V.PEJE_GP_LA_SWA_STEP2
(
ALF_PE BIGINT
, SEX INTEGER
, AGE INTEGER
, AGE_STUDYSTART INTEGER
, WOB DATE
, FIRST_ASSESS DATE
, LAST_ASSESS DATE
, NUMBEROFCARERASSESSMENTCOMPLETED INTEGER
, DEATH_DT DATE
, LSOA2011_CD VARCHAR(20)
, WIMD INTEGER
, RUC11CD VARCHAR(3)
, RUC11_DESC VARCHAR(30)
, LA_NAME VARCHAR(100)
, IdentifiedBy VARCHAR(10)
);

INSERT INTO SAILW1429V.PEJE_GP_LA_SWA_STEP2
WITH cte_gp_keycols AS 
(
	-- b. Get baseline characteristics
	SELECT ALF_PE, SEX, AGE, YEARS_BETWEEN('2021-04-01', WOB) AGE_STUDYSTART, WOB, EVENT_DT AS FIRST_ASSESS, NULL AS LAST_ASSESS
	, NULL AS NUMBEROFCARERASSESSMENTCOMPLETED, DEATH_DT, LSOA2011_CD, WIMD_2019_QUINTILE AS WIMD, RUC11CD, gp.RUC11, LA_NAME 
	, 'GP' AS IdentifiedBy
	FROM  SAILW1429V.LB_GPCARER_SWANSEA gp
	LEFT JOIN (
	-- c. Add RUC to GP cohort (left join)
		SELECT DISTINCT LSOA11CD, RUC11CD, RUC11
		FROM SAILREFRV.RURAL_URBAN_CLASS_2011_OF_LLSOAREAS_IN_ENG_AND_WAL
		) r	ON gp.LSOA2011_CD = r.LSOA11CD
	
),
cte_gp_carers AS 
(
	-- a. Select those who were first identified in the GP
	SELECT Main_ALF_PE FROM SAILW1429V.PEJE_GP_LA_SWA_STEP1
	WHERE First_Identified_By = 'GP'
)
-- c. Join a and b  (left join)
SELECT g.* FROM cte_gp_carers s
LEFT JOIN (SELECT * FROM cte_gp_keycols )g 
ON s.Main_ALF_PE = g.ALF_PE;




-- STEP 3. Select required columns from LA unpaid carers
DROP TABLE SAILW1429V.PEJE_GP_LA_SWA_STEP3;

CREATE TABLE SAILW1429V.PEJE_GP_LA_SWA_STEP3
(
ALF_PE BIGINT
, SEX INTEGER
, AGE INTEGER
, AGE_STUDYSTART INTEGER
, WOB DATE
, FIRST_ASSESS DATE
, LAST_ASSESS DATE
, NUMBEROFCARERASSESSMENTCOMPLETED INTEGER
, DEATH_DT DATE
, LSOA2011_CD VARCHAR(20)
, WIMD INTEGER
, RUC11CD VARCHAR(3)
, RUC11_DESC VARCHAR(30)
, LA_NAME VARCHAR(100)
, IdentifiedBy VARCHAR(10)
);
INSERT INTO SAILW1429V.PEJE_GP_LA_SWA_STEP3
WITH cte_la_keycols AS (
	-- b. Get baseline characteristics
	SELECT ALF_PE, SEX, AGE, YEARS_BETWEEN('2021-04-01', WOB) AGE_STUDYSTART, WOB, FIRST_ASSESS, LAST_ASSESS
	, NUMBEROFCARERASSESSMENTCOMPLETED, DOD_CARER AS DEATH_DT, LSOA2011_CD,WIMD, RUC,RUC_DESC, LA_NAME 
	, 'LA' AS IdentifiedBy
	FROM SAILW1429V.LB_LA_CARER_SWANSEA 
),
cte_la_carers AS 
(
	-- a. Select thos who were first identified in the GP
	SELECT Main_ALF_PE FROM SAILW1429V.PEJE_GP_LA_SWA_STEP1
	WHERE First_Identified_By = 'LA'
)
-- c. Join a and b  (left join)
SELECT l.* FROM cte_la_carers s
LEFT JOIN (SELECT * FROM cte_la_keycols ) l
ON s.Main_ALF_PE = l.ALF_PE; 




-- STEP 4. Union key columns from Steps 2 AND 3 
DROP TABLE SAILW1429V.PEJE_DEDUP_GP_LA_SWANSEA;

CREATE TABLE SAILW1429V.PEJE_DEDUP_GP_LA_SWANSEA
(
ALF_PE BIGINT
, SEX INTEGER
, AGE INTEGER
, AGE_STUDYSTART INTEGER
, WOB DATE
, FIRST_ASSESS DATE
, LAST_ASSESS DATE
, NUMBEROFCARERASSESSMENTCOMPLETED INTEGER
, DEATH_DT DATE
, LSOA2011_CD VARCHAR(20)
, WIMD INTEGER
, RUC11CD VARCHAR(3)
, RUC11_DESC VARCHAR(30)
, LA_NAME VARCHAR(100)
, IdentifiedBy VARCHAR(10)
, FIRST_IDENTIFIED_DATE DATE
);

INSERT INTO SAILW1429V.PEJE_DEDUP_GP_LA_SWANSEA
-- - join with first identified date
SELECT u.*, s1.FIRST_IDENTIFIED_DATE FROM SAILW1429V.PEJE_GP_LA_SWA_STEP1 s1
LEFT JOIN (
	-- - Union GP and LA key columns 
	SELECT * FROM SAILW1429V.PEJE_GP_LA_SWA_STEP2
	UNION ALL
	SELECT * FROM SAILW1429V.PEJE_GP_LA_SWA_STEP3
) u 
ON s1.Main_ALF_PE = u.ALF_PE; 

-- check that one row per ALF
SELECT count(DISTINCT ALF_PE) FROM SAILW1429V.PEJE_DEDUP_GP_LA_SWANSEA; 


-- drop interim tables
DROP TABLE SAILW1429V.PEJE_GP_LA_SWA_STEP1;
DROP TABLE SAILW1429V.PEJE_GP_LA_SWA_STEP2;
DROP TABLE SAILW1429V.PEJE_GP_LA_SWA_STEP3;


