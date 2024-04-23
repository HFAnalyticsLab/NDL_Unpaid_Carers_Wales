-------------------------------------------------------------
/* SWANSEA Inpatients */
-------------------------------------------------------------

/* STEPS:
 * 1. Get PEDW info for those with a planned/emergency admission in the year prior to being identified as a carer
 * 	- 1.1. Select main variables from deduplicated table (PEJE_MATCHED_COHORT_SWANSEA_2_2)
 * 	- 1.2. Left join to PEDW 
 * 	- 1.3. ...where event was in year prior to being identified as a carer
 * 	- 1.4. ...and admission was planned/emergency
 * 	- 1.5. ...and PEDW ALF_STS_CD in 1, 4, 39
 *  - 1.6. Pull in inpatient event details (admis_date, admis_mthd, disch_date)
 * 	- 1.7. flag planned admissions with a 0 and emergency admissions with a 1
 * 2. Join deduplicated carer table to PEDW info for those with a planned/emergency admission in the year prior to being identified as a carer from step 1
 * 
 */

DROP TABLE SAILW1429V.PEJE_MATCHEDCOHORT_INPATIENT_SWANSEA_2_STEP1;

CREATE TABLE SAILW1429V.PEJE_MATCHEDCOHORT_INPATIENT_SWANSEA_2_STEP1
(ALF_PE BIGINT
, SEX INTEGER
, AGE INTEGER
, WOB DATE
, ETHNICITY_DESC VARCHAR(7)
, LSOA2011_CD VARCHAR(15)
, WIMD INTEGER
, RUC VARCHAR(50)
, LA_NAME VARCHAR(30)
, FIRST_IDENTIFIED_DATE DATE
, IDENTIFIEDBY VARCHAR(3)
, TOTAL_NUM_ASSESSMENTS INTEGER
, ADMIS_DT DATE
, ADMIS_MTHD_CD INTEGER
, ADMIS_TYPE INTEGER
, DISCH_DT DATE
);

INSERT INTO SAILW1429V.PEJE_MATCHEDCOHORT_INPATIENT_SWANSEA_2_STEP1
-- 1. Get PEDW info for those with a planned/emergency admission in the year prior to being identified as a carer
-- 1.1. Select main variables from deduplicated table (PEJE_MATCHED_COHORT_SWANSEA_2_2)
SELECT
	DISTINCT J.ALF_PE,
	J.SEX,
	J.AGE_INDEXDATE ,
	J.WOB,
	J.ETHNICITY_DESC, 
	J.LSOA2011_CD,
	J.WIMD,
	J.RUC11_DESC,
	'Swansea' AS LA_NAME,
	J.FIRST_IDENTIFIED_DATE,
	J.FIRST_IDENTIFIED_BY,
	J.TOTAL_NUM_ASSESSMENTS,
	-- 1.6. Pull in inpatient details
	PEDW.ADMIS_DT,
	PEDW.ADMIS_MTHD_CD,
	-- 1.7. flag planned admissions with a 0 and emergency admissions with a 1
	CASE
		WHEN PEDW.ADMIS_MTHD_CD BETWEEN '11' AND '15' THEN 0
		WHEN ((PEDW.ADMIS_MTHD_CD BETWEEN '21' AND '25')
		OR (PEDW.ADMIS_MTHD_CD IN('27',
		'28'))) THEN 1
	END AS ADMIS_TYPE,
	PEDW.DISCH_DT
FROM
	SAILW1429V.PEJE_MATCHED_COHORT_SWANSEA_2 J
-- 1.2. Left join to PEDW 
LEFT JOIN SAIL1429V.PEDW_SPELL_20220704 PEDW ON
	J.ALF_PE = PEDW.ALF_PE
WHERE
	-- 1.3. ...where event was in year prior to being identified as a carer
	PEDW.ADMIS_DT BETWEEN (J.FIRST_IDENTIFIED_DATE - 1 YEAR) AND (J.FIRST_IDENTIFIED_DATE - 1 DAY)
	-- 1.4. ...and admission was planned/emergency
	AND ((PEDW.ADMIS_MTHD_CD BETWEEN '11' AND '15')
	OR (PEDW.ADMIS_MTHD_CD BETWEEN '21' AND '25')
	OR (PEDW.ADMIS_MTHD_CD IN('27',	'28')))
	-- 1.5. ...and PEDW ALF_STS_CD in 1, 4, 39
	AND PEDW.ALF_STS_CD IN(1,4,39); 




DROP TABLE SAILW1429V.PEJE_MATCHEDCOHORT_INPATIENT_SWANSEA_2;

CREATE TABLE SAILW1429V.PEJE_MATCHEDCOHORT_INPATIENT_SWANSEA_2 (
	ALF_PE BIGINT,
	SEX INTEGER,
	SEX_DESC VARCHAR(10),
	AGE INTEGER,
	WOB DATE,
	ETHNICITY_DESC VARCHAR(7),
	LSOA2011_CD VARCHAR(15),
	WIMD INTEGER,
	WIMD_DESC VARCHAR(50),
	RUC_DESC VARCHAR(50),
	LA_NAME VARCHAR(100),
	FIRST_IDENTIFIED_DATE DATE,
	FIRST_IDENTIFIED_BY VARCHAR(10),
	TOTAL_NUM_ASSESSMENTS INTEGER,
	CARER_FLAG INTEGER,
	ADMIS_DT DATE,
	ADMIS_MTHD_CD INTEGER,
	ADMIS_TYPE INTEGER,
	DISCH_DT DATE);



INSERT	INTO SAILW1429V.PEJE_MATCHEDCOHORT_INPATIENT_SWANSEA_2
SELECT 	DISTINCT *
FROM (
	SELECT
		DISTINCT J.ALF_PE,
		J.SEX,
		J.SEX_DESC, 
		J.AGE_INDEXDATE ,
		J.WOB,
		J.ETHNICITY_DESC, 
		J.LSOA2011_CD,
		J.WIMD,
		J.WIMD_DESC, 
		J.RUC11_DESC,
		'Swansea' AS LA_NAME,
		J.FIRST_IDENTIFIED_DATE,
		J.FIRST_IDENTIFIED_BY,
		J.TOTAL_NUM_ASSESSMENTS,
		J.TREATED, 
		B.ADMIS_DT,
		B.ADMIS_MTHD_CD,
		B.ADMIS_TYPE,
		B.DISCH_DT
	FROM
		SAILW1429V.PEJE_MATCHED_COHORT_SWANSEA_2 J
	-- 2.Join deduplicated carer table to PEDW info for those with a planned/emergency admission in the year prior to being identified as a carer from step 1
	LEFT JOIN (SELECT *  FROM SAILW1429V.PEJE_MATCHEDCOHORT_INPATIENT_SWANSEA_2_STEP1 ) B 
	ON 	J.ALF_PE = B.ALF_PE	AND J.FIRST_IDENTIFIED_DATE = B.FIRST_IDENTIFIED_DATE ) C; 
	
	
-- DROP interim TABLE
DROP TABLE SAILW1429V.PEJE_MATCHEDCOHORT_INPATIENT_SWANSEA_2_STEP1;
