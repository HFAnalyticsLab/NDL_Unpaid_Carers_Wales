-------------------------------------------------------------
/* Swansea Outpatients: 1:1 Match */
-------------------------------------------------------------

/* STEPS:
 * 1. Get OPDW infor for those with an attendance in the year prior to being identified as an unpaid crer
 *  - 1.1 Select main variables from deduplicated table (PEJE_MATCHED_COHORT_SWANSEA_2)
 *  - 1.2 Left join to OPDW
 *  - 1.3 ... where event was in year prior to being identified as a carer
 *  - 1.4 ... and OPDW ALF_STS_CD in 1,4, 39
 *  - 1.5 Pull in outpatient event details (attend_dt, attend_cd)
 * 2. Join deduplicated carer table to OPDW info for those with an attendance in the year prior to being identified as a carer from step 1
 * 
 */

DROP TABLE SAILW1429V.GBH_MATCHEDCOHORT_OPDW_SWANSEA_2_STEP1;
DROP TABLE SAILW1439V.GBH_MATCHEDCOHORT_OPDW_SWANSEA_2;

-- Create step 1 table
CREATE TABLE SAILW1429V.GBH_MATCHEDCOHORT_OPDW_SWANSEA_2_STEP1 (
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
ATTEND_DT DATE, 
ATTEND_CD INTEGER);

-- Insert values to the above table
INSERT INTO SAILW1429V.GBH_MATCHEDCOHORT_OPDW_SWANSEA_2_STEP1 
-- 1. Get OPDW info for those with an attendance in the year prior to being identified as an unpaid carer 
-- 1.1 Select main variables from deduplicated table (PEJE_MATCHED_COHORT_SWANSEA)
SELECT
	DISTINCT S.ALF_PE,
	S.SEX,
	S.AGE_INDEXDATE, 
	S.WOB,
	S.WIMD, 
	S.RUC11_DESC,
	'Swansea' AS LA_NAME,
	S.FIRST_IDENTIFIED_DATE,
	S.FIRST_IDENTIFIED_BY,
	S.TOTAL_NUM_ASSESSMENTS,
-- 1.5 Pull in outpatient event details
	OPDW.ATTEND_DT,
	OPDW.ATTEND_CD
FROM SAILW1429V.PEJE_MATCHED_COHORT_SWANSEA_2 S
-- 1.2 Left join to OPDW
LEFT JOIN SAIL1429V.OPDW_OUTPATIENTS_20220501 OPDW ON
	S. ALF_PE = OPDW.ALF_PE
WHERE 
	-- 1.3 ... where event was in year prior to being identified as being a carer 
	OPDW.ATTEND_DT BETWEEN (S.FIRST_IDENTIFIED_DATE - 1 YEAR) AND (S.FIRST_IDENTIFIED_DATE - 1 DAY)
	-- 1.4 ... and OPDW ALF_STS_CD in 1, 4 or 39
	AND OPDW.ALF_STS_CD IN(1,4,39); 


-- In the first table we created a df using the matched cohort with attendances, 
-- and now in the second table we adding back in the rest of the matched cohort that don't have
-- attendances (should be null)
CREATE TABLE SAILW1429V.GBH_MATCHEDCOHORT_OPDW_SWANSEA_2 (
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
	ATTEND_DT DATE,
	ATTEND_CD INTEGER);

INSERT INTO SAILW1429V.GBH_MATCHEDCOHORT_OPDW_SWANSEA_2
SELECT DISTINCT *
FROM(
	SELECT
		DISTINCT S.ALF_PE,
		S.SEX,
		S.SEX_DESC,
		S.AGE_INDEXDATE,
		S.WOB,
		S.WIMD,
		S.WIMD_DESC,
		S.RUC11_DESC,
		'Swansea' AS LA_NAME,
		S.FIRST_IDENTIFIED_DATE,
		S.FIRST_IDENTIFIED_BY,
		S.TOTAL_NUM_ASSESSMENTS,
		S.TREATED,
		B.ATTEND_DT,
		B.ATTEND_CD
	FROM SAILW1429V.PEJE_MATCHED_COHORT_SWANSEA_2 S
	-- 2.Join deduplicated carer table to OPDW info for those with an admission in the year prior to being identified as a carer from step 1
	LEFT JOIN (SELECT * 
		FROM SAILW1429V.GBH_MATCHEDCOHORT_OPDW_SWANSEA_2_STEP1) B
		ON S.ALF_PE = B.ALF_PE AND S.FIRST_IDENTIFIED_DATE = B.FIRST_IDENTIFIED_DATE); 


