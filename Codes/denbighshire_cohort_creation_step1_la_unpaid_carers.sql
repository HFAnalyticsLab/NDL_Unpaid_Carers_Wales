
--------------------------------------------
-- LINKED CARERS' DATA
--------------------------------------------
DROP TABLE SAILW1429V.LB_DEN_CARERS;
DROP TABLE SAILW1429V.LB_LA_CARER_DEN;


-- Count no. assessment 'records'
SELECT 
	COUNT(*)
FROM 
	SAIL1429V.UPCD_DB_CARER_DENBIGHSHIRE_20221216;
	
SELECT 
	COUNT(UNIQUE(SYSTEM_ID_PE))
FROM 
	SAIL1429V.UPCD_DB_CARER_DENBIGHSHIRE_20221216;

-- Count no. carers according to LA data.
SELECT 
	COUNT(*)
FROM 
	SAIL1429V.UPCD_DB_CARER_DENBIGHSHIRE_ALF_20221217; 

SELECT 
	COUNT(UNIQUE(SYSTEM_ID_PE))
FROM 
	SAIL1429V.UPCD_DB_CARER_DENBIGHSHIRE_ALF_20221217; 

SELECT 
	COUNT(UNIQUE(ALF_PE))
FROM 
	SAIL1429V.UPCD_DB_CARER_DENBIGHSHIRE_ALF_20221217; 

-- How many records can be joined on system ID?
SELECT
	COUNT(*)
FROM
	SAIL1429V.UPCD_DB_CARER_DENBIGHSHIRE_20221216 A
-- Join in carer table to get carer ALF.
LEFT JOIN SAIL1429V.UPCD_DB_CARER_DENBIGHSHIRE_ALF_20221217 C ON
	A.SYSTEM_ID_PE = C.SYSTEM_ID_PE; 

SELECT COUNT(*) FROM(
SELECT
	C.ALF_PE,
	A.REFERRAL_DATE 
FROM
	SAIL1429V.UPCD_DB_CARER_DENBIGHSHIRE_20221216 A
-- Join in carer table to get carer ALF.
LEFT JOIN SAIL1429V.UPCD_DB_CARER_DENBIGHSHIRE_ALF_20221217 C ON
	A.SYSTEM_ID_PE = C.SYSTEM_ID_PE 
WHERE 
	C.ALF_STS_CD IN(1,
	4,
	39);
);

-- Create table containing assessment info.
CREATE TABLE SAILW1429V.LB_DEN_CARERS (
ALF_PE BIGINT,
REFERRAL_DATE DATE);

-- Populate with data from assessment table.
INSERT
	INTO
	SAILW1429V.LB_DEN_CARERS
SELECT
	C.ALF_PE,
	MIN(A.REFERRAL_DATE) AS REFERRAL_DATE -- select earliest referral date per person
FROM
	SAIL1429V.UPCD_DB_CARER_DENBIGHSHIRE_20221216 A
-- Join in carer table to get carer ALF.
LEFT JOIN SAIL1429V.UPCD_DB_CARER_DENBIGHSHIRE_ALF_20221217 C ON
	A.SYSTEM_ID_PE = C.SYSTEM_ID_PE 
WHERE 
	C.ALF_STS_CD IN(1,
	4,
	39)
GROUP BY ALF_PE;


-- How many unique carers?
SELECT COUNT(UNIQUE(ALF_PE))
FROM SAILW1429V.LB_DEN_CARERS; 


-- Create table to start adding in demographics (Done in separate stages for ease of creating exclusion flow chart)
 CREATE TABLE SAILW1429V.LB_LA_CARER_DEN_PREP (ALF_PE BIGINT,
SEX INTEGER,
WOB DATE,
REFERRAL_DATE DATE);

-- Add in sex & WOB 
INSERT
	INTO
	SAILW1429V.LB_LA_CARER_DEN_PREP
SELECT
	S.ALF_PE,
	D.GNDR_CD AS SEX,
	D.WOB,
	S.REFERRAL_DATE
FROM
	SAILW1429V.LB_DEN_CARERS S
LEFT JOIN SAIL1429V.WDSD_AR_PERS_20220704 D ON
	S.ALF_PE = D.ALF_PE;


-- Drop sex where not 1 or 2)
DELETE
FROM
	SAILW1429V.LB_LA_CARER_DEN_PREP
WHERE
	SEX NOT IN (1,
	2)
	OR SEX IS NULL;


-- Drop null WOB
DELETE
FROM
	SAILW1429V.LB_LA_CARER_DEN_PREP
WHERE
	WOB IS NULL;


SELECT COUNT(UNIQUE(ALF_PE)) FROM SAILW1429V.LB_LA_CARER_DEN_PREP; 

-- Create table to add geog demographics
CREATE TABLE SAILW1429V.LB_LA_CARER_DEN (ALF_PE BIGINT,
	SEX INTEGER,
	WOB DATE,
	LSOA2011_CD VARCHAR(10),
	LSOA_DESC VARCHAR(17),
	WIMD INTEGER,
	RUC CHARACTER(2),
	RUC_DESC VARCHAR(47),
	REFERRAL_DATE DATE);

-- Add in LSOA (on assess date), WIMD, RUC
INSERT
	INTO
	SAILW1429V.LB_LA_CARER_DEN
SELECT
	S.ALF_PE,
	S.SEX,
	S.WOB,
	G.LSOA2011_CD,
	G.LSOA_DESC,
	G.WIMD_2019_QUINTILE AS WIMD,
	R.RUC11CD AS RUC,
	R.RUC11 AS RUC_DESC,
	S.REFERRAL_DATE
FROM
	SAILW1429V.LB_LA_CARER_DEN_PREP S
LEFT JOIN SAIL1429V.WDSD_CLEAN_ADD_GEOG_CHAR_LSOA2011_20220704 G ON
	G.ALF_PE = S.ALF_PE
LEFT JOIN SAILREFRV.RURAL_URBAN_CLASS_2011_OF_LLSOAREAS_IN_ENG_AND_WAL R ON
	R.LSOA11CD = G.LSOA2011_CD
WHERE
	G.START_DATE <= S.REFERRAL_DATE
	AND G.END_DATE >= S.REFERRAL_DATE;


-- remove rows where LA isn't Denbighshire
DELETE 
FROM 
	SAILW1429V.LB_LA_CARER_DEN
WHERE LSOA_DESC != 'Denbighshire'; 

-- ADD a COLUMN FOR age
 ALTER TABLE SAILW1429V.LB_LA_CARER_DEN ADD COLUMN AGE INTEGER;
-- Calculate age
 UPDATE
	SAILW1429V.LB_LA_CARER_DEN
SET
	AGE = YEARS_BETWEEN(REFERRAL_DATE,
	WOB);


-- remove those under 18
 DELETE
FROM
	SAILW1429V.LB_LA_CARER_DEN
WHERE
	AGE < 18;



-- ARE people IN multiple ROWS?
-- Row count
 SELECT
	COUNT(*)
FROM
	SAILW1429V.LB_LA_CARER_DEN;


-- Person count
 SELECT
	COUNT(UNIQUE(ALF_PE))
FROM
	SAILW1429V.LB_LA_CARER_DEN;


-- Remove those not resident in Wales for 1 yr before first assess date.
-- Add welsh flag column
 ALTER TABLE SAILW1429V.LB_LA_CARER_DEN ADD COLUMN WALES_FLAG INTEGER;
-- Merge WDSD_ADD_WALES table, set WALES_FLAG to 1 for those resident in Wales on their first assess date and for a year before
 MERGE
INTO
	SAILW1429V.LB_LA_CARER_DEN A
		USING (
	SELECT
		A.*
	FROM
		SAILW1429V.LB_LA_CARER_DEN A
	LEFT JOIN SAIL1429V.WDSD_CLEAN_ADD_WALES_20220704 W ON
		A.ALF_PE = W.ALF_PE
	WHERE
		W.WELSH_ADDRESS = 1
		AND (A.REFERRAL_DATE - 1 YEAR >= W.START_DATE)
		AND (A.REFERRAL_DATE <= W.END_DATE)) B ON
	A.ALF_PE = B.ALF_PE
	WHEN MATCHED THEN
UPDATE
SET
	WALES_FLAG = 1;


-- Delete rows where not resident in Wales on their first assess date and for a year before
 DELETE
FROM
	SAILW1429V.LB_LA_CARER_DEN
WHERE
	WALES_FLAG IS NULL; 



-- Remove those not registered at a SAIL GP for 1 yr before first assess date.
-- Add GP flag column
 ALTER TABLE SAILW1429V.LB_LA_CARER_DEN ADD COLUMN SAIL_GP_FLAG INTEGER;
-- Merge LB_SAIL_GP_REG_LOOKUP table, set SAIL_GP_FLAG to 1 for those resident in Wales on their first assess date and for a year before
 MERGE
INTO
	SAILW1429V.LB_LA_CARER_DEN A
		USING (
	SELECT
		A.*
	FROM
		SAILW1429V.LB_LA_CARER_DEN A
	LEFT JOIN SAILW1429V.LB_SAIL_GP_REG_LOOKUP S ON
		A.ALF_PE = S.ALF_PE
	WHERE
		(A.REFERRAL_DATE - 1 YEAR >= S.START_DATE)
		AND (A.REFERRAL_DATE <= S.END_DATE)) B ON
	A.ALF_PE = B.ALF_PE
	WHEN MATCHED THEN
UPDATE
SET
	SAIL_GP_FLAG = 1;


-- Delete rows where not register with SAIL GP on their first assess date and for a year before
 DELETE
FROM
	SAILW1429V.LB_LA_CARER_DEN
WHERE
	SAIL_GP_FLAG IS NULL;



-- Add column for carer DOD
 ALTER TABLE SAILW1429V.LB_LA_CARER_DEN ADD COLUMN DOD_CARER DATE;
-- Add carer DOD
 MERGE
INTO
	SAILW1429V.LB_LA_CARER_DEN A
		USING(
	SELECT
		B.*,
		D.DEATH_DT
	FROM
		SAILW1429V.LB_LA_CARER_DEN B
	LEFT JOIN SAIL1429V.ADDE_DEATHS_20220701 D ON
		B.ALF_PE = D.ALF_PE
	WHERE
		D.ALF_STS_CD IN (1,
		4,
		39)) C ON
	A.ALF_PE = C.ALF_PE
	WHEN MATCHED THEN
UPDATE
SET
	DOD_CARER = C.DEATH_DT;


-- Remove those who died before first assess date
 DELETE
FROM
	SAILW1429V.LB_LA_CARER_DEN
WHERE
	DOD_CARER < REFERRAL_DATE;


-- Check row count
 SELECT
	COUNT(*)
FROM
	SAILW1429V.LB_LA_CARER_DEN;


-- Check person count
 SELECT
	COUNT(UNIQUE(ALF_PE))
FROM
	SAILW1429V.LB_LA_CARER_DEN;


