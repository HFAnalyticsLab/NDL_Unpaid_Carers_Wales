
--------------------------------------------
-- LINKED CARERS' DATA
--------------------------------------------
DROP TABLE SAILW1429V.LB_SWANSEA_CARERS;
DROP TABLE SAILW1429V.LB_LA_CARER_SWANSEA_PREP;
DROP TABLE SAILW1429V.LB_LA_CARER_SWANSEA;


-- COUNT NO. ASSESSMENT 'RECORDS'
SELECT 
	COUNT(*)
FROM 
	SAIL1429V.UPCD_SW_CARER_SWANSEA_20220810; 

-- COUNT NO. CARERS ACCORDING TO LA DATA.
SELECT 
	COUNT(*)
FROM 
	SAIL1429V.UPCD_SW_CARER_SWANSEA_ALF_20220810; 
	
	
-- CREATE TABLE CONTAINING ASSESSMENT INFO.
CREATE TABLE SAILW1429V.LB_SWANSEA_CARERS (ALF_PE BIGINT,
ALF_STS_CD INTEGER,
FIRST_ASSESS DATE,
LAST_ASSESS DATE,
NUMBEROFCARERASSESSMENTCOMPLETED INTEGER);

-- POPULATE WITH DATA FROM ASSESSMENT TABLE.
INSERT
	INTO
	SAILW1429V.LB_SWANSEA_CARERS
SELECT
	C.ALF_PE,
	C.ALF_STS_CD,
	A.FIRSTCARERASSESSMENTDATE AS FIRST_ASSESS,
	A.LASTCARERASSESSMENTDATE AS LAST_ASSESS,
	A.NUMBEROFCARERASSESSMENTCOMPLETED
FROM
	SAIL1429V.UPCD_SW_CARER_SWANSEA_20220810 A
-- JOIN IN CARER TABLE TO GET CARER ALF.
LEFT JOIN SAIL1429V.UPCD_SW_CARER_SWANSEA_ALF_20220810 C ON
	A.SYSTEM_ID_PE = C.SYSTEM_ID_PE
WHERE 
	C.ALF_STS_CD IN(1,
	4,
	39);


-- FOR THOSE WHO HAVE A LAST ASSESS DATE EARLIER THAN THEIR FIRST, NULL THESE LAST ASSESSMENT DATES.
UPDATE
	SAILW1429V.LB_SWANSEA_CARERS
SET
		LAST_ASSESS =
	CASE
		WHEN FIRST_ASSESS <= LAST_ASSESS
		OR LAST_ASSESS IS NULL THEN LAST_ASSESS
		ELSE NULL
	END;


-- CREATE TABLE TO START ADDING IN DEMOGRAPHICS (DONE IN SEPARATE STAGES FOR EASE OF CREATING EXCLUSION FLOW CHART)
CREATE TABLE SAILW1429V.LB_LA_CARER_SWANSEA_PREP (ALF_PE BIGINT,
SEX INTEGER,
WOB DATE,
FIRST_ASSESS DATE,
LAST_ASSESS DATE,
NUMBEROFCARERASSESSMENTCOMPLETED INTEGER);

-- ADD IN SEX & WOB
INSERT
	INTO
	SAILW1429V.LB_LA_CARER_SWANSEA_PREP
SELECT
	S.ALF_PE,
	D.GNDR_CD AS SEX,
	D.WOB,
	S.FIRST_ASSESS,
	S.LAST_ASSESS,
	S.NUMBEROFCARERASSESSMENTCOMPLETED
FROM
	SAILW1429V.LB_SWANSEA_CARERS S
LEFT JOIN SAIL1429V.WDSD_AR_PERS_20220704 D ON
	S.ALF_PE = D.ALF_PE; 
	
-- DROP SEX WHERE NOT 1 OR 2)
DELETE
FROM
	SAILW1429V.LB_LA_CARER_SWANSEA_PREP
WHERE
	SEX NOT IN (1,
	2)
	OR SEX IS NULL;



DELETE
FROM
	SAILW1429V.LB_LA_CARER_SWANSEA_PREP
WHERE
	WOB IS NULL;

	
-- CREATE TABLE TO ADD GEOG DEMOGRAPHICS
CREATE TABLE SAILW1429V.LB_LA_CARER_SWANSEA (ALF_PE BIGINT,
SEX INTEGER,
WOB DATE,
LSOA2011_CD VARCHAR(10),
LSOA_DESC VARCHAR(17),
WIMD INTEGER,
RUC CHARACTER(2),
RUC_DESC VARCHAR(47),
FIRST_ASSESS DATE,
LAST_ASSESS DATE,
NUMBEROFCARERASSESSMENTCOMPLETED INTEGER);

-- ADD IN LSOA (ON ASSESS DATE), WIMD, RUC
INSERT
	INTO
	SAILW1429V.LB_LA_CARER_SWANSEA
SELECT
	S.ALF_PE,
	S.SEX,
	S.WOB,
	G.LSOA2011_CD,
	G.LSOA_DESC,
	G.WIMD_2019_QUINTILE,
	R.RUC11CD AS RUC,
	R.RUC11 AS RUC_DESC,
	S.FIRST_ASSESS,
	S.LAST_ASSESS,
	S.NUMBEROFCARERASSESSMENTCOMPLETED
FROM
	SAILW1429V.LB_LA_CARER_SWANSEA_PREP S
LEFT JOIN SAIL1429V.WDSD_CLEAN_ADD_GEOG_CHAR_LSOA2011_20220704 G ON
	G.ALF_PE = S.ALF_PE
LEFT JOIN SAILREFRV.RURAL_URBAN_CLASS_2011_OF_LLSOAREAS_IN_ENG_AND_WAL R ON
	R.LSOA11CD = G.LSOA2011_CD
WHERE
	G.START_DATE <= S.FIRST_ASSESS
	AND G.END_DATE >= S.FIRST_ASSESS;


-- REMOVE ROWS WHERE LA ISN'T SWANSEA
DELETE 
FROM 
	SAILW1429V.LB_LA_CARER_SWANSEA
WHERE LSOA_DESC != 'Swansea'; 

-- ADD A COLUMN FOR AGE
 ALTER TABLE SAILW1429V.LB_LA_CARER_SWANSEA ADD COLUMN AGE INTEGER;
-- CALCULATE AGE
 UPDATE
	SAILW1429V.LB_LA_CARER_SWANSEA
SET
	AGE = YEARS_BETWEEN(FIRST_ASSESS,
	WOB);


-- REMOVE THOSE UNDER 18
 DELETE
FROM
	SAILW1429V.LB_LA_CARER_SWANSEA
WHERE
	AGE < 18;



-- ARE PEOPLE IN MULTIPLE ROWS?
-- ROW COUNT
 SELECT
	COUNT(*)
FROM
	SAILW1429V.LB_LA_CARER_SWANSEA;


-- PERSON COUNT
 SELECT
	COUNT(UNIQUE(ALF_PE))
FROM
	SAILW1429V.LB_LA_CARER_SWANSEA;


-- WHO IS THIS PERSON?
SELECT
	*
FROM
	SAILW1429V.LB_LA_CARER_SWANSEA
WHERE
	ALF_PE IN(
	SELECT
		ALF_PE
	FROM
		(
		SELECT
			ALF_PE,
			COUNT(*)
		FROM
			SAILW1429V.LB_LA_CARER_SWANSEA
		GROUP BY
			ALF_PE
		HAVING
			COUNT(*) > 1));


		
--SELECT THE EARLIEST FIRST ASSESSMENT DATE AND LATEST LAST ASSESSMENT DATE FROM THE TWO ROWS FOR THIS INDIVIDUAL
UPDATE
	SAILW1429V.LB_LA_CARER_SWANSEA
SET
	(ALF_PE,
	SEX,
	WOB,
	LSOA2011_CD,
	LSOA_DESC,
	WIMD,
	RUC,
	RUC_DESC,
	FIRST_ASSESS,
	LAST_ASSESS,
	NUMBEROFCARERASSESSMENTCOMPLETED,
	AGE) =
	-- SELECT ALL COLUMNS SO CAN GROUP AND SELECT MIN FIRST ASSESS AND MAX LAST ASSESS
(
	SELECT
		DISTINCT ALF_PE,
		SEX,
		WOB,
		LSOA2011_CD,
		LSOA_DESC,
		WIMD,
		RUC,
		RUC_DESC,
		MIN(FIRST_ASSESS),
		NULL AS LAST_ASSESS, -- NULL AS SECOND ROW HAS A LATER FIRST ASSESS, DON'T ACTUALLY KNOW WHAT THEIR LAST ASSESS DATE IS
		NUMBEROFCARERASSESSMENTCOMPLETED,
		MIN(AGE)
	FROM
		SAILW1429V.LB_LA_CARER_SWANSEA
		-- SPECIFY ONLY WANT TO TAKE MIN AND MAX FOR THE ALF WITH MORE THAN 1 ROW (ON REFLECTION, PROBABLY NOT NECCESSARY, AS ALL OTHERS WILL JUST FORM A GROUP OF ONE AND STAY AS THEY ARE?)
	WHERE ALF_PE IN(
		SELECT
			ALF_PE
		FROM
			(
			SELECT
				ALF_PE,
				COUNT(*)
			FROM
				SAILW1429V.LB_LA_CARER_SWANSEA
			GROUP BY
				ALF_PE
			HAVING
				COUNT(*) > 1))
		-- GROUP BY ALL VARIABLES OTHER THAN ASSESS DATES
		GROUP BY ALF_PE,
		SEX,
		WOB,
		LSOA2011_CD,
		LSOA_DESC,
		WIMD,
		RUC,
		RUC_DESC,
		NUMBEROFCARERASSESSMENTCOMPLETED)
	-- SPECIFY ONLY OVERWRITE ROWS WHERE ALF IS IN MULTIPLE ROWS
	WHERE ALF_PE IN(
	SELECT
		ALF_PE
	FROM
		(
		SELECT
			ALF_PE,
			COUNT(*)
		FROM
			SAILW1429V.LB_LA_CARER_SWANSEA
		GROUP BY
			ALF_PE
		HAVING
			COUNT(*) > 1));
 

-- GENERATE ROW NUMBERS, AND REMOVE THOSE GREATER THAN 1
 DELETE
FROM
	(
	SELECT
		ROW_NUMBER() OVER (PARTITION BY ALF_PE,
		SEX,
		WOB,
		LSOA2011_CD,
		LSOA_DESC,
		WIMD,
		RUC,
		RUC_DESC,
		FIRST_ASSESS,
		LAST_ASSESS,
		NUMBEROFCARERASSESSMENTCOMPLETED,
		AGE) AS ROW_NUM
	FROM
		SAILW1429V.LB_LA_CARER_SWANSEA)
WHERE
	ROW_NUM > 1;




-- REMOVE THOSE NOT RESIDENT IN WALES FOR 1 YR BEFORE FIRST ASSESS DATE.
-- ADD WELSH FLAG COLUMN
 ALTER TABLE SAILW1429V.LB_LA_CARER_SWANSEA ADD COLUMN WALES_FLAG INTEGER;
-- MERGE WDSD_ADD_WALES TABLE, SET WALES_FLAG TO 1 FOR THOSE RESIDENT IN WALES ON THEIR FIRST ASSESS DATE AND FOR A YEAR BEFORE
 MERGE
INTO
	SAILW1429V.LB_LA_CARER_SWANSEA A
		USING (
	SELECT
		A.*
	FROM
		SAILW1429V.LB_LA_CARER_SWANSEA A
	LEFT JOIN SAIL1429V.WDSD_CLEAN_ADD_WALES_20220704 W ON
		A.ALF_PE = W.ALF_PE
	WHERE
		W.WELSH_ADDRESS = 1
		AND (A.FIRST_ASSESS - 1 YEAR >= W.START_DATE)
		AND (A.FIRST_ASSESS <= W.END_DATE)) B ON
	A.ALF_PE = B.ALF_PE
	WHEN MATCHED THEN
UPDATE
SET
	WALES_FLAG = 1;


-- DELETE ROWS WHERE NOT RESIDENT IN WALES ON THEIR FIRST ASSESS DATE AND FOR A YEAR BEFORE
 DELETE
FROM
	SAILW1429V.LB_LA_CARER_SWANSEA
WHERE
	WALES_FLAG IS NULL; 



-- REMOVE THOSE NOT REGISTERED AT A SAIL GP FOR 1 YR BEFORE FIRST ASSESS DATE.
-- ADD GP FLAG COLUMN
 ALTER TABLE SAILW1429V.LB_LA_CARER_SWANSEA ADD COLUMN SAIL_GP_FLAG INTEGER;
-- MERGE LB_SAIL_GP_REG_LOOKUP TABLE, SET SAIL_GP_FLAG TO 1 FOR THOSE RESIDENT IN WALES ON THEIR FIRST ASSESS DATE AND FOR A YEAR BEFORE
 MERGE
INTO
	SAILW1429V.LB_LA_CARER_SWANSEA A
		USING (
	SELECT
		A.*
	FROM
		SAILW1429V.LB_LA_CARER_SWANSEA A
	LEFT JOIN SAILW1429V.LB_SAIL_GP_REG_LOOKUP S ON
		A.ALF_PE = S.ALF_PE
	WHERE
		(A.FIRST_ASSESS - 1 YEAR >= S.START_DATE)
		AND (A.FIRST_ASSESS <= S.END_DATE)) B ON
	A.ALF_PE = B.ALF_PE
	WHEN MATCHED THEN
UPDATE
SET
	SAIL_GP_FLAG = 1;


-- DELETE ROWS WHERE NOT REGISTER WITH SAIL GP ON THEIR FIRST ASSESS DATE AND FOR A YEAR BEFORE
 DELETE
FROM
	SAILW1429V.LB_LA_CARER_SWANSEA
WHERE
	SAIL_GP_FLAG IS NULL;



-- ADD COLUMN FOR CARER DOD
 ALTER TABLE SAILW1429V.LB_LA_CARER_SWANSEA ADD COLUMN DOD_CARER DATE;
-- ADD CARER DOD
 MERGE
INTO
	SAILW1429V.LB_LA_CARER_SWANSEA A
		USING(
	SELECT
		B.*,
		D.DEATH_DT
	FROM
		SAILW1429V.LB_LA_CARER_SWANSEA B
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


-- REMOVE THOSE WHO DIED BEFORE FIRST ASSESS DATE
 DELETE
FROM
	SAILW1429V.LB_LA_CARER_SWANSEA
WHERE
	DOD_CARER < FIRST_ASSESS;


-- CHECK ROW COUNT
 SELECT
	COUNT(*)
FROM
	SAILW1429V.LB_LA_CARER_SWANSEA;


-- CHECK PERSON COUNT
 SELECT
	COUNT(UNIQUE(ALF_PE))
FROM
	SAILW1429V.LB_LA_CARER_SWANSEA;



