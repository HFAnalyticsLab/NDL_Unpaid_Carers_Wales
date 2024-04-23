-----------------------------------------
/* CAMBRIDGE MORBIDITY SCORE - LTCs */
-----------------------------------------

/* STEPS:
 * 1. CANCER
 * 	1.1. select all from GP EVENT table...
 * 	1.2. ...with a cancer cms code
 * 	1.3. Join to deduplicated carer list to get all cancer Read code events for carers
 * 	1.4 Group by ALF_PE
 * 	1.5 Select earliest cancer Read code recorded
 * 	1.6 Limit to records within 5 years of index date
 * 2. KIDNEY
 * 	2.1. select all from GP EVENT table...
 * 	2.2. ...with a CKD cms code
 * 	2.3 Join to deduplicated carer list to get all CKD Read code events for carers
 * 	2.4 Group by ALF_PE, order by reverse event_dt (most recent first), add row number
 * 	2.5 Select most recent 2 rows per ALF (rownum <=2)
 * 	2.6. select max eGFR reading of the most recent 2 readings
 * 	2.7 Select patients where this max test value is <60ml/min
 * 3. ALL OTHER MEDCODES
 * 	3.1 select all from CAM_CODELIST with a 'medcode' readcode that's not cancer or CKD (these are addressed separately above)
 * 	3.2 Limit Read codes to first 5 chr
 * 	3.3 Join to GP_EVENTS
 * 	3.3 Join to deduplicated carer list...
 * 	3.4 ...where event_dt between first_identified_date - read and first_identified_date - 1
 * 4. PRODCODES
 * 	4.1 Select all from CAM_CODELIST with a 'prodcode' Read code
 * 	4.2 Limit Read codes to first 5 chr
 * 	4.3 Join to GP_EVENTS
 * 	4.4 Join to deduplicated carer list... 
 * 	4.5 ...where condition is scz and readcode ever recorded...
 * 	4.6 ...or condition not scz and event_dt between first_identified_date - 365 and first_identified_date - 1
 */


DROP TABLE SAILW1429V.LB_CMS_CANCER_LKUP_DENBIGHSHIRE;
DROP TABLE SAILW1429V.LB_CMS_CKD_LKUP_DENBIGHSHIRE_2;
DROP TABLE SAILW1429V.LB_CMS_MEDCODE_LKUP_DENBIGHSHIRE;
DROP TABLE SAILW1429V.LB_CMS_PRODCODE_LKUP_DENBIGHSHIRE;


/* -- ONLY RUN IF CAM_CODELIST TABLE JUST IMPORTED
-- 0. Convert "Read" column from days to years (copy days column to keep incase needed to check)
ALTER TABLE SAILW1429V.LB_CAM_CODELIST
ADD COLUMN READ_DAYS INTEGER;

UPDATE
	SAILW1429V.LB_CAM_CODELIST
SET
	READ_DAYS = "READ",
	"READ" =
	CASE
		WHEN "READ" = 365 THEN 1
		WHEN "READ" = 1826 THEN 5 -- cancer
		WHEN "READ" = 99999 THEN 200 --(this is just a large number to allow look back for "ever recorded" conditions)
		ELSE "READ"
	END;
*/
-- 1. CANCER
CREATE TABLE SAILW1429V.LB_CMS_CANCER_LKUP_DENBIGHSHIRE(
	ALF_PE BIGINT,
	FIRST_IDENTIFIED_DATE DATE,
	COND VARCHAR(50), 
    REF VARCHAR(50),
    TYPE VARCHAR(50),
    READ INTEGER,
    RX INTEGER,
    LOGIC VARCHAR(10),
    SPECIAL VARCHAR(10),
    UD VARCHAR(500),
	EVENT_DT DATE);

-- Creates lookup table unpaid carers with earliest recorded cancer Read code within 5 years of index date.
INSERT INTO SAILW1429V.LB_CMS_CANCER_LKUP_DENBIGHSHIRE
SELECT 
	-- Drop read code columns and select distinct to avoid multiple entries for individuals with more than 1 applicable read code on a given date. 
	DISTINCT
	ALF_PE,
	FIRST_IDENTIFIED_DATE,
	COND,
	REF,
	TYPE,
	READ,
	RX,
	LOGIC,
	SPECIAL,
	UD,
	EVENT_DT
FROM
	(
	SELECT
		ALF_PE,
		FIRST_IDENTIFIED_DATE,
		COND,
		REF,
		TYPE,
		READ,
		RX,
		LOGIC,
		SPECIAL,
		UD,
	-- 1.5 Select earliest cancer Read code recorded
		MIN(EVENT_DT) AS EVENT_DT
	FROM
		(
		SELECT
			DENBIGHSHIRE.ALF_PE,
			DENBIGHSHIRE.FIRST_IDENTIFIED_DATE,
			CANEVENT.COND,
			CANEVENT.REF,
			CANEVENT.TYPE,
			CANEVENT.READ,
			CANEVENT.RX,
			CANEVENT.LOGIC,
			CANEVENT.SPECIAL,
			CANEVENT.UD,
			CANEVENT.READCODE,
			CANEVENT.EVENT_DT,
			CANEVENT.EVENT_CD
		FROM
			SAILW1429V.PEJE_MATCHED_COHORT_DENBIGHSHIRE_2 DENBIGHSHIRE
			-- 1.3. Join to deduplicated carer list to get all cancer Read code events for carers
		INNER JOIN (
			SELECT
				GP.ALF_PE,
				CAN.*,
				GP.EVENT_DT,
				GP.EVENT_CD
			FROM
				-- 1.1. select all from GP EVENT table
				SAIL1429V.WLGP_GP_EVENT_CLEANSED_20220301 GP
			INNER JOIN (
				SELECT
					*,
					-- NB: limit Cassell Read codes to first 5 characters (SAIL read codes are only 5 chr)
	 				CHAR(READCODE, 5) AS READCODE_SAIL
				FROM
					SAILW1429V.LB_CAM_CODELIST CAM
					-- 1.2. with a cancer cms code
					WHERE COND = 'CAN') CAN ON
				GP.EVENT_CD = CAN.READCODE_SAIL) CANEVENT ON
			DENBIGHSHIRE.ALF_PE = CANEVENT.ALF_PE)
	-- 1.4 Group to select earliest cancer Read code date
	GROUP BY 
		ALF_PE,
		FIRST_IDENTIFIED_DATE,
		COND,
		REF,
		TYPE,
		READ,
		RX,
		LOGIC,
		SPECIAL,
		UD)
-- 1.6 Limit to records within 5 years of index date
WHERE EVENT_DT BETWEEN FIRST_IDENTIFIED_DATE - READ YEARS AND FIRST_IDENTIFIED_DATE - 1 DAYS;


-- 2. KIDNEY
CREATE TABLE SAILW1429V.LB_CMS_CKD_LKUP_DENBIGHSHIRE_2(
	ALF_PE BIGINT,
	FIRST_IDENTIFIED_DATE DATE,
	COND VARCHAR(50), 
    REF VARCHAR(50),
    TYPE VARCHAR(50),
    READ INTEGER,
    RX INTEGER,
    LOGIC VARCHAR(10),
    SPECIAL VARCHAR(10),
    UD VARCHAR(500),
    READCODE VARCHAR(40),
	EVENT_DT DATE,
	EVENT_CD VARCHAR(40));

-- Creates lookup table unpaid carers with CKD where highest value of 2 most recent tests prior to index date was <60ml/min. 
INSERT INTO SAILW1429V.LB_CMS_CKD_LKUP_DENBIGHSHIRE_2
SELECT 
	ALF_PE,
	FIRST_IDENTIFIED_DATE,
	COND,
	"REF",
	"TYPE",
	"READ",
	RX,
	LOGIC,
	SPECIAL,
	UD,
	READCODE,
	EVENT_DT,
	EVENT_CD
FROM
	(
	SELECT
		*,
		-- 2.7 Group by ALF_PE, order by reverse EVENT_VAL (biggest first), add row number
		ROW_NUMBER() OVER(PARTITION BY ALF_PE ORDER BY ALF_PE, EVENT_VAL DESC) AS MAX_VAL
	FROM
		(
		SELECT
			DENBIGHSHIRE.ALF_PE,
			DENBIGHSHIRE.FIRST_IDENTIFIED_DATE,
			CKDEVENT.COND,
			CKDEVENT."REF",
			CKDEVENT."TYPE",
			CKDEVENT."READ",
			CKDEVENT.RX,
			CKDEVENT.LOGIC,
			CKDEVENT.SPECIAL,
			CKDEVENT.UD,
			CKDEVENT.READCODE,
			CKDEVENT.EVENT_DT,
			CKDEVENT.EVENT_CD,
			CKDEVENT.EVENT_VAL,
			-- 2.5 Group by ALF_PE, order by reverse event_dt (most recent first), add row number
			ROW_NUMBER() OVER(PARTITION BY DENBIGHSHIRE.ALF_PE ORDER BY DENBIGHSHIRE.ALF_PE, EVENT_DT DESC, "SEQUENCE" DESC) AS RN
		FROM
			SAILW1429V.PEJE_MATCHED_COHORT_DENBIGHSHIRE_2 DENBIGHSHIRE
			-- 2.3. Join to deduplicated carer list to get all CKD Read code events for carers
		INNER JOIN (
			SELECT
				GP.ALF_PE,
				CKD.*,
				GP.EVENT_DT,
				GP.EVENT_CD,
				GP.EVENT_VAL,
				GP."SEQUENCE"
			FROM
				-- 2.1. select all from GP EVENT table
				SAIL1429V.WLGP_GP_EVENT_CLEANSED_20220301 GP
			INNER JOIN (
				SELECT
					*,
					-- NB: limit Cassell Read codes to first 5 characters
	 				CHAR(READCODE, 5) AS READCODE_SAIL
				FROM
					SAILW1429V.LB_CAM_CODELIST CAM
					-- 2.2. with a ckd cms code
					WHERE COND = 'CKD') CKD ON
				GP.EVENT_CD = CKD.READCODE_SAIL
				WHERE GP.EVENT_VAL IS NOT NULL) CKDEVENT ON
			DENBIGHSHIRE.ALF_PE = CKDEVENT.ALF_PE
		-- 2.4 Select records that occured before being identified as an unpaid carer
		WHERE CKDEVENT.EVENT_DT < DENBIGHSHIRE.FIRST_IDENTIFIED_DATE)
	-- 2.6 Select most recent 2 rows per ALF (rownum <=2)
	WHERE
		RN <= 2)
WHERE 
	-- 2.8. Select max eGFR reading of the most recent 2 readings
	MAX_VAL = 1 AND
	-- 2.9 Select patients where this max test value is <60ml/min
	EVENT_VAL < 60;


-- 3. ALL OTHER 'MEDCODES' 
CREATE TABLE SAILW1429V.LB_CMS_MEDCODE_LKUP_DENBIGHSHIRE(
	ALF_PE BIGINT,
	FIRST_IDENTIFIED_DATE DATE,
	COND VARCHAR(50), 
    REF VARCHAR(50),
    TYPE VARCHAR(50),
    READ INTEGER,
    RX INTEGER,
    LOGIC VARCHAR(10),
    SPECIAL VARCHAR(10),
    UD VARCHAR(500));

  -- Creates lookup table of unpaid carers with a CMS 'medcode' readcode (not cancer or CKD) within the specified period prior to index date.
INSERT INTO SAILW1429V.LB_CMS_MEDCODE_LKUP_DENBIGHSHIRE
SELECT 
	-- select one row per person per condition
	-- NB: This could be done in the internal SELECT, nested to allow easy interrogation of internal output if necessary.
	DISTINCT ALF_PE,
	FIRST_IDENTIFIED_DATE,
	COND,
	"REF",
	"TYPE",
	"READ",
	RX,
	LOGIC,
	SPECIAL,
	UD
FROM 
	(
	SELECT
		DENBIGHSHIRE.ALF_PE,
		DENBIGHSHIRE.FIRST_IDENTIFIED_DATE,
		MEDEVENT.COND,
		MEDEVENT."REF",
		MEDEVENT."TYPE",
		MEDEVENT."READ",
		MEDEVENT.RX,
		MEDEVENT.LOGIC,
		MEDEVENT.SPECIAL,
		MEDEVENT.UD,
		MEDEVENT.READCODE,
		MEDEVENT.EVENT_DT
	FROM 
		SAILW1429V.PEJE_MATCHED_COHORT_DENBIGHSHIRE_2 DENBIGHSHIRE
	-- 3.4 Join to deduplicated carer list on ALF_PE...
	INNER JOIN (
		SELECT
			GP.ALF_PE,
			MED.*,
			GP.EVENT_DT 
		FROM
			SAIL1429V.WLGP_GP_EVENT_CLEANSED_20220301 GP
		-- 3.3 Join to GP EVENT table
		INNER JOIN (
		-- 3.1 select all from CAM_CODELIST with a 'medcode' readcode that's not cancer or CKD (these are addressed separately above)
			SELECT
				*,
				-- 3.2 Limit Read codes to first 5 chr
				CHAR(READCODE, 5) AS READCODE_SAIL
			FROM
				SAILW1429V.LB_CAM_CODELIST CAM
				WHERE 
					TYPE = 'MEDCODES'
					AND COND NOT IN ('CAN',
						'CKD')) MED ON 
			GP.EVENT_CD = MED.READCODE_SAIL) MEDEVENT ON
		DENBIGHSHIRE.ALF_PE = MEDEVENT.ALF_PE
	-- 3.5 ...where event_dt between first_identified_date - read and first_identified_date - 1
	WHERE MEDEVENT.EVENT_DT BETWEEN DENBIGHSHIRE.FIRST_IDENTIFIED_DATE - MEDEVENT."READ" YEARS AND DENBIGHSHIRE.FIRST_IDENTIFIED_DATE - 1 DAYS);


-- 4. PRODCODES
CREATE TABLE SAILW1429V.LB_CMS_PRODCODE_LKUP_DENBIGHSHIRE(
	ALF_PE BIGINT,
	FIRST_IDENTIFIED_DATE DATE,
	COND VARCHAR(50), 
    REF VARCHAR(50),
    TYPE VARCHAR(50),
    READ INTEGER,
    RX INTEGER,
    LOGIC VARCHAR(10),
    SPECIAL VARCHAR(10),
    UD VARCHAR(500));
   
  -- Creates lookup table of unpaid carers with a CMS 'prodcode' readcode within the specified period prior to index date (SCZ ever recorded, everything else 1 year).
-- Select rows where pcount meets the Rx criteria (PCOUNT >= RX)
INSERT INTO SAILW1429V.LB_CMS_PRODCODE_LKUP_DENBIGHSHIRE
SELECT 
	ALF_PE,
	FIRST_IDENTIFIED_DATE,
	COND,
	"REF",
	"TYPE",
	"READ",
	RX,
	LOGIC,
	SPECIAL,
	UD
FROM 
	(
	SELECT 
		-- add count of number of prodcodes for Rx criteria and select one row per person per condition using group
		DISTINCT ALF_PE,
		FIRST_IDENTIFIED_DATE,
		COND,
		"REF",
		"TYPE",
		"READ",
		RX,
		LOGIC,
		SPECIAL,
		UD,
		COUNT(*) AS PCOUNT
	FROM
		(
		SELECT 
			DENBIGHSHIRE.ALF_PE,
			DENBIGHSHIRE.FIRST_IDENTIFIED_DATE,
			PRODEVENT.COND,
			PRODEVENT."REF",
			PRODEVENT."TYPE",
			PRODEVENT."READ",
			PRODEVENT.RX,
			PRODEVENT.LOGIC,
			PRODEVENT.SPECIAL,
			PRODEVENT.UD,
			PRODEVENT.READCODE,
			PRODEVENT.EVENT_DT
		FROM 
			SAILW1429V.PEJE_MATCHED_COHORT_DENBIGHSHIRE_2 DENBIGHSHIRE
		-- 4.4 Join 'prodcode' events to deduplicated carer list...
		INNER JOIN (
			SELECT 
				GP.ALF_PE,
				PROD. *,
				GP.EVENT_DT 
			FROM 
				SAIL1429V.WLGP_GP_EVENT_CLEANSED_20220301 GP
			-- 4.3 Join cam 'prodcodes' to GP EVENT table
			INNER JOIN (
				-- 4.1 Select all from CAM_CODELIST with a 'prodcode' Read code
				SELECT 
					*,
					-- 4.2 Limit Read codes to first 5 chr
					CHAR(READCODE, 5) AS READCODE_SAIL
				FROM 
					SAILW1429V.LB_CAM_CODELIST CAM
				WHERE 
					"TYPE" = 'PRODCODES') PROD ON 
					GP.EVENT_CD = PROD.READCODE_SAIL) PRODEVENT
				ON PRODEVENT.ALF_PE = DENBIGHSHIRE.ALF_PE 
		WHERE
			-- 4.5 ...where condition is scz and readcode ever recorded...
			(COND = 'SCZ' AND PRODEVENT.EVENT_DT < DENBIGHSHIRE.FIRST_IDENTIFIED_DATE) OR
			-- 4.6 ...or condition not scz and event_dt between first_identified_date - 365 and first_identified_date - 1 
			(COND != 'SCZ' AND PRODEVENT.EVENT_DT BETWEEN DENBIGHSHIRE.FIRST_IDENTIFIED_DATE - 1 YEARS AND DENBIGHSHIRE.FIRST_IDENTIFIED_DATE - 1 DAYS))
	GROUP BY 
		ALF_PE,
		FIRST_IDENTIFIED_DATE,
		COND,
		"REF",
		"TYPE",
		"READ",
		RX,
		LOGIC,
		SPECIAL,
		UD)
WHERE 
	PCOUNT >= RX;