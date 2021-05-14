DROP FUNCTION IF EXISTS findAvailableSlots CASCADE;
DROP FUNCTION IF EXISTS intArrayToTextArray CASCADE;
DROP FUNCTION IF EXISTS calculateWorkHours CASCADE;
DROP FUNCTION IF EXISTS calculateWorkDays CASCADE;
DROP FUNCTION IF EXISTS end_of_month CASCADE;
DROP FUNCTION IF EXISTS array_unique CASCADE;

DROP PROCEDURE IF EXISTS add_Employee CASCADE;
DROP PROCEDURE IF EXISTS update_credit_card CASCADE;
DROP FUNCTION IF EXISTS get_available_rooms CASCADE;
DROP FUNCTION IF EXISTS view_summary_report CASCADE;
DROP PROCEDURE IF EXISTS buy_course_package CASCADE;
DROP PROCEDURE IF EXISTS register_session CASCADE;
DROP PROCEDURE IF EXISTS update_instructor CASCADE;
DROP FUNCTION IF EXISTS pay_salary CASCADE;

DROP FUNCTION IF EXISTS get_my_course_package CASCADE;
DROP PROCEDURE IF EXISTS remove_employee CASCADE;
DROP FUNCTION IF EXISTS find_instructors CASCADE;
DROP PROCEDURE IF EXISTS add_course_offering CASCADE;
DROP FUNCTION IF EXISTS get_my_registrations CASCADE;
DROP PROCEDURE IF EXISTS update_room CASCADE;
DROP FUNCTION IF EXISTS promote_courses CASCADE;
DROP FUNCTION IF EXISTS view_manager_report CASCADE;

DROP PROCEDURE IF EXISTS add_customer CASCADE;
DROP FUNCTION IF EXISTS get_available_instructors CASCADE;
DROP PROCEDURE IF EXISTS add_course_package CASCADE;
DROP FUNCTION IF EXISTS get_available_course_offerings CASCADE;
DROP PROCEDURE IF EXISTS update_course_session CASCADE;
DROP PROCEDURE IF EXISTS remove_session CASCADE;
DROP FUNCTION IF EXISTS top_packages CASCADE;

DROP PROCEDURE IF EXISTS add_course CASCADE;
DROP FUNCTION IF EXISTS find_rooms CASCADE;
DROP FUNCTION IF EXISTS get_available_course_packages CASCADE;
DROP FUNCTION IF EXISTS get_available_course_sessions CASCADE;
DROP PROCEDURE IF EXISTS cancel_registration CASCADE;
DROP PROCEDURE IF EXISTS add_session CASCADE;
DROP FUNCTION IF EXISTS popular_courses CASCADE;

--no.1
CREATE OR REPLACE PROCEDURE add_Employee (IN e_name TEXT, 
        IN home_address TEXT, IN contact_number CHAR(9),
        IN email_address TEXT, IN salary_info NUMERIC, 
        IN e_type CHAR(1), IN date_joined date, IN course_areas TEXT[]) AS $$
DECLARE
    mid INTEGER; 
    iid INTEGER;
    aid INTEGER;
	area TEXT;
BEGIN
    IF array_length(course_areas, 1) > 0 THEN   
            
        IF (e_type = 'M') THEN 

            -- add into Employees table first, this is using monthly_rate
            INSERT INTO Employees(name, home_add, contact, email_add, monthly_rate, e_type, date_joined) 
            VALUES(e_name, home_address, contact_number, email_address, salary_info, e_type, date_joined);

            -- find the eid of manager and save into local variable 
            SELECT eid INTO mid
            FROM Employees E
            WHERE e_name  = E.name
            AND home_address = E.home_add
            AND email_address = E.email_add;

            -- add into Managers table 
            INSERT INTO Managers(mid) VALUES(mid);

            -- start looping through the course areas so see which area does not have a manager 
            -- which is equivalent to checking which area does not already exist 
            FOREACH area IN ARRAY course_areas
            LOOP 
                IF NOT EXISTS (
                    SELECT 1 
                    FROM Course_areas
                    WHERE name = area
                ) 
                THEN 
                -- assign the area to the manager 
                INSERT INTO Course_areas(name, mid) VALUES(name, mid);
				
				END IF;
            END LOOP;

        ELSIF (e_type = 'I') THEN

            -- add into Employees table first, can either be monthly or hourly rate so have to check
            IF (salary_info > 100.00 ) THEN 
                -- means that it is monthly rate 
                INSERT INTO Employees(name, home_add, contact, email_add, monthly_rate, e_type, date_joined) 
                VALUES(e_name, home_address, contact_number, email_address, salary_info, e_type, date_joined);
            ELSE 
                INSERT INTO Employees(name, home_add, contact, email_add, hourly_rate, e_type, date_joined) 
                VALUES(e_name, home_address, contact_number, email_address, salary_info, e_type, date_joined);

            END IF; 
            
            -- find the eid of instructor and save into local variable 
            SELECT eid INTO iid
            FROM Employees E
            WHERE e_name  = E.name
            AND home_address = E.home_add
            AND email_address = E.email_add;

            -- add into Instructors table 
            INSERT INTO Instructors(iid) VALUES(iid);

            -- start looping through the course areas and add into specializes table 
            FOREACH area IN ARRAY course_areas 
            LOOP
                INSERT INTO Specializes(iid, area_name) VALUES(iid, i);
            END LOOP;
        END IF;
   
    ---- means that the array is empty
    ELSE 
        IF (e_type = 'A') THEN 
             -- add into Employees table first, this is using monthly_rate
            INSERT INTO Employees(name, home_add, contact, email_add, monthly_rate, e_type, date_joined) 
            VALUES(e_name, home_address, contact_number, email_address, salary_info, e_type, date_joined);
            
            -- find the eid of manager and save into local variable 
            SELECT eid INTO aid
            FROM Employees E
            WHERE e_name  = E.name
            AND home_address = E.home_add
            AND email_address = E.email_add;

            -- add into Administrators table 
            INSERT INTO Administrators(aid) VALUES(aid);
        END IF;
	END IF;
END;
$$ LANGUAGE PLPGSQL;

-- no.2
CREATE OR REPLACE PROCEDURE remove_employee(IN _eid INTEGER, IN _date_departed DATE) AS $$
	UPDATE Employees 
    SET date_departed = _date_departed
    WHERE eid = _eid;
$$ LANGUAGE SQL;

-- no.3
CREATE OR REPLACE PROCEDURE add_customer(IN _name TEXT, IN _address TEXT, IN _phone CHAR(9),
										 IN _email TEXT, IN _ccNum CHAR(16), IN _expiry_date DATE, IN _cvv CHAR(3))
AS $$
DECLARE
	custID INTEGER; 
BEGIN
	INSERT INTO 
	Customers (name, email, address, phone)
	VALUES (_name, _email, _address, _phone);
	
	SELECT cid INTO custID FROM Customers WHERE name = _name;
	
	-- assuming this is a new customer and hence, cc_id = 1 
	INSERT INTO
	Credit_cards (num, cid, cc_id, cvv, expiry_date, activation_date)
	VALUES (_ccNum, custID, 1, _cvv, _expiry_date, NOW());
END
$$ LANGUAGE plpgsql;

-- no.4
CREATE OR REPLACE PROCEDURE update_credit_card(IN cid_update INTEGER, IN cc_num_update CHAR(16), IN cvv_update INTEGER, IN expiry_update DATE) AS $$
DECLARE
    credit_card_count INTEGER;
BEGIN
   
    -- check if the new credit card is not owned by the current user 
	IF NOT EXISTS (
		SELECT 1 
		FROM Credit_cards 
		WHERE cid = cid_update 
		AND num = cc_num_update
	) 
	THEN 
        SELECT COUNT(NUM) INTO credit_card_count
        FROM Credit_cards 
        WHERE cid = cid_update;

        INSERT INTO Credit_cards(num, cid, cc_id, cvv, expiry_date, activation_date) 
        VALUES(cc_num_update, cid_update, credit_card_count+1, cvv_update, expiry_update, CURRENT_DATE);

    ELSE 
        RAISE NOTICE 'This card already exists and is valid, please input a new card number instead!';
        
    END IF;
END; 
$$ LANGUAGE PLPGSQL;

--no.5
CREATE OR REPLACE PROCEDURE add_course(IN _course_area CHAR(1), IN _title TEXT, IN _desc TEXT, IN _duration INTEGER)
AS $$
BEGIN
    INSERT INTO Courses(crs_id, course_area, title, description, duration) 
    VALUES(DEFAULT, _course_area, _title, _desc, _duration);   
END;
$$ 
LANGUAGE plpgsql;

--no.6
CREATE OR REPLACE FUNCTION find_instructors(IN _crsid INTEGER, IN _ssn_date DATE, IN _start_time INTEGER) RETURNS TABLE(_iid INTEGER, _name TEXT) AS $$
DECLARE
    curs CURSOR FOR (SELECT * FROM Instructors);
    r RECORD;
    _dur INTEGER;
    _dur_sum INTEGER;
    _end_time INTEGER;
    _area char(1);
    num_hours INTEGER;
BEGIN
    OPEN curs;
    LOOP
        FETCH curs INTO r;
        EXIT WHEN NOT FOUND;
        _dur := (SELECT C.duration FROM Courses C WHERE C.crs_id = _crsid); 
        _end_time := _start_time + _dur;
        _area := (SELECT course_area FROM Courses C WHERE C.crs_id = _crsid);

        -- 1 check if the employee has already departed or has yet to join
        IF EXISTS (
            SELECT 1 FROM Employees E 
            WHERE E.eid = r.iid 
            AND ((E.date_joined > _ssn_date)
            OR (E.date_departed < _ssn_date))) THEN
            CONTINUE;

        -- 2, if current iid doesnt specialize in area, then continue loop to next iid
        ELSIF NOT EXISTS (SELECT 1 FROM Specializes S WHERE S.iid = r.iid AND S.area_name = _area) THEN
            CONTINUE; 

        ELSIF EXISTS (
            SELECT 1 FROM Sessions
            -- 3, check if there's any overlap in the new session's timing and the old one's 
			WHERE (
                ((_start_time >= (SELECT start_time FROM Sessions S WHERE S.iid = r.iid AND S.ssn_date=_ssn_date)) 
                AND (_start_time <= (SELECT end_time FROM Sessions S WHERE S.iid = r.iid AND S.ssn_date=_ssn_date))) 
                OR 
                ((_start_time <= (SELECT start_time FROM Sessions S WHERE S.iid = r.iid AND S.ssn_date=_ssn_date)) 
                AND (_end_time >= (SELECT end_time FROM Sessions S WHERE S.iid = r.iid AND S.ssn_date=_ssn_date))))) THEN
            CONTINUE;

        -- 4, if iid is PT, then check if exists this PT instructor that is exceeding the 30 hours if this session's duration is added to it 
        ELSIF EXISTS (SELECT 1 FROM Instructors I WHERE I.iid = _iid AND e_status = 'P') THEN 
            _dur_sum := (
                SELECT SUM(I.duration) 
                FROM instructorsWorkDuration I 
                WHERE I.iid= r.iid 
                AND I.yr = EXTRACT(YEAR FROM _ssn_date)
                AND I.mth = EXTRACT(MONTH FROM _ssn_date));
            IF (_dur_sum + _dur) > 30 THEN
                CONTINUE;
            END IF;

        -- 5, if isntructor has back to back sessions then, dont include and continue loop
        ELSIF EXISTS( 
            SELECT 1 FROM Sessions S 
            WHERE S.ssn_date = _ssn_date 
            AND S.iid = r.iid 
            AND ((_start_time - 1 = S.end_time) 
            OR (_end_time + 1 = S.start_time))) THEN
            CONTINUE;
        END IF;

        -- assign return values
        _name := (SELECT name FROM Employees WHERE eid = r.iid);
        _iid := r.iid;
        RETURN NEXT; 
    END LOOP;
    CLOSE curs;
END;
$$ LANGUAGE plpgsql;

--no.7
CREATE OR REPLACE FUNCTION get_available_instructors(IN crs_id INTEGER, IN start_date DATE, IN end_date DATE)
RETURNS TABLE(_eid INTEGER, _name TEXT, _total_hours INTEGER, _day DATE, _avail_hours INTEGER[])
AS $$
DECLARE
	num_interval INTEGER;
	cursor1 CURSOR FOR (SELECT * FROM allDates);
	REC1 RECORD;
	cursor2 CURSOR FOR (SELECT * FROM currentInstructors);
	REC2 RECORD;
	cursor3 CURSOR FOR (SELECT * FROM findAllInstructors);
	REC3 RECORD;
	availStartTime INTEGER[] := '{9,10,11,14,15,16,17}';
	timeslot INTEGER;
	_start_time INTEGER;
BEGIN
	-- generation of parameter for the generate series function
	SELECT DATE_PART('day', end_date::timestamp - start_date::timestamp) INTO num_interval;
	
	-- returns all the dates in the table to insert into the function
	CREATE TEMP TABLE allDates AS (
		SELECT start_date + S.A AS _dates 
		FROM generate_series(0,num_interval,1) AS S(A)
	); -- REC1
	
	-- creating a table to include ALL start_time and instructors that are avail
	CREATE TEMP TABLE findAllInstructors(_iid INTEGER, _name TEXT, _date DATE, _start_time INTEGER); -- REC3
	
	OPEN cursor1;
	LOOP
		-- looping through the dates
		FETCH cursor1 INTO REC1;
		EXIT WHEN NOT FOUND;
		-- looping through each start time
		FOREACH timeslot IN ARRAY availStartTime
		LOOP
			-- inserting results from find_instructors into a table
			CREATE TEMP TABLE currentInstructors AS (SELECT * FROM find_instructors(crs_id, REC1._dates, timeslot)); -- REC2
			-- loop over results from find_instructors function
			OPEN cursor2;
			LOOP
				FETCH cursor2 INTO REC2;
				EXIT WHEN NOT FOUND;
				_eid := REC2._iid;
				_name := REC2._name;
				_day := REC1._dates;
				_start_time := timeslot;
				INSERT INTO findAllInstructors VALUES (_eid, _name, _day, _start_time);
			END LOOP;
			CLOSE cursor2;
			-- drop the table before assigning new time slot
			DROP TABLE currentInstructors;
		END LOOP;
		
	END LOOP;
	CLOSE cursor1;
	
	CREATE TEMP TABLE returnResults(_eid INTEGER, _name TEXT, _total_hours INTEGER, _day DATE, _avail_hours INTEGER[]);
	
	OPEN cursor3;
	LOOP
		FETCH cursor3 INTO REC3;
		EXIT WHEN NOT FOUND;
		-- if record does not exist, fill up all the first few columns and append the first timing
		IF NOT EXISTS (
			SELECT 1
			FROM returnResults RR
			WHERE REC3._iid = RR._eid 
			AND REC3._name = RR._name
			AND REC3._date = RR._day) THEN
				_eid := REC3._iid;
				_name := REC3._name;
				_day := REC3._date;
				
				-- cte to assign total hours to instructors
				WITH cte AS (
					SELECT iid, ssn_date, end_time - start_time AS duration
					FROM Instructors NATURAL JOIN Sessions)
				SELECT COALESCE(sum(duration),0) INTO _total_hours
				FROM cte
				WHERE iid = _eid
				AND ssn_date >= start_date
				AND ssn_date <= end_date;
				
				_avail_hours := '{}';
				_avail_hours := array_append(_avail_hours, REC3._start_time);
				INSERT INTO returnResults VALUES (_eid, _name, _total_hours, _day, _avail_hours);
				CONTINUE;
				
		-- if record exists, append the time
		ELSIF EXISTS (
			SELECT 1
			FROM returnResults RR
			WHERE REC3._iid = RR._eid 
			AND REC3._name = RR._name
			AND REC3._date = RR._day) THEN
				UPDATE returnResults 
				SET _avail_hours = returnResults._avail_hours || REC3._start_time
				WHERE REC3._iid = returnResults._eid
				AND REC3._name = returnResults._name
				AND REC3._date = returnResults._day;
		END IF;
	END LOOP;
	CLOSE cursor3;
	
	
	RETURN QUERY
	SELECT * 
	FROM returnResults;
	
	DROP TABLE allDates;
	DROP TABLE findAllInstructors;
	DROP TABLE returnResults;
END
$$ LANGUAGE plpgsql;

--no.8
CREATE OR REPLACE FUNCTION find_rooms(IN _ssn_date DATE, IN _start_time INTEGER, IN _duration INTEGER)
RETURNS TABLE (_rid INTEGER) AS $$
DECLARE 
    curs CURSOR FOR (select * from Rooms);
    r record;
    _end_time INTEGER;
BEGIN
    _end_time := _start_time + _duration;
    OPEN CURS; 
    LOOP
        FETCH curs INTO r;
        EXIT WHEN NOT FOUND;

    	--checking if room is being used on tha date and that time
        IF NOT EXISTS (
            SELECT 1 
            FROM Sessions S
			WHERE ((_start_time BETWEEN S.start_time AND S.end_time) OR (_end_time BETWEEN S.start_time AND S.end_time))
			AND (_ssn_date = S.ssn_date)
			AND (r.rid = rid)
        ) THEN
            _rid := r.rid;
        ELSE
            CONTINUE;
	    END IF;
        RETURN NEXT;
    END LOOP;
    CLOSE curs;
END;
$$ 
LANGUAGE plpgsql;	

--no.9
CREATE OR REPLACE FUNCTION get_available_rooms(IN start_date DATE, IN end_date DATE ) 
RETURNS TABLE (_room_id INTEGER, _r_cty INTEGER, _day DATE, _avail_hours TEXT[]) AS $$
DECLARE 
    fullyAvailableRooms INTEGER[];
	room INTEGER;
    availableStartTimes INTEGER[] := '{9,10,11,14,15,16,17}'; -- REMOVED 12, 13, 18 ON PURPOSE 
	timeslot TEXT;
	finalTimeSlots TEXT[];
	ST TEXT[];
	r_cty INTEGER;
BEGIN
    CREATE TEMP TABLE SessionsDuringRange AS(
        -- get the sessions that fall between the range 
        SELECT rid, ssn_date AS SessionDate, ARRAY_AGG(start_time || '-' || end_time) AS SessionTimes
        FROM Sessions NATURAL JOIN Rooms 
        WHERE ssn_date >= start_date
        AND ssn_date <= end_date
		GROUP BY rid, ssn_date
    );
 
    CREATE TEMP TABLE RoomsToReturn (ROOMID INTEGER, RCTY INTEGER, DAY DATE, AVAILHOURS TEXT[]);
	WHILE (start_date <= end_date)
	LOOP 
		fullyAvailableRooms := ARRAY(
			SELECT rid
			FROM Rooms 
		); 

		FOREACH room IN ARRAY fullyAvailableRooms
		LOOP
			SELECT room_cty INTO r_cty
			FROM Rooms 
			WHERE room = rid
			GROUP BY room_cty;
			
			IF EXISTS (
			SELECT 1 
			FROM SessionsDuringRange
			WHERE start_date = SessionDate -- CHECK THAT START DATE IS THE SESSION DATE
			AND rid = room -- CHECK IF THE SESSION IS BEING HELD IN THE CURRENT ROOM
			)
			THEN 
				-- select the session times so that you can pass into the method to loop 
				SELECT SessionTimes INTO ST
				FROM SessionsDuringRange
				WHERE start_date = SessionDate
				AND rid = room;

				FOREACH timeslot IN ARRAY ST
					LOOP 
						availableStartTimes := findAvailableSlots(timeslot, availableStartTimes);
						finalTimeSlots := intArrayToTextArray(availableStartTimes);
					END LOOP;
				INSERT INTO RoomsToReturn VALUES (room, r_cty, start_date, finalTimeSlots);
			ELSE 
				availableStartTimes := '{9,10,11,14,15,16,17}';
                finalTimeSlots := intArrayToTextArray(availableStartTimes);	
                INSERT INTO RoomsToReturn VALUES (room, r_cty, start_date, finalTimeSlots);

			END IF;
		END LOOP; 
		start_date := start_date + 1;

	END LOOP;

	-- The output is sorted in ascending order of room identifier and day, and the array entries are sorted in ascending order of hour.

	RETURN QUERY 
	SELECT *
	FROM RoomsToReturn
	ORDER BY ROOMID, DAY;
	
    DROP TABLE SessionsDuringRange;
    DROP TABLE RoomsToReturn;
	
END; 
$$ LANGUAGE PLPGSQL;

--helper fn for 9
CREATE OR REPLACE FUNCTION findAvailableSlots(IN timeslot TEXT, INOUT startTimingsAvailable INTEGER[]) 
RETURNS INTEGER[] AS $$
DECLARE 
	startHRText TEXT;
	endHRText TEXT;
	startIndex INTEGER;
	endIndex INTEGER; 
	startHRInt INTEGER;
	endHRInt INTEGER;
BEGIN
        -- timeslot is in {15-17}, {10-11} format
		startHRText := SPLIT_PART(timeslot, '-', 1); -- {15}
		endHRText := SPLIT_PART(timeslot, '-', 2); -- {17}
		startHRInt := CAST (startHRText AS INTEGER);
		endHRInt := CAST (endHRText AS INTEGER);
		
		WHILE startHRInt <> endHRInt
        LOOP
            startIndex = array_position(startTimingsAvailable, startHRInt); 
            startTimingsAvailable := array_remove(startTimingsAvailable, startTimingsAvailable[startIndex]);
            startHRInt := startHRInt + 1 ;
        END LOOP;		
END; 
$$ LANGUAGE PLPGSQL;

CREATE OR REPLACE FUNCTION intArrayToTextArray(IN startTimingsAvailable INTEGER[], OUT hrOUTPUT TEXT[]) 
RETURNS TEXT[] AS $$
DECLARE 
	concatTime TEXT;
	HR INTEGER;
BEGIN
    FOREACH HR IN ARRAY startTimingsAvailable 
    LOOP
        concatTime := CONCAT(HR,'-',(HR+1));
        hrOUTPUT := array_append(hrOUTPUT, concatTime);
    END LOOP;
END; 
$$ LANGUAGE PLPGSQL;

--no.10
CREATE OR REPLACE PROCEDURE add_course_offering(IN _crsid INTEGER, IN _fees NUMERIC, IN _launch_date DATE, IN _reg_deadline DATE, IN _target_num INTEGER, IN _aid INTEGER, IN _ssn_info JSON) 
AS $$
DECLARE
    i JSON;
    _start_date DATE := '2099-01-01'; -- init to a very new date 
    _new_start_date DATE;
    _end_date DATE := '2000-01-01'; -- init to an old date
    _new_end_date DATE;

	_dur INTEGER;
    _temp_sid INTEGER;
    _temp_rid INTEGER;
    _temp_ssn_date DATE;
    _temp_ssn_start_time INTEGER;
	_temp_ssn_end_time INTEGER;
    _temp_iid INTEGER;
    _arr_ssntimings INTEGER[];
    _total_hrs INTEGER;
    _crs_area TEXT;
    _inserted BOOLEAN;

    curs12 CURSOR FOR (SELECT * FROM get_available_instructors(_crsid, _start_date, _end_date));
    r RECORD;
BEGIN
	DROP TABLE IF EXISTS SessionsToInsert;
	CREATE TEMP TABLE SessionsToInsert(
        temp_sid INTEGER,
        temp_crs_id INTEGER,
        temp_launch_date DATE,
        temp_rid INTEGER,
        temp_iid INTEGER,
        temp_ssn_date DATE,
        temp_start_time INTEGER,
        temp_end_time INTEGER
    );
    -- step 1 is to check if the offerings information is correct
    INSERT INTO Offerings (crs_id, aid, launch_date, reg_deadline, target_num, fees) VALUES (_crsid, _aid, _launch_date, _reg_deadline, _target_num, _fees);

    -- step 2(a) is to extract out the start and end dates from the ssn info for the cursor
    -- (b) start inserting into the ssn temp table the iid 
    FOR i IN SELECT * FROM json_array_elements(_ssn_info)
    LOOP
        _new_start_date := (i->>'_ssn_date')::DATE;
        _new_end_date := (i->>'_ssn_date')::DATE;

        IF (_new_start_date < _start_date) THEN 
            _start_date := _new_start_date;
        ELSIF (_new_end_date > _end_date) THEN 
            _end_date := _new_end_date;
        END IF;
    END LOOP;
	
    _dur := (SELECT duration FROM Courses WHERE crs_id = _crsid);
    _crs_area := (SELECT course_area FROM Courses WHERE crs_id = _crsid);
    FOR i IN SELECT * FROM json_array_elements(_ssn_info) -- for each session
    LOOP
        _temp_sid := (SELECT COUNT(1) FROM SessionsToInsert);
        _temp_rid := (i->>'_rid')::INTEGER;
        _temp_ssn_date := (i->>'_ssn_date')::DATE;
        _temp_ssn_start_time := (i->>'_ssn_start_time')::INTEGER;
        _temp_ssn_end_time := _temp_ssn_start_time + _dur;

        -- this gives a range of values that the instructor needs to be free for
        SELECT ARRAY(
			(SELECT * FROM generate_series(_temp_ssn_start_time,_temp_ssn_end_time-1))) INTO _arr_ssntimings; 
        -- initialize having found a valid instructor assignment as false 
        _inserted := FALSE;
        OPEN curs12;
        LOOP
            FETCH curs12 INTO r;
            EXIT WHEN NOT FOUND;
			IF (_inserted = TRUE) THEN CONTINUE; END IF;
            _temp_iid := r._eid;
            -- check if the _day matches the ssn_date
            IF _temp_ssn_date != r._day THEN CONTINUE; END IF;

            -- check if specializes in the area
            IF NOT EXISTS (
		        SELECT 1 
                FROM Specializes
                WHERE iid = _temp_iid
                AND area_name = _crs_area) THEN 
				CONTINUE; END IF;
            
            -- need to reinitialize this everytime a new instructor is looped over
            _total_hrs := r._total_hours + (
                SELECT SUM(temp_start_time - temp_end_time) 
                FROM SessionsToInsert
                WHERE _temp_iid = temp_iid
                AND EXTRACT(YEAR FROM _temp_ssn_date) = EXTRACT(YEAR FROM temp_ssn_date)
                AND EXTRACT(MONTH FROM _temp_ssn_date) = EXTRACT(MONTH FROM temp_ssn_date));

            -- if PT need to check that we didnt exceed the 30 hours before assigning them
            IF EXISTS (SELECT 1 FROM Instructors I WHERE I.iid = _temp_iid AND e_status = 'P') THEN
                IF (_total_hrs + _dur) > 30 THEN
                    CONTINUE;
                END IF;
			END IF;

            -- check that this particular employee has no sessions that ends an hour before this one or starts an hour after this one
            IF EXISTS( 
                    SELECT 1 FROM Sessions S 
                    WHERE S.ssn_date = _temp_ssn_date 
                    AND S.iid = _temp_iid
                    AND ((_temp_ssn_start_time - 1 = S.end_time) 
                    OR (_temp_ssn_end_time + 1 = S.start_time))) THEN
                    CONTINUE;
			END IF;

            -- check this iid doesnt have the same situation for the other new inserted sessions
            IF EXISTS( 
                    SELECT 1 FROM SessionsToInsert S 
                    WHERE S.temp_ssn_date = _temp_ssn_date 
                    AND S.temp_iid = _temp_iid
                    AND ((_temp_ssn_start_time - 1 = S.temp_end_time) 
                    OR (_temp_ssn_end_time + 1 = S.temp_start_time))) THEN
                    CONTINUE;
            END IF;

           -- check if instructor is free during that time, using subset
            IF (SELECT _arr_ssntimings <@ r._avail_hours) THEN 
                IF (_inserted = FALSE) THEN 
                INSERT INTO SessionsToInsert VALUES (_temp_sid+1, _crsid, _launch_date, _temp_rid, _temp_iid, _temp_ssn_date, _temp_ssn_start_time, _temp_ssn_end_time);
                _inserted := TRUE;
				END IF;
            END IF;
        END LOOP;
        CLOSE curs12;
        IF (_inserted = False) THEN 
            RAISE EXCEPTION 'No valid instructor assignment for at least one session!'; 
        END IF;
    END LOOP;

    INSERT INTO Sessions ( 
        SELECT *
        FROM SessionsToInsert);
	RAISE NOTICE 'Inserted successfully!';
END;
$$ LANGUAGE plpgsql;

--no.11
CREATE OR REPLACE PROCEDURE add_course_package(IN pkgName TEXT, IN num_free_ssn INTEGER, IN start_date DATE, IN end_date DATE, price NUMERIC)
AS $$
BEGIN
	-- schema will check for the validity of the insertion to Course_package
	INSERT INTO 
	Course_packages (name, price, sale_start, sale_end, num_free_ssn)
	VALUES (pkgName, price, start_date, end_date, num_free_ssn);
END
$$ LANGUAGE plpgsql;


--no.12
CREATE OR REPLACE FUNCTION get_available_course_packages()
RETURNS TABLE (_name TEXT, _price NUMERIC, _sale_end DATE, _num_free_ssn INTEGER) AS $$

DECLARE 
    curs CURSOR FOR (select * from Course_packages);
    r record;
BEGIN
    OPEN CURS; 
    LOOP
        FETCH curs INTO r;
        EXIT WHEN NOT FOUND;

        IF (current_date BETWEEN r.sale_start AND r.sale_end) THEN
            _name := r.name;
            _price := r.price;
            _sale_end := r.sale_end;
            _num_free_ssn := r.num_free_ssn; 
        ELSE 
            CONTINUE;
        END IF;
        RETURN NEXT;
    END LOOP;
    CLOSE curs;
END;
$$ 
LANGUAGE plpgsql;	

--no.13
CREATE OR REPLACE PROCEDURE buy_course_package(IN _cid INTEGER, IN _pid INTEGER) AS $$
DECLARE 
    creditCardID INTEGER;
    ssns INTEGER; 
BEGIN

    SELECT cc_id INTO creditCardID
    FROM Credit_cards CC
    WHERE CC.cid = _cid
    ORDER BY activation_date 
    DESC LIMIT 1;
    
    SELECT num_free_ssn INTO ssns
    FROM Course_packages CP
    WHERE CP.pid = _pid;
        
    INSERT INTO Purchases(trn_date, remaining_ssn, cc_id, cid, pid)
    VALUES(CURRENT_DATE, ssns, creditCardID, _cid, _pid );
    
END; 
$$ LANGUAGE PLPGSQL;

--no.14
CREATE OR REPLACE FUNCTION get_my_course_package(IN _cid INTEGER, OUT _crs_package JSON) RETURNS JSON AS $$
DECLARE
    _pid INTEGER;
    _pname TEXT;
    _cc_id INTEGER;
    _crsname TEXT;
    _purchase_date DATE;
    _price NUMERIC;
    _num_free_ssn INTEGER;
    _remaining_ssn INTEGER;
    _ssn_date DATE;
    _start_time INTEGER;
    _redeemed_ssn json;
BEGIN
   _pid :=  (SELECT pid FROM Purchases WHERE cid = _cid AND remaining_ssn > 0);

    IF (_pid IS NULL) THEN 
        -- assign the pid for partially active package
        -- if there exists, theres only 1 since trigger ensures this
        SELECT pid INTO _pid
        FROM Purchases NATURAL JOIN Redeems RS
        WHERE cid = _cid 
        AND EXISTS (
            SELECT 1 FROM
            Sessions S
            WHERE CURRENT_DATE + 7 < S.ssn_date
            AND cid = RS.cid
            AND RS.sid = S.sid
            AND RS.launch_date = S.launch_date
            AND RS.crs_id = S.crs_id
        );
    END IF;

    IF (_pid IS NULL) THEN 
        RAISE EXCEPTION 'No partially active or active packages.';
    END IF;

    SELECT DISTINCT name, trn_date, price, num_free_ssn, remaining_ssn, cc_id 
    INTO _pname, _purchase_date, _price, _num_free_ssn, _remaining_ssn, _cc_id
    FROM Sessions 
    NATURAL JOIN Course_packages -- on crs_id
    NATURAL JOIN Purchases -- on pid
    WHERE pid = _pid
    AND cid = _cid;

    _redeemed_ssn :=(
        SELECT json_agg(redeemed_ssn)
        FROM (
            SELECT 
                json_build_object(
                            'crs_name', title,
                            'ssn_date', ssn_date,
                            'ssn_start_time', start_time) AS redeemed_ssn
                FROM Redeems
                NATURAL JOIN Courses
                NATURAL JOIN Sessions
                WHERE cid = _cid -- PK of purchases
                AND trn_date = _purchase_date
                AND cc_id = _cc_id
                AND pid = _pid
                GROUP by (sid, launch_date, crs_id, title, ssn_date, start_time, redeem_date)
                ORDER BY ssn_date, start_time
        )s);

    _crs_package := 
    json_build_object(
        'pname', _pname,
        'purchase_date', _purchase_date, 
        'price', _price, 
        'num_free_ssn', _num_free_ssn, 
        'remaining_ssn', _remaining_ssn,
        'redeemed_ssn', _redeemed_ssn);
    RETURN;
END;
$$ LANGUAGE plpgsql;

--no.15
DROP TABLE IF EXISTS availCourseOfferings;
CREATE OR REPLACE FUNCTION get_available_course_offerings()
RETURNS TABLE (_crs_title TEXT, _crs_area CHAR(1), _start_date DATE, 
			   _end_date DATE, _reg_deadline DATE, _fees NUMERIC, _num_vacancy INTEGER)
AS $$
DECLARE 
	cursor1 CURSOR FOR (SELECT * FROM OFFERINGS);
	REC RECORD;
	total_people INTEGER;
	total_capacity INTEGER;
BEGIN
	-- create temp table for order by clause at the end
	CREATE TEMP TABLE availCourseOfferings (title TEXT, area CHAR(1), startDate DATE,
										   endDate DATE, regDeadline DATE, fees NUMERIC, vacancy INTEGER);
										   
	OPEN cursor1;
	LOOP
		FETCH cursor1 INTO REC;
		EXIT WHEN NOT FOUND;
		
		-- check for vacancy first
		-- step 1 is to count how many people have registered / redeemed for that offering
		SELECT count(cid) INTO total_people
		FROM allRegisteredCustomers V NATURAL JOIN Sessions S
		WHERE launch_date = REC.launch_date
		AND crs_id = REC.crs_id;
		
		-- step 2 is to get the vacancy of the offering
		SELECT REC.seating_cty INTO total_capacity;
		
		-- offering still available to sign up for 
		IF REC.reg_deadline > CURRENT_DATE AND (total_capacity - total_people > 0) THEN
			-- assign the values to output
			_crs_title := (SELECT title 
						   FROM Courses C
						   WHERE C.crs_id = REC.crs_id);
			_crs_area := (SELECT course_area
						  FROM Courses C
						  WHERE C.crs_id = REC.crs_id);
			_start_date := (SELECT start_date FROM Offerings O
							WHERE O.crs_id = REC.crs_id
							AND O.launch_date = REC.launch_date);
			_end_date := (SELECT end_date FROM Offerings O
						  WHERE O.crs_id = REC.crs_id
						  AND O.launch_date = REC.launch_date);
			_reg_deadline := (SELECT reg_deadline FROM Offerings O
							  WHERE O.crs_id = REC.crs_id
							  AND O.launch_date = REC.launch_date);
			_fees := (SELECT fees FROM Offerings O
					  WHERE O.crs_id = REC.crs_id
					  AND O.launch_date = REC.launch_date);
			_num_vacancy := total_capacity - total_people;
			INSERT INTO availCourseOfferings VALUES (_crs_title, _crs_area, _start_date,
													 _end_date, _reg_deadline, _fees, _num_vacancy);
		ELSE
			CONTINUE;
		END IF;
	END LOOP;
	CLOSE cursor1;
	
	RETURN QUERY
	SELECT * FROM availCourseOfferings
	ORDER BY regDeadline, title;
	
	DROP TABLE availCourseOfferings;
	
END;
$$ LANGUAGE plpgsql;

--no.16
CREATE OR REPLACE FUNCTION get_available_course_sessions(_crs_id INTEGER, _launch_date DATE)
RETURNS TABLE (_ssn_date DATE, _start_time INTEGER, _iid INTEGER, _num_remaining_seats INTEGER) AS $$

DECLARE 
    curs CURSOR FOR (select * from Sessions S WHERE _crs_id = S.crs_id AND _launch_date = S.launch_date ORDER BY ssn_date, start_time ASC); --loop through the sessions that are for the course offering that has been input
    r record;
    _reg_deadline DATE;
    _num_of_registrations INTEGER;
    _room_cty INTEGER;
BEGIN
    --getting the reg_deadline for the course offering that has been input
    SELECT reg_deadline INTO _reg_deadline 
    FROM Offerings O
    WHERE _crs_id = O.crs_id
    AND _launch_date = O.launch_date;
    
    OPEN CURS; 
    LOOP
        FETCH curs INTO r;
        EXIT WHEN NOT FOUND;

        SELECT count(cid) INTO _num_of_registrations
        FROM allRegisteredCustomers A
        WHERE r.sid = A.sid
        AND r.launch_date = A.launch_date
        AND r.crs_id = A.crs_id
		AND COALESCE(reg_date, redeem_date) <= CURRENT_DATE;

        SELECT Z.room_cty INTO _room_cty
        FROM Rooms Z 
        WHERE r.rid = Z.rid;

        --checking for available course sessions that can be registered 
        --1: current date is between launch_date and reg_deadline 
        --2: current _num_of_registrations has not exceeded room_cty 
        IF ((current_date <= _reg_deadline AND current_date >= _launch_date) AND ((_room_cty - _num_of_registrations ISNULL) OR (_room_cty - _num_of_registrations > 0))) THEN 
            _ssn_date := r.ssn_date;
            _start_time := r.start_time;
            _iid := r.iid;
            _num_remaining_seats := _room_cty - _num_of_registrations;
        ELSE 
            CONTINUE;
        END IF;
        RETURN NEXT;
    END LOOP;
    CLOSE curs;
END;
$$ 
LANGUAGE plpgsql;

--no.17
CREATE OR REPLACE PROCEDURE register_session(IN customerID INTEGER, IN courseID INTEGER, 
                    launchDate DATE, sessionID INTEGER, paymentMethod TEXT) AS $$
DECLARE 
    cc_ID_C INTEGER;
    packageID INTEGER;
    transactionDate DATE;
    cc_ID_R INTEGER;
BEGIN
   
    -- payment by credit card 
   IF (paymentMethod = 'C') THEN 
        
        SELECT cc_id INTO cc_ID_C
        FROM Credit_cards CC
        WHERE CC.cid = customerID
        ORDER BY activation_date 
        DESC LIMIT 1;
    
        INSERT INTO Registers(sid, launch_date, crs_id, reg_date, cc_id, cid)
        VALUES(sessionId, launchDate, courseID, CURRENT_DATE, cc_ID_C, customerID);
        

    -- payment by redemption from active package 
    ELSIF (paymentMethod = 'R') THEN
        SELECT pid, trn_date, cc_id INTO packageID, transactionDate, cc_ID_R 
        FROM Purchases 
        WHERE cid = customerID 
        AND remaining_ssn >= 1; -- means that the package is active 

		INSERT INTO Redeems(cid, pid, cc_id, trn_date, sid, launch_date, crs_id, redeem_date)
		VALUES(customerID, packageID, cc_ID_R, transactionDate, sessionID, launchDate, courseID, CURRENT_DATE);
    
    END IF; 
END; 
$$ LANGUAGE PLPGSQL;

--no.18
CREATE OR REPLACE FUNCTION get_my_registrations(IN _cid INTEGER) RETURNS TABLE(_crsname TEXT, _fees NUMERIC, _ssn_date DATE, _start_time INTEGER, _duration INTEGER, _iidname TEXT) AS $$
BEGIN
    RETURN QUERY 
    SELECT title, fees, ssn_date, start_time, duration, E.name 
    FROM allRegisteredCustomers 
    NATURAL JOIN Sessions S
    NATURAL JOIN Courses -- on crs_id
    NATURAL JOIN Offerings -- on crs_id and launch_date
    INNER JOIN Employees E ON E.eid=S.iid
    WHERE _cid = cid
    AND ssn_date > CURRENT_DATE
    ORDER BY ssn_date, start_time;
END;
$$ LANGUAGE plpgsql;

--no.19
CREATE OR REPLACE PROCEDURE update_course_session(IN _cid INTEGER, IN _crs_id INTEGER, IN _launch_date DATE, IN _new_sid INTEGER)
AS $$
DECLARE
	reg_exists BOOLEAN;
	red_exists BOOLEAN;
	current_sid INTEGER;
BEGIN
	-- check if reg exists or not
	SELECT (SELECT 1 FROM Registers R 
			WHERE R.cid = _cid
			AND R.crs_id = _crs_id
			AND R.launch_date = _launch_date) INTO reg_exists;
	
	-- check if redeems exists or not
	SELECT (SELECT 1 FROM Redeems R 
			WHERE R.cid = _cid
			AND R.crs_id = _crs_id
			AND R.launch_date = _launch_date) INTO red_exists;
			
	IF (reg_exists = True) THEN
		-- get the current sid of the registation
		SELECT sid INTO current_sid
		FROM Registers 
		WHERE cid = _cid 
		AND crs_id = _crs_id 
		AND launch_date = _launch_date;	
		
		IF (current_sid <> _new_sid) THEN
			-- trigger 3 will check the validity for ssn vacancy and reg_date on registers table
			UPDATE Registers 
			SET sid = _new_sid, reg_date = CURRENT_DATE
			WHERE cid = _cid 
			AND crs_id = _crs_id 
			AND launch_date = _launch_date;
		ELSE 
			RAISE EXCEPTION 'This is the same sid that you have registered for. Please insert a new sid!';
		END IF;
		
	ELSIF (red_exists = True) THEN
		-- get the current sid of the registation
		SELECT sid INTO current_sid
		FROM Redeems
		WHERE cid = _cid 
		AND crs_id = _crs_id 
		AND launch_date = _launch_date;
		
		IF (current_sid <> _new_sid) THEN
			-- trigger 3 will check the validity for ssn vacancy and reg_date on redeems table
			UPDATE Redeems 
			SET sid = _new_sid, redeem_date = CURRENT_DATE
			WHERE cid = _cid 
			AND crs_id = _crs_id 
			AND launch_date = _launch_date;
		ELSE 
			RAISE EXCEPTION 'This is the same sid that you have registered (redeemed) for. Please insert a new sid!';
		END IF;
	
	-- if reg & red does not exist
	ELSE			
		RAISE EXCEPTION 'Registration / Redemption does not exist. Please ensure that you key in a valid cid, course id and launch date!';
	END IF;
END
$$ LANGUAGE plpgsql;

--no.20
CREATE OR REPLACE PROCEDURE cancel_registration(IN _cid INTEGER, IN _crs_id INTEGER,  IN _launch_date DATE)
AS $$

DECLARE 
    _sid INTEGER;
    _cancel_date DATE;
    _fees NUMERIC; 
    _refund_amt NUMERIC;
    _package_credit INTEGER;
    _redeem_date DATE;
    _reg_date DATE;

BEGIN

    _cancel_date := current_date;
    _package_credit := 0;

    IF EXISTS( --checking if the customer has registered for this course session that we are deleting now 
        SELECT 1 
        FROM allRegisteredCustomers R
        WHERE R.crs_id = _crs_id
        AND R.launch_date = _launch_date
        AND R.cid = _cid
    ) 
    THEN

        --get the sid that the customer registered for
        SELECT sid INTO _sid
        FROM allRegisteredCustomers R
        WHERE R.crs_id = _crs_id
        AND R.launch_date = _launch_date
        AND R.cid = _cid;

        --get the fees of the registered course session
        SELECT fees INTO _fees
        FROM Offerings O
        WHERE O.crs_id = _crs_id
        AND O.launch_date = _launch_date;

        _refund_amt := ROUND(0.9 * _fees, 2);

        INSERT INTO Cancels(sid, launch_date, crs_id, cancel_date, refund_amt, package_credit, cid)
        VALUES(_sid, _launch_date, _crs_id, _cancel_date, _refund_amt, _package_credit, _cid);
    ELSE
        RAISE EXCEPTION 'This record does not exist in Registers or Redeems.';
    END IF;

END;
$$ 
LANGUAGE plpgsql;

--no.21
CREATE OR REPLACE PROCEDURE update_instructor(courseID INTEGER, launchDate DATE, sessionID INTEGER, newIID INTEGER) AS $$
DECLARE 

BEGIN  
    UPDATE Sessions 
    SET iid = newIID
    WHERE sid = sessionID
    AND launch_date = launchDate
    AND crs_id = courseID;
END; 
$$ LANGUAGE PLPGSQL;

--no.22
CREATE OR REPLACE PROCEDURE update_room(IN _crsid INTEGER, IN _launch_date DATE, IN _sid INTEGER, IN _rid INTEGER) AS $$
    -- will invoke trigger 
    UPDATE Sessions 
    SET rid = _rid
    WHERE crs_id = _crsid
    AND launch_date = _launch_date
    AND sid = _sid;
$$ LANGUAGE SQL;

--no.23
CREATE OR REPLACE PROCEDURE remove_session(IN _crs_id INTEGER, IN _launch_date DATE, IN _sid INTEGER)
AS $$
DECLARE 
	have_reg BOOLEAN;
	have_redeem BOOLEAN;
	_ssn_date DATE;
	room_capacity INTEGER;
BEGIN
	
	IF NOT EXISTS (SELECT 1
				  FROM Sessions S
				  WHERE S.crs_id = _crs_id
				  AND S.launch_date = _launch_date
				  AND S.sid = _sid) THEN
		RAISE EXCEPTION 'Session does not exist. Please ensure that you key in a valid course id, launch date and sid';
	END IF;
	
	SELECT 1 
	WHERE EXISTS (SELECT 1 FROM Registers R
				  WHERE R.launch_date = _launch_date
				  AND R.crs_id = _crs_id
				  AND R.sid = _sid)
	INTO have_reg;
	
	SELECT 1 
	WHERE EXISTS (SELECT 1 FROM Redeems R
				  WHERE R.launch_date = _launch_date
				  AND R.crs_id = _crs_id
				  AND R.sid = _sid)
	INTO have_redeem;

	-- get the ssn_date 
	SELECT ssn_date INTO _ssn_date
	FROM Sessions S
	WHERE S.crs_id = _crs_id
	AND S.launch_Date = _launch_date
	AND S.sid = _sid;
	
    IF (_ssn_date < CURRENT_DATE or have_reg = True or have_redeem = True) THEN
        RAISE EXCEPTION 'You cannot delete this session';
		

	ELSE
		-- extracting the room capacity of the session that is going to be deleted
		SELECT R.room_cty INTO room_capacity
		FROM Sessions S NATURAL JOIN Rooms R
		WHERE S.crs_id = _crs_id
		AND S.launch_date = _launch_date
		AND S.sid = _sid;
		
		-- delete it from sessions table
		DELETE FROM Sessions S
		WHERE S.crs_id = _crs_id 
		AND S.launch_date = _launch_date 
		AND S.sid = _sid;
		
		-- update seating capacity in offerings table
		UPDATE Offerings 
		SET seating_cty = seating_cty - room_capacity
		WHERE crs_id = _crs_id
		AND launch_date = _launch_date;
	END IF;
END
$$ LANGUAGE plpgsql;

--no.24
CREATE OR REPLACE PROCEDURE add_session(IN _sid INTEGER, IN _crs_id INTEGER, IN _launch_date DATE, IN _rid INTEGER, IN _iid INTEGER, IN _ssn_date DATE, IN _start_time INTEGER)
AS $$

DECLARE 
    _end_time INTEGER;
    _duration INTEGER;
    _reg_deadline DATE;

BEGIN

    --get the duration of course from Courses table to calculate end_time 
    SELECT duration INTO _duration
    FROM Courses C
    WHERE C.crs_id = _crs_id;

    --using the duration to calculate end_time
    _end_time := _start_time + _duration;

    --getting reg_deadline for a check later on 
    SELECT reg_deadline INTO _reg_deadline
    FROM Offerings O 
    WHERE O.crs_id = _crs_id
    AND O.launch_date = _launch_date;

    --checking if session insertion is valid (offering's reg_deadline should not have passed yet)
    IF (_reg_deadline < current_date) THEN
        INSERT INTO Sessions(sid, crs_id, launch_date, rid, iid, ssn_date , start_time, end_time) 
        VALUES(_sid, _crs_id, _launch_date, _rid, _iid, _ssn_date , _start_time, _end_time);
    ELSE
        RAISE EXCEPTION 'Session cannot be inserted for this course offering because reg_deadline of this offering has passed';
    END IF;

END;
$$ 
LANGUAGE plpgsql;	

--no.25
CREATE OR REPLACE FUNCTION pay_salary(IN paymentDate DATE) 
RETURNS TABLE (e_id INTEGER, e_name TEXT, e_status TEXT, work_days_mth INTEGER, work_hours_mth INTEGER, hour_rate NUMERIC, month_rate NUMERIC, salary_amount NUMERIC) AS $$
DECLARE 
    -- check for employees that are valid to be paid 
    curs CURSOR FOR 
    (SELECT * 
    FROM Employees 
    WHERE (date_joined <= paymentDate AND date_departed >= paymentDate)
    OR (date_joined <= paymentDate AND date_departed ISNULL));
    r RECORD;
    days_in_current_mth INTEGER;
    lastDayOfMonth DATE := end_of_month(paymentDate);
BEGIN
	
    -- only proceed with function if it is the last day of the month
    IF (paymentDate = lastDayOfMonth) THEN
    OPEN curs; 
    LOOP 
        FETCH curs INTO r;
        EXIT WHEN NOT FOUND;
        -- finding the e_status, whether PT or FT 
        -- this is FT 
        IF (r.hourly_rate ISNULL AND r.monthly_rate > 0) THEN
            e_status := 'Full Time';
            month_rate := r.monthly_rate;
			hour_rate := NULL;
			
            -- calculate work days in a month, and the salary amount
            work_days_mth := calculateWorkDays(r.eid, lastDayOfMonth);
            days_in_current_mth := EXTRACT(DAYS FROM date_trunc('month', lastDayOfMonth) + interval '1 month - 1 day');
            salary_amount := ROUND((month_rate * work_days_mth / days_in_current_mth),2); 
           
            -- set the work hours to be null
            work_hours_mth := NULL; 

        -- this is PT 
        ELSIF (r.hourly_rate > 0 AND r.monthly_rate ISNULL) THEN 
            e_status := 'Part Time';
            hour_rate := r.hourly_rate;
			month_rate := NULL;
			
            -- calculate work hours in a month, and the salary amount
            work_hours_mth := calculateWorkHours(r.eid, lastDayOfMonth);
            salary_amount := ROUND((work_hours_mth *  hour_rate),2);

            -- set the work days to be null
            work_days_mth := NULL;

        END IF; 

        -- add the pay slip for that employee
        INSERT INTO Pay_slips(payment_date, amount, num_work_hours, num_work_days, eid)
        VALUES (lastDayOfMonth, salary_amount, work_hours_mth, work_days_mth, r.eid);

        -- assign the rest of the values 
        e_id := r.eid;
        e_name := r.name;
        RETURN NEXT;

    END LOOP;
	CLOSE curs;
    ELSE 
        RAISE EXCEPTION 'Not the last day of the month yet! You cannot pay your employees';
 	
	END IF;   
END; 
$$ LANGUAGE PLPGSQL;

--helper fn 1 for no.25
CREATE OR REPLACE FUNCTION end_of_month(INOUT date DATE)
RETURNS DATE AS $$
SELECT (date_trunc('month', $1) + interval '1 month' - interval '1 day')::date;
$$ LANGUAGE 'sql'
immutable strict;

--helper fn 2 for no.25
CREATE OR REPLACE FUNCTION calculateWorkDays(IN e_id INTEGER, IN lastDayOfMonth DATE, OUT work_days INT)
RETURNS INTEGER AS $$
DECLARE 
    dateJoined DATE;
    dateDeparted DATE; 
    monthJoined INTEGER;
    yearJoined INTEGER;
    monthDeparted INTEGER; 
    yearDeparted INTEGER;
    currentMonth INTEGER;
    currentYear INTEGER; 
    firstDay INTEGER;
    lastDay INTEGER;
    days_in_current_mth INTEGER;
BEGIN 
    SELECT date_joined, date_departed INTO dateJoined, dateDeparted
    FROM fullTimerWorkPeriod
    WHERE eid = e_id;

    monthJoined := EXTRACT(MONTH FROM dateJoined);
    yearJoined := EXTRACT(YEAR FROM dateJoined);

    monthDeparted := EXTRACT(MONTH FROM dateDeparted);
    yearDeparted := EXTRACT(YEAR FROM dateDeparted);

    currentMonth := EXTRACT(MONTH FROM lastDayOfMonth); 
    currentYear := EXTRACT(YEAR FROM lastDayOfMonth); 

    -- date joined is within current month 
    IF (monthJoined = currentMonth AND yearJoined = currentYear) THEN 
        firstDay := EXTRACT(DAY FROM dateJoined); 
    -- first day would be start of the month 
    ELSE 
        firstDay := 1;
   	END IF;
	
    -- date departed is within current month, last day would be the day of departure  
    IF (monthDeparted = currentMonth AND yearDeparted = currentYear) THEN
        lastDay := EXTRACT(DAY FROM dateDeparted);  
    -- last day would be end of the month 
    ELSE 
        lastDay := EXTRACT(DAYS FROM date_trunc('month', lastDayOfMonth) + interval '1 month - 1 day');
	END IF;
   
   	work_days := lastDay - firstDay + 1;  
	
END;
$$ LANGUAGE PLPGSQL;

--helper fn 3 for no.25
CREATE OR REPLACE FUNCTION calculateWorkHours(IN e_id INTEGER, IN lastDayOfMonth DATE, OUT work_hours INTEGER)
RETURNS INTEGER AS $$
DECLARE
    currentMonth INTEGER;
    currentYear INTEGER; 
BEGIN

    currentMonth := EXTRACT(MONTH FROM lastDayOfMonth); 
    currentYear := EXTRACT(YEAR FROM lastDayOfMonth); 

    -- get the sum of the hours that the iid worked for current month and year
    SELECT SUM(duration) INTO work_hours 
    FROM instructorsWorkDuration I
    WHERE e_id = I.iid
    AND currentMonth = I.mth 
    AND currentYear = I.yr;
	
	IF work_hours ISNULL -- so that salary amount won't be NULL but 0
	THEN
	work_hours := 0; 
	END IF;
	
END;
$$ LANGUAGE PLPGSQL;

--no.26
--helper fn for no.26
create or replace function array_unique(arr anyarray)
returns anyarray as $body$
    select array( select distinct unnest($1) )
$body$ language 'sql';

CREATE OR REPLACE FUNCTION promote_courses() RETURNS TABLE (_cid INTEGER, _custname TEXT, _crsarea TEXT, _crsid INTEGER, _title TEXT, _launch_date DATE, _reg_deadline DATE, _fees NUMERIC) AS $$
DECLARE
-- all inactive customers
curs CURSOR FOR (
    -- find all cid that have never redeemed or registered before
    (SELECT cid FROM Customers EXCEPT SELECT cid FROM allRegisteredCustomers) 
    UNION 
    -- find all cid that dont have records from the past 6 months
    SELECT cid FROM allRegisteredCustomers A
    -- if there isnt at least one record in the past 6 months
    WHERE NOT EXISTS (
        -- find existence of a record in the past 6 months
        SELECT 1
        FROM allRegisteredCustomers A1
        WHERE CURRENT_DATE >= COALESCE(reg_date, redeem_date) 
        AND COALESCE(reg_date, redeem_date) >= (CURRENT_DATE - INTERVAL '6 months')
        AND A1.cid = A.cid)
    ORDER BY cid ASC -- takes care of ascending order
);
-- all available course offerings, fn takes care of asc order 
curs2 CURSOR FOR (SELECT * FROM get_available_course_offerings());
r RECORD;
r2 RECORD;
_areas TEXT[]; 
BEGIN
    OPEN curs; -- for customers
    LOOP
        FETCH curs INTO r;
        EXIT WHEN NOT FOUND; 
        _cid := r.cid;
        _custname := (SELECT name FROM Customers WHERE cid = _cid);

        SELECT array_unique(ARRAY(
            SELECT course_area
            FROM allRegisteredCustomers 
            NATURAL JOIN Courses
            WHERE cid = _cid
            AND COALESCE(reg_date, redeem_date) < CURRENT_DATE
            ORDER BY COALESCE(reg_date, redeem_date) DESC
            LIMIT 3)) INTO _areas;
		
        -- if no areas of interest, assign areas to be equivalent to all possible areas within the course area table
        IF (_areas= '{}') THEN
            _areas := (SELECT ARRAY(
                SELECT name FROM Course_areas));
        END IF;
        
        OPEN curs2; -- loop over all the available course offerings 
        LOOP
            FETCH curs2 INTO r2;
            EXIT WHEN NOT FOUND;
            -- if area of the specific record doesnt match any in the array of interests of the customer, continue the loop
            IF (r2._crs_area != ALL(_areas)) THEN
                CONTINUE;
            END IF;

            _crsarea := r2._crs_area;
            _crsid := (
                SELECT crs_id 
                FROM Courses 
                WHERE title = r2._crs_title);
            _title := r2._crs_title;

            _launch_date := (
                SELECT launch_date 
                FROM Offerings 
                NATURAL JOIN Courses
                WHERE start_date = r2._start_date
                AND end_date = r2._end_date
                AND reg_deadline = r2._reg_deadline
                AND course_area = r2._crs_area
                AND title = r2._crs_title);
            _reg_deadline := r2._reg_deadline;
            _fees := r2._fees;
            RETURN NEXT;
        END LOOP;
        CLOSE curs2;
    END LOOP;
    CLOSE curs;
END;
$$ LANGUAGE plpgsql;

--no.27
DROP TABLE IF EXISTS tempCount, topNCourses;
CREATE OR REPLACE FUNCTION top_packages(IN n INTEGER)
RETURNS TABLE (_pid INTEGER, _num_free_ssn INTEGER, _price NUMERIC,
			   _sale_start DATE, _sale_end DATE, _total_sold INTEGER)
AS $$ 
DECLARE
	cursor1 CURSOR FOR (SELECT * FROM tempCount); 
	REC1 RECORD;
	top_n_min INTEGER;
BEGIN
	CREATE TEMP TABLE tempCount AS (
		SELECT pid, COUNT(pid) AS count_pid
		FROM Purchases NATURAL JOIN Course_packages
		WHERE EXTRACT (YEAR FROM sale_start) = EXTRACT (YEAR FROM CURRENT_DATE)
		AND trn_date <= CURRENT_DATE
		GROUP BY pid
		ORDER BY count(pid) DESC
	);
	
	CREATE TEMP TABLE topNCourses(pid INTEGER, num_free_ssn INTEGER, price NUMERIC,
								  sale_start DATE, sale_end DATE, total_sold INTEGER);

	SELECT MAX(count_pid) INTO top_n_min FROM tempCount;
	
	OPEN cursor1;
	LOOP
		FETCH cursor1 INTO REC1;
		EXIT WHEN NOT FOUND;
		
		-- base case
		IF n = 1 THEN
			IF REC1.count_pid = top_n_min THEN
				_pid := (SELECT pid FROM Course_packages CP WHERE CP.pid = REC1.pid);
				_num_free_ssn := (SELECT num_free_ssn FROM Course_packages CP WHERE CP.pid = REC1.pid);
				_price := (SELECT price FROM Course_packages CP WHERE CP.pid = REC1.pid);
				_sale_start := (SELECT sale_start FROM Course_packages CP WHERE CP.pid = REC1.pid);
				_sale_end := (SELECT sale_end FROM Course_packages CP WHERE CP.pid = REC1.pid);
				_total_sold := (SELECT count_pid FROM tempCount TC WHERE TC.pid = REC1.pid);
				INSERT INTO topNCourses VALUES (_pid, _num_free_ssn, _price, _sale_start, _sale_end, _total_sold);
				CONTINUE;
			ELSE
				EXIT;
			END IF;
		
		ELSIF n > 1 THEN
			IF REC1.count_pid = top_n_min THEN
				_pid := (SELECT pid FROM Course_packages CP WHERE CP.pid = REC1.pid);
				_num_free_ssn := (SELECT num_free_ssn FROM Course_packages CP WHERE CP.pid = REC1.pid);
				_price := (SELECT price FROM Course_packages CP WHERE CP.pid = REC1.pid);
				_sale_start := (SELECT sale_start FROM Course_packages CP WHERE CP.pid = REC1.pid);
				_sale_end := (SELECT sale_end FROM Course_packages CP WHERE CP.pid = REC1.pid);
				_total_sold := (SELECT count_pid FROM tempCount TC WHERE TC.pid = REC1.pid);
				INSERT INTO topNCourses VALUES (_pid, _num_free_ssn, _price, _sale_start, _sale_end, _total_sold);
				CONTINUE;
			ELSE
				_pid := (SELECT pid FROM Course_packages CP WHERE CP.pid = REC1.pid);
				_num_free_ssn := (SELECT num_free_ssn FROM Course_packages CP WHERE CP.pid = REC1.pid);
				_price := (SELECT price FROM Course_packages CP WHERE CP.pid = REC1.pid);
				_sale_start := (SELECT sale_start FROM Course_packages CP WHERE CP.pid = REC1.pid);
				_sale_end := (SELECT sale_end FROM Course_packages CP WHERE CP.pid = REC1.pid);
				_total_sold := (SELECT count_pid FROM tempCount TC WHERE TC.pid = REC1.pid);
				INSERT INTO topNCourses VALUES (_pid, _num_free_ssn, _price, _sale_start, _sale_end, _total_sold);
				top_n_min := REC1.count_pid;
				n := n - 1;
				CONTINUE;
			END IF;
		END IF;
	END LOOP;
	CLOSE cursor1;
	
	RETURN QUERY 
	SELECT * FROM topNCourses
	ORDER BY total_sold DESC, price DESC;
	
	DROP TABLE tempCount;
	DROP TABLE topNCourses;

END 
$$ LANGUAGE plpgsql;

--no.28
CREATE OR REPLACE FUNCTION popular_courses()
RETURNS TABLE (_crs_id INTEGER, _title TEXT, _course_area CHAR(1), _num_offerings INTEGER, _num_registrations INTEGER) AS $$

DECLARE 
    curs CURSOR FOR (
        select * 
        from Offerings O 
        WHERE EXTRACT(YEAR FROM O.start_date)  = EXTRACT(YEAR FROM current_date)
        AND O.reg_deadline <= current_date
        ORDER BY crs_id ASC, start_date ASC); --loop through the offerings that are within this year and before current_date 
    r record;
    NumOfferings INTEGER;
    NumRegistrations INTEGER;
    Title TEXT;
    CourseArea CHAR(1);
    prev_crs_id INTEGER;
    prev_NumOfferings INTEGER;
    final_NumRegistrations INTEGER;

BEGIN

    CREATE TEMP TABLE PopularCoursesToReturn (_Crs_Id INTEGER, _Title TEXT, _Course_Area CHAR(1), _Num_Offerings INTEGER, _Num_Registrations INTEGER);

    prev_crs_id := -1;
    final_NumRegistrations := -1;
    prev_NumOfferings := -1;

    OPEN CURS; 
    LOOP

        FETCH curs INTO r;
        EXIT WHEN NOT FOUND;

        SELECT count(*) INTO NumOfferings 
        FROM Offerings O
        WHERE O.crs_id = r.crs_id
		AND EXTRACT(YEAR FROM O.start_date)  = EXTRACT(YEAR FROM current_date)
        AND O.reg_deadline <= current_date
        GROUP BY O.crs_id;

        --getting the number of registrations for the course offering 
        SELECT count(cid) INTO NumRegistrations
        FROM allRegisteredCustomers A
        WHERE r.crs_id = A.crs_id
        AND r.launch_date = A.launch_date; --PK of Offerings 

        SELECT C.title, C.course_area INTO Title, CourseArea
        FROM Courses C
        WHERE C.crs_id = prev_crs_id;

        IF (NumOfferings >= 2) THEN 
            IF (prev_crs_id >= 0) THEN 
				IF (prev_crs_id = r.crs_id) THEN
					IF (final_NumRegistrations >= 0 AND NumRegistrations > final_NumRegistrations) THEN 
						final_NumRegistrations := NumRegistrations;
					END IF;  
				ELSEIF (prev_crs_id <> r.crs_id) THEN  --when reach a tuple with the next crs_id 
					INSERT INTO PopularCoursesToReturn(_Crs_Id, _Title, _Course_Area, _Num_Offerings, _Num_Registrations) VALUES(prev_crs_id, Title, CourseArea, prev_NumOfferings, final_NumRegistrations);
					final_NumRegistrations := NumRegistrations; 
				END IF;
			ELSE --goes here when first tuple (because prev_crs_id is initialised as -1)
				final_NumRegistrations := NumRegistrations;
			END IF;
        ELSE
            CONTINUE;
        END IF; 
        prev_crs_id := r.crs_id;
        prev_NumOfferings := NumOfferings;    

    END LOOP;
	IF (prev_crs_id <> -1) THEN 
		INSERT INTO PopularCoursesToReturn(_Crs_Id, _Title, _Course_Area, _Num_Offerings, _Num_Registrations) VALUES(prev_crs_id, Title, CourseArea, prev_NumOfferings, final_NumRegistrations);
    END IF;
	CLOSE curs;
	
    RETURN QUERY 
	SELECT *
	FROM PopularCoursesToReturn
	ORDER BY _num_registrations DESC, _crs_id ASC;
	
	DROP TABLE PopularCoursesToReturn;

END;
$$ 
LANGUAGE plpgsql;	

--no.29
CREATE OR REPLACE FUNCTION view_summary_report(num_mths INTEGER) 
RETURNS TABLE (mth INTEGER, yr INTEGER, salaries_paid NUMERIC, sales_course_package NUMERIC, reg_fees_cc NUMERIC, refunded_reg_fees NUMERIC, reg_package_redemption INTEGER) AS $$
-- salaries_paid : total salary paid for the month 
-- sales_course_package : total amount of sales of course packages for the month
-- reg_fees_cc : total registration fees paid via credit card payment for the month
-- refunded_reg_fees : total amount of refunded registration fees (due to cancellations) for the month
-- reg_package_redemption : total number of course registrations via course package redemptions for the month
DECLARE 
    currentMonth INTEGER;
    currentYear INTEGER;
BEGIN
    currentMonth := EXTRACT(MONTH FROM CURRENT_DATE); 
    currentYear := EXTRACT(YEAR FROM CURRENT_DATE); 

    CREATE TEMP TABLE SummaryReport (mth INTEGER, yr INTEGER, salaries_paid NUMERIC, sales_course_package NUMERIC, reg_fees_cc NUMERIC, refunded_reg_fees NUMERIC, reg_package_redemption INTEGER);

    WHILE num_mths > 0 
    LOOP 
        -- make sure that if it's the next year, reset the month and year +1
        IF (currentMonth = 0 ) THEN 
            currentMonth := 12;
            currentYear := currentYear - 1;
        END IF; 
	
        -- salaries_paid 
        SELECT SUM(amount) INTO salaries_paid
        FROM Pay_slips 
        WHERE EXTRACT(MONTH FROM payment_date) = currentMonth 
        AND EXTRACT(YEAR FROM payment_date) = currentYear; 
    
        -- sales_course_package 
        SELECT SUM(price) INTO sales_course_package 
        FROM Course_packages NATURAL JOIN Purchases 
        WHERE EXTRACT(MONTH FROM trn_date) = currentMonth 
        AND EXTRACT(YEAR FROM trn_date) = currentYear;

        -- reg_fees_cc
        WITH RegistrationsInMonth AS (
            SELECT launch_date, crs_id, fees -- so that i can find the offerings 
            FROM Registers NATURAL JOIN Offerings
            WHERE EXTRACT(MONTH FROM reg_date) = currentMonth 
            AND EXTRACT(YEAR FROM reg_date) = currentYear
        )
        SELECT SUM(fees) INTO reg_fees_cc 
        FROM RegistrationsInMonth;

        -- refunded_reg_fees
        SELECT SUM(refund_amt) INTO refunded_reg_fees
        FROM Cancels 
        WHERE EXTRACT(MONTH FROM cancel_date) = currentMonth 
        AND EXTRACT(YEAR FROM cancel_date) = currentYear; 

        -- reg_package_redemption 
        SELECT COUNT(*) INTO reg_package_redemption
        FROM Redeems 
        WHERE EXTRACT(MONTH FROM redeem_date) = currentMonth 
        AND EXTRACT(YEAR FROM redeem_date) = currentYear; 

        INSERT INTO SummaryReport VALUES(currentMonth,currentYear,salaries_paid,sales_course_package, reg_fees_cc, refunded_reg_fees,reg_package_redemption );  
        currentMonth := currentMonth - 1;
        num_mths := num_mths - 1; 
    END LOOP;
    RETURN QUERY SELECT * FROM SummaryReport;
   	DROP TABLE SummaryReport;
END; 
$$ LANGUAGE PLPGSQL;

--no.30
CREATE OR REPLACE FUNCTION view_manager_report() RETURNS TABLE(_mname TEXT, _total_areas INTEGER, _total_offerings INTEGER, _max_total_reg_fees NUMERIC, _crs_title TEXT) AS $$
DECLARE
	_crsid INTEGER;
	_eid INTEGER;
    _launch_date DATE;
	curs2 CURSOR FOR (
        SELECT * 
        FROM Course_areas CA 
        INNER JOIN Courses C ON CA.name = C.course_area
        NATURAL JOIN Offerings -- join on crs_id
        WHERE mid = _eid
        AND end_date <= CURRENT_DATE
        AND date_part('year', end_date) = date_part('year', CURRENT_DATE)); -- before today's date
    curs CURSOR FOR (SELECT eid, name FROM Employees WHERE e_type = 'M');
    r RECORD;

    _temp_mname TEXT;
    _temp_total_areas INTEGER;
    _temp_total_offerings INTEGER;
    _temp_total_fees NUMERIC;
    _temp_crs_title TEXT;

    r2 RECORD;
    _temp_total_reg_fees NUMERIC;
    _temp_total_redeem_fees NUMERIC;
    _fees NUMERIC;
    _cancellation_fees NUMERIC;
BEGIN
    CREATE TEMP TABLE TempToAgg(
        temp_eid INTEGER,
		temp_crsid INTEGER,
        temp_launch_date DATE, 
        temp_mname TEXT,
        temp_total_areas INTEGER,
        temp_total_offerings INTEGER,
        temp_total_fees NUMERIC,
        temp_crs_title TEXT
    );

    OPEN curs;
    LOOP -- loop over each manager
        FETCH curs INTO r;
        EXIT WHEN NOT FOUND;

        _temp_mname := r.name;
        _eid := r.eid;
        _temp_total_areas := (SELECT COUNT(1) FROM Course_areas WHERE mid = _eid);
        _temp_total_offerings := (
            SELECT COUNT(*)
            FROM Course_areas CA 
            INNER JOIN Courses C ON CA.name = C.course_area
            NATURAL JOIN Offerings -- joins on crs_id
            WHERE mid = _eid  
            AND end_date <= CURRENT_DATE
            AND date_part('year', end_date) = date_part('year', CURRENT_DATE)); 

        IF (_temp_total_areas > 0 AND _temp_total_offerings > 0) THEN 
            OPEN curs2;
            LOOP -- loop over each offering's course the manager manages
                FETCH curs2 INTO r2;
                EXIT WHEN NOT FOUND;

				_crsid := r2.crs_id;
                _temp_crs_title := r2.title;
                _launch_date := r2.launch_date;
                _fees := (SELECT fees FROM Offerings WHERE crs_id = r2.crs_id AND launch_date = _launch_date);

                -- only for CURRENTLY registered
                _temp_total_reg_fees := (
                    SELECT COALESCE((COUNT(*)*_fees), 0)
                    FROM Registers 
                    WHERE crs_id = r2.crs_id
                    AND launch_date = r2.launch_date);
                -- only for those who registered AND then cancelled
                _cancellation_fees := ROUND((
                    SELECT COALESCE(SUM(_fees - refund_amt),0)
                    FROM Cancels
                    WHERE crs_id = r2.crs_id
                    AND launch_date = r2.launch_date
                    AND refund_amt > 0), 2);
                _temp_total_reg_fees := _temp_total_reg_fees + _cancellation_fees;
				
                -- for redemptions
                _temp_total_redeem_fees := ROUND((
                    SELECT COALESCE(SUM(price/num_free_ssn), 0)
                    FROM Redeems NATURAL JOIN Course_packages -- on pid
                    WHERE crs_id = r2.crs_id
                    AND launch_date = r2.launch_date
                ));
				_temp_total_fees := ROUND(_temp_total_redeem_fees + _temp_total_reg_fees, 2); 

                INSERT INTO TempToAgg VALUES (_eid, _crsid, _launch_date, _temp_mname, _temp_total_areas, _temp_total_offerings, _temp_total_fees, _temp_crs_title);
            END LOOP;
			CLOSE curs2;

        -- no course areas being managed OR no offerings associated with that area
        ELSE  
			_crsid := NULL;
            _temp_total_reg_fees := 0;
            _temp_crs_title := NULL;    

            INSERT INTO TempToAgg VALUES (_eid, _crsid, _launch_date, _temp_mname, _temp_total_areas, _temp_total_offerings, _temp_total_reg_fees, _temp_crs_title); 
        END IF;
    END LOOP;
    CLOSE curs;

    RETURN QUERY 
	SELECT temp_mname, temp_total_areas, temp_total_offerings, ROUND(temp_total_fees, 0), temp_crs_title
	FROM TempToAgg S1
	WHERE temp_total_fees = (
		SELECT MAX(temp_total_fees)
		FROM TempToAgg S2
        WHERE S1.temp_eid = S2.temp_eid)
	ORDER BY temp_mname ASC;
	DROP TABLE TempToAgg;
END;
$$ LANGUAGE plpgsql;