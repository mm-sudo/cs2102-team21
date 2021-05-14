-- set search_path to team21;

DROP TABLE IF EXISTS Course_areas, Courses, Employees, Administrators, Managers, Pay_slips, 
					  Instructors, Offerings, Rooms, Sessions, Customers, Course_packages, 
					  Credit_cards, Specializes, Registers, Purchases, Redeems, Cancels CASCADE;

create table Course_packages(
    pid serial primary key, 
    name text not null,
    price numeric not null, 
    sale_start date not null, 
    sale_end date not null,
    num_free_ssn integer not null
);

create table Customers(
    cid serial primary key, 
    name text not null,
    email text unique not null,
    address text not null,
    phone char(9) not null
);

create table Rooms(
    rid serial primary key, 
    location text not null,
    room_cty integer not null
);

create table Employees( 
    eid serial primary key,
	e_type char(1) not null CHECK(e_type IN('M', 'I', 'A')),
    name text not null,
    home_add text not null,
    email_add text unique not null,
    contact char(9) not null,
    date_departed date,
    date_joined date not null,
    hourly_rate  numeric,
    monthly_rate  numeric,
	constraint CHECK_employees CHECK(
		(e_type = 'M' and hourly_rate  ISNULL and monthly_rate>0) or
		(e_type = 'I' and hourly_rate  ISNULL and monthly_rate >0) or
		(e_type = 'I' and monthly_rate ISNULL and hourly_rate>0) or
		(e_type = 'A' and hourly_rate  ISNULL and monthly_rate>0)),
	constraint CHECK_departure CHECK(
	    date_departed > date_joined),
	unique(name, home_add, email_add)
);

create table Instructors(
    iid integer primary key references Employees(eid)
        on delete cascade
        on update cascade,
    e_status char(1) not null 
		check (e_status in ('F', 'P'))
);

create table Managers(
    mid integer primary key references Employees(eid)
        on delete cascade
        on update cascade
);

create table Administrators(
    aid integer primary key references Employees(eid)
        on delete cascade
        on update cascade
);

create table Pay_slips(
    payment_date date not null,
    amount numeric not null,
    num_work_hours integer,
    num_work_days integer,
    eid integer references Employees(eid)
		on delete cascade
        on update cascade,
    primary key(payment_date, eid)
);

create table Course_areas(
    name char(1) primary key,
    mid integer not null references Managers(mid)
);

create table Courses(
    crs_id serial primary key,
    course_area char(1) references Course_areas(name),
    title text unique not null,
    description text not null,
    duration integer not null
        check(duration > 0)
);

create table Offerings(
    crs_id integer references Courses(crs_id)
        on delete cascade,
    aid integer references Administrators(aid),
    launch_date date not null,
	start_date date, 
	end_date date,
	reg_deadline date not null,
	target_num integer not null,
	seating_cty integer default 0 not null, 
	fees numeric not null,
	primary key(crs_id, launch_date),
	constraint CHECK_offerings check (start_date - reg_deadline >= 10) 
);

create table Sessions(
    sid integer,
	crs_id integer,
	launch_date date, 
	rid integer references Rooms(rid),
	iid integer references Instructors(iid),
    ssn_date date not null, 
    start_time integer not null check(
		((start_time >= 9) and (start_time <= 12)) or 
		(start_time >= 14 and start_time < 18)),
    end_time integer not null check (
		((end_time >= 9) and (end_time <= 12)) or 
		(end_time >= 14 and end_time <= 18)),
	primary key(sid, launch_date, crs_id), 
    constraint FK_sessions foreign key(launch_date, crs_id) 
		references Offerings(launch_date, crs_id) 
		on delete cascade,
    constraint CHECK_sessions check (end_time > start_time),
    unique(crs_id, launch_date, start_time, ssn_date),
    unique(iid, ssn_date, start_time)
);

create table Credit_cards(
    num char(16) unique not null,
    cid integer references Customers(cid)
		on delete cascade,
    cc_id integer not null, 
    cvv char(3) not null,
    expiry_date date not null,
    activation_date date not null default CURRENT_DATE, 
    primary key(cid, cc_id),
	unique(cid, activation_date)
);

create table Specializes(
    iid integer references Instructors(iid),
    area_name char(1) references Course_areas(name),
    primary key (iid, area_name)
); 

create table Registers(
    sid integer not null,
    launch_date date not null,
    crs_id integer not null,
    reg_date date not null,
    cc_id integer not null,
    cid integer not null,
    constraint FK_registers_cc foreign key(cid, cc_id) 
		references Credit_cards(cid, cc_id),
    constraint FK_registers_s foreign key(sid, launch_date, crs_id) 
		references Sessions(sid, launch_date, crs_id),
    primary key(reg_date, sid, launch_date, crs_id, cc_id, cid),
    unique(cid, crs_id, launch_date)
);

create table Purchases(
    trn_date date not null,
    remaining_ssn integer not null,
    cc_id integer not null,
    cid integer not null,
	pid integer references Course_packages(pid),
	constraint FK_purchases foreign key(cid, cc_id) 
		references Credit_cards(cid, cc_id),
    primary key(trn_date, cid, pid, cc_id)
);

create table Redeems(
    cid integer,
    pid integer,
    cc_id integer,
    trn_date date,
    sid integer,
    launch_date date,
    crs_id integer,
    redeem_date date not null,
    constraint FK_redeems_s foreign key(sid, launch_date, crs_id) 
		references Sessions(sid, launch_date, crs_id),
    constraint FK_redeems_p foreign key(trn_date, cid, pid, cc_id) 
		references Purchases(trn_date, cid, pid, cc_id),
    primary key(sid, launch_date, crs_id, cid, pid, cc_id, trn_date, redeem_date) 
);

create table Cancels(
    sid integer,
	launch_date date, 
	crs_id integer, 
	cancel_date date,
	refund_amt numeric check(refund_amt > 0),
	package_credit integer default 1,
	cid integer references Customers(cid),
	primary key(sid, launch_date, crs_id, cancel_date, cid), 
	constraint FK_cancels foreign key(sid, launch_date, crs_id) 
		references Sessions(sid, launch_date, crs_id),
	constraint CHECK_null CHECK(
	    (refund_amt ISNULL and package_credit=1) or
	    (refund_amt IS NOT NULL and package_credit=0)) 
); 

CREATE OR REPLACE VIEW allRegisteredCustomers AS
    SELECT sid, launch_date, crs_id, cid, cc_id, pid, trn_date, redeem_date, NULL AS reg_date FROM Redeems 
    UNION  
    SELECT sid, launch_date, crs_id, cid, cc_id, NULL, NULL, NULL, reg_date FROM Registers;
	
CREATE OR REPLACE VIEW instructorsWorkDuration AS
	SELECT iid, EXTRACT(MONTH FROM ssn_date) AS mth, EXTRACT(YEAR FROM ssn_date) AS yr, end_time - start_time AS duration
	FROM Instructors NATURAL JOIN Sessions
	WHERE e_status = 'P';
	
CREATE OR REPLACE VIEW fullTimerWorkPeriod AS
	SELECT eid, date_joined, date_departed
	FROM Employees
	WHERE hourly_rate ISNULL AND monthly_rate > 0;	
	
	
DROP FUNCTION IF EXISTS checkValidRegDateRedeems() CASCADE;
DROP FUNCTION IF EXISTS checkValidRegDateRegisters() CASCADE;
DROP FUNCTION IF EXISTS checkSsnVacancy() CASCADE;
DROP FUNCTION IF EXISTS checkValidCC_Registers() CASCADE;
DROP FUNCTION IF EXISTS checkValidCC_Purchases() CASCADE;
DROP FUNCTION IF EXISTS checkValidCancellation() CASCADE;
DROP FUNCTION IF EXISTS checkAdmins() CASCADE;
DROP FUNCTION IF EXISTS checkInstructors() CASCADE;
DROP FUNCTION IF EXISTS checkManagers() CASCADE;
DROP FUNCTION IF EXISTS checkEmployeeDeparture() CASCADE;
DROP FUNCTION IF EXISTS checkSessionsInsert() CASCADE;
DROP FUNCTION IF EXISTS checkAdminOfferings() CASCADE;
DROP FUNCTION IF EXISTS checkSessionsUpdate() CASCADE;
DROP FUNCTION IF EXISTS seatingCtyCheck() CASCADE;
DROP FUNCTION IF EXISTS check_all_inactive() CASCADE;
DROP FUNCTION IF EXISTS check_redeem_and_register() CASCADE;
DROP FUNCTION IF EXISTS updatePurchases() CASCADE;

--no.1-- check seating capacity (AFTER INSERT ON SESSIONS)
CREATE OR REPLACE FUNCTION seatingCtyCheck() RETURNS TRIGGER AS $$
DECLARE 
    sum_room_cty INTEGER;
    current_seating_cty INTEGER;
	targetNumber INTEGER;
    crsId INTEGER;
    launchDate DATE;
    startDate DATE;
    endDate DATE;
BEGIN 	
	IF (TG_OP = 'INSERT') THEN 
        SELECT S.crs_id, S.launch_date, SUM(R.room_cty), (
            SELECT S1.ssn_date 
            FROM new_table S1
            WHERE S.crs_id = S1.crs_id
            AND S.launch_date = S1.launch_date
            ORDER BY ssn_date DESC LIMIT 1 ), 
            (SELECT S1.ssn_date
            FROM new_table S1
            WHERE S.crs_id = S1.crs_id
            AND S.launch_date = S1.launch_date
            ORDER BY ssn_date ASC LIMIT 1) INTO crsId, launchDate, sum_room_cty, startDate, endDate
        FROM new_table S NATURAL JOIN Rooms R
        GROUP BY S.crs_id, S.launch_date; 
        
        SELECT target_num, seating_cty INTO targetNumber, current_seating_cty
        FROM Offerings, new_table
        WHERE new_table.crs_id = Offerings.crs_id
        AND new_table.launch_date = Offerings.launch_date;
	
        IF (sum_room_cty + current_seating_cty) > targetNumber THEN 
            UPDATE Offerings
            SET seating_cty = sum_room_cty, start_date = startDate, end_date = endDate
            WHERE crs_id = CrsId
            AND launch_date = launchDate;
            RETURN NEW; 
        ELSE
            RAISE EXCEPTION 'Seating_cty cannot be less than target_num.';
        END IF; 
	END IF;
END;
$$ 
LANGUAGE plpgsql;

CREATE TRIGGER seatingCtyCheck
AFTER INSERT ON Sessions 
REFERENCING NEW TABLE AS new_table
FOR EACH STATEMENT EXECUTE FUNCTION seatingCtyCheck();

-- no.2 -- check_all_inactive (BEFORE INSERT ON PURCHASES)
CREATE OR REPLACE FUNCTION check_all_inactive() RETURNS TRIGGER AS $$
DECLARE 
    remaining_count INTEGER;
    session_count INTEGER;
    _sale_start DATE;  
    _sale_end DATE;

BEGIN 
  SELECT COUNT(remaining_ssn) INTO remaining_count --number of active packages
  FROM Purchases
  WHERE NEW.cid = Purchases.cid
  AND remaining_ssn > 0;
  
  SELECT COUNT(ssn_date) INTO session_count --number of partially active packages
  FROM Redeems NATURAL JOIN Sessions 
  WHERE NEW.cid = Redeems.cid
  AND current_date + 7 <= ssn_date;

  SELECT sale_start, sale_end INTO _sale_start,  _sale_end
  FROM Course_packages C
  WHERE C.pid = NEW.pid;
  
    --checking if have any inactive or partially active
  IF remaining_count = 0 AND session_count = 0 AND (NEW.trn_date <=  _sale_end AND NEW.trn_date >= _sale_start) THEN 
    RETURN NEW;
  ELSE
    RAISE EXCEPTION 'Cannot purchase course package because customer already has an existing partially active/active package.';
  END IF;
  
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER check_all_inactive
BEFORE INSERT ON Purchases
FOR EACH ROW EXECUTE FUNCTION check_all_inactive();

-- no.3--checkSessionsInsert(BEFORE INSERT ON SESSIONS) 
CREATE OR REPLACE FUNCTION checkSessionsInsert() RETURNS TRIGGER
AS $$
DECLARE 
    crs_area CHAR(1);
	num_hours INTEGER;
	back_to_back BOOLEAN;
    departed BOOLEAN;
	_reg_deadline DATE;
	
BEGIN
   
	SELECT reg_deadline INTO _reg_deadline
	FROM Offerings O
	WHERE O.crs_id = NEW.crs_id
	AND O.launch_date = NEW.launch_date;
	
	IF (NEW.ssn_date < _reg_deadline) THEN 
		RAISE EXCEPTION 'Session cannot be inserted because registration deadline has passed!';
	END IF; 
	
	-- ssn_date needs to be a weekday
    IF EXTRACT(DOW FROM NEW.ssn_date) = 0 or EXTRACT(DOW FROM NEW.ssn_date) = 6 THEN
        RAISE EXCEPTION 'Session has to be conducted on a weekday';
    END IF; 

	-- ensure that room not in use
	IF EXISTS (SELECT 1 FROM Sessions
			  WHERE ((NEW.start_time BETWEEN start_time AND end_time) 
			  OR (NEW.end_time BETWEEN start_time AND end_time))
			  AND (NEW.ssn_date = ssn_date)
			  AND (NEW.rid = rid)
			  ) THEN
			  RAISE EXCEPTION 'Room in use';  
	END IF;
	
	-- check if employee has already departed or has yet to join the company 
	IF EXISTS (SELECT 1 FROM Employees E
			  WHERE E.eid = NEW.iid
			  AND ((E.date_joined > NEW.ssn_date) OR
				  (E.date_departed < NEW.ssn_date))
			   ) THEN
			   RAISE EXCEPTION 'No such Instructor in the company';
	END IF;
	
	-- selecting course area for comparison later
	SELECT course_area INTO crs_area FROM Courses WHERE NEW.crs_id = Courses.crs_id;
	
	-- finding of the total sum of working hours for the particular iid
	SELECT SUM(duration) INTO num_hours
	FROM instructorsWorkDuration
	WHERE (NEW.iid = iid) AND (EXTRACT(MONTH FROM NEW.ssn_date) = mth);
	
	-- back_to_back TRUE if there are instructors with back to back sessions 
	SELECT (SELECT 1 FROM Sessions S WHERE NEW.ssn_date = S.ssn_date AND NEW.iid = iid AND (NEW.start_time - 1 = end_time OR NEW.end_time + 1 = start_time)) INTO back_to_back;
	
	-- Instructor does not specialize in that area
	IF NOT EXISTS (
		SELECT 1 
		FROM Specializes
		WHERE iid = NEW.iid
		AND area_name = crs_area
	) THEN 
		RAISE EXCEPTION 'Instructor does not specialize in this area';
	
	--Instructor is teaching another session at this time slot
    ELSIF EXISTS (SELECT 1 FROM Sessions
        WHERE (
        (NEW.start_time >= (SELECT start_time FROM Sessions S WHERE S.iid = NEW.iid AND S.ssn_date = NEW.ssn_date) AND
        NEW.start_time <= (SELECT end_time FROM Sessions S WHERE S.iid = NEW.iid AND S.ssn_date = NEW.ssn_date)) 
        OR                                                                         
        (NEW.start_time <= (SELECT start_time FROM Sessions S WHERE S.iid = NEW.iid AND S.ssn_date = NEW.ssn_date) AND 
        NEW.end_time >= (SELECT end_time FROM Sessions S WHERE S.iid = NEW.iid AND S.ssn_date = NEW.ssn_date)))) THEN
        RAISE EXCEPTION 'Instructor is teaching another session now';
				 
	-- If new session added, pt Instructors teach > 30 hours in a month
	ELSEIF ((NEW.end_time - NEW.start_time) + num_hours) > 30 THEN
		RAISE EXCEPTION 'Part time instructors cannot teach for > 30 hours in a month!';
		
	-- Instructors teaching back to back sessions
	ELSEIF back_to_back = True THEN
		RAISE EXCEPTION 'Instructor has a back to back session';
	END IF;
	RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER checkSessionsInsert
BEFORE INSERT ON Sessions
FOR EACH ROW EXECUTE FUNCTION checkSessionsInsert();

--no.4--checkSessionsUpdate (BEFORE UPDATE OF rid ON SESSIONS)
CREATE OR REPLACE FUNCTION checkSessionsUpdate() RETURNS TRIGGER
AS $$
DECLARE 
	num_reg INTEGER;
	total_cty INTEGER;
	new_cty INTEGER;
BEGIN
	-- session already over, don't need to update the rid
	IF (NEW.ssn_date < CURRENT_DATE) THEN
		RAISE EXCEPTION 'Session has already happened. Not valid';
	END IF;

	-- getting the number of registrations, total capacity for session & room capacity for new session
	SELECT count(*) INTO num_reg FROM Registers R WHERE (NEW.sid = R.sid AND NEW.crs_id = R.crs_id);
	SELECT SUM(room_cty) INTO total_cty 
    FROM Sessions S NATURAL JOIN Rooms R 
    WHERE (NEW.sid = sid AND NEW.crs_id = crs_id);

	SELECT room_cty INTO new_cty FROM Rooms R WHERE NEW.rid = R.rid;
	
	-- checking if the room is available or not before updating new rid
	IF EXISTS (SELECT 1 FROM Sessions
			   WHERE ((NEW.start_time BETWEEN start_time AND end_time)
					  OR (NEW.end_time BETWEEN start_time AND end_time))
			   AND (NEW.ssn_date = ssn_date)
			   AND (NEW.rid = rid)
			  ) THEN
			  RAISE EXCEPTION 'Room in use';
			  
	-- when you update with a rid that has capacity that is too small to host the amt of people that registered for the course
	ELSEIF num_reg > total_cty + new_cty THEN
		RAISE EXCEPTION 'Room is too small to be used';
	END IF;
	
	-- rid is valid, update the seating_cty in Offerings 
	UPDATE Offerings 
	SET seating_cty = seating_cty + new_cty
	WHERE (crs_id = NEW.crs_id) AND (launch_date = NEW.launch_date);
	RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER checkSessionsUpdate
BEFORE UPDATE OF rid ON Sessions
FOR EACH ROW EXECUTE FUNCTION checkSessionsUpdate();

--no.5--checkValidRegDateRegisters (AFTER INSERT OR UPDATE ON Registers)

CREATE OR REPLACE FUNCTION checkValidRegDateRegisters() RETURNS TRIGGER AS $$ 
DECLARE 
    regDeadline date;
BEGIN
    -- offering is uniquely identified by crs_id + launch_date 
    SELECT reg_deadline INTO regDeadline 
    FROM Offerings
    WHERE crs_id = NEW.crs_id
    AND launch_date = NEW.launch_date;
	
    IF NEW.reg_date <= regDeadline AND NEW.reg_date >= NEW.launch_date THEN 
        RETURN NEW;
    ELSE 
        RAISE EXCEPTION 'Cannot register as the registration deadline has past or course offering has not been launched yet!';
	END IF;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER checkValidRegDateRegisters
AFTER INSERT OR UPDATE ON Registers
FOR EACH ROW EXECUTE FUNCTION checkValidRegDateRegisters();

--no.6--checkValidRegDateRedeems(AFTER INSERT OR UPDATE ON Redeems)
CREATE OR REPLACE FUNCTION checkValidRegDateRedeems() RETURNS TRIGGER AS $$ 
DECLARE 
    regDeadline date;
BEGIN
    -- offering is uniquely identified by crs_id + launch_date 
    SELECT reg_deadline INTO regDeadline 
    FROM Offerings
    WHERE crs_id = NEW.crs_id
    AND launch_date = NEW.launch_date;

    IF NEW.redeem_date <= regDeadline AND NEW.redeem_date >= NEW.launch_date THEN 
        RETURN NEW;
    ELSE 
        RAISE EXCEPTION 'Cannot redeem as the registration deadline has past or course offering has not been launched yet!';
	END IF;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER checkValidRegDateRedeems
AFTER INSERT OR UPDATE ON Redeems
FOR EACH ROW EXECUTE FUNCTION checkValidRegDateRedeems();

--no.7/8--checkSsnVacancy(BEFORE INSERT OR UPDATE ON Registers/Redeems)
CREATE OR REPLACE FUNCTION checkSsnVacancy() RETURNS TRIGGER AS $$ 
DECLARE  
    num_people integer;
    roomcty integer;
    room integer;
BEGIN
   -- step 1 is to count how many people have registered/redeemed for that session 
    SELECT S.rid, count(cid) INTO room, num_people 
    FROM allRegisteredCustomers V NATURAL JOIN Sessions S
    WHERE sid = NEW.sid
    AND launch_date = NEW.launch_date
    AND crs_id = NEW.crs_id
	GROUP BY S.rid;

    -- step 2 is to get the vacancy of the new registration
    SELECT room_cty INTO roomcty
    FROM Sessions S INNER JOIN Rooms R 
    ON S.rid = R.rid
    WHERE S.sid = NEW.sid
    AND launch_date = NEW.launch_date
    AND crs_id = NEW.crs_id;

	IF roomcty - num_people ISNULL THEN
		RETURN NEW;
    ELSIF roomcty - num_people > 0 THEN 
        RETURN NEW;
    ELSE 
		RAISE EXCEPTION 'There is not enough space in this class!';
    END IF;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER ssnVacancy_Registers
BEFORE INSERT OR UPDATE ON Registers
FOR EACH ROW EXECUTE FUNCTION checkSsnVacancy();

CREATE TRIGGER ssnVacancy_Redeems
BEFORE INSERT OR UPDATE ON Redeems
FOR EACH ROW EXECUTE FUNCTION checkSsnVacancy();

--no.9--checkValidCC(BEFORE INSERT OR UPDATE ON Registers)
CREATE OR REPLACE FUNCTION checkValidCC_Registers() RETURNS TRIGGER AS $$ 
DECLARE  
    activationDate DATE;

BEGIN
        -- check if the activation date of the credit card used is before the registeration date
        SELECT activation_date INTO activationDate
        FROM Credit_cards 
        WHERE NEW.cid = cid
        AND NEW.cc_id = cc_id;
		
		IF EXISTS(
            SELECT 1 
            FROM Credit_cards C
            WHERE C.cid = NEW.cid 
            AND C.activation_date > activationDate
            AND C.activation_date <= NEW.reg_date
            AND C.cc_id != NEW.cc_id) THEN
            RAISE EXCEPTION 'There is a newer activated credit card that should be used to complete this transaction!';
        ELSIF (activationDate > NEW.reg_date) THEN
            RAISE EXCEPTION 'Invalid registration as your credit card is not activated!';
        ELSE    
			RETURN NEW;
        END IF;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER ccValid_trigger_Registers
BEFORE INSERT OR UPDATE ON Registers
FOR EACH ROW EXECUTE FUNCTION checkValidCC_Registers();

--no.10--checkValidCancellation(BEFORE INSERT ON Cancels)
CREATE OR REPLACE FUNCTION checkValidCancellation() RETURNS TRIGGER AS $$ 
DECLARE   
	_ssn_date date;
    _pid integer;
    _cid integer;
    _cc_id integer;
    _trn_date date;
    _offering_fee numeric;
	
BEGIN
    -- need to find which is the session start date 
    SELECT S.ssn_date INTO _ssn_date
    FROM SESSIONS S 
    WHERE NEW.sid = S.sid 
    AND NEW.launch_date = S.launch_date
    AND NEW.crs_id = S.crs_id;

	IF (NEW.cancel_date > _ssn_date) THEN 
		RAISE EXCEPTION 'Session has already passed, cannot be cancelled!';
		
    -- Check whether the cancellation date + 7 <= session.start_date
    ELSEIF (NEW.cancel_date + 7 <= _ssn_date) THEN 
        
        -- check if (sid, launch_date, crs_id) is in redeems table 
        IF EXISTS(
            SELECT 1 
            FROM Redeems R 
            WHERE NEW.sid = R.sid
            AND NEW.launch_date = R.launch_date
            AND NEW.crs_id = R.crs_id
            AND NEW.cid = R.cid
        )   

        -- customer did redeem that session
        THEN 
            -- get the pid and cid to retrieve the record from the purchases table 
            SELECT pid, cid, cc_id, trn_date INTO _pid, _cid, _cc_id, _trn_date 
            FROM Redeems R 
            WHERE NEW.sid = R.sid
            AND NEW.launch_date = R.launch_date
            AND NEW.crs_id = R.crs_id;
            
            -- then delete from the Redeems table 
            DELETE FROM Redeems R 
            WHERE NEW.sid = R.sid
            AND NEW.launch_date = R.launch_date
            AND NEW.crs_id = R.crs_id
            AND NEW.cid = R.cid;
			
            -- find the package linked to the customer and increase the number of remaining sessions 
            UPDATE Purchases 
            SET remaining_ssn = remaining_ssn + 1
            WHERE cid = _cid
            AND pid = _pid
            AND cc_id = _cc_id 
            AND trn_date = _trn_date; 

            RETURN NEW;
        
        -- check if (sid, launch_date, crs_id) is in Registers table 
        ELSIF EXISTS(
            SELECT 1 
            FROM Registers R 
            WHERE NEW.sid = R.sid
            AND NEW.launch_date = R.launch_date
            AND NEW.crs_id = R.crs_id
            AND NEW.cid = R.cid
            ) 
            THEN
            -- need to find the offering first 
                SELECT fees INTO _offering_fee
                FROM Offerings O
                WHERE NEW.crs_id = O.crs_id
                AND NEW.launch_date = O.launch_date;

            -- check that cancel.refund_amt = 0.9 * offerings.fees
                IF (NEW.refund_amt = ROUND(0.9 * _offering_fee, 2)) THEN 
                    -- then delete from the Registers table 
                    DELETE FROM Registers R 
                    WHERE NEW.sid = R.sid
                    AND NEW.launch_date = R.launch_date
                    AND NEW.crs_id = R.crs_id
                    AND NEW.cid = R.cid;
                    
                    RETURN NEW;
                ELSE 
                    RAISE EXCEPTION 'Deletion from Registers table not allowed as refund amount is incorrect!';
    			END IF;
        ELSE  
            RAISE EXCEPTION 'No record found in the Redeems or Registers table';
		END IF;
    ELSE
        RAISE EXCEPTION 'Cannot cancel less than 7 days before the session start date!';
	END IF;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER validCancellation_trigger
BEFORE INSERT ON Cancels
FOR EACH ROW EXECUTE FUNCTION checkValidCancellation();

--no.11/12--check_redeem_and_register(BEFORE INSERT ON Registers/Redeems)
CREATE OR REPLACE FUNCTION check_redeem_and_register() RETURNS TRIGGER AS $$
DECLARE 
    customer INTEGER;
BEGIN 
  IF EXISTS(
        SELECT 1
        FROM allRegisteredCustomers
        --checking based on the PK of offerings (crs_id, launch_date) to ensure the cust registers/redeems for only 1 session in each course offering 
        WHERE NEW.launch_date = allRegisteredCustomers.launch_date 
        AND NEW.crs_id = allRegisteredCustomers.crs_id
        AND NEW.cid = allRegisteredCustomers.cid) THEN 
        RAISE EXCEPTION 'Customer has already redeemed or registered a session for this offering!';
  ELSE
        RETURN NEW;
  END IF;
END;
$$ LANGUAGE plpgsql;  

CREATE TRIGGER check_redeem_and_register
BEFORE INSERT ON Registers
FOR EACH ROW EXECUTE FUNCTION check_redeem_and_register();

CREATE TRIGGER check_redeem_and_register
BEFORE INSERT ON Redeems
FOR EACH ROW EXECUTE FUNCTION check_redeem_and_register();

--no.13--checkAdmins(BEFORE INSERT OR UPDATE ON Administrators)
CREATE OR REPLACE FUNCTION checkAdmins() RETURNS TRIGGER AS $$
BEGIN
	IF EXISTS (SELECT 1 FROM Managers WHERE mid = NEW.aid) THEN
        RAISE EXCEPTION 'Admin is already present in Managers table.';
    ELSIF EXISTS (SELECT 1 FROM Instructors WHERE iid = NEW.aid) THEN 
        RAISE EXCEPTION 'Admin is already present in Instructors table.';
	END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER checkAdmins
BEFORE INSERT OR UPDATE ON Administrators
FOR EACH ROW
EXECUTE FUNCTION checkAdmins();

-- no.14--checkInstructors(BEFORE INSERT OR UPDATE ON Instructors)
CREATE OR REPLACE FUNCTION checkInstructors() RETURNS TRIGGER AS $$
BEGIN
	IF EXISTS (SELECT 1 FROM Managers WHERE mid = NEW.iid) THEN
        RAISE EXCEPTION 'Instructor is already present in Managers table.';
    ELSIF EXISTS (SELECT 1 FROM Administrators WHERE aid = NEW.iid) THEN 
        RAISE EXCEPTION 'Instructor is already present in Administrators table.';
	END IF;
    RETURN NEW; 
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER checkInstructors
BEFORE INSERT OR UPDATE ON Instructors
FOR EACH ROW
EXECUTE FUNCTION checkInstructors();

--no.15--checkManagers(BEFORE INSERT OR UPDATE ON Managers)
CREATE OR REPLACE FUNCTION checkManagers() RETURNS TRIGGER AS $$
BEGIN
	IF EXISTS (SELECT 1 FROM Administrators WHERE aid = NEW.mid) THEN
        RAISE EXCEPTION 'Manager is already present in Administrators table.';
    ELSIF EXISTS (SELECT 1 FROM Instructors WHERE iid = NEW.mid) THEN 
        RAISE EXCEPTION 'Manager is already present in Instructors table.';
	END IF;
    RETURN NEW; 
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER checkManagers
BEFORE INSERT OR UPDATE ON Managers
FOR EACH ROW
EXECUTE FUNCTION checkManagers();

--no.16--checkEmployeeDeparture(BEFORE UPDATE OF date_departed ON Employees)
CREATE OR REPLACE FUNCTION checkEmployeeDeparture() RETURNS TRIGGER AS $$
DECLARE
    area TEXT;
    ssn TEXT;
    offering TEXT;
BEGIN
    -- for manager check that they dont have an existing associated course area
	IF OLD.e_type = 'M' THEN
        IF EXISTS (SELECT 1 FROM Course_areas WHERE mid = OLD.eid) THEN
            -- assign all the associated areas
            area := (SELECT string_agg(name, ', ') FROM Course_areas WHERE mid = OLD.eid);
            RAISE EXCEPTION 'Manager cannot leave; manages course areas: %.', area;
        END IF;
    -- if instructor check they arent teaching a session that has yet to occur
    ELSEIF OLD.e_type = 'I' THEN
        IF EXISTS (
            SELECT 1 FROM Sessions 
            WHERE ssn_date > NEW.date_departed
            AND iid = OLD.eid
        ) THEN 
            -- assign all the associated sessions
            ssn := (SELECT string_agg(sid||' '||launch_date||' '||crs_id, ', ') FROM Sessions WHERE iid = OLD.eid);
            RAISE EXCEPTION 'Instructor cannot leave; teaches future occuring sessions: %', ssn;
        END IF;
    -- if admin check they arent handling any offerings where reg_deadline is after departure date
    ELSE 
        IF EXISTS (
            SELECT 1 FROM Offerings
            WHERE reg_deadline > NEW.date_departed
            AND aid = OLD.eid) THEN 

            -- assign all the associated offerings
            offering := (SELECT string_agg(launch_date || ' ' || crs_id, ', ') FROM Offerings WHERE aid = OLD.eid);
            RAISE EXCEPTION 'Admin cannot leave; handles offerings: %', offering;
        END IF;
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER checkEmployeeDeparture
BEFORE UPDATE OF date_departed
    ON Employees
FOR EACH ROW
EXECUTE FUNCTION checkEmployeeDeparture();

--no.17--checkAdminOfferings(BEFORE INSERT ON Offerings)
CREATE OR REPLACE FUNCTION checkAdminOfferings() RETURNS TRIGGER AS $$
DECLARE
    dd DATE;
    dj DATE;
BEGIN
    SELECT date_departed, date_joined INTO dd, dj FROM Employees WHERE eid = NEW.aid;
	
    -- if departure date is before deadline then already departed
	IF (dd IS NOT NULL AND (NEW.reg_deadline > dd)) THEN
        RAISE EXCEPTION 'Admin has already departed company.';
		
	ELSIF (NEW.launch_date < dj) THEN
        RAISE EXCEPTION 'Admin has yet to join the company.';
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER checkAdminOfferings
BEFORE INSERT ON Offerings
FOR EACH ROW
EXECUTE FUNCTION checkAdminOfferings();

--no.18--updatePurchases(AFTER INSERT ON Redeems)
CREATE OR REPLACE FUNCTION updatePurchases() RETURNS TRIGGER AS 
$$
DECLARE
    _remaining_ssn INTEGER;
BEGIN 
    SELECT remaining_ssn INTO _remaining_ssn
    FROM Purchases   
    WHERE cid = NEW.cid 
    AND pid = NEW.pid
    AND trn_date = NEW.trn_date
    AND cc_id = NEW.cc_id;

    IF (_remaining_ssn -1 < 0) THEN
        DELETE FROM Redeems 
        WHERE cid = NEW.cid 
        AND pid = NEW.pid
        AND trn_date = NEW.trn_date
        AND cc_id = NEW.cc_id
        AND sid = NEW.sid
        AND launch_date = NEW.launch_date
        AND crs_id = NEW.crs_id
        AND cid = NEW.cid
        AND redeem_date = NEW.redeem_date;
        RAISE EXCEPTION 'Customer cannot redeem from purchases, does not have anymore free sessions left.';
    END IF;

    UPDATE Purchases 
    SET remaining_ssn = remaining_ssn-1
    WHERE cid = NEW.cid 
    AND pid = NEW.pid
    AND trn_date = NEW.trn_date
    AND cc_id = NEW.cc_id;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER updatePurchases
AFTER INSERT ON Redeems
FOR EACH ROW EXECUTE FUNCTION updatePurchases();

--no. 19--checkValidCC (BEFORE INSERT OR UPDATE ON Purchases)
CREATE OR REPLACE FUNCTION checkValidCC_Purchases() RETURNS TRIGGER AS $$ 
DECLARE  
    activationDate DATE;
    _check BOOLEAN;

BEGIN
        -- check if the activation date of the credit card used is before the registeration date
        SELECT activation_date INTO activationDate
        FROM Credit_cards 
        WHERE NEW.cid = cid
        AND NEW.cc_id = cc_id;
        
        IF EXISTS(
            SELECT 1 
            FROM Credit_cards C
            WHERE C.cid = NEW.cid 
            AND C.activation_date > activationDate
            AND C.activation_date <= NEW.trn_date
            AND C.cc_id != NEW.cc_id) THEN
			RAISE NOTICE 'NEW.cid: %, NEW.trn_date: %', NEW.cid, NEW.trn_date;
            RAISE EXCEPTION 'There is a newer activated credit card that should be used to complete this transaction!';
        ELSIF (activationDate > NEW.trn_date) THEN
            RAISE EXCEPTION 'Invalid purchase as your credit card is not activated!';
        ELSE    
			RETURN NEW;
        END IF;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER ccValid_trigger_Purchases
BEFORE INSERT OR UPDATE ON Purchases
FOR EACH ROW EXECUTE FUNCTION checkValidCC_Purchases();