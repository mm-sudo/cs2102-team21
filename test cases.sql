/*no.1 test*/
CALL add_Employee ('Vera', '345 Toa Payoh Ave 2', '95662222','verakohyingsim@gmail.com', '1000.00', 'M', '2021-03-21', ARRAY['A', 'B']);

/*no.2 test*/
CALL remove_employee(20, CURRENT_DATE); -- will work since iid has no sessions
CALL remove_employee(29, CURRENT_DATE); -- will not work since iid teaches a ssn after departure date, i want to edit the trigger to show a nicer exception statement
CALL remove_employee(5, CURRENT_DATE); -- wont work since aid's reg_deadline is after today's date

/*no.3 test*/
CALL add_customer('Gina', 'Rivervale Drive', '9765 4321', 'gina@gmail.com', '1234567891234567', '2024/05/03', '456');

/*no.4 test*/
CALL update_credit_card(1, '6450516111111111', 526, '2022/03/26'); -- will work 
CALL update_credit_card(1, '6450516111111111', 526, '2022/03/26'); -- calling it again would not work because the same cid can only update their credit card once a day (even if they have more than one credit card) 

/*no.5 test*/
call add_course('A', 'sSgarggthh', 'sfgasrgd', 5); -- will work 
call add_course('F', 'sdthsth', 'sfgasrgd', 5); -- will not work 
call add_course('A', 'srgerg', 'sagrarg', 5); -- will work 
select * from Courses; -- to see if the ones that are working have been inserted 

/*no.6 test*/
-- SELECT * FROM SESSIONS;
SELECT * FROM find_instructors(1, '2021/01/17', 14); -- 8 records
SELECT * FROM find_instructors(2, '2021/03/02', 14); -- doesnt contain 26 that teaches the session

/*no.7 test*/
SELECT * FROM get_available_instructors(13, '2021/05/07', '2021/05/08');

/*no.8 test*/
select * from find_rooms('2021/02/03', 14, 3); --shouldnt have 324
select * from Offerings;

/*no.9 test*/
SELECT * FROM get_available_rooms('2021-03-02', '2021-03-02'); -- select sessions on the same day for easy checking
	-- sessions are taking place in 
	-- (1) rid 306 11am - 12pm 
	-- (2) rid 319 2pm-3pm, 3pm - 4pm, 4pm - 5pm
	-- (3) rid 349 2pm-3pm, 3pm - 4pm
    -- SELECT * FROM SESSIONS WHERE ssn_date = '2021-03-02' ORDER BY RID ASC;

/*no.10 test*/
CALL add_course_offering(10, 400.50, '2021/06/03', '2021/06/13', 30, 2, 
'[{"_ssn_date": "2021/06/23", "_ssn_start_time": 15, "_rid": 303}, 
{"_ssn_date": "2021/06/25", "_ssn_start_time": 14, "_rid": 310}, 
{"_ssn_date": "2021/06/29", "_ssn_start_time": 10, "_rid": 316}]');

SELECT * FROM Sessions WHERE launch_date = '2021/06/03' AND crs_id = 10;
SELECT * FROM Courses WHERE crs_id = 13;

-- wrong example, ssn has to be conducted on a weekday
CALL add_course_offering(10, 400.50, '2021/06/03', '2021/06/13', 30, 2, 
'[{"_ssn_date": "2021/06/23", "_ssn_start_time": 15, "_rid": 303}, 
{"_ssn_date": "2021/06/26", "_ssn_start_time": 14, "_rid": 310}, 
{"_ssn_date": "2021/06/29", "_ssn_start_time": 10, "_rid": 316}]');

/*no.11 test*/
CALL add_course_package('Premium pkg', 4, '2021/04/21', '2021/05/03', 500);

/*no.12 test*/
select * from get_available_course_packages();

/*no.13 test*/
CALL buy_course_package(5, 5);  -- not supposed to work because customer already has an existing partially active/active package
	-- SELECT * FROM PURCHASES WHERE CID=5; // will return 1 record where cid 5 has purchased pid 16

CALL buy_course_package(35, 5); -- will work, only cid 35, 46 and 47 don't have packages 
	-- SELECT * FROM PURCHASES ORDER BY cid ASC;
	
/*no.14 test*/
SELECT * FROM Redeems WHERE cid = 2;
SELECT * FROM Purchases WHERE cid = 40;
SELECT * FROM get_my_course_package(40); -- no redeemed sessions

SELECT * FROM Purchases WHERE cid = 2;
SELECT * FROM get_my_course_package(2); -- 2 redeemed sessions

/*no.15 test*/
SELECT * FROM get_available_course_offerings();

/*no.16 test*/
select * from get_available_course_sessions(2, '2021/05/26'); -- return empty tables
select * from get_available_course_sessions(6, '2021/03/31'); -- contains values 

/*no.17 test*/
CALL register_session(39, 6, '2021-03-31', 2, 'C'); 
-- SELECT * FROM registers where cid = 39; 

/*no.18 test*/
SELECT cid, ssn_date, redeem_date, reg_date FROM allRegisteredCustomers NATURAL JOIN Sessions WHERE ssn_date > CURRENT_DATE ORDER BY cid;
SELECT cid, ssn_date, redeem_date, reg_date FROM allRegisteredCustomers NATURAL JOIN Sessions WHERE cid = 2;
SELECT * FROM get_my_registrations(2); -- has 2 active reg, 1 thats over
SELECT * FROM get_my_registrations(3); -- has no reg

/*no.19 test*/
CALL update_course_session(23, 10, '2021/07/01', 2); -- cannot change as launch date is not there yet 
CALL update_course_session(41, 6, '2021/03/31', 2); -- test case that works 

/*no.20 test*/
call cancel_registration(7, 11,  '2021/06/03'); --correct 
call cancel_registration(19, 1,  '2021/01/17'); -- wrong 
select * from Cancels; --to check if the correct one got inserted into the cancels table 

/*no.21 test*/
CALL update_instructor(6, '2021-03-31', 1, 27);
-- SELECT * FROM Instructors WHERE iid=27;

/*no.22 test*/
SELECT * FROM Sessions WHERE ssn_date < CURRENT_DATE;
CALL update_room(1, '2021-01-17', 1, 324); -- ssn has alr happened not valid
CALL update_room(9, '2021-06-09', 1, 345); -- ssn works 
--SELECT * FROM Sessions WHERE 

/*no.23 test*/
CALL remove_session(1, '2021/01/17', 3); -- cannot remove ssn as it is already over
CALL remove_session(6, '2021/03/31', 3); -- valid removal 

/*no.24 test*/
call add_session(4, 1, '2021/01/17', 319, 1, '2021/01/23', 9); --wrong: check_offerings table constraint 
call add_session(4, 1, '2021/01/17', 319, 26, '2021/02/10', 17); --wrong: 17 + 3 hours is after 18
call add_session(4, 1, '2021/01/17', 319, 26, '2021/02/10', 9); --correct 
select * from Sessions;

/*no.25 test*/
SELECT * FROM PAY_SALARY('2021-01-04'); -- not supposed to work because not last day of the month

/*no.26 test*/
SELECT * FROM allRegisteredCustomers NATURAL JOIN Courses WHERE cid = 22; -- crs area is D
SELECT * FROM promote_courses(); -- look at customer 18, 21, 22

-- has one record on '2021-04-06'  hence it wont be there in the records of promote_courses()
SELECT * FROM allRegisteredCustomers WHERE cid = 2; 
-- dont have any in the last 6 mths so its there in promote_courses()
SELECT * FROM allRegisteredCustomers WHERE cid = 4; 

/*no.27 test case*/
SELECT * FROM top_packages(2);

/*no.28 test*/
select * from popular_courses();

/*no.29 test*/
SELECT * FROM view_summary_report(1);

/*no.30 test*/
SELECT * FROM view_manager_report();