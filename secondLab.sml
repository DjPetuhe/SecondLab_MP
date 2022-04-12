(*First task*)
fun is_older ((y1,m1,d1),(y2,m2,d2))=
    if y1 > y2 then false else if y2 > y1 then true
    else if m1 > m2 then false else if m2 > m1 then true
    else if d1 < d2 then true else false

(*Second task*)
fun number_in_month (dateList : (int*int*int) list, month : int) =
    if null dateList
    then 0
    else if (#2 (hd dateList)) = month
    then 1 + number_in_month (tl dateList, month)
    else number_in_month(tl dateList, month)

(*Also second task. Just alternative*)
fun number_in_month2 ([], _) = 0
  | number_in_month2 ((_,m,_)::dates,month) =
    if m = month then 1 + number_in_month2(dates, month)
    else number_in_month2(dates, month)

(*Third task*)
fun number_in_months (_, []) = []
  | number_in_months (dateList : (int*int*int) list, month::monthes) =
    number_in_month2(dateList, month) :: number_in_months(dateList, monthes)

(*fourth task*)
fun dates_in_month ([], _) = []
  | dates_in_month (dateList : (int*int*int) list, month : int) =
    if (#2 (hd dateList)) = month then (hd dateList) :: dates_in_month((tl dateList), month)
    else dates_in_month((tl dateList), month)

(*fifth task*)
fun dates_in_months (_, []) = []
  | dates_in_months (dateList, month::monthes) =
    dates_in_month(dateList, month) @ dates_in_months(dateList, monthes)

(*sixth task*)
fun get_nth (strList : string list, num : int) =
    let fun get_nth_count ([], _) = ""
          | get_nth_count (str::strs, from : int) =
            if from = num
            then str
            else get_nth_count(strs, from + 1)
    in get_nth_count(strList, 1) end

(*seventh task*)
fun date_to_string (date : int*int*int) =
    let val month_str = ["January",
        "February", "March", "April",
        "May", "June", "July", "August",
        "September", "November", "October",
        "December"] : string list
        fun get_str_month ([], _) = ""
          | get_str_month (month::monthes, cnt : int) =
            if (#2 date) = cnt then month
            else get_str_month(monthes, cnt + 1)
    in 
        get_str_month(month_str, 1) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
    end

(*eighth* task*)
fun number_before_reaching_sum (_, []) = 1
  | number_before_reaching_sum (sum, number::numbers) =
    if (sum - number > 0)
    then number_before_reaching_sum(sum - number, numbers) + 1
    else 1

(*ninth* task*)
fun what_month (day : int) =
    let val month_days = [31, 28,31,30,
        31,30,31,31,30,31,30,31] : int list
    in number_before_reaching_sum(day, month_days) end

(*tenth task*)
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1 + 1, day2)

(*eleventh task*)
fun oldest ([]) = NONE
  | oldest (date::dates) =
    let fun older ([], oldestDate) = SOME oldestDate
    | older (date::dates, oldestDate) =
        if is_older(oldestDate, date)
        then older(dates, oldestDate)
        else older(dates, date)
    in older(dates, date) end

(*Test for first task*)
fun provided_test1 () = 
    let val date1 = (2022,6,21)
        val date2 = (2022,6,20)
        val date3 = (2020,6,20)
        val date4 = (2020,7,20)
    in
    (is_older (date1, date2),
    is_older (date2, date1),
    is_older (date1, date2),
    is_older (date1, date1),
    is_older (date3, date4),
    is_older (date2, date3))
    end

(*Test for second task. Both functions*)
fun provided_test2 () = 
    let val dateList1 = [(2022,4,3),(2022,2,12),(2021,2,1)] : (int*int*int) list
        val dateList2 = [(2010,2,1),(2015,2,5),(2020,2,10),(2025,2,3)] : (int*int*int) list
    in
    (number_in_month(dateList1, 4),
    number_in_month(dateList1, 2),
    number_in_month(dateList1, 10),
    number_in_month(dateList2, 2),
    number_in_month2(dateList1, 4),
    number_in_month2(dateList1, 2),
    number_in_month2(dateList1, 10),
    number_in_month2(dateList2, 2))
    end

(*Test for third task*)
fun provided_test3 () = 
    let val dateList1 = [(3,4,2022),(12,2,2022),(1,2,2021)] : (int*int*int) list
        val dateList2 = [(1,2,2010),(5,2,2015),(10,2,2020),(3,2,2025)] : (int*int*int) list
        val monthList = [4,10,2] : int list
    in
    (number_in_months(dateList1, monthList),
    number_in_months(dateList2, monthList))
    end

(*Test for the fourth task*)
fun provided_test4 () = 
    let val dateList1 = [(2022,4,3),(2022,2,12),(2021,2,1)] : (int*int*int) list
        val dateList2 = [(2010,2,1),(2015,2,5),(2020,2,10),(2025,2,3)] : (int*int*int) list
    in
    (dates_in_month(dateList1, 4),
    dates_in_month(dateList1, 2),
    dates_in_month(dateList2, 2),
    dates_in_month(dateList2, 10))
    end

(*Test for the fifth task*)
fun provided_test5 () = 
    let val dateList1 = [(2022,4,3),(2022,2,12),(2021,2,1)] : (int*int*int) list
        val dateList2 = [(2010,2,1),(2015,2,5),(2020,2,10),(2025,2,3)] : (int*int*int) list
        val monthList1 = [4,10,2] : int list
        val monthList2 = [5,4,3] : int list
    in
    (dates_in_months(dateList1, monthList1),
    dates_in_months(dateList1, monthList2),
    dates_in_months(dateList2, monthList1),
    dates_in_months(dateList2, monthList2))
    end

(*Test for the sixth task*)
fun provided_test6 () = 
    let val strList = ["first","second","third","fourth","fifth"] : string list
    in
    (get_nth(strList, 4),
    get_nth(strList, 2),
    get_nth(strList, 1),
    get_nth(strList, 6))
    end

(*Test for seventh task*)
fun provided_test7 () = 
    let val date1 = (2022,3,21)
        val date2 = (2021,8,8)
        val date3 = (2014,12,30)
        val date4 = (2005,1,18)
    in
    (date_to_string(date1),
    date_to_string(date2),
    date_to_string(date3),
    date_to_string(date4))
    end

(*Test for eighth task*)
fun provided_test8 () = 
    let val list1 = [12,44,12,67,2] : int list
        val list2 = [1,2,3,4,5,6] : int list
        val list3 = [1,1,1,2,2,1,1,1,1] : int list
    in
    (number_before_reaching_sum(58, list1),
    number_before_reaching_sum(10, list2),
    number_before_reaching_sum(4, list3))
    end

(*Test for ninth task*)
fun provided_test9() =
    let val day1 = 51 : int
        val day2 = 365 : int
        val day3 = 1 : int
    in
    (what_month(day1),
    what_month(day2),
    what_month(day3))
    end

(*Test for tenth task*)
fun provided_test10() =
    let val day1 = 31 : int
        val day2 = 32 : int
        val day3 = 365 : int
        val day4 = 330 : int
    in
    (month_range(day1, day2),
    month_range(day3, day4),
    month_range(day4, day3))
    end

(*Test for eleventh task*)
fun provided_test11() =
    let val dateList1 = [(2020,12,10),(2022,1,5),(2015,7,1),(2022,1,4)] : (int*int*int) list
        val dateList2 = [(2022,10,23),(2022,11,1),(2022,1,2)] : (int*int*int) list
        val dateList3 = [] : (int*int*int) list
    in
    (oldest(dateList1),
    oldest(dateList2),
    oldest(dateList3))
    end


val ans_first = provided_test1()
val ans_second = provided_test2()
val ans_third = provided_test3()
val ans_fourth = provided_test4()
val ans_fifth = provided_test5()
val ans_sixth = provided_test6()
val ans_seventh = provided_test7()
val ans_eighth = provided_test8()
val ans_ninth = provided_test9()
val ans_tenth = provided_test10()
val ans_eleventh = provided_test11()