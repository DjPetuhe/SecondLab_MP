(*First task*)
fun is_order (firstDate : int*int*int, secondDate : int*int*int) =
    if (#1 firstDate) > (#1 secondDate)
    then false
    else if (#1 firstDate) < (#1 secondDate)
    then true
    else
        if (#2 firstDate) > (#2 secondDate)
        then false
        else if (#2 firstDate) < (#2 secondDate)
        then true
        else
            if (#3 firstDate) < (#3 secondDate)
            then true
            else false

(*Second task*)
fun number_in_month (dateList : (int*int*int) list, month : int) =
    if null dateList
    then 0
    else if (#2 (hd dateList)) = month
    then 1 + number_in_month (tl dateList, month)
    else number_in_month(tl dateList, month)

(*Also second task. Just alternative*)
fun number_in_month2 (dateList : (int*int*int) list, month : int) =
    case dateList of
    [] => 0
    | (_,m,_)::tl => if m = month then 1 + number_in_month2(tl, month)
                     else number_in_month2(tl, month)

(*Third task*)
fun number_in_months (dateList : (int*int*int) list, monthList : int list) =
    case monthList of
    [] => []
    | _ => number_in_month2(dateList, (hd monthList)) :: number_in_months(dateList, (tl monthList))

(*fourth task*)
fun dates_in_month (dateList : (int*int*int) list, month : int) =
    case dateList of
    [] => []
    | (_,m,_)::tl => if m = month then (hd dateList) :: dates_in_month(tl, month)
                     else dates_in_month(tl, month)

(*fifth task*)
fun dates_in_months (dateList : (int*int*int) list, monthList : int list) =
    case monthList of
    [] => []
    | _ => dates_in_month(dateList, (hd monthList)) @ dates_in_months(dateList, (tl monthList))

(*sixth*)
fun get_nth (strList : string list, num : int) =
    let fun get_nth_count(strList2 : string list, from : int) =
        if null strList2
        then ""
        else if from = num
        then hd strList2
        else get_nth_count(tl strList2, from + 1)
    in get_nth_count(strList, 1) end

(*seventh*)
fun date_to_string (date : int*int*int) =
    let val month_str = ["January",
    "February", "March", "April",
    "May", "June", "July", "August",
    "September", "November", "October",
    "December"] : string list
    fun get_str_month (months : string list, cnt : int) =
        case months of
        [] => ""
        | _ => if (#2 date) = cnt then (hd months)
               else get_str_month(tl months, cnt + 1)
    in 
        get_str_month(month_str, 1) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
    end

(*Test for first task*)
fun provided_test1 () = 
    let val date1 = (2022,6,21)
        val date2 = (2022,6,20)
        val date3 = (2020,6,20)
        val date4 = (2020,7,20)
    in
    (is_order (date1, date2),
    is_order (date2, date1),
    is_order (date1, date2),
    is_order (date1, date1),
    is_order (date3, date4),
    is_order (date2, date3))
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

val ans_first = provided_test1()
val ans_second = provided_test2()
val ans_third = provided_test3()
val ans_fourth = provided_test4()
val ans_fifth = provided_test5()
val ans_sixth = provided_test6()
val ans_seventh = provided_test7()