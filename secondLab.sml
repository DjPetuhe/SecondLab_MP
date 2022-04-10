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

(*Test for first task*)
fun provided_test1 () = 
    let val date1 = (21,6,2022)
        val date2 = (20,6,2022)
        val date3 = (20,6,2020)
        val date4 = (20,7,2020)
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
    let val dateList1 = [(3,4,2022),(12,2,2022),(1,2,2021)] : (int*int*int) list
        val dateList2 = [(1,2,2010),(5,2,2015),(10,2,2020),(3,2,2025)] : (int*int*int) list
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
    let val dateList1 = [(3,4,2022),(12,2,2022),(1,2,2021)] : (int*int*int) list
        val dateList2 = [(1,2,2010),(5,2,2015),(10,2,2020),(3,2,2025)] : (int*int*int) list
    in
    (dates_in_month(dateList1, 4),
    dates_in_month(dateList1, 2),
    dates_in_month(dateList2, 2),
    dates_in_month(dateList2, 10))
    end

val ans_first = provided_test1()
val ans_second = provided_test2()
val ans_third = provided_test3()
val ans_fourth = provided_test4()