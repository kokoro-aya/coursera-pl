use "delayed_evaluation.sml";

val one_to_ten = take 10 (generate (fn n => if n > 10 then NONE else SOME (n, n + 1)) 1)
val one_to_nine_step_two = take 5 (generate_inf (fn n => (n, n + 2)) 1)
val nine_through_five = take 5 (generate_simple (fn x => x - 1) 9)

val seq_one_nine = safe_take 10 (seq 1 6)
val const_five = take 5 (const 5)
val same_list_from_list = safe_take 6 (fromList [5,8,2,1,9])
val replay_three_times = take 9 (replay (fromList [2,9,6]))
val cycle_list_example = take 13 (cycleList [2,5,6])
val doubled = take 9 (map (fn x => x * 2) (cycleList [2,3,5,4]))
val take_until_eq_25 = safe_take 6 (until (fn x => x >= 25) (map (fn x => x * 6) (seq 1 99)))
val filter_is_even = take 3 (filter (fn x => x mod 2 = 0) (seq 2 12))
val alt = take 3 (zip (filter (fn x => x mod 3 = 0) (seq 2 20)) (filter (fn x => x mod 3 = 1) (seq 2 20)))
val take_from_f = take 5 (from_f (fn x => SOME (x * 2 + 1)))
val iter = iterate (seq 1 6)
val iter_1st = iter ()
val iter_2nd = iter ()
val iter_3rd = iter ()