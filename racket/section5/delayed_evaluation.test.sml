use "delayed_evaluation.sml";

val x = take 10 (generate_simple (fn n => n + 1) 1)