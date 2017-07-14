let max = 100;
let func = fn(iter) { if (iter < max) { func(iter + 1) } else { stats() } };

func(1)

func(1)