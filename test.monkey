let max = 10;
let func = fn(iter) { if (iter < max) { func(iter + 1) } else { yield(); }; };

func(1) { || stats(); };

print("last stats");

stats();
