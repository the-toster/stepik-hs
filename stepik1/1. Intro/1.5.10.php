<?php
declare(strict_types=1);

// в ночи не мог решить 1.5.10, написал это, потом уже .hs
function fib(int $n): int {

    $preLast = 0;
    $last = 1;
    $current = 0;
    for ($i = 0; $i < $n; $i++) {
        $preLast = $last;
        $last = $current;
        $current = $preLast + $last;
    }
    return $current;
}

for ($i = 0; $i < 11; $i++) {
    echo $i.": ".fib($i)."\n";
}
