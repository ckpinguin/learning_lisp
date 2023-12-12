(loop for i in (loop repeat 100 collect (random 10000))
	 counting (evenp i) into evens
	 counting (oddp i) into odds
	 summing i into total
	 maximizing i into max
	 minimizing i into min
	 finally (return (list min max total evens odds)))