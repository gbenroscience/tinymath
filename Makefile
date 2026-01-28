all:
	gcc -O2 -std=c99 -Wall -lm -o tinymath.exe tinymath.c && \
	gcc -O2 -std=c99 -Wall -o test_runner.exe test_runner.c && \
	test_runner.exe tinymath.exe