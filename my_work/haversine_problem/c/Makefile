all: generate_random_points parse_pair_points test

generate_random_points: generate_random_points.c reference_haversine.c
	gcc -O2 -Wall -Wextra -Wno-strict-aliasing -ggdb -o generate_random_points generate_random_points.c -lm

parse_pair_points: parse_pair_points.c reference_haversine.c
	gcc -O2 -Wall -Wextra -Wno-strict-aliasing -ggdb -o parse_pair_points parse_pair_points.c -lm

test: test.c
	gcc -Wall -Wextra -ggdb -o test test.c
