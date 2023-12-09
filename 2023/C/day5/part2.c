#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <limits.h>

#define NO_OF_SEEDS 9

typedef struct {
	u_long base;
	u_long len;
} seed_t;

int main(void){
	// Read all from file
	FILE* inp = fopen("inp.txt", "r");
	fseek(inp, 0, SEEK_END);
	int size = ftell(inp);
	char* str = malloc(sizeof(char) * size);
	char* ptr = str;
	rewind(inp);
	fread(str, sizeof(char), size, inp);
	fclose(inp);

	// Get the seeds
	seed_t seeds[NO_OF_SEEDS] = {0};
	char* seeds_inp = NULL;
	seeds_inp = strchr(str, ':') + 1;
	errno = 0;
	int i = 0;
	while (1) {
		char* end;
		seeds[i].base = strtoul(seeds_inp, &end, 10);
		if (errno == ERANGE){
			errno = 0;
			printf("Apparently u_long is short");
		}
		if (seeds_inp == end || *end == '\n')
			break;
		seeds_inp = end;
		seeds[i].len = strtoul(seeds_inp, &end, 10);
		seeds_inp = end;
		i++;
	}

	// Make a src no. which will continue mapping
	unsigned long long src, min_loc = ULLONG_MAX;
	for (int i = 0; i < NO_OF_SEEDS; i++) {
		printf("Seed range %d started\n", i);
		for (unsigned long long j = seeds[i].base; j < (seeds[i].len + seeds[i].base); j++){
			src = j;
			int found_src = 0;
			ptr = str;
			ptr = strchr(ptr, '\n') + 1;
			while ((ptr = strchr(ptr, '\n')) != NULL) {
				ptr++;
				if(*ptr == '\n'){
					found_src = 0;
				}
				if(!isdigit(*ptr) || found_src)
					continue;
				u_long from, to, len;
				char* end_of_num;
				to = strtoul(ptr, &end_of_num, 10);
				ptr = end_of_num;
				from = strtoul(ptr, &end_of_num, 10);
				ptr = end_of_num;
				len = strtoul(ptr, NULL, 10);
				if (src >= from && src <= (from + len - 1)) {
					found_src = 1;
					src = to + (src - from);
				}
			}
			if (src < min_loc){
				min_loc = src;
			}
		}
	}
	printf("min: %llu\n", min_loc);

	free(str);
	return 0;
}
