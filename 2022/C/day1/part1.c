// Calorie counter
// Can use builtin qsort
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char const *argv[]){
	int size;
	FILE* inp = fopen("inp.txt", "r");
	fseek(inp, 0, SEEK_END);
	size = ftell(inp);
	char* str = malloc(size * sizeof(char));
	rewind(inp);
	fread(str, sizeof(char), size, inp);
	fclose(inp);

	char* str_start = &str[0];
	char* end;
	int i = 0, sum_of_cals = 0, max = 0;
	char* end_of_str = &str[size];
	int j = (int)(end_of_str - str);
	const char marker = '\n';

	while(1){
		i = 0;
		end = strchr(str_start, marker);
		i = strtol(str_start, &end, 10);
		if(*str_start == marker){
			max = (max > sum_of_cals)? max: sum_of_cals;
			sum_of_cals = 0;
		}
		sum_of_cals += i;
		if(j <= 0){
			break;
		}
		str_start = end + 1;
		j = (int)(end_of_str - str_start);
	}

	max = (max > sum_of_cals)? max: sum_of_cals;
	printf("%d\n", max);
	free(str);
	return 0;
}
