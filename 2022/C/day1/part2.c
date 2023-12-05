// Calorie counter
// Can use builtin qsort
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void shift(int*, int, int);

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
	int i = 0, sum_of_cals = 0, max[] = {0, 0, 0};
	char* end_of_str = &str[size];
	int j = (int)(end_of_str - str);
	const char marker = '\n';

	while(1){
		i = 0;
		end = strchr(str_start, marker);
		i = strtol(str_start, &end, 10);
		if(*str_start == marker){
			if(sum_of_cals > max[2]){
				shift(max, 2, sum_of_cals);
			} else if(sum_of_cals > max[1]){
				shift(max, 1, sum_of_cals);
			} else if(sum_of_cals > max[0]){
				shift(max, 0, sum_of_cals);
			}
			sum_of_cals = 0;
		}
		sum_of_cals += i;
		if(j <= 0){
			break;
		}
		str_start = end + 1;
		j = (int)(end_of_str - str_start);
	}

	if(sum_of_cals > max[2]){
		shift(max, 2, sum_of_cals);
	} else if(sum_of_cals > max[1]){
		shift(max, 1, sum_of_cals);
	} else if(sum_of_cals > max[0]){
		shift(max, 0, sum_of_cals);
	}
	printf("%d\n", max[0] + max[1] + max[2]);
	free(str);
	return 0;
}

void shift(int* arr, int index, int val){
	int temp;
	while(index >= 0){
		temp = arr[index];
		arr[index] = val;
		val = temp;
		index--;
	}
}
