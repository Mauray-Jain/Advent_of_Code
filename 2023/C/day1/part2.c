#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define MAX_LENGTH 255

char* table[9] = {
	"one",
	"two",
	"three",
	"four",
	"five",
	"six",
	"seven",
	"eight",
	"nine"
};

int num_name(char* str);

int main(void){
	FILE* inp;
	inp = fopen("inp.txt", "r");
	char str[MAX_LENGTH];
	int sum = 0, first = 0, last = 0;

	while (fgets(str, MAX_LENGTH, inp)) {
		char* ptr = &str[0];
		char* end = strchr(str, '\n');
		first = 0;
		last = 0;
		while (1){
			int match = num_name(ptr);
			first = first == 0? match: first;
			last = match == 0? last: match;
			if (isdigit(*ptr) != 0){
				first = first == 0? *ptr - '0': first;
				last = *ptr - '0';
			}
			if(ptr == end) break;
			ptr++;
		}
		sum += first*10 + last;
	}
	printf("sum: %d\n", sum);
	fclose(inp);
	return 0;
}

int num_name(char* str){
	int len = 0;
	for (int i = 1; i <= 9; i++){
		len = strlen(table[i-1]);
		if (strncmp(str, table[i-1], len) == 0){
			return i;
		}
	}
	return 0;
}
