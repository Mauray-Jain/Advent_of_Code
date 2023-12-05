#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define MAX_LENGTH 255

int main(void){
	FILE* inp;
	inp = fopen("inp.txt", "r");
	char str[MAX_LENGTH];
	int sum = 0;
	while (fgets(str, MAX_LENGTH, inp)) {
		char* ptr = &str[0];
		while (isdigit(*ptr) == 0) ptr++;
		int first = *ptr - '0';
		char* end = strchr(str, '\n');
		while (isdigit(*end) == 0) end--;
		int last = *end - '0';
		sum += first*10 + last;
	}
	printf("%d\n", sum);
	fclose(inp);
	return 0;
}
