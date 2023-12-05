#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LEN 512
#define MAX_RED 12
#define MAX_GREEN 13
#define MAX_BLUE 14

int is_entry_valid(char* str);
int get_id(char* str);

int main(void){
	FILE* inp = fopen("inp.txt", "r");
	char str[MAX_LEN];
	char* ptr = NULL, *entry = NULL;
	int sum = 0, valid = 1;

	while (fgets(str, MAX_LEN, inp)) {
		ptr = strchr(str, ':') + 1;
		entry = strtok(ptr, ";");
		valid = 1;
		while (entry){
			entry++;
			if (!is_entry_valid(entry)) {
				valid = 0;
				break;
			}
			entry = strtok(NULL, ";");
		}
		sum += ((valid)? get_id(str): 0);
	}

	printf("Sum: %d\n", sum);
	fclose(inp);
	return 0;
}

int is_entry_valid(char* str){
	// str = "x blue, y red, z green"
	char* colour = &str[0];

	for (;;) {
		int x = strtol(str, &colour, 10);
		colour++; // as colour points at space b/w num and colour
		if (*colour == 'b'){
			if (x > MAX_BLUE) {return 0;}
		} else if (*colour == 'g'){
			if (x > MAX_GREEN) {return 0;}
		} else if (*colour == 'r'){
			if (x > MAX_RED) {return 0;}
		}
		str = strchr(str, ',');
		if(str == NULL){
			break;
		}
		str++; // Point after the ,
	}
	return 1;
}

int get_id(char* str){
	char* start = strchr(str, ' '); // As first space is after Game id
	return strtol(start, NULL, 10);
}
