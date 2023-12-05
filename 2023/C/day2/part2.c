#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LEN 512

void get_rgb(int* arr, char* str);

int main(void){
	FILE* inp = fopen("inp.txt", "r");
	char str[MAX_LEN];
	char* ptr = NULL, *entry = NULL;
	int sum = 0, power = 0;

	while (fgets(str, MAX_LEN, inp)) {
		ptr = strchr(str, ':') + 1;
		entry = strtok(ptr, ";");
		int max_rgb[3] = {0}, arr[3] = {0};
		while (entry){
			entry++;
			get_rgb(arr, entry);
			for (int i = 0; i < 3; i++) {
				max_rgb[i] = (max_rgb[i] > arr[i])? max_rgb[i]: arr[i];
			}
			entry = strtok(NULL, ";");
		}
		power = max_rgb[0] * max_rgb[1] * max_rgb[2];
		sum += power;
	}

	printf("Sum: %d\n", sum);
	fclose(inp);
	return 0;
}

void get_rgb(int* arr, char* str){
	// str = "x blue, y red, z green"
	char* colour = &str[0];

	for (;;) {
		int cnt = strtol(str, &colour, 10);
		colour++; // as colour points at space b/w num and colour
		if (*colour == 'b'){
			arr[2] = cnt;
		} else if (*colour == 'g'){
			arr[1] = cnt;
		} else if (*colour == 'r'){
			arr[0] = cnt;
		}
		str = strchr(str, ',');
		if(str == NULL){
			break;
		}
		str++; // Point after the ,
	}
}
