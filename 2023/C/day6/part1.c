#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LEN 256

int main(void){
	FILE* inp = fopen("inp.txt", "r");
	char times[MAX_LEN];
	fgets(times, MAX_LEN, inp);
	char distances[MAX_LEN];
	fgets(distances, MAX_LEN, inp);
	puts(times);
	puts(distances);
	fclose(inp);
	return 0;
}
