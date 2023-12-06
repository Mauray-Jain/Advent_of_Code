#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <tgmath.h>

#define MAX_LEN 256

int make_arr(int* arr, char* str);
int comp(const void* a, const void* b);
int find_duplicates(int* arr1, int len_arr1, int* arr2, int len_arr2);

int main(void){
	FILE* inp = fopen("inp.txt", "r");
	char str[MAX_LEN];
	int win[30] = {0}, us[30] = {0};
	int sum = 0;
	while (fgets(str, MAX_LEN, inp)) {
		char* us_str = strchr(str, '|') + 1;
		char* win_str = strchr(str, ':') + 1;
		win_str = strtok(win_str, "|");
		int len_us = make_arr(us, us_str);
		qsort(us, len_us, sizeof(int), comp);
		int len_win = make_arr(win, win_str);
		qsort(win, len_win, sizeof(int), comp);
		int dup = find_duplicates(us, len_us, win, len_win);
		sum += exp2(dup - 1);
	}
	printf("%d\n", sum);
	fclose(inp);
	return 0;
}

int comp(const void* a, const void* b){
	int arg1 = *(const int*) a;
	int arg2 = *(const int*) b;
	return (arg1 > arg2) - (arg1 < arg2);
}

int make_arr(int* arr, char* str){
	char* num = strtok(str, " ");
	int i = 0;
	while(num){
		*(arr + i) = strtol(num, NULL, 10);
		num = strtok(NULL, " ");
		i++;
	}
	return i;
}

int find_duplicates(int* arr1, int len_arr1, int* arr2, int len_arr2){
	int count = 0;
	for (int i = 0; i < len_arr1; i++) {
		int elem = arr1[i];
		for (int j = 0; j < len_arr2; j++) {
			if(elem == arr2[j]) count++;
			if(arr2[j] > elem) break;
		}
	}
	return count;
}
