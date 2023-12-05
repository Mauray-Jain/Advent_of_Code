#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LEN 256

typedef struct {
	int x, y, len, num;
} coord_data;

typedef struct {
	int x, y;
} coord;

int is_sym(char ch);
int is_sym_adjacent(coord_data number, coord* sym_table, int sym_len, int line_len);

int main(void){
	FILE* inp = fopen("inp.txt", "r");
	char str[MAX_LEN];
	coord_data number = {0};
	coord* sym_table = malloc(sizeof(coord) * 2000);
	coord_data* num_table = malloc(sizeof(coord_data) * 2000);
	if(sym_table == NULL || num_table == NULL){
		perror("Heap me\n");
		exit(EXIT_FAILURE);
	}
	int line_num = 0, sum = 0, len_sym_table = 0, len_num_table = 0, len_line = 0;

	while (fgets(str, MAX_LEN, inp)) {
		char* ptr = &str[0];
		char* end = ptr;
		len_line = strlen(str);

		while (*ptr != '\n') {
			if (isdigit(*ptr) != 0){
				number.num = strtol(ptr, &end, 10);
				number.x = ptr - str;
				number.y = line_num;
				number.len = end - ptr;
				num_table[len_num_table] = number;
				len_num_table++;
				ptr = end;
			} else {
				if (is_sym(*ptr) != 0){
					sym_table[len_sym_table].x = ptr - str;
					sym_table[len_sym_table].y = line_num;
					len_sym_table++;
				}
				ptr++;
			}
		}
		line_num++;
	}
	fclose(inp);

	for (int i = 0; i < len_num_table; i++) {
		if (is_sym_adjacent(num_table[i], sym_table, len_sym_table, len_line) == 1){
			sum += num_table[i].num;
		}
	}

	printf("Sum %d\n", sum);

	free(sym_table);
	free(num_table);
	return 0;
}

int is_sym(char ch){
	if(ch == '.')
		return 0;
	return ispunct(ch);
}

int is_sym_adjacent(coord_data number, coord* sym_table, int sym_len, int line_len){
	int x_from = (number.x == 0)? number.x: number.x - 1;
	int x_to = ((number.x + number.len) == (line_len - 1))? (number.x + number.len): (number.x + number.len + 1);

	// check top and bottom
	int y = number.y - 1, y2 = number.y + 1, x = number.x - 1, x2 = number.x + number.len;
	for (int i = x_from; i < x_to; i++) {
		for (int j = 0; j < sym_len; j++) {
			if ((sym_table[j].x == i) && ((sym_table[j].y == y) || (sym_table[j].y == y2))){
				return 1;
			}
		}
	}

	// check left and right
	for (int j = 0; j < sym_len; j++) {
		if ((sym_table[j].y == number.y) && ((sym_table[j].x == x) || (sym_table[j].x == x2))){
			return 1;
		}
	}

	return 0;
}
