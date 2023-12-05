#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LEN 256

typedef struct {
	int x, y;
} coord;

typedef struct {
	int x, y, len, num;
	coord sym; // Relate symbol coordinates to number
	int done; // Is it already added in sum
} coord_data;

int is_sym(char ch);
void find_sym_adjacent(coord_data* number, coord* sym_table, int sym_len, int line_len);

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
				number.sym.x = number.sym.y = -1;
				number.done = 0;
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
		find_sym_adjacent(&num_table[i], sym_table, len_sym_table, len_line);
	}

	// Find coord_data having same sym coord
	for (int i = 0; i < len_num_table; i++){
		number = num_table[i];
		for (int j = 0; j < len_num_table; j++) {
			if (number.sym.y == -1 || number.sym.x == -1){
				continue;
			}
			if (number.sym.x == num_table[j].sym.x && number.sym.y == num_table[j].sym.y){
				if(number.done == 0 && num_table[j].done == 0 && number.num != num_table[j].num){
					sum += number.num * num_table[j].num;
					number.done = 1;
					num_table[j].done = 1;
				}
			}
		}
	}

	printf("Sum %d\n", sum);

	free(sym_table);
	free(num_table);
	return 0;
}

int is_sym(char ch){
	if(ch == '*')
		return 1;
	return 0;
}

void find_sym_adjacent(coord_data* number, coord* sym_table, int sym_len, int line_len){
	int x_from = (number->x == 0)? number->x: number->x - 1;
	int x_to = ((number->x + number->len) == (line_len - 1))? (number->x + number->len): (number->x + number->len + 1);

	// check top and bottom
	int y = number->y - 1, y2 = number->y + 1, x = number->x - 1, x2 = number->x + number->len;
	for (int i = x_from; i < x_to; i++) {
		for (int j = 0; j < sym_len; j++) {
			if ((sym_table[j].x == i) && ((sym_table[j].y == y) || (sym_table[j].y == y2))){
				number->sym.x = sym_table[j].x;
				number->sym.y = sym_table[j].y;
				return;
			}
		}
	}

	// check left and right
	for (int j = 0; j < sym_len; j++) {
		if ((sym_table[j].y == number->y) && ((sym_table[j].x == x) || (sym_table[j].x == x2))){
			number->sym.x = sym_table[j].x;
			number->sym.y = sym_table[j].y;
			return;
		}
	}
}
