#include <stdio.h>
#include <stdlib.h>

void shift(int* arr, int index, int val){
	int temp;
	while(index >= 0){
		temp = arr[index];
		arr[index] = val;
		val = temp;
		index--;
	}
}

int main(int argc, char const *argv[]){
	// int max[] = {1, 2, 3};
	// shift(max, 0, 4);
	// for(int i = 0; i < 3; i++){
	// 	printf("max[%d]:%d\n", i, max[i]);
	// }
	int i;
	while(scanf("%d", &i)){
		printf("%d", i);
	}
	return 0;
}
