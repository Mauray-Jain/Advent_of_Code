// Rock paper scissors
#include <stdio.h>
#include <stdlib.h>

enum Rps {ROCK = 1, PAPER, SCISSORS};
enum Outcome {WIN = 6, DRAW = 3, LOSE = 0};
enum Rps map_rps(char);
enum Outcome determine_win(enum Rps, enum Rps);

int main(int argc, char const *argv[]){
	FILE* inp = fopen("inp.txt", "r");
	char opp_inp, me_inp;
	char line[5];
	int opp, me, score = 0;
	while(fgets(line, 5, inp)){
		if(*line == '\n')
			continue;
		sscanf(line, "%c %c", &opp_inp, &me_inp);
		opp = map_rps(opp_inp);
		me = map_rps(me_inp);
		score += determine_win(opp, me) + me;
	}
	// fgets(line, 4, inp);
	// puts(line);
	// sscanf(line, "%c %c", &opp_inp, &me_inp);
	fclose(inp);
	printf("%d\n", score);
	return 0;
}

enum Rps map_rps(char inp){
	switch(inp){
		case 'A': case 'X': return ROCK;
		case 'B': case 'Y': return PAPER;
		case 'C': case 'Z': return SCISSORS;
		default:
			perror("Error: Invalid");
			exit(EXIT_FAILURE);
	}
}

enum Outcome determine_win(enum Rps opp, enum Rps me){
	if(opp == me)
		return DRAW;
	else if(opp == ROCK && me == SCISSORS)
		return LOSE;
	else if(me == ROCK && opp == SCISSORS)
		return WIN;
	else if(me < opp)
		return LOSE;
	else if(opp < me)
		return WIN;
	else {
		perror("Some exception ocurred!");
		exit(EXIT_FAILURE);
	}
}
