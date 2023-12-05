// Rock paper scissors
#include <stdio.h>
#include <stdlib.h>

enum Rps {ROCK = 1, PAPER, SCISSORS};
enum Outcome {WIN = 6, DRAW = 3, LOSE = 0};
enum Rps map_rps(char);
enum Outcome map_outcome(char);
enum Rps determine_me(enum Rps, enum Outcome);

int main(int argc, char const *argv[]){
	FILE* inp = fopen("inp.txt", "r");
	char opp_inp, outcome;
	char line[5];
	int opp, out, score = 0;
	while(fgets(line, 5, inp)){
		if(*line == '\n')
			continue;
		sscanf(line, "%c %c", &opp_inp, &outcome);
		opp = map_rps(opp_inp);
		out = map_outcome(outcome);
		score += determine_me(opp, out) + out;
	}
	fclose(inp);
	printf("%d\n", score);
	return 0;
}

enum Rps map_rps(char inp){
	switch(inp){
		case 'A': return ROCK;
		case 'B': return PAPER;
		case 'C': return SCISSORS;
		default:
			perror("Error: Invalid");
			exit(EXIT_FAILURE);
	}
}

enum Outcome map_outcome(char inp){
	switch(inp){
		case 'X': return LOSE;
		case 'Y': return DRAW;
		case 'Z': return WIN;
		default:
			perror("Error: Invalid");
			exit(EXIT_FAILURE);
	}
}

enum Rps determine_me(enum Rps opp, enum Outcome out){
	if(out == LOSE){
		if(opp == ROCK) return SCISSORS;
		else return opp - 1;
	} else if(out == DRAW){
		return opp;
	} else if(out == WIN){
		if(opp == SCISSORS) return ROCK;
		else return opp + 1;
	} else {
		perror("Some exception");
		exit(EXIT_FAILURE);
	}
}
