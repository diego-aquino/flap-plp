:- module(localStorage,[readHighestScore/1, saveHighestScore/1]).

saveHighestScore(Score):- open("score.txt",write,Stream),
         				  write(Stream,Score),
         				  write(Stream,"."),
         				  nl(Stream),
         				  close(Stream).

readHighestScore(Score):- (exists_file("score.txt") ->  open("score.txt",read,HighestScore),
			          		        read(HighestScore,Score),
        			  	   	        close(HighestScore);
        		   Score = 0).
