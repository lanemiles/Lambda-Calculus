all: asgt6

Tokens.hs : Tokens.x
	alex Tokens.x

Grammar.hs : Grammar.y
	happy Grammar.y
    
asgt6 : Tokens.hs Grammar.hs main.hs
	ghc --make main
    
clean:
	rm -f main Grammar.hs Tokens.hs *.o *.hi
    