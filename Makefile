interp : interpreter parser ast
	ghc --make main -o interp

ast : ast.hs

interpreter: interp.hs

parser: parse.hs Tokens.hs Grammar.hs

Tokens.hs : Tokens.x
	alex Tokens.x

Grammar.hs : Grammar.y
	happy Grammar.y

clean:
	rm -f interp Grammar.hs Tokens.hs *.o *.hi tests
    
