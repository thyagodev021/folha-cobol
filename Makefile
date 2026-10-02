all:
	cobc -x -free -o folha FOLHA.cob
run: all
	./folha && cat RELATORIO.TXT
