all:
	erlc -o ebin src/*.erl

clean:
	rm ebin/*.erl
