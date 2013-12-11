all:
	gcc -o shared/erl_pid.so -fpic -shared c_src/pid.c c_src/erl_pid.c
	gcc -o shared/erl_kalman.so -fpic -shared c_src/kalman.c c_src/erl_kalman.c
	erlc src/* -o ebin/
