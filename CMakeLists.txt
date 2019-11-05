cmake_minimum_required(VERSION 3.15)

project(mlisp C)

SET(CMAKE_C_FLAGS "-Wall -std=c11 -rdynamic")

add_executable(mlisp main.c lisp.c cfunc.c dict.c eval.c gc.c parse.c v2s.c str.c stream.c init.c)
