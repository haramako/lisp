cmake_minimum_required(VERSION 3.15)

project(mlisp C)

add_executable(mlisp main.c lisp.c cfunc.c dict.c eval.c gc.c)
