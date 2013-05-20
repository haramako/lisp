import os
import glob

env = DefaultEnvironment(CC='cc', **os.environ)
env.AppendUnique(
    CCFLAGS=['-Wall', '-O2', '-std=c99', '-g'],
    LIBS=['m'] )

LISP_LIBS = ['lisp.c', 'gc.c', 'dict.c', 'cfunc.c']

env.Default('mlisp', 'test_mlisp')
env.Program('mlisp', ['main.c'] + LISP_LIBS)
env.Program('test_mlisp', ['test.c'] + LISP_LIBS)

def do_test(target, source, env):
    for file in glob.glob('test*.sch'):
        os.system( "./mlisp "+file )
    for file in glob.glob('*.ss'):
        os.system( "./mlisp "+file )

env.Command('test', ['test_mlisp','mlisp'], ['./test_mlisp', do_test])
