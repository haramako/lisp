import os
import glob
import commands

env = DefaultEnvironment(CC='cc', **os.environ)
env.AppendUnique(
    CCFLAGS=['-Wall', '-O2', '-std=c99', '-g'],
    LIBS=['m'] )

LISP_LIBS = ['lisp.c', 'gc.c', 'dict.c', 'cfunc.c']

env.Default('mlisp', 'test_mlisp')
env.Program('mlisp', ['main.c'] + LISP_LIBS)
env.Program('test_mlisp', ['test.c'] + LISP_LIBS)

def do_test(target, source, env):
    for file in glob.glob('test*.sch') + glob.glob('*.ss'):
        print "testing", file, "..."
        result, out = commands.getstatusoutput( "./mlisp "+file )
        if result != 0:
            print "="*80
            print "ERROR: in", file
            print "-"*80
            print out
            print "="*80

env.Command('test', ['test_mlisp','mlisp'], ['./test_mlisp', do_test])
