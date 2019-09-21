import os
import glob
#import commands

def do_test(target, source, env):
    for file in glob.glob('tests/test*.scm') + glob.glob('tests/*.ss'):
        print ("testing", file, "...")
        result, out = commands.getstatusoutput( "./mlisp "+file )
        if result != 0:
            print( "="*80)
            print( "ERROR: in", file)
            print( "-"*80)
            print( out)
            print( "="*80)


env = DefaultEnvironment(CC='cc', **os.environ)
env.AppendUnique(
    CCFLAGS=['-Wall', '-O2', '-std=c99', '-g'],
    LIBS=['m'] )

LISP_LIBS = ['lisp.c', 'eval.c', 'gc.c', 'dict.c', 'cfunc.c']

env.Program('mlisp', ['main.c'] + LISP_LIBS)
env.Program('test_mlisp', ['test.c'] + LISP_LIBS)

env.Command('do_src_filter', [], './src-filter lisp.h lisp.c cfunc.c')
env.Command('test', ['test_mlisp','mlisp'], ['./test_mlisp', do_test])
env.Command('tags', [], ['gtags'])
env.Clean('tags',['GPATH','GTAGS','GRTAGS'])

env.Default('do_src_filter', 'mlisp', 'test_mlisp')
