#!/usr/bin/python
import os, subprocess, sys
import os.path as path

# Assumes that we're located at the toplevel hasquito
# directory
COMPILER = './dist/build/hasquitoc/hasquitoc'
JS_RTS = 'nodejs'
TEST_PROGRAM_DIR = "test/programs/"

# Evil, but I'm lazy
while path.split(os.getcwd())[1] != "hasquito":
    os.chdir('..')

# Ensure that the compiler is built
subprocess.call(["cabal", "build", "hasquitoc"])

def gather_names():
    ''' Gather the names of all test programs '''
    program_names = []
    for f in os.listdir(TEST_PROGRAM_DIR):
        (name, ext) = path.splitext(f)
        if ext == ".hq":
            program_names.append(name)
    return program_names

def compile(prog_name):
    subprocess.call([COMPILER, prog_name + ".hq"])

def run():
    return subprocess.check_output([JS_RTS, "out.js"])[0:-1] # Ignore the newline

def test(prog_name):
    print "Compiling %s ..." % (prog_name)
    compile(prog_name)
    print "Running %s ..." % (prog_name)
    output = run()

    expected = open(path.join(TEST_PROGRAM_DIR, prog_name + ".result")).read()
    if output == expected:
        print "%s succeeded" % (prog_name)
    else:
        print "FAILURE: %s, got %s not %s" % (prog_name, output, expected)

for name in gather_names():
    test(name)

print "Finished"
