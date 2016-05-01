#!/usr/bin/env python
# encoding: utf-8

import unittest
import subprocess as sp
import os
import sys

qemuCmd = "qemu-arm -L /usr/arm-linux-gnueabihf".split()
amacc = "./amacc"
gcc = "arm-linux-gnueabihf-gcc"
amaccdir = "amaccelf"
gccdir = "gccelf"


class TestAmacc(unittest.TestCase):
    pass


def testGenerator(f):
    def test(self):
        print("verify file: %s" % (f))

        # amaccexe = os.path.join(amaccdir, os.path.splitext(os.path.basename(f))[0])
        gccexe = os.path.join(gccdir, os.path.splitext(os.path.basename(f))[0])
        gccparams = [gcc, "-o", gccexe, f]
        sp.check_call(gccparams)

        amaccout = sp.Popen(qemuCmd + [amacc, f, "2"], stdout=sp.PIPE).communicate()[0]
        gccout = sp.Popen(qemuCmd + [gccexe, "2"], stdout=sp.PIPE).communicate()[0]

        self.maxDiff = None
        self.assertEqual(amaccout.decode("utf-8"), gccout.decode("utf-8"))

    return test

if __name__ == '__main__':
    if not os.access(amaccdir, os.F_OK):
        os.mkdir(amaccdir)
    if not os.access(gccdir, os.F_OK):
        os.mkdir(gccdir)

    namePattern = ""
    if len(sys.argv) > 1:
        namePattern = sys.argv[1]

    for dirpath, _, filenames in os.walk("tests"):
        for f in filter(lambda name: namePattern in name, filenames):
            testfile = os.path.abspath(os.path.join(dirpath, f))
            test_func = testGenerator(testfile)
            setattr(TestAmacc, 'test_%s' % (os.path.splitext(f)[0]), test_func)
    unittest.main(argv=[sys.argv[0]])
