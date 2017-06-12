#!/usr/bin/env python
# encoding: utf-8

import unittest
import subprocess as sp
import os
import sys

qemuCmd = 'qemu-arm -L /usr/arm-linux-gnueabihf'.split()
amacc = './amacc'
gcc = 'arm-linux-gnueabihf-gcc'
amaccdir = 'amaccelf'
gccdir = 'gccelf'


class TestAmacc(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.maxDiff = None

        if not os.access(amaccdir, os.F_OK):
            os.mkdir(amaccdir)
        if not os.access(gccdir, os.F_OK):
            os.mkdir(gccdir)


class TestAmacc_UC(unittest.TestCase):
    """ Test cases without -fsigned-char (default) """
    pass


class TestAmacc_SC(unittest.TestCase):
    """ Test cases with -fsigned-char """
    pass


def _generate_test(test_name, test_file, extra_cflags):
    def test(self):
        args = ['2']

        test_file_name = os.path.splitext(os.path.basename(test_file))[0]

        # compile test program with gcc and run the output executable
        prog_exe = os.path.join(gccdir, test_file_name)
        gcc_params = [gcc, '-o', prog_exe, test_file] + extra_cflags
        sp.check_call(gcc_params)

        proc = sp.Popen(qemuCmd + [prog_exe] + args, stdout=sp.PIPE)
        gcc_out, gcc_err = proc.communicate()

        # run amacc in jit mode
        amacc_params = [amacc] + extra_cflags + [test_file] + args
        proc = sp.Popen(qemuCmd + amacc_params, stdout=sp.PIPE)
        amacc_out, amacc_err = proc.communicate()
        self.assertEqual(amacc_out.decode('utf-8'), gcc_out.decode('utf-8'))

        # run amacc in compiler mode
        prog_exe = os.path.join(amaccdir, test_file_name)
        amacc_params = [amacc] + extra_cflags + ['-o', prog_exe, test_file]
        sp.check_call(qemuCmd + amacc_params)

        proc = sp.Popen(qemuCmd + [prog_exe] + args, stdout=sp.PIPE)
        amacc_out, amacc_err = proc.communicate()
        self.assertEqual(amacc_out.decode('utf-8'), gcc_out.decode('utf-8'))

    return test


def _define_tests():
    for dirpath, _, filenames in os.walk('tests'):
        for f in filenames:
            test_file = os.path.abspath(os.path.join(dirpath, f))
            test_name = 'test_%s' % (os.path.splitext(f)[0])

            # test without -fsigned-char (default ABI)
            test_func = _generate_test(test_name, test_file, [])
            setattr(TestAmacc_UC, test_name, test_func)

            # test with -fsigned-char
            test_func = _generate_test(test_name, test_file, ['-fsigned-char'])
            setattr(TestAmacc_SC, test_name, test_func)

_define_tests()


if __name__ == '__main__':
    unittest.main()
