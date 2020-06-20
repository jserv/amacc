#!/usr/bin/env python
# encoding: utf-8

import unittest
import subprocess as sp
import os
import sys

amacc = './amacc'
gcc = 'arm-linux-gnueabihf-gcc'
amaccdir = 'elf'
gccdir = 'out-gcc'


def mkdir_p(path):
    try:
        os.makedirs(path)
    except OSError as exc:  # Python >2.5
        if exc.errno == errno.EEXIST and os.path.isdir(path):
            pass
        else:
            raise


class TestCC_UC(unittest.TestCase):
    """ Test cases without -fsigned-char (default) """
    pass


class TestCC_SC(unittest.TestCase):
    """ Test cases with -fsigned-char """
    pass


def _generate_test(test_name, test_file, extra_cflags):
    def test(self):
        args = ['3']

        test_file_name = os.path.splitext(os.path.basename(test_file))[0]

        # compile test program with gcc and run the output executable
        prog_exe = os.path.join(gccdir, test_file_name)
        # parameter '-w' inhibits all warning messages of gcc
        gcc_params = [gcc, '-w', '-o', prog_exe, test_file] + extra_cflags
        sp.run(gcc_params)

        proc = sp.run(qemuCmd + [prog_exe] + args, timeout=10, stdout=sp.PIPE)
        gcc_out, gcc_err, gcc_retcode = proc.stdout, proc.stderr, proc.returncode

        # run amacc in jit mode
        amacc_params = [amacc] + extra_cflags + [test_file] + args
        proc = sp.run(qemuCmd + amacc_params, timeout=10, stdout=sp.PIPE)
        amacc_out, amacc_err, amacc_retcode = proc.stdout, proc.stderr, proc.returncode
        self.assertEqual(amacc_out.decode('utf-8'), gcc_out.decode('utf-8'))
        self.assertEqual(amacc_retcode, gcc_retcode)

        # run amacc in compiler mode
        prog_exe = os.path.join(amaccdir, test_file_name)
        amacc_params = [amacc] + extra_cflags + ['-o', prog_exe, test_file]
        sp.run(qemuCmd + amacc_params)

        proc = sp.run(qemuCmd + [prog_exe] + args, timeout=10, stdout=sp.PIPE)
        amacc_out, amacc_err, amacc_retcode = proc.stdout, proc.stderr, proc.returncode
        self.assertEqual(amacc_out.decode('utf-8'), gcc_out.decode('utf-8'))
        self.assertEqual(amacc_retcode, gcc_retcode)

    return test


def _define_tests():
    if not os.access(amaccdir, os.F_OK):
        mkdir_p(amaccdir)
    if not os.access(gccdir, os.F_OK):
        mkdir_p(gccdir)
    for dirpath, _, filenames in os.walk('tests'):
        for f in filenames:
            if f.endswith('.c'):
                test_file = os.path.abspath(os.path.join(dirpath, f))
                test_name = 'test_%s' % (os.path.splitext(f)[0])

                # test without -fsigned-char (default ABI)
                test_func = _generate_test(test_name, test_file, [])
                setattr(TestCC_UC, test_name, test_func)

                # test with -fsigned-char
                test_func = _generate_test(test_name, test_file, ['-fsigned-char'])
                setattr(TestCC_SC, test_name, test_func)

_define_tests()


if __name__ == '__main__':
    try:
        qemuCmd = os.getenv('ARM_EXEC').split()
    except AttributeError:
        qemuCmd = 'qemu-arm -L /usr/arm-linux-gnueabihf'.split()
    unittest.main()
