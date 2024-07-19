#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# This file is part of the card10 project card10.badge.events.ccc.de
#
# The MIT License (MIT)
#
# Copyright (c) 2019 Alexander Böhm
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

"""
card10 interface

This module provides is an extenstion of the Pyboard class, used to
communicate with the card10 badge over the serial connection via USB.

For description of the card10 board look at the project website <https://card10.badge.events.ccc.de>.

Python2 isn't supported.

Example usage:

    import pycard10
    card10 = pycard10.PyCard10('/dev/ttyACM0')

Then:

    card10.enter_raw_repl()
    card10.exec('import leds')
    card10.exec('leds.set_rocket(0, 31)')
    card10.exit_raw_repl()

    import pycard10
    pycard10.execfile('test.py', device='/dev/ttyACM0')

This script can also be run directly.  To execute a local script, use:

    tools/pycard10.py test.py

Or:
    python3 tools/pycard10.py test.py

"""

__author__ = "Alexander Böhm"
__copyright__ = "Copyright 2019, Alexander Böhm"
__license__ = "MIT"
__email__ = "alexander.boehm@malbolge.net"


import sys
import os
import time

try:
    stdout = sys.stdout.buffer
except AttributeError:
    # Python2 doesn't have buffer attr
    stdout = sys.stdout


def stdout_write_bytes(b):
    b = b.replace(b"\x04", b"")
    stdout.write(b)
    stdout.flush()

class ProcessToSerial:
    "Execute a process and emulate serial connection using its stdin/stdout."

    def __init__(self, cmd):
        import subprocess

        self.subp = subprocess.Popen(
            cmd,
            bufsize=0,
            shell=True,
            preexec_fn=os.setsid,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
        )

        # Initially was implemented with selectors, but that adds Python3
        # dependency. However, there can be race conditions communicating
        # with a particular child process (like QEMU), and selectors may
        # still work better in that case, so left inplace for now.
        #
        # import selectors
        # self.sel = selectors.DefaultSelector()
        # self.sel.register(self.subp.stdout, selectors.EVENT_READ)

        import select

        self.poll = select.poll()
        self.poll.register(self.subp.stdout.fileno())

    def close(self):
        import signal

        os.killpg(os.getpgid(self.subp.pid), signal.SIGTERM)

    def read(self, size=1):
        data = b""
        while len(data) < size:
            data += self.subp.stdout.read(size - len(data))
        return data

    def write(self, data):
        self.subp.stdin.write(data)
        return len(data)

    def inWaiting(self):
        # res = self.sel.select(0)
        res = self.poll.poll(0)
        if res:
            return 1
        return 0


class ProcessPtyToTerminal:
    """Execute a process which creates a PTY and prints slave PTY as
    first line of its output, and emulate serial connection using
    this PTY."""

    def __init__(self, cmd):
        import subprocess
        import re
        import serial

        self.subp = subprocess.Popen(
            cmd.split(),
            bufsize=0,
            shell=False,
            preexec_fn=os.setsid,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
        pty_line = self.subp.stderr.readline().decode("utf-8")
        m = re.search(r"/dev/pts/[0-9]+", pty_line)
        if not m:
            print("Error: unable to find PTY device in startup line:", pty_line)
            self.close()
            sys.exit(1)
        pty = m.group()
        # rtscts, dsrdtr params are to workaround pyserial bug:
        # http://stackoverflow.com/questions/34831131/pyserial-does-not-play-well-with-virtual-port
        self.ser = serial.Serial(pty, interCharTimeout=1, rtscts=True, dsrdtr=True)

    def close(self):
        import signal

        os.killpg(os.getpgid(self.subp.pid), signal.SIGTERM)

    def read(self, size=1):
        return self.ser.read(size)

    def write(self, data):
        return self.ser.write(data)

    def inWaiting(self):
        return self.ser.inWaiting()



class PyboardError(Exception):
    pass

class Pyboard:
    def __init__(
        self, device, baudrate=115200, user="micro", password="python", wait=0
    ):
        if device.startswith("exec:"):
            self.serial = ProcessToSerial(device[len("exec:") :])
        elif device.startswith("execpty:"):
            self.serial = ProcessPtyToTerminal(device[len("qemupty:") :])
        elif (
            device
            and device[0].isdigit()
            and device[-1].isdigit()
            and device.count(".") == 3
        ):
            # device looks like an IP address
            self.serial = TelnetToSerial(device, user, password, read_timeout=10)
        else:
            import serial

            delayed = False
            for attempt in range(wait + 1):
                try:
                    self.serial = serial.Serial(
                        device, baudrate=baudrate, interCharTimeout=1
                    )
                    break
                except (OSError, IOError):  # Py2 and Py3 have different errors
                    if wait == 0:
                        continue
                    if attempt == 0:
                        sys.stdout.write("Waiting {} seconds for pyboard ".format(wait))
                        delayed = True
                time.sleep(1)
                sys.stdout.write(".")
                sys.stdout.flush()
            else:
                if delayed:
                    print("")
                raise PyboardError("failed to access " + device)
            if delayed:
                print("")

    def close(self):
        self.serial.close()

    def read_until(self, min_num_bytes, ending, timeout=10, data_consumer=None):
        # if data_consumer is used then data is not accumulated and the ending must be 1 byte long
        assert data_consumer is None or len(ending) == 1

        data = self.serial.read(min_num_bytes)
        if data_consumer:
            data_consumer(data)
        timeout_count = 0
        while True:
            if data.endswith(ending):
                break
            elif self.serial.inWaiting() > 0:
                new_data = self.serial.read(1)
                if data_consumer:
                    data_consumer(new_data)
                    data = new_data
                else:
                    data = data + new_data
                timeout_count = 0
            else:
                timeout_count += 1
                if timeout is not None and timeout_count >= 100 * timeout:
                    break
                time.sleep(0.01)
        return data

    def enter_raw_repl(self):
        self.serial.write(b"\r\x03\x03")  # ctrl-C twice: interrupt any running program

        # flush input (without relying on serial.flushInput())
        n = self.serial.inWaiting()
        while n > 0:
            self.serial.read(n)
            n = self.serial.inWaiting()

        self.serial.write(b"\r\x01")  # ctrl-A: enter raw REPL
        data = self.read_until(1, b"raw REPL; CTRL-B to exit\r\n>")
        if not data.endswith(b"raw REPL; CTRL-B to exit\r\n>"):
            print(data)
            raise PyboardError("could not enter raw repl")

        self.serial.write(b"\x04")  # ctrl-D: soft reset
        data = self.read_until(1, b"soft reboot\r\n")
        if not data.endswith(b"soft reboot\r\n"):
            print(data)
            raise PyboardError("could not enter raw repl")
        # By splitting this into 2 reads, it allows boot.py to print stuff,
        # which will show up after the soft reboot and before the raw REPL.
        data = self.read_until(1, b"raw REPL; CTRL-B to exit\r\n")
        if not data.endswith(b"raw REPL; CTRL-B to exit\r\n"):
            print(data)
            raise PyboardError("could not enter raw repl")

    def exit_raw_repl(self):
        self.serial.write(b"\r\x02")  # ctrl-B: enter friendly REPL

    def follow(self, timeout, data_consumer=None):
        # wait for normal output
        data = self.read_until(1, b"\x04", timeout=timeout, data_consumer=data_consumer)
        if not data.endswith(b"\x04"):
            raise PyboardError("timeout waiting for first EOF reception")
        data = data[:-1]

        # wait for error output
        data_err = self.read_until(1, b"\x04", timeout=timeout)
        if not data_err.endswith(b"\x04"):
            raise PyboardError("timeout waiting for second EOF reception")
        data_err = data_err[:-1]

        # return normal and error output
        return data, data_err

    def exec_raw_no_follow(self, command):
        if isinstance(command, bytes):
            command_bytes = command
        else:
            command_bytes = bytes(command, encoding="utf8")

        # check we have a prompt
        data = self.read_until(1, b">")
        if not data.endswith(b">"):
            raise PyboardError("could not enter raw repl")

        # write command
        for i in range(0, len(command_bytes), 256):
            self.serial.write(command_bytes[i : min(i + 256, len(command_bytes))])
            time.sleep(0.01)
        self.serial.write(b"\x04")

        # check if we could exec command
        data = self.serial.read(2)
        if data != b"OK":
            raise PyboardError("could not exec command (response: %r)" % data)

    def exec_raw(self, command, timeout=10, data_consumer=None):
        self.exec_raw_no_follow(command)
        return self.follow(timeout, data_consumer)

    def eval(self, expression):
        ret = self.exec_("print({})".format(expression))
        ret = ret.strip()
        return ret

    def exec_(self, command):
        ret, ret_err = self.exec_raw(command)
        if ret_err:
            raise PyboardError("exception", ret, ret_err)
        return ret

    def execfile(self, filename):
        with open(filename, "rb") as f:
            pyfile = f.read()
        return self.exec_(pyfile)

    def get_time(self):
        t = str(self.eval("pyb.RTC().datetime()"), encoding="utf8")[1:-1].split(", ")
        return int(t[4]) * 3600 + int(t[5]) * 60 + int(t[6])


# in Python2 exec is a keyword so one must use "exec_"
# but for Python3 we want to provide the nicer version "exec"
setattr(Pyboard, "exec", Pyboard.exec_)


class PyCard10(Pyboard):
    """
    Python card10 connector.
    """

    def __init__(self, device, wait=0):
        """
        Open a connection to the card10 over the serial device *device*.
        """
        Pyboard.__init__(
            self, device=device, baudrate=115200, user=None, password=None, wait=wait
        )

    def exec_raw_no_follow(self, command):
        """
        Execute a the command 'command' on the card10.

        Parameters:
        command (bytes): Command or multiple commands

        Returns:
        None
        """

        if isinstance(command, bytes):
            command_bytes = command
        else:
            command_bytes = bytes(command, encoding="utf8")

        data = self.read_until(1, b">")
        if not data.endswith(b">"):
            raise PyboardError("card10 not in raw repl mode: (response: %r)" % (data))

        # write command
        for i in range(0, len(command_bytes), 256):
            self.serial.write(command_bytes[i : min(i + 256, len(command_bytes))])
            time.sleep(0.01)

        self.serial.write(b"\x04")

        # check if we could exec command
        data = self.serial.read(2)
        if data != b"OK":
            raise PyboardError("could not exec command (response: %r)" % data)

    def enter_raw_repl(self):
        """
        Enter the RAW repl mode. After the prompt character ('>') left in the buffer of the serial line.

        Returns:
        None
        """

        self.serial.write(b"\x03\x03")  # ctrl-C twice: interrupt any running program

        self.serial.write(b"\x02")  # ctrl-B: ensue it's the normal mode

        self.serial.write(b'__import__("os").exec("")\r')  # Reset to REPL

        # flush input (without relying on serial.flushInput())
        n = self.serial.inWaiting()
        while n > 0:
            self.serial.read(n)
            n = self.serial.inWaiting()

        self.serial.write(b"\x01")  # ctrl-A: enter raw REPL
        data = self.read_until(1, b"raw REPL; CTRL-B to exit\r\n>")
        if not data.endswith(b"raw REPL; CTRL-B to exit\r\n>"):
            raise PyboardError("could not enter raw repl")

        self.serial.write(b"\r\x04")  # execute nothing
        data = self.serial.read(2)
        if data != b"OK":
            raise PyboardError("could not enter raw repl")

    def exec(self, command):
        """
        Execute a command on the card10 and return the output of the card10.

        Parameters:
        command (bytes): Command or multiple commands

        Returns:
        data(bytes), data_err(bytes): data is standard out, data_err is standard error
        """
        return self.exec_(command)

    def soft_reset(self):
        """
        Doing a soft reset on the board and going to menu.

        Returns:
        None
        """

        self.serial.write(b"\x03\x03")  # ctrl-C twice: interrupt any running program

        self.serial.write(b"\x02")  # ctrl-B: ensue it's the normal mode

        self.serial.write(b"\x04")  # ctrl-D: do the reset

        n = self.serial.inWaiting()
        while n > 0:
            self.serial.read(n)
            n = self.serial.inWaiting()


def execfile(filename, device="/dev/ttyACM0"):
    """
    Execute python source from *filename* via the RAW repl mode on the card10 connected via serial line *device*.

    Parameters:
    filename(str): Path text file with commands
    device(str): Path to the card10 device.

    Returns:
    None
    """
    c = PyCard10(device)
    c.enter_raw_repl()
    output = c.execfile(filename)
    stdout_write_bytes(output)
    c.exit_raw_repl()
    c.close()


def main():
    """
    The main method.

    Returns:
    None
    """

    import argparse

    cmd_parser = argparse.ArgumentParser(description="Run scripts on the card10.")
    cmd_parser.add_argument(
        "--device", default="/dev/ttyACM0", help="the serial device of the card10"
    )
    cmd_parser.add_argument("-c", "--command", help="program passed in as string")
    cmd_parser.add_argument(
        "--set-time",
        action="store_true",
        help="Set card10 system time to this host's time",
    )
    cmd_parser.add_argument(
        "-w",
        "--wait",
        default=0,
        type=int,
        help="seconds to wait for USB connected board to become available",
    )
    cmd_parser.add_argument(
        "--follow",
        action="store_true",
        help="follow the output after running the scripts [default if no scripts given]",
    )
    cmd_parser.add_argument(
        "--reset", action="store_true", help="Soft reseting the card10"
    )
    cmd_parser.add_argument("files", nargs="*", help="input files")
    args = cmd_parser.parse_args()

    # open the connection to the card10
    try:
        card10 = PyCard10(args.device, args.wait)
    except PyboardError as er:
        print(er)
        sys.exit(1)

    if args.reset:
        card10.soft_reset()

    elif args.set_time or args.command is not None or len(args.files):
        # we must enter raw-REPL mode to execute commands
        # this will do a soft-reset of the board
        try:
            card10.enter_raw_repl()
        except PyboardError as er:
            print(er)
            card10.close()
            sys.exit(1)

        def execbuffer(buf):
            try:
                ret, ret_err = card10.exec_raw(
                    buf, timeout=None, data_consumer=stdout_write_bytes
                )
            except PyboardError as er:
                print(er)
                card10.close()
                sys.exit(1)
            except KeyboardInterrupt:
                sys.exit(1)
            if ret_err:
                card10.exit_raw_repl()
                card10.close()
                stdout_write_bytes(ret_err)
                sys.exit(1)

        # Set card10 system time
        if args.set_time:
            now = round(time.time())
            code = """\
import utime
utime.set_unix_time({time})
print("Time was set to {time}!")
"""
            execbuffer(code.format(time=now))

        # run the command, if given
        if args.command is not None:
            execbuffer(args.command.encode("utf-8"))

        # run any files
        for filename in args.files:
            with open(filename, "rb") as f:
                pyfile = f.read()
                execbuffer(pyfile)

        # exiting raw-REPL just drops to friendly-REPL mode
        card10.exit_raw_repl()

    # if asked explicitly, or no files given, then follow the output
    elif args.follow or (args.command is None and len(args.files) == 0):
        try:
            ret, ret_err = card10.follow(timeout=None, data_consumer=stdout_write_bytes)
        except PyboardError as er:
            print(er)
            sys.exit(1)
        except KeyboardInterrupt:
            sys.exit(1)
        if ret_err:
            card10.close()
            stdout_write_bytes(ret_err)
            sys.exit(1)

    # close the connection to the card10
    card10.close()


if __name__ == "__main__":
    main()
