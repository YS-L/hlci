import os
import argparse
import subprocess
from pprint import pprint


class TestCase(object):

    MATCH = 0
    DOES_NOT_MATCH = 1
    SHOULD_ERROR = 2
    SHOULD_NOT_ERROR = 3

    def __init__(self, path, source, content, expected_out, expected_error, readme):
        self.path = path
        self.source = source
        self.content = content
        self.expected_out = expected_out
        self.expected_error = expected_error
        self.readme = readme
        self.status = None
        self.output = None

    @classmethod
    def from_directory(cls, path):
        filenames = os.listdir(path)
        if 'test.lol' not in filenames:
            raise ValueError('Source code not found in {}'.format(directory))
        source = os.path.join(path, 'test.lol')
        with open(source) as f:
            content = f.read()
        if 'test.out' in filenames:
            with open(os.path.join(path, 'test.out')) as f:
                expected_out = f.read()
            expected_error = None
        else:
            with open(os.path.join(path, 'test.err')) as f:
                pass
            expected_out = None
            expected_error = True
        if 'test.readme' in filenames:
            with open(os.path.join(path, 'test.readme')) as f:
                readme = f.read()
        else:
            readme = ''
        return cls(path, source, content, expected_out, expected_error, readme)

    def run(self, executable):
        cmd = [executable]
        cmd.append(self.source)
        try:
            self.out = subprocess.check_output(cmd, stderr=subprocess.STDOUT)
            if self.expected_error is not None:
                self.status = self.SHOULD_ERROR
            elif self.out != self.expected_out:
                self.status = self.DOES_NOT_MATCH
            else:
                self.status = self.MATCH
        except subprocess.CalledProcessError as e:
            self.out = e.output
            if self.expected_error is None:
                self.status = self.SHOULD_NOT_ERROR
            else:
                self.status = self.MATCH

    def __repr__(self):
        s = ''
        s += '\n========== TestCase ==========\n'
        s += '{}: {}'.format(self.path, self.readme)
        s += '\n========== Source ==========\n'
        s += self.content
        s += '\n========== Expected ==========\n'
        s += str(self.expected_out)
        if self.status != self.MATCH:
            s += '\n========== Got ============\n'
            s += str(self.out)
        return s


def gen_tests(base):
    for root, dirs, files in os.walk(base):
        if 'test.lol' not in files:
            continue
        test_case = TestCase.from_directory(root)
        yield test_case


def main():
    parser = argparse.ArgumentParser('lci_suite')
    parser.add_argument('suite_directory')
    parser.add_argument('executable')
    args = parser.parse_args()

    tests = list(gen_tests(args.suite_directory))
    for tc in tests:
        tc.run(args.executable)

    should_error = [tc for tc in tests if tc.status == TestCase.SHOULD_ERROR]
    does_not_match = [tc for tc in tests if tc.status == TestCase.DOES_NOT_MATCH]
    should_not_error = [tc for tc in tests if tc.status == TestCase.SHOULD_NOT_ERROR]
    match = [tc for tc in tests if tc.status == TestCase.MATCH]

    total = len(tests)

    def summary(category, s, details=True):
        print('# {}: {}/{}'.format(category, len(s), total))
        if details:
            pprint(s)

    summary('should error', should_error)
    summary('does not match', does_not_match)
    summary('should not error', should_not_error)
    summary('pass', match, details=False)


if __name__ == '__main__':
    main()
