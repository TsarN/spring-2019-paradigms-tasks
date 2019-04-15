#!/usr/bin/env python3
import pytest

from model import *
from printer import pretty_print


def test_conditional(capsys):
    pretty_print(Conditional(Number(42), [], []))
    captured = capsys.readouterr()
    assert captured.out == """\
if (42) {
}
"""


def test_function_definition(capsys):
    pretty_print(FunctionDefinition("foo", Function([], [])))
    captured = capsys.readouterr()
    assert captured.out == """\
def foo() {
}
"""


def test_print(capsys):
    pretty_print(Print(Number(42)))
    captured = capsys.readouterr()
    assert captured.out == "print 42;\n"


def test_read(capsys):
    pretty_print(Read('x'))
    captured = capsys.readouterr()
    assert captured.out == "read x;\n"


def test_number(capsys):
    pretty_print(Number(10))
    captured = capsys.readouterr()
    assert captured.out == "10;\n"


def test_reference(capsys):
    pretty_print(Reference('x'))
    captured = capsys.readouterr()
    assert captured.out == "x;\n"


def test_binary_operation(capsys):
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(0), '*', add)
    pretty_print(mul)
    captured = capsys.readouterr()
    assert captured.out == "(0 * (2 + 3));\n"


def test_unary_operation(capsys):
    pretty_print(UnaryOperation('-', Number(42)))
    captured = capsys.readouterr()
    assert captured.out == "(-(42));\n"


def test_function_call(capsys):
    pretty_print(FunctionCall(Reference('foo'), [Number(1), Number(2), Number(3)]))
    captured = capsys.readouterr()
    assert captured.out == "foo(1, 2, 3);\n"


def test_end_to_end(capsys):
    program = FunctionDefinition('main', Function(['arg1'], [
        Read('x'),
        Print(Reference('x')),
        Conditional(
            BinaryOperation(Number(2), '==', Number(3)),
            [
                Conditional(Number(1), [], [])
                ],
            [
                FunctionCall(Reference('exit'), [
                    UnaryOperation('-', Reference('arg1'))
                    ])
                ],
            ),
        ]))
    pretty_print(program)
    captured = capsys.readouterr()
    assert captured.out == """\
def main(arg1) {
	read x;
	print x;
	if ((2 == 3)) {
		if (1) {
		}
	} else {
		exit((-(arg1)));
	}
}
"""


if __name__ == "__main__":
    pytest.main()
