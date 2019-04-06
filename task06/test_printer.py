#!/usr/bin/env python3
import pytest

from model import *
from printer import PrettyPrinter


def pprint(program):
    printer = PrettyPrinter()
    program.accept(printer)
    return str(printer)


def test_conditional():
    assert pprint(Conditional(Number(42), [], [])) == """\
if (42) {
}"""


def test_function_definition():
    assert pprint(FunctionDefinition("foo", Function([], []))) == """\
def foo() {
}"""


def test_print():
    assert pprint(Print(Number(42))) == "print 42;"


def test_read():
    assert pprint(Read('x')) == "read x;"


def test_number():
    assert pprint(Number(10)) == "10;"


def test_reference():
    assert pprint(Reference('x')) == "x;"


def test_binary_operation():
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(0), '*', add)
    assert pprint(mul) == "(0 * (2 + 3));"


def test_unary_operation():
    assert pprint(UnaryOperation('-', Number(42))) == "-42;"


def test_function_call():
    assert pprint(FunctionCall(Reference('foo'),
                  [Number(1), Number(2), Number(3)])) == "foo(1, 2, 3);"


def test_end_to_end():
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
    assert pprint(program) == """\
def main(arg1) {
    read x;
    print x;
    if ((2 == 3)) {
        if (1) {
        }
    } else {
        exit(-arg1);
    }
}"""


if __name__ == "__main__":
    pytest.main()
