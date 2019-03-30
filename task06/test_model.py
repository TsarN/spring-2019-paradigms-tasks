#!/usr/bin/env python3
import io
import pytest
from model import *


def test_this_scope():
    scope = Scope()
    scope['abc'] = 123
    scope['test'] = "True"
    assert scope['abc'] == 123
    assert scope['test'] == "True"


def test_grandparent_scope():
    grandparent = Scope()
    parent = Scope(grandparent)
    scope = Scope(parent)
    grandparent['abc'] = 123
    grandparent['test'] = "True"
    assert scope['abc'] == 123
    assert scope['test'] == "True"


def test_missing_scope():
    grandparent = Scope()
    parent = Scope(grandparent)
    scope = Scope(parent)
    with pytest.raises(KeyError):
        v = scope['does_not_exist']


def test_function_definition():
    scope = Scope()
    fun = Function([], [])
    FunctionDefinition("a", fun).evaluate(scope)
    assert scope["a"] == fun


def test_conditional():
    scope = Scope()

    assert Conditional(
        Number(1),
        [Number(24)],
        [Number(42)]
    ).evaluate(scope).value == 24

    assert Conditional(
        Number(0),
        [Number(24)],
        [Number(42)]
    ).evaluate(scope).value == 42


def test_print(capsys):
    scope = Scope()
    assert Print(Number(42)).evaluate(scope).value == 42

    captured = capsys.readouterr()
    assert captured.out == "42\n"


def test_read(monkeypatch):
    monkeypatch.setattr("sys.stdin", io.StringIO("42\n"))

    scope = Scope()
    assert Read("ans").evaluate(scope).value == 42
    assert scope["ans"].value == 42


def test_function_call():
    scope = Scope()
    fun = Function(["a"], [Reference("a")])
    call = FunctionCall(fun, [Number(42)])
    assert call.evaluate(scope).value == 42


def test_reference():
    scope = Scope()
    scope["a"] = Number(42)
    assert Reference("a").evaluate(scope).value == 42


def test_binary_operation():
    scope = Scope()
    for op in "+-*%":
        req = eval("14 {} 6".format(op))
        bop = BinaryOperation(Number(14), op, Number(6))
        assert bop.evaluate(scope).value == req


def test_unary_operation():
    scope = Scope()
    assert UnaryOperation("-", Number(42)).evaluate(scope).value == -42
    assert UnaryOperation("!", Number(0)).evaluate(scope).value == 1
    assert UnaryOperation("!", Number(1)).evaluate(scope).value == 0


def test_factorial():
    scope = Scope()
    FunctionDefinition(
        "factorial",
        Function(["n"], [
            Conditional(
                BinaryOperation(
                    Reference("n"),
                    "==",
                    Number(0)
                ),
                [Number(1)],
                [
                    BinaryOperation(
                        FunctionCall(
                            Reference("factorial"),
                            [BinaryOperation(Reference("n"), "-", Number(1))]
                        ), "*", Reference("n")
                    )
                ]
            )
        ]
        )
    ).evaluate(scope)

    assert FunctionCall(
        Reference("factorial"),
        [Number(10)]
    ).evaluate(scope).value == 3628800


if __name__ == "__main__":
    pytest.main()
