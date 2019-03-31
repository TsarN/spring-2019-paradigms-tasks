#!/usr/bin/env python3
import pytest

from model import *
from folder import fold_constants


def test_fold_const_bin_op():
    assert fold_constants(
        BinaryOperation(
            Number(42),
            '+',
            Number(24))) == Number(66)


def test_fold_const_un_op():
    assert fold_constants(UnaryOperation('-', Number(123))) == Number(-123)


def test_fold_left_zero_mul():
    assert fold_constants(
        BinaryOperation(
            Number(0),
            '*',
            Reference('n'))) == Number(0)


def test_fold_right_zero_mul():
    assert fold_constants(
        BinaryOperation(
            Reference('n'),
            '*',
            Number(0))) == Number(0)


def test_fold_subtract_self():
    assert fold_constants(
        BinaryOperation(
            Reference('x'),
            '-',
            Reference('x'))) == Number(0)


def test_fold_subtract_other():
    assert fold_constants(
        BinaryOperation(
            Reference('x'),
            '-',
            Reference('y'))) == BinaryOperation(
        Reference('x'),
        '-',
        Reference('y'))


def test_e2e_foldable():
    assert fold_constants(
        BinaryOperation(
            Number(10),
            '-',
            UnaryOperation(
                '-',
                BinaryOperation(
                    Number(3),
                    '+',
                    BinaryOperation(
                        Reference('x'),
                        '-',
                        Reference('x')
                    )
                )
            )
        )
    ) == Number(13)


def test_e2e_unfoldable():
    factorial = FunctionDefinition(
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
    )

    assert fold_constants(factorial) == factorial


if __name__ == "__main__":
    pytest.main()
