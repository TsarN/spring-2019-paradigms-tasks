from model import *


class ConstantFolder(ASTNodeVisitor):
    def visit_number(self, node):
        return Number(node.value)

    def visit_function(self, node):
        body = [stmt.accept(self) for stmt in node.body]
        return Function(node.args, body)

    def visit_function_definition(self, node):
        return FunctionDefinition(node.name, node.function.accept(self))

    def visit_conditional(self, node):
        condition = node.condition.accept(self)
        if_true = [stmt.accept(self) for stmt in node.condition.if_true]
        if_false = []
        if node.condition.if_false:
            if_false = [stmt.accept(self) for stmt in node.condition.if_false]
        return Conditional(condition, if_true, if_false)

    def visit_print(self, node):
        return Print(node.accept(self))

    def visit_read(self, node):
        return Read(node.accept(self))

    def visit_function_call(self, node):
        fun_expr = node.fun_expr.accept(self)
        args = [arg.accept(self) for arg in node.args]
        return FunctionCall(fun_expr, args)

    def visit_reference(self, node):
        return Reference(node.name)

    def visit_binary_operation(self, node):
        lhs = node.lhs.accept(self)
        rhs = node.rhs.accept(self)

        if isinstance(lhs, Number):
            if isinstance(rhs, Number):
                # BinaryOperation(Number, AnyBinOp, Number)
                return BinaryOperation(lhs, node.op, rhs).evaluate(None)
            if lhs.value == 0 and isinstance(rhs, Reference):
                # BinaryOperation(Number(0), '*', Reference)
                return Number(0)
        elif isinstance(rhs, Number):
            if rhs.value == 0 and isinstance(lhs, Reference):
                # BinaryOperation(Reference, '*', Number(0))
                return Number(0)
        elif isinstance(lhs, Reference) and isinstance(rhs, Reference):
            if lhs.name == rhs.name and node.op == '-':
                # BinaryOperation(Reference(name), ‘-’, Reference(name))
                return Number(0)

        return BinaryOperation(lhs, node.op, rhs)

    def visit_unary_operation(self, node):
        expr = node.expr.accept(self)

        if isinstance(expr, Number):
            # UnaryOperation(AnyUnOp, Number)
            return UnaryOperation(node.op, expr).evaluate(None)

        return UnaryOperation(node.op, expr)


def fold_constants(program):
    folder = ConstantFolder()
    return program.accept(folder)
