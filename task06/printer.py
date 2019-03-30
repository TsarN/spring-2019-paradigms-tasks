from model import ASTNodeVisitor


class PrettyPrinter(ASTNodeVisitor):
    def __init__(self):
        self.result = ""
        self.indent = 0

    def __str__(self):
        return self.result + ";"

    def newline(self):
        self.result += "\n"
        self.result += "    " * self.indent

    def visit_block(self, block):
        if not block:
            self.newline()
            return
        self.indent += 1
        self.newline()
        for i, stmt in enumerate(block):
            if i != 0:
                self.newline()
            stmt.accept(self)
            self.result += ";"
        self.indent -= 1
        self.newline()

    def visit_number(self, node):
        self.result += str(node.value)

    def visit_function(self, node):
        self.result += " {"
        self.visit_block(node.body)
        self.result += "}"

    def visit_function_definition(self, node):
        self.result += "def {}(".format(node.name)
        self.result += ', '.join(node.function.args)
        self.result += ")"
        node.function.accept(self)

    def visit_conditional(self, node):
        self.result += "if ("
        node.condition.accept(self)
        self.result += ") {"
        self.visit_block(node.if_true)
        self.result += "}"
        if node.if_false:
            self.result += " else {"
            self.visit_block(node.if_false)
            self.result += "}"

    def visit_print(self, node):
        self.result += "print "
        node.expr.accept(self)

    def visit_read(self, node):
        self.result += "read "
        node.expr.accept(self)

    def visit_function_call(self, node):
        node.fun_expr.accept(self)
        self.result += "("
        for i, arg in enumerate(node.args):
            if i != 0:
                self.result += ", "
            arg.accept(self)
        self.result += ")"

    def visit_reference(self, node):
        self.result += node.name

    def visit_binary_operation(self, node):
        self.result += "("
        node.lhs.accept(self)
        self.result += " {} ".format(node.op)
        node.rhs.accept(self)
        self.result += ")"

    def visit_unary_operation(self, node):
        self.result += "{}(".format(node.op)
        node.expr.accept(self)
        self.result += ")"


def pretty_print(program):
    printer = PrettyPrinter()
    program.accept(printer)
    print(str(printer))
