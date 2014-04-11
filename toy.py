#!/usr/bin/env python2

import re
from llvm.core import Module, Constant, Type, Function, Builder, FCMP_ULT
from llvm.ee import ExecutionEngine, TargetData
from llvm.passes import FunctionPassManager

from llvm.core import FCMP_ULT, FCMP_ONE
from llvm.passes import (PASS_INSTRUCTION_COMBINING,
                         PASS_REASSOCIATE,
                         PASS_GVN,
                         PASS_CFG_SIMPLIFICATION)

g_llvm_module = Module.new('my cool jit')
g_llvm_builder = None
g_named_values = {}
g_llvm_pass_manager = FunctionPassManager.new(g_llvm_module)
g_llvm_executor = ExecutionEngine.new(g_llvm_module)
g_binop_precedence = {}

class EOFToken(object): pass

class DefToken(object): pass

class ExternToken(object): pass

class IdentifierToken(object):
    def __init__(self, name):
        self.name = name

class NumberToken(object):
    def __init__(self, value):
        self.value = value


class CharacterToken(object):
    def __init__(self, char):
        self.char = char
    def __eq__(self, other):
        return isinstance(other, CharacterToken) and self.char == other.char
    def __ne__(self, other):
        return not self == other

class IfToken(object):
    pass
class ThenToken(object):
    pass
class ElseToken(object):
    pass
class ForToken(object):
    pass
class InToken(object):
    pass
class BinaryToken(object):
    pass
class UnaryToken(object):
    pass

# Regular expressions that tokens and comments of our language.
REGEX_NUMBER = re.compile('[0-9]+(?:.[0-9]+)?')
REGEX_IDENTIFIER = re.compile('[a-zA-Z][a-zA-Z0-9]*')
REGEX_COMMENT = re.compile('#.*')

def Tokenize(string):
    while string: # Skip whitespace
        if string[0].isspace():
            string = string[1:]
            continue

        # Run regexes
        comment_match = REGEX_COMMENT.match(string)
        number_match = REGEX_NUMBER.match(string)
        identifier_match = REGEX_IDENTIFIER.match(string)

        if comment_match:
            comment = comment_match.group(0)
            string = string[len(comment):]
        elif number_match:
            number = number_match.group(0)
            yield NumberToken(float(number))
            string = string[len(number):]
        elif identifier_match:
            identifier = identifier_match.group(0)
            if identifier == 'def':
                yield DefToken()
            elif identifier == 'extern':
                yield ExternToken()
            elif identifier == 'if':
                yield IfToken()
            elif identifier == 'then':
                yield ThenToken()
            elif identifier == 'else':
                yield ElseToken()
            elif identifier == 'for':
                yield ForToken()
            elif identifier == 'in':
                yield InToken()
            elif identifier == 'binary':
                yield BinaryToken()
            elif identifier == 'unary':
                yield UnaryToken()
            else:
                yield IdentifierToken(identifier)
            string = string[len(identifier):]
        else:
            yield CharacterToken(string[0])
            string = string[1:]

    yield EOFToken()


# Base class for all expression nodes.
class ExpressionNode(object): pass

class NumberExpressionNode(ExpressionNode):
    def __init__(self, value):
        self.value = value
    def CodeGen(self):
        return Constant.real(Type.double(), self.value)

class VariableExpressionNode(ExpressionNode):
    def __init__(self, name):
        self.name = name

    def CodeGen(self):
        if self.name in g_named_values:
            return g_named_values[self.name]
        else:
            raise RuntimeError('Unknown variable name: ' + self.name)

class BinaryOperationExpressionNode(ExpressionNode):
    def __init__(self, operator, left, right):
        self.operator = operator
        self.left = left
        self.right = right

    def CodeGen(self):
        left = self.left.CodeGen()
        right = self.right.CodeGen()

        if self.operator == '+':
            return g_llvm_builder.fadd(left, right, 'addtmp')
        elif self.operator == '-':
            return g_llvm_builder.fsub(left, right, 'subtmp')
        elif self.operator == '*':
            return g_llvm_builder.fmul(left, right, 'multmp')
        elif self.operator == '<':
            result = g_llvm_builder.fcmp(FCMP_ULT, left, right, 'cmptmp')
            return g_llvm_builder.uitofp(result, Type.double(), 'booltmp')
        else:
            function = g_llvm_module.get_function_named('binary' + self.operator)
            return g_llvm_builder.call(function, [left, right], 'binop')

class CallExpressionNode(ExpressionNode):
    def __init__(self, callee, args):
        self.callee = callee
        self.args = args

    def CodeGen(self):
        callee = g_llvm_module.get_function_named(self.callee)

        if len(callee.args) != len(self.args):
            raise RuntimeError('Incorrect number of arguments passed.')

        arg_values = [i.CodeGen() for i in self.args]

        return g_llvm_builder.call(callee, arg_values, 'calltmp')

class IfExpressionNode(ExpressionNode):
    def __init__(self, condition, then_branch, else_branch):
        self.condition = condition
        self.then_branch = then_branch
        self.else_branch = else_branch

    def CodeGen(self):
        condition = self.condition.CodeGen()

        condition_bool = g_llvm_builder.fcmp(
            FCMP_ONE, condition, Constant.real(Type.double(), 0), 'ifcond')

        function = g_llvm_builder.basic_block.function

        then_block = function.append_basic_block('then')
        else_block = function.append_basic_block('else')
        merge_block = function.append_basic_block('ifcond')

        g_llvm_builder.cbranch(condition_bool, then_block, else_block)

        g_llvm_builder.position_at_end(then_block)
        then_value = self.then_branch.CodeGen()
        g_llvm_builder.branch(merge_block)

        then_block = g_llvm_builder.basic_block

        g_llvm_builder.position_at_end(else_block)
        else_value = self.else_branch.CodeGen()
        g_llvm_builder.branch(merge_block)

        else_block = g_llvm_builder.basic_block

        g_llvm_builder.position_at_end(merge_block)
        phi = g_llvm_builder.phi(Type.double(), 'ifmp')
        phi.add_incoming(then_value, then_block)
        phi.add_incoming(else_value, else_block)

        return phi

class ForExpressionNode(ExpressionNode):
    def __init__(self, loop_variable, start, end, step, body):
        self.loop_variable = loop_variable
        self.start = start
        self.end = end
        self.step = step
        self.body = body
    
    def CodeGen(self):
        start_value = self.start.CodeGen()

        function = g_llvm_builder.basic_block.function
        pre_header_block = g_llvm_builder.basic_block
        loop_block = function.append_basic_block('loop')

        g_llvm_builder.branch(loop_block)

        g_llvm_builder.position_at_end(loop_block)

        variable_phi = g_llvm_builder.phi(Type.double(), self.loop_variable)
        variable_phi.add_incoming(start_value, pre_header_block)

        old_value = g_named_values.get(self.loop_variable, None)
        g_named_values[self.loop_variable] = variable_phi

        self.body.CodeGen()

        if self.step:
            step_value = self.step.CodeGen()
        else:
            step_value = Constant.real(Type.double(), 1)

        next_value = g_llvm_builder.fadd(variable_phi, step_value, 'next')

        end_condition = self.end.CodeGen()
        end_condition_bool = g_llvm_builder.fcmp(
            FCMP_ONE, end_condition, Constant.real(Type.double(), 0), 'loopcond')

        loop_end_block = g_llvm_builder.basic_block
        after_block = function.append_basic_block('afterloop')

        g_llvm_builder.cbranch(end_condition_bool, loop_block, after_block)

        g_llvm_builder.position_at_end(after_block)

        variable_phi.add_incoming(next_value, loop_end_block)

        if old_value:
            g_named_values[self.loop_variable] = old_value
        else:
            del g_named_values[self.loop_variable]

        return Constant.real(Type.double(), 0)

class UnaryExpressionNode(ExpressionNode):
    def __init__(self, operator, operand):
        self.operator = operator
        self.operand = operand

    def CodeGen(self):
        operand = self.operand.CodeGen()
        function = g_llvm_module.get_function_named('unary' + self.operator)
        return g_llvm_builder.call(function, [operand], 'unop')


class PrototypeNode(object):
    def __init__(self, name, args, is_operator=False, precedence=0):
        self.name = name
        self.args = args
        self.is_operator = is_operator
        self.precedence = precedence

    def IsBinaryOp(self):
        return self.is_operator and len(self.args) == 2

    def GetOperatorName(self):
        assert self.is_operator
        return self.name[-1]

    def CodeGen(self):
        funct_type = Type.function(
            Type.double(), [Type.double()] * len(self.args), False)

        function = Function.new(g_llvm_module, funct_type, self.name)

        if function.name != self.name:
            function.delete()
            function = g_llvm_module.get_function_named(self.name)

        if not function.is_declaration:
            raise RuntimeError('Redefinition of function.')

        if len(function.args) != len(self.args):
            raise RuntimeError('Redeclaration of a function with different number of args.')

        for arg, arg_name in zip(function.args, self.args):
            arg.name = arg_name
            g_named_values[arg_name] = arg # TODO ???

        return function

class FunctionNode(object):
    def __init__(self, prototype, body):
        self.prototype = prototype
        self.body = body

    def CodeGen(self):
        g_named_values.clear()

        function = self.prototype.CodeGen()

        if self.prototype.IsBinaryOp():
            operator = self.prototype.GetOperatorName()
            g_binop_precedence[operator] = self.prototype.precedence

        block = function.append_basic_block('entry')
        global g_llvm_builder
        g_llvm_builder = Builder.new(block)

        try:
            return_value = self.body.CodeGen()
            g_llvm_builder.ret(return_value)

            function.verify()

            g_llvm_pass_manager.run(function)
        except:
            function.delete()
            if self.prototype.IsBinaryOp():
                del g_binop_precedence[self.prototype.GetOperatorName()]
            raise

        return function

class Parser(object):
    def __init__(self, tokens):
        self.tokens = tokens
        self.Next()

    def Next(self):
        self.current = self.tokens.next()

    def ParseNumberExpr(self):
        result = NumberExpressionNode(self.current.value)
        self.Next()
        return result

    def ParseParenExpr(self):
        self.Next()

        contents = self.ParseExpression()

        if self.current != CharacterToken(')'):
            raise RuntimeError('Expected ")".')
        self.Next()

        return contents

    def ParseIdentifierExpr(self):
        identifier_name = self.current.name
        self.Next()

        if self.current != CharacterToken('('):
            return VariableExpressionNode(identifier_name)

        self.Next()
        args = []
        if self.current != CharacterToken(')'):
            while True:
                args.append(self.ParseExpression())
                if self.current == CharacterToken(')'):
                    break
                elif self.current != CharacterToken(','):
                    raise RuntimeError('Expected ")" or "," in argument list.')
                self.Next()

        self.Next()
        return CallExpressionNode(identifier_name, args)

    def ParsePrimary(self):
        if isinstance(self.current, IdentifierToken):
            return self.ParseIdentifierExpr()
        elif isinstance(self.current, NumberToken):
            return self.ParseNumberExpr()
        elif isinstance(self.current, IfToken):
            return self.ParseIfExpr()
        elif self.current == CharacterToken('('):
            return self.ParseParenExpr()
        else:
            raise RuntimeError('Unknown token when expecting an expression.')

    def GetCurrentTokenPrecedence(self):
        if isinstance(self.current, CharacterToken):
            return g_binop_precedence.get(self.current.char, -1)
        else:
            return -1

    def ParseExpression(self):
        left = self.ParseUnary()
        return self.ParseBinOpRHS(left, 0)

    def ParseBinOpRHS(self, left, left_precedence):
        while True:
            precedence = self.GetCurrentTokenPrecedence()

            if precedence < left_precedence:
                return left

            binary_operator = self.current.char
            self.Next()

            right = self.ParseUnary()

            next_precedence = self.GetCurrentTokenPrecedence()
            if precedence < next_precedence:
                right = self.ParseBinOpRHS(right, precedence + 1)

            left = BinaryOperationExpressionNode(binary_operator, left, right)
    
    def ParseUnary(self):
        if (not isinstance(self.current, CharacterToken) or
            self.current in [CharacterToken('('), CharacterToken(',')]):
            return self.ParsePrimary()

        operator = self.current.char
        self.Next()
        return UnaryExpressionNode(operator, self.ParseUnary())

    def ParsePrototype(self):
        precedence = None
        if isinstance(self.current, IdentifierToken):
            kind = 'normal'
            function_name = self.current.name
            self.Next()
        elif isinstance(self.current, UnaryToken):
            kind = 'unary'
            self.Next()
            if not isinstance(self.current, CharacterToken):
                raise RuntimeError('Expected an operator after "unary".')
            function_name = 'unary' + self.current.char
            self.Next()
        elif isinstance(self.current, BinaryToken):
            kind = 'binary'
            self.Next()
            if not isinstance(self.current, CharacterToken):
                raise RuntimeError('Expected an operator after "binary".')
            function_name = 'binary' + self.current.char
            self.Next()
            if isinstance(self.current, NumberToken):
                if not 1 <= self.current.value <= 100:
                    raise RuntimeError('Invalid precedence: must be in range [1 .. 100].')
                precedence = self.current.value
                self.Next()
        else:
            raise RuntimeError('Expected function name, "unary" or "binary" in prototype.')

        if self.current != CharacterToken('('):
            raise RuntimeError('Expected "(" in prototype.')
        self.Next()

        arg_names = []
        while isinstance(self.current, IdentifierToken):
            arg_names.append(self.current.name)
            self.Next()

        if self.current != CharacterToken(')'):
            raise RuntimeError('Expected ")" in prototype.')

        self.Next()

        if kind == 'unary' and len(arg_names) != 1:
            raise RuntimeError('Invalid number of arguments for a unary operator.')
        elif kind == 'binary' and len(arg_names) != 2:
            raise RuntimeError('Invalid number of arguments for a binary operator.')

        return PrototypeNode(function_name, arg_names, kind != 'normal', precedence)

    def ParseDefinition(self):
        self.Next()
        proto = self.ParsePrototype()
        body = self.ParseExpression()
        return FunctionNode(proto, body)

    def ParseExtern(self):
        self.Next()
        return self.ParsePrototype()

    def ParseIfExpr(self):
        self.Next()

        condition = self.ParseExpression()

        if not isinstance(self.current, ThenToken):
            raise RuntimeError('Expected "then".')
        self.Next()

        then_branch = self.ParseExpression()

        if not isinstance(self.current, ElseToken):
            raise RuntimeError('Expected "else".')
        self.Next()

        else_branch = self.ParseExpression()

        return IfExpressionNode(condition, then_branch, else_branch)

    def ParseForExpr(self):
        self.Next()

        if not isinstance(self.current, IdentifierToken):
            raise RuntimeError('Expected identifier after for.')

        loop_variable = self.current.name
        self.Next()

        if self.current != CharacterToken('='):
            raise RuntimeError('Expected "=" after for variable.')
        self.Next()

        start = self.ParseExpression()

        if self.current != CharacterToken(','):
            raise RuntimeError('Expected "," after for start value.')
        self.Next()

        end = self.ParseExpression()

        if self.current == CharacterToken(','):
            self.Next()
            step = self.ParseExpression()
        else:
            step = None

        if not isinstance(self.current, InToken):
            raise RuntimeError('Expected "in" after for variable specification.')
        self.Next()

        body = self.ParseExpression()

        return ForExpressionNode(loop_variable, start, end, step, body)

    def ParseTopLevelExpr(self):
        proto = PrototypeNode('', [])
        return FunctionNode(proto, self.ParseExpression())

    def HandleDefinition(self):
        self.Handle(self.ParseDefinition, 'Parsed a function definition.')

    def HandleExtern(self):
        self.Handle(self.ParseExtern, 'Parsed an extern.')

    def HandleTopLevelExpression(self):
        try:
            function = self.ParseTopLevelExpr().CodeGen()
            result = g_llvm_executor.run_function(function, [])
            print 'Evaluated to:', result.as_real(Type.double())
        except Exception,e:
            print 'Error:', e
            try:
                self.Next()
            except:
                pass

    def Handle(self, function, message):
        try:
            print message, function().CodeGen()
        except Exception, e:
            print 'Error:', e
            try:
                self.Next()
            except:
                pass

def main():
    g_llvm_pass_manager.add(g_llvm_executor.target_data)
    g_llvm_pass_manager.add(PASS_INSTRUCTION_COMBINING)
    g_llvm_pass_manager.add(PASS_REASSOCIATE)
    g_llvm_pass_manager.add(PASS_GVN)
    g_llvm_pass_manager.add(PASS_CFG_SIMPLIFICATION)

    g_llvm_pass_manager.initialize()

    g_binop_precedence['<'] = 10
    g_binop_precedence['+'] = 20
    g_binop_precedence['-'] = 20
    g_binop_precedence['*'] = 40

    while True:
        print 'ready>',
        try:
            raw = raw_input()
        except (KeyboardInterrupt, EOFError):
            break

        parser = Parser(Tokenize(raw))
        while True:
            if isinstance(parser.current, EOFToken):
                break
            if isinstance(parser.current, DefToken):
                parser.HandleDefinition()
            elif isinstance(parser.current, ExternToken):
                parser.HandleExtern()
            else:
                parser.HandleTopLevelExpression()
    print '\n', g_llvm_module

if __name__ == '__main__':
    main()
