#!/usr/bin/env python2
'''Basic McCarthy lisp written in LLVMPY. First stage of IBCL.'''

import re
import string
from llvm.core import Module, Constant, Type, Function, Builder
from llvm.ee import ExecutionEngine, TargetData
from llvm.passes import FunctionPassManager

from llvm.core import FCMP_ULT, FCMP_ONE
from llvm.passes import (PASS_PROMOTE_MEMORY_TO_REGISTER,
                         PASS_INSTRUCTION_COMBINING,
                         PASS_REASSOCIATE,
                         PASS_GVN,
                         PASS_CFG_SIMPLIFICATION)

G_LLVM_MODULE = Module.new('my cool jit')
G_LLVM_BUILDER = None
G_NAMED_VALUES = {}
G_LLVM_PASS_MANAGER = FunctionPassManager.new(G_LLVM_MODULE)
G_LLVM_EXECUTOR = ExecutionEngine.new(G_LLVM_MODULE)
G_BINOP_PRECEDENCE = {}

def create_entry_block_alloca(function, var_name):
    '''Create stack allocation instructions for a variable'''
    entry = function.get_entry_basic_block()
    builder = Builder.new(entry)
    builder.position_at_beginning(entry)
    return builder.alloca(Type.double(), var_name)

class ConsIterator(object):
    '''Class for pythonish iteration over cons-based lists.'''
    def __init__(self, cons):
        self.cur_cons = cons

    def __iter__(self):
        return self

    def __next__(self):
        if self.cur_cons is None:
            raise StopIteration
        else:
            cur = self.cur_cons.car
            self.cur_cons = self.cur_cons.cdr
            return cur

class Cons(object):
    '''Elementary cons-cell'''
    def __init__(self, car=None, cdr=None):
        self.car = car
        self.cdr = cdr

    def __iter__(self):
        return ConsIterator(self)

    def __repr__(self):
        return self.PrintCons(True)

    def PrintCons(self, toplevel=False):
        if self.cdr is None:
            tail = ")"
        else:
            tail = self.cdr.PrintCons()

        if toplevel:
            return string.join(["(%s" % self.car, tail],
                               " ")
        else:
            return string.join(["%s" % self.car, tail],
                               " ")


def car(cons):
    '''Return cargo of a cons-cell'''
    assert isinstance(cons, Cons)
    return cons.car
def cdr(cons):
    '''Return CDR of a cons-cell'''
    assert isinstance(cons, Cons)
    return cons.cdr

def cons_list(*args):
    if len(args) == 0:
        None
    else:
        return Cons(args[0], cons_list(*(args[1:])))

class EOFToken(object):
    pass
class LBrToken(object):
    pass
class RBrToken(object):
    pass
class QuoteToken(object):
    pass

class SymbolToken(object):
    def __init__(self, name):
        self.name = name

class Symbol(SymbolToken):
    def __init__(self, name, number):
        super().__init__(self, name)
        self.number = number
    def __repr__(self):
        return self.name

class NumberToken(object):
    def __init__(self, value):
        self.value = value

# Regular expressions that tokens and comments of our language.
REGEX_NUMBER = re.compile('[0-9]+(?:.[0-9]+)?')
REGEX_SYMBOL = re.compile("[^()' ]+")
REGEX_COMMENT = re.compile(';.*')

def tokenize(string):
    '''Consume string, outputting tokens.'''
    while string: # Skip whitespace
        if string[0].isspace():
            string = string[1:]
            continue

        if string[0] == '(':
            yield LBrToken()
            string = string[1:]
        elif string[0] == ')':
            yield RBrToken()
            string = string[1:]
        elif string[0] == "'":
            yield QuoteToken()
            string = string[1:]
        else:
            comment_match = REGEX_COMMENT.match(string)
            number_match = REGEX_NUMBER.match(string)
            symbol_match = REGEX_SYMBOL.match(string)

            if comment_match:
                comment = comment_match.group(0)
                string = string[len(comment):]
            elif number_match:
                number = number_match.group(0)
                yield NumberToken(float(number))
                string = string[len(number):]
            elif symbol_match:
                symbol = symbol_match.group(0)
                yield SymbolToken(symbol)
                string = string[len(symbol):]
            else:
                raise RuntimeError('Something should''ve matched in tokenizer')
    
    yield EOFToken()


class ExpressionNode(object):
    '''Base class for all expression nodes.'''
    pass

def codegen_for_data(expr):
    if isinstance(expr, int) or isinstance(expr, double):
        return Constant.real(Type.double(), expr)
    elif isinstance(expr, Symbol):
        return Constant.integer(Type.int(), expr.number)
    elif isinstance(expr, Cons):
        pass
    else:
        raise RuntimeError("Don't know how to codewalk following data type %s"
                           % type(expr))
                           


class QuoteExpressionNode(object):
    def __init__(self, expr):
        self.expr = expr
    def CodeGen(self):
        return codegen_for_data(self.expr)


class NumberExpressionNode(ExpressionNode):
    def __init__(self, value):
        self.value = value
    def CodeGen(self):
        return Constant.real(Type.double(), self.value)

class VariableExpressionNode(ExpressionNode):
    def __init__(self, name):
        self.name = name

    def CodeGen(self):
        if self.name in G_NAMED_VALUES:
            return G_LLVM_BUILDER.load(G_NAMED_VALUES[self.name], self.name)
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

        if self.operator == '=':
            if not isinstance(self.left, VariableExpressionNode):
                raise RuntimeError('Destination of "=" must be a variable.')
            value = self.right.CodeGen()
            variable = G_NAMED_VALUES[self.left.name]
            G_LLVM_BUILDER.store(value, variable)
            return value
        elif self.operator == '+':
            return G_LLVM_BUILDER.fadd(left, right, 'addtmp')
        elif self.operator == '-':
            return G_LLVM_BUILDER.fsub(left, right, 'subtmp')
        elif self.operator == '*':
            return G_LLVM_BUILDER.fmul(left, right, 'multmp')
        elif self.operator == '<':
            result = G_LLVM_BUILDER.fcmp(FCMP_ULT, left, right, 'cmptmp')
            return G_LLVM_BUILDER.uitofp(result, Type.double(), 'booltmp')
        else:
            function = G_LLVM_MODULE.get_function_named('binary' + self.operator)
            return G_LLVM_BUILDER.call(function, [left, right], 'binop')

class CallExpressionNode(ExpressionNode):
    def __init__(self, callee, args):
        self.callee = callee
        self.args = args

    def CodeGen(self):
        callee = G_LLVM_MODULE.get_function_named(self.callee)

        if len(callee.args) != len(self.args):
            raise RuntimeError('Incorrect number of arguments passed.')

        arg_values = [i.CodeGen() for i in self.args]

        return G_LLVM_BUILDER.call(callee, arg_values, 'calltmp')

class IfExpressionNode(ExpressionNode):
    def __init__(self, condition, then_branch, else_branch):
        self.condition = condition
        self.then_branch = then_branch
        self.else_branch = else_branch

    def CodeGen(self):
        condition = self.condition.CodeGen()

        condition_bool = G_LLVM_BUILDER.fcmp(
            FCMP_ONE, condition, Constant.real(Type.double(), 0), 'ifcond')

        function = G_LLVM_BUILDER.basic_block.function

        then_block = function.append_basic_block('then')
        else_block = function.append_basic_block('else')
        merge_block = function.append_basic_block('ifcond')

        G_LLVM_BUILDER.cbranch(condition_bool, then_block, else_block)

        G_LLVM_BUILDER.position_at_end(then_block)
        then_value = self.then_branch.CodeGen()
        G_LLVM_BUILDER.branch(merge_block)

        then_block = G_LLVM_BUILDER.basic_block

        G_LLVM_BUILDER.position_at_end(else_block)
        else_value = self.else_branch.CodeGen()
        G_LLVM_BUILDER.branch(merge_block)

        else_block = G_LLVM_BUILDER.basic_block

        G_LLVM_BUILDER.position_at_end(merge_block)
        phi = G_LLVM_BUILDER.phi(Type.double(), 'ifmp')
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
        function = G_LLVM_BUILDER.basic_block.function

        alloca = create_entry_block_alloca(function, self.loop_variable)

        start_value = self.start.CodeGen()

        G_LLVM_BUILDER.store(start_value, alloca)

        loop_block = function.append_basic_block('loop')

        G_LLVM_BUILDER.branch(loop_block)

        G_LLVM_BUILDER.position_at_end(loop_block)

        old_value = G_NAMED_VALUES.get(self.loop_variable, None)
        G_NAMED_VALUES[self.loop_variable] = alloca

        self.body.CodeGen()

        if self.step:
            step_value = self.step.CodeGen()
        else:
            step_value = Constant.real(Type.double(), 1)

        end_condition = self.end.CodeGen()
        
        cur_value = G_LLVM_BUILDER.load(alloca, self.loop_variable)
        next_value = G_LLVM_BUILDER.fadd(cur_value, step_value, 'nextvar')
        G_LLVM_BUILDER.store(next_value, alloca)

        end_condition_bool = G_LLVM_BUILDER.fcmp(
            FCMP_ONE, end_condition, Constant.real(Type.double(), 0), 'loopcond')

        after_block = function.append_basic_block('afterloop')

        G_LLVM_BUILDER.cbranch(end_condition_bool, loop_block, after_block)

        G_LLVM_BUILDER.position_at_end(after_block)

        if old_value:
            G_NAMED_VALUES[self.loop_variable] = old_value
        else:
            del G_NAMED_VALUES[self.loop_variable]

        return Constant.real(Type.double(), 0)

class UnaryExpressionNode(ExpressionNode):
    def __init__(self, operator, operand):
        self.operator = operator
        self.operand = operand

    def CodeGen(self):
        operand = self.operand.CodeGen()
        function = G_LLVM_MODULE.get_function_named('unary' + self.operator)
        return G_LLVM_BUILDER.call(function, [operand], 'unop')

class VarExpressionNode(ExpressionNode):
    def __init__(self, variables, body):
        self.variables = variables
        self.body = body

    def CodeGen(self):
        old_bindings = {}
        function = G_LLVM_BUILDER.basic_block.function

        for var_name, var_expression in self.variables.iteritems():
            if var_expression is not None:
                var_value = var_expression.CodeGen()
            else:
                var_value = Constant.real(Type.double(), 0)

            alloca = create_entry_block_alloca(function, var_name)
            G_LLVM_BUILDER.store(var_value, alloca)

            old_bindings[var_name] = G_NAMED_VALUES.get(var_name, None)

            G_NAMED_VALUES[var_name] = alloca

        body = self.body.CodeGen()

        for var_name in self.variables:
            if old_bindings[var_name] is not None:
                G_NAMED_VALUES[var_name] = old_bindings[var_name]
            else:
                del G_NAMED_VALUES[var_name]

        return body

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

        function = Function.new(G_LLVM_MODULE, funct_type, self.name)

        if function.name != self.name:
            function.delete()
            function = G_LLVM_MODULE.get_function_named(self.name)

        if not function.is_declaration:
            raise RuntimeError('Redefinition of function.')

        if len(function.args) != len(self.args):
            raise RuntimeError('Redeclaration of a function with different number of args.')

        for arg, arg_name in zip(function.args, self.args):
            arg.name = arg_name

        return function

    def CreateArgumentAllocas(self, function):
        for arg_name, arg in zip(self.args, function.args):
            alloca = create_entry_block_alloca(function, arg_name)
            G_LLVM_BUILDER.store(arg, alloca)
            G_NAMED_VALUES[arg_name] = alloca

class FunctionNode(object):
    def __init__(self, prototype, body):
        self.prototype = prototype
        self.body = body

    def CodeGen(self):
        G_NAMED_VALUES.clear()

        function = self.prototype.CodeGen()

        if self.prototype.IsBinaryOp():
            operator = self.prototype.GetOperatorName()
            G_BINOP_PRECEDENCE[operator] = self.prototype.precedence

        block = function.append_basic_block('entry')
        global G_LLVM_BUILDER
        G_LLVM_BUILDER = Builder.new(block)

        self.prototype.CreateArgumentAllocas(function)
        
        try:
            return_value = self.body.CodeGen()
            G_LLVM_BUILDER.ret(return_value)

            function.verify()

            G_LLVM_PASS_MANAGER.run(function)
        except:
            function.delete()
            if self.prototype.IsBinaryOp():
                del G_BINOP_PRECEDENCE[self.prototype.GetOperatorName()]
            raise

        return function

G_SYMBOL_TABLE = {}
SYMBOL_COUNT = 0
def intern(string):
    '''Try to find a new symbol in a symbol-table, if not, create new.'''
    sym = G_SYMBOL_TABLE.get(string, None)
    if sym is not None:
        return sym
    else:
        G_SYMBOL_TABLE[string] = Symbol(string, SYMBOL_COUNT)
        SYMBOL_COUNT += 1
        return G_SYMBOL_TABLE[string]

class Reader(object):
    '''On each iteration returns Lisp form'''
    def __init__(self, tokenizer):
        self.tokenizer = tokenizer
        self.NextToken()

    def NextToken(self):
        self.current = self.tokenizer.next()

    def __iter__(self):
        return self

    def ReadExpression(self):
        cur = self.current
        self.NextToken()
        if isinstance(cur, LBrToken):
            return self.ReadList()
        elif isinstance(cur, QuoteToken):
            return Cons(intern("quote"),
                        Cons(self.ReadExpression(),
                             None))
        elif isinstance(cur, NumberToken):
            return cur.value
        elif isinstance(cur, SymbolToken):
            return intern(cur.name)
        elif isinstance(cur, EOFToken):
            raise StopIteration
        else:
            raise RuntimeError('Got unknown type of token: %s'
                               % type(cur))

    def ReadList(self):
        if isinstance(self.current, RBrToken):
            self.NextToken()
            return None
        elif isinstance(self.current, EOFToken):
            raise RuntimeError('Got EOF while reading a list.')
        else:
            return Cons(self.ReadExpression(),
                        self.ReadList())

    def next(self):
        return self.ReadExpression()
        

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

    def ParseVarExpr(self):
        self.Next()

        variables = {}

        if not isinstance(self.current, IdentifierToken):
            raise RuntimeError('Expected identifier after "var".')

        while True:
            var_name = self.current.name
            self.Next()

            if self.current == CharacterToken('='):
                self.Next()
                variables[var_name] = self.ParseExpression()
            else:
                variables[var_name] = None

            if self.current != CharacterToken(','):
                break
            self.Next()

            if not isinstance(self.current, IdentifierToken):
                raise RuntimeError('Expected identifier after "," in a var expression.')

        if not isinstance(self.current, InToken):
            raise RuntimeError('Expected "in" keyword after "var".')
        self.Next()

        body = self.ParseExpression()

        return VarExpressionNode(variables, body)

    def ParsePrimary(self):
        if isinstance(self.current, IdentifierToken):
            return self.ParseIdentifierExpr()
        elif isinstance(self.current, NumberToken):
            return self.ParseNumberExpr()
        elif isinstance(self.current, IfToken):
            return self.ParseIfExpr()
        elif isinstance(self.current, VarToken):
            return self.ParseVarExpr()
        elif self.current == CharacterToken('('):
            return self.ParseParenExpr()
        else:
            raise RuntimeError('Unknown token when expecting an expression.')

    def GetCurrentTokenPrecedence(self):
        if isinstance(self.current, CharacterToken):
            return G_BINOP_PRECEDENCE.get(self.current.char, -1)
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
            result = G_LLVM_EXECUTOR.run_function(function, [])
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
    G_LLVM_PASS_MANAGER.add(G_LLVM_EXECUTOR.target_data)
    G_LLVM_PASS_MANAGER.add(PASS_PROMOTE_MEMORY_TO_REGISTER)
    G_LLVM_PASS_MANAGER.add(PASS_INSTRUCTION_COMBINING)
    G_LLVM_PASS_MANAGER.add(PASS_REASSOCIATE)
    G_LLVM_PASS_MANAGER.add(PASS_GVN)
    G_LLVM_PASS_MANAGER.add(PASS_CFG_SIMPLIFICATION)

    G_LLVM_PASS_MANAGER.initialize()

    G_BINOP_PRECEDENCE['='] = 2
    G_BINOP_PRECEDENCE['<'] = 10
    G_BINOP_PRECEDENCE['+'] = 20
    G_BINOP_PRECEDENCE['-'] = 20
    G_BINOP_PRECEDENCE['*'] = 40

    while True:
        print 'ready>',
        try:
            raw = raw_input()
        except (KeyboardInterrupt, EOFError):
            break

        parser = Parser(tokenize(raw))
        while True:
            if isinstance(parser.current, EOFToken):
                break
            if isinstance(parser.current, DefToken):
                parser.HandleDefinition()
            elif isinstance(parser.current, ExternToken):
                parser.HandleExtern()
            else:
                parser.HandleTopLevelExpression()
    print '\n', G_LLVM_MODULE

if __name__ == '__main__':
    main()
