#!/usr/bin/env python2
'''Basic McCarthy lisp written in LLVMPY. First stage of IBCL.'''

import re
import string
from llvm.core import Module, Constant, Type, Function, Builder
from llvm.ee import ExecutionEngine, TargetData
from llvm.passes import FunctionPassManager

import llvm.core
from llvm.core import FCMP_ULT, FCMP_ONE, ICMP_NE
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

llvm.core.load_library_permanently('/home/popolit/code/ibcl/putchard.so')
llvm.core.load_library_permanently('/home/popolit/code/ibcl/intern.so')
llvm.core.load_library_permanently('/home/popolit/code/ibcl/repr.so')
llvm.core.load_library_permanently('/home/popolit/code/ibcl/eq.so')

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

    def next(self):
        if self.cur_cons is None or self.cur_cons == intern("nil"):
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
        if self.cdr is None or self.cdr == intern("nil"):
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
        super(Symbol, self).__init__(name)
        self.number = number
    def __repr__(self):
        return self.name

class NumberToken(object):
    def __init__(self, value):
        self.value = value

class StringToken(object):
    def __init__(self, value):
        self.value = value

# Regular expressions that tokens and comments of our language.
REGEX_NUMBER = re.compile('[0-9]+(?:\.[0-9]+)?')
REGEX_SYMBOL = re.compile("[^()' ]+")
REGEX_COMMENT = re.compile(';.*')

def read_literal_string(string):
    res = ""
    is_escaped = False
    for i in range(1, len(string)):
        if is_escaped:
            res += string[i]
            is_escaped = False
        else:
            if string[i] == '"':
                return (res, i + 1)
            elif string[i] == '\\':
                is_escaped = True
            else:
                res += string[i]
    else:
        raise RuntimeError('Input finished while reading literal string.')

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
        elif string[0] == '"':
            (res, pos) = read_literal_string(string)
            yield StringToken(res)
            string = string[pos:]
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
    if isinstance(expr, int) or isinstance(expr, float):
        return Constant.real(Type.double(), expr)
    elif isinstance(expr, Symbol):
        return CallExpressionNode("intern",
                                  [StringExpressionNode(expr.name)]).CodeGen()
    elif expr is None:
        return CallExpressionNode("intern",
                                  [StringExpressionNode("nil")]).CodeGen()
    elif isinstance(expr, Cons):
        raise RuntimeError("Conses are not supported as data for now, sorry.")
    else:
        raise RuntimeError("Don't know how to codewalk following data type %s"
                           % type(expr))
                           


class QuoteExpressionNode(object):
    def __init__(self, expr):
        self.expr = expr
    def CodeGen(self):
        print "codegening quote node"
        return codegen_for_data(self.expr)

class PrognExpressionNode(object):
    def __init__(self, forms):
        self.forms = forms
    def CodeGen(self):
        print "codegening progn node"

        function = G_LLVM_BUILDER.basic_block.function

        progn_block = function.append_basic_block('progn')

        G_LLVM_BUILDER.branch(progn_block)

        G_LLVM_BUILDER.position_at_end(progn_block)
        for form in self.forms:
            value = form.CodeGen()

        return value

class NumberExpressionNode(ExpressionNode):
    def __init__(self, value):
        self.value = value
    def CodeGen(self):
        print "codegening number node"
        return Constant.real(Type.double(), self.value)

class StringExpressionNode(ExpressionNode):
    def __init__(self, value):
        self.value = value
    def CodeGen(self):
        print "codegening string node"
        k = Constant.stringz(self.value)
        # TODO: memory leak???
        ptr = G_LLVM_BUILDER.alloca(k.type)
        G_LLVM_BUILDER.store(k, ptr)
        return ptr


class VariableExpressionNode(ExpressionNode):
    def __init__(self, name):
        self.name = name

    def CodeGen(self):
        print "codegening variable node"
        if self.name in G_NAMED_VALUES:
            return G_LLVM_BUILDER.load(G_NAMED_VALUES[self.name], self.name)
        else:
            raise RuntimeError('Unknown variable name: ' + self.name)

class CallExpressionNode(ExpressionNode):
    def __init__(self, callee, args):
        self.callee = callee
        self.args = args

    def CodeGen(self):
        print "codegening call node"
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
        print "codegening if node"
        condition = self.condition.CodeGen()

        condition_bool = G_LLVM_BUILDER.icmp(
            ICMP_NE,
            condition,
            CallExpressionNode("intern",
                               [StringExpressionNode("nil")]).CodeGen(),
            'ifcond')

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
        phi = G_LLVM_BUILDER.phi(Type.pointer(Type.int(8)), 'ifmp')
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
        print "codegening for node"
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

class VarExpressionNode(ExpressionNode):
    def __init__(self, variables, body):
        self.variables = variables
        self.body = body

    def CodeGen(self):
        print "codegening let node"
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
        print "codegening prototype node"
        funct_type = Type.function(
            Type.pointer(Type.int(8)),
            [Type.pointer(Type.int(8))] * len(self.args), False)

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

    def CreateArgumentAllocas(self, function, old_bindings):
        for arg_name, arg in zip(self.args, function.args):
            alloca = create_entry_block_alloca(function, arg_name)
            G_LLVM_BUILDER.store(arg, alloca)
            G_NAMED_VALUES[arg_name] = alloca

    def RestoreArguments(self, old_bindings):
        for arg_name in self.args:
            if old_bindings[arg_name] is not None:
                G_NAMED_VALUES[var_name] = old_bindings[var_name]
            else:
                del G_NAMED_VALUES[var_name]

class FunctionNode(object):
    def __init__(self, prototype, body):
        self.prototype = prototype
        self.body = body

    def CodeGen(self):
        print "codegening function node"
        # G_NAMED_VALUES.clear()
        old_bindings = {}

        function = self.prototype.CodeGen()

        if self.prototype.IsBinaryOp():
            operator = self.prototype.GetOperatorName()
            G_BINOP_PRECEDENCE[operator] = self.prototype.precedence

        block = function.append_basic_block('entry')
        global G_LLVM_BUILDER
        G_LLVM_BUILDER = Builder.new(block)

        self.prototype.CreateArgumentAllocas(function, old_bindings)
        
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

        self.prototype.RestoreArguments(old_bindings)

        return function

G_SYMBOL_TABLE = {}
SYMBOL_COUNT = 0
def intern(string):
    '''Try to find a new symbol in a symbol-table, if not, create new.'''
    global SYMBOL_COUNT
    sym = G_SYMBOL_TABLE.get(string, None)
    if sym is not None:
        return sym
    else:
        G_SYMBOL_TABLE[string] = Symbol(string, SYMBOL_COUNT)
        SYMBOL_COUNT += 1
        return G_SYMBOL_TABLE[string]

def atom(obj):
    if obj is None:
        return intern("t")
    elif isinstance(obj, Symbol):
        return intern("t")
    else:
        return intern("nil")

def eq(obj1, obj2):
    if isinstance(obj1, Symbol) and isinstance(obj2, Symbol):
        if obj1 == obj2:
            return intern("t")
        else:
            return intern("nil")
    elif obj1 is None and obj2 is None:
        return intern("t")
    else:
        return intern("nil")

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
            return cur
        elif isinstance(cur, StringToken):
            return cur
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

def codewalk_atom(atom):
    if isinstance(atom, Symbol):
        if atom.name == "nil" or atom.name == "t":
            return codewalk(Cons(intern("intern"),
                                 Cons(StringToken(atom.name),
                                      None)))
        else:
            return VariableExpressionNode(atom.name)
    elif isinstance(atom, NumberToken):
        return NumberExpressionNode(atom.value)
    elif isinstance(atom, StringToken):
        return StringExpressionNode(atom.value)
    else:
        raise RuntimeError('Do not know how to codewalk this type of atom: %s'
                           % type(atom))

def codewalk_functoid(form):
    car_name = form.car.name
    args = [codewalk(x) for x in form.cdr]

    return CallExpressionNode(car_name, args)

def codewalk_if(form):
    '''It's really a shame that I need to represent falsity as 0.0'''
    condition = codewalk(form.car)
    then_branch = codewalk(form.cdr.car)
    if form.cdr.cdr is None:
        else_branch = codewalk(intern("nil"))
    else:
        else_branch = codewalk(form.cdr.cdr.car)

    return IfExpressionNode(condition, then_branch, else_branch)
    

def codewalk_let(forms):
    variables = {}

    for varspec in forms.car:
        if isinstance(varspec, Symbol):
            variables[varspec.name] = None
        elif isinstance(varspec, Cons):
            if isinstance(varspec.car, Symbol):
                variables[varspec.car.name] = codewalk(varspec.cdr.car)
            else:
                RuntimeError('Car of varspec should be a symbol')
        else:
            raise RuntimeError('Malformed let variable list')

    body = codewalk(Cons(intern("progn"),
                         forms.cdr))

    return VarExpressionNode(variables, body)

def codewalk_progn(forms):
    return PrognExpressionNode([codewalk(x) for x in forms])

def codewalk_prototype(name, args):
    if not isinstance(name, Symbol):
        raise RuntimeError("Name of a function should be a symbol")
    return PrototypeNode(name.name, [x.name for x in args])

def codewalk_definition(form):
    proto = codewalk_prototype(form.car, form.cdr.car)
    body = codewalk(Cons(intern("progn"),
                         form.cdr.cdr))
    return FunctionNode(proto, body)

def codewalk_extern(form):
    return codewalk_prototype(form.car, form.cdr.car)

def codewalk_for(form):
    '''(for (var from to [step]) &body body)'''
    varspec = form.car
    if not isinstance(varspec.car, Symbol):
        raise RuntimeError('Expected symbol at car of varspec.')

    loop_variable = varspec.car.name

    start = codewalk(varspec.cdr.car)

    end = codewalk(varspec.cdr.cdr.car)

    if varspec.cdr.cdr.cdr is not None:
        step = codewalk(varspec.cdr.cdr.cdr.car)
    else:
        step = None

    body = codewalk(Cons(intern("progn"),
                         form.cdr))

    return ForExpressionNode(loop_variable, start, end, step, body)

def codewalk_quote(forms):
    return QuoteExpressionNode(forms.car)

def codewalk_top_level_expr(form):
    proto = PrototypeNode('', [])
    return FunctionNode(proto, codewalk(Cons(intern("repr"),
                                             Cons(form, None))))
        
def codewalk(form):
    '''Generate AST from a cons-expression'''
    if isinstance(form, Cons):
        if not isinstance(form.car, Symbol):
            raise RuntimeError("Bad function call - %s in CAR position."
                               % form.car)
        else:
            if form.car == intern("if"):
                return codewalk_if(form.cdr)
            elif form.car == intern("let"):
                return codewalk_let(form.cdr)
            elif form.car == intern("progn"):
                return codewalk_progn(form.cdr)
            elif form.car == intern("defun"):
                return codewalk_definition(form.cdr)
            elif form.car == intern("extern"):
                return codewalk_extern(form.cdr)
            elif form.car == intern("quote"):
                return codewalk_quote(form.cdr)
            elif form.car == intern("for"):
                return codewalk_for(form.cdr)
            else: 
                return codewalk_functoid(form)
    else:
        return codewalk_atom(form)

def handle_expression(form):
    try:
        function = codewalk(form).CodeGen()
        print function
    except Exception,e:
        print 'Error:', e
    

def handle_top_level_expression(form):
    try:
        function = codewalk_top_level_expr(form).CodeGen()
        print function
        result = G_LLVM_EXECUTOR.run_function(function, [])
        print 'Evaluated to:', result.as_pointer()
    except Exception,e:
        print 'Error:', e

def handle_form(form):
    if isinstance(form, Cons):
        if form.car == intern("extern"):
            handle_expression(form)
        elif form.car == intern("defun"):
            handle_expression(form)
        else:
            handle_top_level_expression(form)
    else:
        handle_top_level_expression(form)

INIT = '''
(extern putchard (x))
(extern sin (x))
(extern cos (x))
(extern intern (str))
(extern repr (str))
(extern eq (x y))
'''

def init_runtime():
    # G_NAMED_VALUES["nil"] = intern("nil")
    # G_NAMED_VALUES["t"] = intern("t")

    for form in Reader(tokenize(INIT)):
        handle_form(form)

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

    init_runtime()

    while True:
        print 'ready>',
        try:
            raw = raw_input()
        except (KeyboardInterrupt, EOFError):
            break

        reader = Reader(tokenize(raw))
        for form in reader:
            handle_form(form)
    print '\n', G_LLVM_MODULE

if __name__ == '__main__':
    main()
