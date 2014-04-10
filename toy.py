#!/usr/bin/env python2

import re

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

class VariableExpressionNode(ExpressionNode):
    def __init__(self, name):
        self.name = name

class BinaryOperationExpressionNode(ExpressionNode):
    def __init__(self, operator, left, right):
        self.operator = operator
        self.left = left
        self.right = right

class CallExpressionNode(ExpressionNode):
    def __init__(self, callee, args):
        self.callee = callee
        self.args = args

class PrototypeNode(object):
    def __init__(self, name, args):
        self.name = name
        self.args = args

class FunctionNode(object):
    def __init__(self, prototype, body):
        self.prototype = prototype
        self.body = body

class Parser(object):
    def __init__(self, tokens, binop_precedence):
        self.tokens = tokens
        self.binop_precedence = binop_precedence
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
        elif self.current == CharacterToken('('):
            return self.ParseParenExpr()
        else:
            raise RuntimeError('Unknown token when expecting an expression.')

    def GetCurrentTokenPrecedence(self):
        if isinstance(self.current, CharacterToken):
            return self.binop_precedence.get(self.current.char, -1)
        else:
            return -1

    def ParseExpression(self):
        left = self.ParsePrimary()
        return self.ParseBinOpRHS(left, 0)

    def ParseBinOpRHS(self, left, left_precedence):
        while True:
            precedence = self.GetCurrentTokenPrecedence()

            if precedence < left_precedence:
                return left

            binary_operator = self.current.char
            self.Next()

            right = self.ParsePrimary()

            next_precedence = self.GetCurrentTokenPrecedence()
            if precedence < next_precedence:
                right = self.ParseBinOpRHS(right, precedence + 1)

            left = BinaryOperationExpressionNode(binary_operator, left, right)
    
    def ParsePrototype(self):
        # print self.current
        if not isinstance(self.current, IdentifierToken):
            raise RuntimeError('Expected function name in prototype.')

        function_name = self.current.name
        self.Next()

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

        return PrototypeNode(function_name, arg_names)

    def ParseDefinition(self):
        self.Next()
        proto = self.ParsePrototype()
        body = self.ParseExpression()
        return FunctionNode(proto, body)

    def ParseExtern(self):
        self.Next()
        return self.ParsePrototype()

    def ParseTopLevelExpr(self):
        proto = PrototypeNode('', [])
        return FunctionNode(proto, self.ParseExpression())

    def HandleDefinition(self):
        self.Handle(self.ParseDefinition, 'Parsed a function definition.')

    def HandleExtern(self):
        self.Handle(self.ParseExtern, 'Parsed an extern.')

    def HandleTopLevelExpression(self):
        self.Handle(self.ParseTopLevelExpr, 'Parsed a top-level expression.')

    def Handle(self, function, message):
        try:
            function()
            print message
        except Exception, e:
            print 'Error:', e
            try:
                self.Next()
            except:
                pass

def main():
    operator_precedence = {
        '<' : 10,
        '+' : 20,
        '-' : 20,
        '*' : 40
    }

    while True:
        print 'ready>',
        try:
            raw = raw_input()
        except (KeyboardInterrupt, EOFError):
            print
            return


        parser = Parser(Tokenize(raw), operator_precedence)
        while True:
            if isinstance(parser.current, EOFToken):
                break
            if isinstance(parser.current, DefToken):
                parser.HandleDefinition()
            elif isinstance(parser.current, ExternToken):
                parser.HandleExtern()
            else:
                parser.HandleTopLevelExpression()

if __name__ == '__main__':
    main()
