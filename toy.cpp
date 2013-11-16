#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <forward_list>
#include <unordered_map>
#include <map>
#include <string>
#include <vector>

enum Token {
  tok_eof = -1,

  tok_lb = -2, // #\(
  tok_rb = -3, // #\)

  tok_sym = -4,
  tok_inval = -6,

  // tok_quote = -4,
  // tok_atom = -5,
  // tok_eq = -6,
  // tok_car = -7,
  // tok_cdr = -8,
  // tok_cons = -9,
  // tok_cond = -10,
  // tok_lambda = -11,
  // tok_label = -12,
  // tok_setq = -13,
  // tok_defun = -14,

};

static std::string IdentifierStr;
static double NumVal;

static int gettok () {
  static int LastChar = ' ';

  while (isspace(LastChar) || LastChar == '\n')
    LastChar = getchar();

  if (isalpha(LastChar)) {
    IdentifierStr = LastChar;
    while (isalnum((LastChar = getchar())))
      IdentifierStr += LastChar;

    return tok_sym;
  }

  if (LastChar == '(') {
    LastChar = getchar();
    return tok_lb;
  }

  if (LastChar == ')') {
    LastChar = getchar();
    return tok_rb;
  }

  if (LastChar == EOF)
    return tok_eof;

  LastChar = getchar();
  return tok_inval;
}

class Form {
public:
  virtual ~Form() {}
  virtual void Print() = 0;
};

class Symbol : public Form {
  std::string Name;
public:
  Symbol(const std::string &name) : Name(name) {}
  virtual void Print();
};

class List : public Form {
  std::forward_list<Form *> Content;
public:
  List(std::forward_list<Form *> &content)
    : Content(content) {}
  virtual void Print();
};

void Symbol::Print() {
  fprintf(stderr, "%s", Name.c_str());
  return;
}

void List::Print() {
  for (auto it = Content.begin(); it != Content.end(); ++it) {
    (*it)->Print();
    fprintf(stderr, " ");
  }
  return;
}



class ExprAST {
 public:
  virtual ~ExprAST() {}
};

ExprAST *Error(const char *Str) { fprintf(stderr, "Error: %s\n", Str); return 0; }
Form *ErrorF(const char *Str) { Error(Str); return 0; }

static std::unordered_map<std::string, Symbol *> SymbolTable;

static Symbol *Intern(std::string name) {
  std::unordered_map<std::string, Symbol *>::const_iterator got = SymbolTable.find(name);

  if (got == SymbolTable.end()) {
    Symbol *new_sym = new Symbol(name);
    SymbolTable[name] = new_sym;
    Error("interned new symbol");
    return new_sym;
  } else {
    return got->second;
  } 
};

// class NumberExprAST : public ExprAST {
//   double Val;
//  public:
//  NumberExprAST(double val) : Val(val) {}
// };

// class VariableExprAST : public ExprAST {
//   std::string Name;
//  public:
//  VariableExprAST(const std::string &name) : Name(name) {}
// };

// class BinaryExprAST : public ExprAST {
//   char Op;
//   ExprAST *LHS, *RHS;
//  public:
//   BinaryExprAST(char op, ExprAST *lhs, ExprAST *rhs)
//     : Op(op), LHS(lhs), RHS(rhs) {}
// };

// class CallExprAST : public ExprAST {
//   std::string Callee;
//   std::vector<ExprAST*> Args;
//  public:
//   CallExprAST(const std::string &callee, std::vector<ExprAST*> &args)
//     : Callee(callee), Args(args) {}
// };

// class PrototypeAST {
//   std::string Name;
//   std::vector<std::string> Args;
//  public:
//   PrototypeAST(const std::string &name, const std::vector<std::string> &args)
//     : Name(name), Args(args) {}
// };

// class FunctionAST {
//   PrototypeAST *Proto;
//   ExprAST *Body;
//  public:
//   FunctionAST(PrototypeAST *proto, ExprAST *body)
//     : Proto(proto), Body(body) {}
// };

static int CurTok;
static int getNextToken() {
  return CurTok = gettok();
}

// PrototypeAST *ErrorP(const char *Str) { Error(Str); return 0; }

static ExprAST *ParseExpression ();

// static ExprAST *ParseNumberExpr() {
//   ExprAST *Result = new NumberExprAST(NumVal);
//   getNextToken();
//   return Result;
// }

static bool EvalContext = true;

// static ExprAST *ParseParenExpr(bool evalp) {
//   getNextToken();

//   if (evalp) {
//     // when evaling, we must dispatch on some CARs
//     switch (CurTok) {
//     default: ;// try to parse it as a funcall
//     case "quote": ;
//     case "atom": ;
//     case "eq": ;
//     case "car": ;
//     case "cdr": ;
//     case "cons": ;
//     case "cond": ;
//     }
//   };
//   tok_lambda = -11,
//   tok_label = -12,
//   tok_setq = -13,
//   tok_defun = -14,

//   ExprAST *V = ParseExpression(evalp);
//   if (!V) return 0;

//   if (CurTok != ')')
//     return Error("expected ')'");
//   getNextToken();
//   return V;
// }

// static ExprAST *ParseIdentifierExpr() {
//   std::string IdName = IdentifierStr;

//   getNextToken();
  
//   if (CurTok != '(')
//     return new VariableExprAST(IdName);

//   getNextToken();
//   std::vector<ExprAST*> Args;
//   if (CurTok != ')') {
//     while (1) {
//       ExprAST *Arg = ParseExpression();
//       if (!Arg) return 0;
//       Args.push_back(Arg);

//       if (CurTok == ')') break;

//       if (CurTok != ',')
//         return Error("Expected ')' or ',' in argument list");
//       getNextToken();
//     }
//   }

//   getNextToken();

//   return new CallExprAST(IdName, Args);
// }


// static ExprAST *ParsePrimary(bool evalp) {
//   switch (CurTok) {
//   default: return Error("unknown token when expecting an expression");
//   case tok_inval: return Error("invalid token");
//   case tok_identifier: return ParseIdentifierExpr(evalp);
//   case tok_lb: return ParseParenExpr(evalp);
//   case tok_rb: return Error("mismatched close parenthesis");
//     // case tok_number: return ParseNumberExpr();
//     // case '(': return ParseParenExpr();
//   }
// }

// static std::map<char, int> BinopPrecedence;

// static int GetTokPrecedence() {
//   if (!isascii(CurTok))
//     return -1;

//   int TokPrec = BinopPrecedence[CurTok];
//   if (TokPrec <= 0) return -1;
//   return TokPrec;
// }


// static ExprAST *ParseBinOpRHS(int ExprPrec, ExprAST *LHS) {
//   while (1) {
//     int TokPrec = GetTokPrecedence();

//     if (TokPrec < ExprPrec)
//       return LHS;

//     int BinOp = CurTok;
//     getNextToken();
    
//     ExprAST *RHS = ParsePrimary();
//     if (!RHS) return 0;

//     int NextPrec = GetTokPrecedence();
//     if (TokPrec < NextPrec) {
//       RHS = ParseBinOpRHS(TokPrec + 1, RHS);
//       if (RHS == 0) return 0;
//     }

//     LHS = new BinaryExprAST(BinOp, LHS, RHS);
//   }
// }

static Form *ReadList() {
  std::forward_list<Form *> res;

  switch (CurTok) {
  case tok_lb: getNextToken(); res.push_front(ReadList());
  case tok_rb: getNextToken(); res.reverse(); return new List(res);
  case tok_sym: res.push_front(Intern(IdentifierStr));
  case tok_inval: getNextToken(); return ErrorF("got invalid token, skipping.");
  default: return ErrorF("should not reached this clause while reading a list");
  };

  return ErrorF("should not be here!");
}

static Form *ReadForm() {
  switch (CurTok) {
  case tok_lb: getNextToken(); return ReadList();
  case tok_rb: return ErrorF("got closed parenthesis while expecting an atom");
  case tok_sym: return Intern(IdentifierStr);
  case tok_inval: getNextToken(); return ErrorF("got invalid token, skipping.");
  default: return ErrorF("should not reached this clause while reading a form");
  };
}

static ExprAST *ParseExpression () {
  Form *form = ReadForm();
  if (form) {
    form->Print();
    return 0;
    // return ParseForm(form, true);
  } else {
    return Error("unable to read form");
  }
}

// static PrototypeAST *ParsePrototype() {
//   if (CurTok != tok_identifier)
//     return ErrorP("Expected function name in prototype");

//   std::string FnName = IdentifierStr;
//   getNextToken();

//   if (CurTok != '(')
//     return ErrorP("Expected '(' in prototype");

//   std::vector<std::string> ArgNames;
//   while (getNextToken() == tok_identifier)
//     ArgNames.push_back(IdentifierStr);
//   if (CurTok != ')')
//     return ErrorP("Expected ')' in prototype");

//   getNextToken();

//   return new PrototypeAST(FnName, ArgNames);
// }

// static FunctionAST *ParseDefinition() {
//   getNextToken();
//   PrototypeAST *Proto = ParsePrototype();
//   if (Proto == 0) return 0;

//   if (ExprAST *E = ParseExpression())
//     return new FunctionAST(Proto, E);
//   return 0;
// }

// static PrototypeAST *ParseExtern() {
//   getNextToken();
//   return ParsePrototype();
// }

// static FunctionAST *ParseTopLevel() {
//   if (ExprAST *E = ParseExpression()) {
//     PrototypeAST *Proto = new PrototypeAST("", std::vector<std::string>());
//     return new FunctionAST(Proto, E);
//   }
//   return 0;
// }

// static void HandleDefinition() {
//   if (ParseDefinition()) {
//     fprintf(stderr, "Parsed a function definition.\n");
//   } else {
//     getNextToken();
//   }
// }

// static void HandleExtern() {
//   if (ParseExtern()) {
//     fprintf(stderr, "Parsed an extern\n");
//   } else {
//     getNextToken();
//   }
// }

static void HandleTopLevel() {
  if (ParseExpression()) {
    fprintf(stderr, "Parsed a top-level expr\n");
  } else {
    getNextToken();
  }
}

static std::string Prompt = "wisp> ";
// static std::string ContPrompt = "...> ";

// static bool NeedContPrompt = false;

static void MainLoop() {
  while (1) {
    fprintf(stderr, "%s", Prompt.c_str());

    switch (CurTok) {
    case tok_eof: return;
    default: HandleTopLevel(); break;
    }
  }
}

int main () {
  fprintf(stderr, "%s", Prompt.c_str());
  getNextToken();
  
  MainLoop();
  
  return 0;
}


