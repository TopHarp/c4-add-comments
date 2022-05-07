// c4.c - C in four functions

// char, int, and pointer types
// if, while, return, and expression statements
// just enough features to allow self-compilation and a bit more

// Written by Robert Swierczek

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <fcntl.h>
#define int long long

char *p, *lp, // current position in source code // gyxu lp每行起始位置
     *data;   // data/bss pointer    // gyxu 20220415 是否是 bss段Block Started by Symbol 存放程序中未初始化全局变量的一块内存区域 静态区域 程序一开始就将其清零

int *e, *le,  // current position in emitted code     // gyxu 代码段 内存指针  已经是汇编语言了的代码
    *id,      // currently parsed identifier
    *sym,     // symbol table (simple list of identifiers)
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line,     // current line number
    src,      // print source and assembly flag
    debug;    // print executed instructions

// gyxu 由于没有实现struct/union 自身代码避免使用之 用数组代替结构体 用枚举来做符号表的地址跳转
// gyxu id总是指向sym全局变量为开头的int数组
// gyux id为int型指针 int *id 和 int id[] 区别是什么

// gyxu C4所支持的输入源码的字符集限定在7-bit ASCII上，所以每个输入的字符只可能在[0, 127]的闭区间范围内
// gyxu 于是所有单词（token）只有一个字符的 例如括号 花括号 乘号 用他们的ascii值直接表示即可作为实际token类别
// gyxu 对于多于一个字符的 如数字字面量 关键字 或需要区分一个或多个字符的 例如加号或自增 则用大于ascii码范围的数字表示其token类别
// gyxu 所以这个枚举从128开始
// gyxu 
// gyxu 其实从0到127，加上这Num（128）到Brak（164）的范围都可以表示token类别
// gyxu 
// gyxu 
// gyxu 标记 标记顺序与优先级有关 Mul优先级高于Add
// gyxu Num常量  Fun函数 Sys系统调用 Glo全局变量 Loc局部变量
// gyxu Assign  =  Cond    ?   Lor     ||  Lan     &&   Or      |    Xor     ^   And     &   Eq      ==
// gyxu Ne      !=   Lt      <   Gt      >   Le      <=   Ge      >=   Shl     <<  Shr     >>  Add     +
// gyxu Sub     -    Mul     *   Div     /   Mod     %    Inc     ++   Dec     --  Brak    [     */
// tokens and classes (operators last and in precedence order)
enum {
  Num = 128, Fun, Sys, Glo, Loc, Id,
  Char, Else, Enum, If, Int, Return, Sizeof, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

// gyxu 指令集 基于x86？ 顺序为了打印调试信息更方便 带参指令在前？
// gyxu MOV指令Inter风格 MOV dest source 虚拟机只有一个寄存器a 因此将Mov拆分为5个指令
// gyxu IMM <num> 将num放入寄存器a中
// gyxu LC 将a中存储地址中的字符载入 ax 中，要求 ax 中存放地址。
// gyxu LI 将a中存储地址中的整数载入 ax 中，要求 ax 中存放地址。
// gyxu SC 将 ax 中的数据作为字符存放入地址中，要求栈顶存放地址。
// gyxu SI 将 ax 中的数据作为整数存放入地址中，要求栈顶存放地址。
// gyxu JSR 跳转到子函数
// gyxu LEA 基址+pc当前位置存的数值 放入a  为了获取函数参数 我们只能操作a寄存器 所以定义了LEA
// gyxu JMP 无条件pc指向pc中存的地址 pc是指向下一条指令的地址 因此该地址里存的就是需要跳转到的目的地址
// gyxu BZ  a不为0 下一个指令 a为0 跳转到pc地址中存的地址数值
// gyxu BNZ a为0 下一个指令 a不为0 跳转到pc地址中存的地址数值
// gyxu ENT 进入子函数 bp基址指针 : 基址bp值入栈; sp当前值入bp; 栈sp进入pc指令地址处;pc内容是什么？栈减少若干个，入栈了若干个字节？保留一定的栈空间！
// gyxu ADJ pc所指地址值赋给sp栈 是将压入栈的数据清除
// gyxu 函数返回值：约定如果有返回值 返回值放在a中；
// gyxu 函数参数，各种语言约定不同
// gyxu C语言约定：1由调用者将参数入栈 2调用结束时由调用者出栈 3参数逆序入栈
// gyxu LEV 从子函数中返回：基址寄存器赋回给sp 栈顶地址存放的值是之前的bp，再存回bp中；ret？栈顶存的内容是pc
// gyxu PUSH 将a入栈
// opcodes
enum { LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
       OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT };

// types
enum { CHAR, INT, PTR };


// gyxu attribute属性 表示某个程序构造相关的任意的量 比如表达式的数据类型、生成代码的指令数目 或为某个构造生成的代码中第一条指令的位置

// gyxu 标识符结构体 符号表sym int型数组
// gyxu Name 指向的是这个identifier标识符的Name
// gyxu Type 为数据类型(比如返回值类型),如CHAR,INT,INT+PTR
// gyxu Class 为类型,如Num(常量数值),Fun(函数),Sys(系统调用),Glo全局变量,Loc 局部变量
// identifier offsets (since we can't create an ident struct)
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };


// gyxu 用于词法分析，获取下一个标记token，自动忽略字符串
// gyxu 词法分析源码以字符串输入，输出为标记流，标记包含(token, token value)
// gyxu 2 + 3 * (4 - 5)
// gyxu =>
// gyxu (Number, 2) Add (Number, 3) Multiply Left-Bracket (Number, 4) Subtract (Number, 5) Right-Bracket
// gyxu                    +-------+                      +--------+
// gyxu -- source code --> | lexer | --> token stream --> | parser | --> assembly
// gyxu                    +-------+                      +--------+
// gyxu 词法分析工具 lex flex
// gyxu 不一次性将所有代码全部转换成标记，1.标记与上下文有关，是有状态的，2.保存所有标记浪费空间没有意义。实际是提供一个函数，调用函数返回下一个标记
// gyxu 返回由tk带出  ival带出数字数值
void next()
{
  char *pp;

  // gyxu 用while跳过空格(ascii 32) 跳过不认识的符号 while到有认识的字符才处理并return     while(exp) 先赋值 后判断左值
  while (tk = *p) {
    ++p;
    // gyxu====== 换行
    if (tk == '\n') {
      if (src) {      // gyxu 编译器选项 标志位 是否打印代码和汇编
        printf("%d: %.*s", line, p - lp, lp);
        lp = p;
        while (le < e) {
          printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                           "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                           "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[*++le * 5]);
          if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n");
        }
      }
      ++line;   // gyxu 换行符 行号加一
    }
    // gyxu====== 不支持宏定义 跳过
    else if (tk == '#') {
      while (*p != 0 && *p != '\n') ++p;
    }
    // gyxu====== 标识符 首字母以大小写字母 数字 下划线开始 包括进了关键字
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') {
      pp = p - 1;     // gyxu 记录初始位置
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
        tk = tk * 147 + *p++;         // gyxu 计算hash 向后读取字节
      tk = (tk << 6) + (p - pp);      // gyxu p-pp 变量名字符数   上一行while包括本行 目的是计算hash
      id = sym;    // gyxu 从符号表开始位置遍历一遍
      while (id[Tk]) {    // gyxu 查看符号表里是否已经存在该变量  如果已存在 返回变量tk sym未赋值的表id[Tk]是0
        if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { tk = id[Tk]; return; }
        id = id + Idsz;   // gyxu 步进到下一个 Idsz是enum最后一个 如果未找到已存在变量 找到sym表的最后一个为止
      }
      id[Name] = (int)pp;   // gyxu 标志符起始位置
      id[Hash] = tk;
      tk = id[Tk] = Id;     // gyxu Id在enum中值是133 Id是类型  待定20220414  tk返回有其他用途 所以被赋值 最后tk返回133 Id
      return;
    }
    // gyxu====== 数字 支持0xff十六进制数据
    else if (tk >= '0' && tk <= '9') {
      // gyxu 十进制
      if (ival = tk - '0') { while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; }
      // gyxu 十六进制
      else if (*p == 'x' || *p == 'X') {
        while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
          ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
      }
      // gyxu 八进制
      else { while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0'; }
      tk = Num;
      return;
    }
    // gyxu======  注释或除法
    else if (tk == '/') {
      if (*p == '/') {
        ++p;
        while (*p != 0 && *p != '\n') ++p;
      }
      else {
        tk = Div;
        return;
      }
    }
    // gyxu====== 字符串 反斜杠转义字符 '  或 "    静态区 代码中定义的字符串
    else if (tk == '\'' || tk == '"') {
      pp = data;
      while (*p != 0 && *p != tk) {     // gyxu tk中目前放的是 ' 或 "
        if ((ival = *p++) == '\\') {    // gyxu  表示反斜杠
          if ((ival = *p++) == 'n') ival = '\n';
        }
        if (tk == '"') *data++ = ival;   // gyxu 将p中字符串装入data区   ++优先级高于*
      }
      ++p;
      if (tk == '"') ival = (int)pp; else tk = Num;   // gyxu ival返回本次data起始位置 如果只是单个字符 返回Num类
      return;
    }
    // gyxu====== p已经++  tk第一个字符 p第二个字符
    else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; }
    else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; }
    else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; }
    else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; }
    else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; }
    else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; }
    else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; }
    else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; }
    else if (tk == '^') { tk = Xor; return; }
    else if (tk == '%') { tk = Mod; return; }
    else if (tk == '*') { tk = Mul; return; }
    else if (tk == '[') { tk = Brak; return; }
    else if (tk == '?') { tk = Cond; return; }
    // gyxu====== 这些符号本身就构成了标记 他们不涉及优先级关系
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return;
  }
}

// gyxu 表达式分析 编译原理 expr代表表达式 非终结符  stmt语句
void expr(int lev)
{
  int t, *d;

  // gyxu === tk 0 
  if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); }
  // gyxu === Num       IMM将数值放入寄存器a
  else if (tk == Num) { *++e = IMM; *++e = ival; next(); ty = INT; }   // gyxu ty 缓存数据类型
  // gyxu === "
  else if (tk == '"') {
    *++e = IMM; *++e = ival; next();
    while (tk == '"') next();   // gyxu 连续" 处理 多个字符串 都保存到data "aaa" "bbb"
    // gyxu 追加字符串结尾 '\0'，所有的data默认是0，所以只需要将data向前移动一个位置即可 append the end of string character '\0',all the data are default to 0, so just move data one position forward.
    // gyxu data = (char *)(((int)data + sizeof(int)) & (-sizeof(int)))
    data = (char *)((int)data + sizeof(int) & -sizeof(int)); ty = PTR;  // gyxu 符号优先级 先正负号 然后加法+ 后按位与&
  }
  // gyxu === sizeof
  else if (tk == Sizeof) {
    next(); if (tk == '(') next(); else { printf("%d: open paren expected in sizeof\n", line); exit(-1); }
    // gyxu 读sizeof内容 int char
    ty = INT; if (tk == Int) next(); else if (tk == Char) { next(); ty = CHAR; }
    // gyxu ptr
    while (tk == Mul) { next(); ty = ty + PTR; }
    if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); }
    *++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);
    ty = INT;
  }
  // gyxu === Id
  else if (tk == Id) {
    d = id; next();   // gyxu next读取到Id 当前符号表id=d
    // gyxu 如果是函数fun 第一个读到符号表 第二个读到括号 是函数 
    if (tk == '(') {
      next();
      t = 0;   // gyxu t 形参数量
      // gyxu fun(expr) 递归expr PSH入栈 形参数量++
      while (tk != ')') { expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); }
      next();
      if (d[Class] == Sys) *++e = d[Val];  // gyxu 是系统调用
      else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; }  // gyxu 是函数 JSR跳入子函数
      else { printf("%d: bad function call\n", line); exit(-1); }
      if (t) { *++e = ADJ; *++e = t; }      // gyxu ADJ pc所指地址值赋给sp栈 是将压入栈的数据清除
      ty = d[Type];
    }
    // gyxu 是常量 将常量值放入立即数寄存器
    else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; }
    // gyxu 其他情况
    else {
      if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; }  // gyxu id是局部变量 LEA 将当前位置数据放入a
      else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; }    // gyxu id是全局变量 将全局变量值放入寄存器啊 
      else { printf("%d: undefined variable\n", line); exit(-1); }
      *++e = ((ty = d[Type]) == CHAR) ? LC : LI;      // gyxu LC/LI将寄存器a中存放地址中的数据内容放入a中
    }
  }
  else if (tk == '(') {
    next();
    if (tk == Int || tk == Char) {
      t = (tk == Int) ? INT : CHAR; next();
      while (tk == Mul) { next(); t = t + PTR; }
      if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }
      expr(Inc);
      ty = t;
    }
    else {
      expr(Assign);
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    }
  }
  else if (tk == Mul) {
    next(); expr(Inc);
    if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); }
    *++e = (ty == CHAR) ? LC : LI;
  }
  else if (tk == And) {
    next(); expr(Inc);
    if (*e == LC || *e == LI) --e; else { printf("%d: bad address-of\n", line); exit(-1); }
    ty = ty + PTR;
  }
  else if (tk == '!') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; }
  else if (tk == '~') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; }
  else if (tk == Add) { next(); expr(Inc); ty = INT; }
  else if (tk == Sub) {
    next(); *++e = IMM;
    if (tk == Num) { *++e = -ival; next(); } else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }
    ty = INT;
  }
  else if (tk == Inc || tk == Dec) {
    t = tk; next(); expr(Inc);
    if (*e == LC) { *e = PSH; *++e = LC; }
    else if (*e == LI) { *e = PSH; *++e = LI; }
    else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
    *++e = PSH;
    *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
    *++e = (t == Inc) ? ADD : SUB;
    *++e = (ty == CHAR) ? SC : SI;
  }
  else { printf("%d: bad expression\n", line); exit(-1); }

  while (tk >= lev) { // "precedence climbing" or "Top Down Operator Precedence" method
    t = ty;
    if (tk == Assign) {
      next();
      if (*e == LC || *e == LI) *e = PSH; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
      expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
    }
    else if (tk == Cond) {
      next();
      *++e = BZ; d = ++e;
      expr(Assign);
      if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
      *d = (int)(e + 3); *++e = JMP; d = ++e;
      expr(Cond);
      *d = (int)(e + 1);
    }
    else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (int)(e + 1); ty = INT; }
    else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (int)(e + 1); ty = INT; }
    else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; }
    else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; }
    else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; }
    else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; }
    else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; }
    else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; }
    else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; }
    else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; }
    else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; }
    else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; }
    else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; }
    else if (tk == Add) {
      next(); *++e = PSH; expr(Mul);
      if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
      *++e = ADD;
    }
    else if (tk == Sub) {
      next(); *++e = PSH; expr(Mul);
      if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV; ty = INT; }
      else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB; }
      else *++e = SUB;
    }
    else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }
    else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }
    else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }
    else if (tk == Inc || tk == Dec) {
      if (*e == LC) { *e = PSH; *++e = LC; }
      else if (*e == LI) { *e = PSH; *++e = LI; }
      else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? ADD : SUB;
      *++e = (ty == CHAR) ? SC : SI;
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? SUB : ADD;
      next();
    }
    else if (tk == Brak) {
      next(); *++e = PSH; expr(Assign);
      if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
      if (t > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
      else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }
      *++e = ADD;
      *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
    }
    else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); }
  }
}


// gyxu 语法分析 分析函数除声明之外的部分  语句分析statement  stmt表示语句 expr代表表达式
void stmt()
{
  int *a, *b;

  // gyxu === if()
  if (tk == If) {
    next();
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e;       // gyxu BZ a不为0跳转到下一个指令 为0跳转到pc地址中保存的地址
    stmt();
    if (tk == Else) {
      *b = (int)(e + 3); *++e = JMP; b = ++e;
      next();
      stmt();
    }
    *b = (int)(e + 1);
  }
  // gyxu === while()
  else if (tk == While) {
    next();
    a = e + 1;
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign);
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e;
    stmt();
    *++e = JMP; *++e = (int)a;
    *b = (int)(e + 1);
  }
  // gyxu === return
  else if (tk == Return) {
    next();
    if (tk != ';') expr(Assign);
    *++e = LEV;
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }  // gyxu 需要分号
  }
  // gyxu === {
  else if (tk == '{') {
    next();
    while (tk != '}') stmt();
    next();
  }
  // gyxu === ; 空语句
  else if (tk == ';') {
    next();
  }
  // gyxu === 
  else {
    expr(Assign);
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
}

// gyxu main()的后半部分实现虚拟机 它的编译器部分不直接生成任何实际的机器汇编代码 而是在内存生成c4自己的虚拟机指令
int main(int argc, char **argv)
{
  // gyxu idmain符号表“main” poolsz虚拟机各个存储区大小  bt basetype ty存储刚刚读到的数据类型char int或ptr 
  int fd, bt, ty, poolsz, *idmain;

  // gyxu 虚拟机
  // gyxu a累加器 为栈顶缓存
  // gyxu pc 程序计数器 下一条要执行指令的地址
  // gyxu sp 指针寄存器 指向栈顶 通常栈由高地址向低地址生长时 入栈时sp减小
  // gyxu bp 基址指针 指向栈的某一位置
  // gyxu a 通用寄存器 存结果？
  int *pc, *sp, *bp, a, cycle; // vm registers
  int i, *t; // temps

  // gyxu 编译命令 -s src -d debug
  --argc; ++argv;
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; }
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; }
  if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; }

  // gyxu 无法打开源文件
  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }

  // gyxu 初始化虚拟机内存
  poolsz = 256*1024; // arbitrary size
  // gyxu 符号区 作用未知20220407 存放未初始化数据？ 是存放符号表20220411 数据结构为identifier offsets
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }
  // gyxu 代码段  le for dump text segment
  if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }
  // gyxu 数据区 数据段
  if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }
  // gyxu 栈区
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; }

  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);

  // gyxu 在词法分析前将关键字加入符号表
  p = "char else enum if int return sizeof while "
      "open read close printf malloc free memset memcmp exit void main";
  // gyxu next()中 sym被赋给id p此时指向的内容为此处上一行字符串 所以sym表表头是关键字char else enum if int return sizeof while
  i = Char; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table
  // gyxu 添加外部函数库到符号表
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // add library to symbol table
  // gyxu 接着是默认符号
  next(); id[Tk] = Char; // handle void type
  // gyxu 符号表“main”
  next(); idmain = id; // keep track of main

  // gyxu p源码文件开辟读取空间 读取源码
  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; }
  p[i] = 0;
  close(fd);

  // gyxu 解析声明 全局变量声明 此编译器不支持#include 源码开头应该是全局变量或函数定义
  // parse declarations
  line = 1;     // gyxu line在next()中增加
  next();       // gyxu 读取源码第一个字节
  // gyxu 把所有源码检查一遍？
  while (tk) {
    bt = INT; // basetype
    // gyxu 读声明 包括Int Char Enum变量  fun函数  Glo全局变量
    if (tk == Int) next();      // gyxu 关键字int类型 再next一个
    else if (tk == Char) { next(); bt = CHAR; }    // gyux 是int或char 再next一个 并且bt赋为char
    else if (tk == Enum) {            // gyxu 枚举 将enum读入id符号表中
      next();
      if (tk != '{') next();
      if (tk == '{') {
        next();
        i = 0;        // gyxu 初始化成0
        while (tk != '}') {
          if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; }
          next();
          if (tk == Assign) {     // gyxu assign 赋值  如果枚举有赋值
            next();
            if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; }
            i = ival;     // gyxu 给枚举赋值
            next();
          }
          id[Class] = Num; id[Type] = INT; id[Val] = i++;    // gyxu 写入符号表  i++ i是枚举的值 枚举的Class设置成Num常量类
          if (tk == ',') next();
        }
        next();
      }
    }
    // gyxu 除int char enum之外
    while (tk != ';' && tk != '}') {
      ty = bt;
      // gyxu 乘法* 指针 枚举PTR为2 while处理多级指针  ty存储刚刚读到的数据类型char int或ptr 
      while (tk == Mul) { next(); ty = ty + PTR; }
      // gyxu 有类型却无具体符号 语法错误 退出
      if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; }
      // gyxu 符号表中已经存在了 重复定义   next()中id已经存在 id指针会返回 id的Class已经存在值了 默认初始化为0
      if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; }
      // gyxu 继续读一个 再读一个 看是不是括号 如果是括号 就是函数 否则就是全局变量
      next();
      id[Type] = ty;    // gyxu 刚才读取到并缓存的数据类型  为什么再读一个才给id赋值   赋值类型
      // gyxu 函数 tk是括号 是函数
      if (tk == '(') { // function
        id[Class] = Fun;
        id[Val] = (int)(e + 1);   // gyxu e代码段地址 函数地址  此处应该是代码的第一个函数main
        next(); i = 0;            // gyxu 继续读一个 i初始化0
        // gyxu 读函数参数 和读全局变量一样 先读数据类型 指针 在读符号
        while (tk != ')') {
          ty = INT;
          if (tk == Int) next();
          else if (tk == Char) { next(); ty = CHAR; }
          while (tk == Mul) { next(); ty = ty + PTR; }
          if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; }
          if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; }
          id[HClass] = id[Class]; id[Class] = Loc;    // gyxu HClass 备份符号信息 进入上下文有用？ 最后跳出函数后unwind
          id[HType]  = id[Type];  id[Type] = ty;
          id[HVal]   = id[Val];   id[Val] = i++;      // gyxu 参数序号 归入局部变量 编号
          next();
          if (tk == ',') next();
        }
        next();
        // gyxu 读函数体内容
        if (tk != '{') { printf("%d: bad function definition\n", line); return -1; }
        loc = ++i;   // gyxu loc是函数参数数量？  上一行i记录函数参数序号  局部变量偏移  函数参数数量
        next();
        // gyxu 函数体内 处理Int Char 不处理enum fun 似乎不处理声明且赋值 
        while (tk == Int || tk == Char) {
          bt = (tk == Int) ? INT : CHAR;
          next();
          // gyxu 处理本行 直到;
          while (tk != ';') {
            ty = bt;
            while (tk == Mul) { next(); ty = ty + PTR; }
            if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }
            // gyxu 此处next如已经找到同名变量且是Loc局部变量 重定义并return
            if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; }
            // gyxu 此处如果next找到同名变量 则局部变量覆盖全局变量 用完后再unwind解绑变量
            id[HClass] = id[Class]; id[Class] = Loc;
            id[HType]  = id[Type];  id[Type] = ty;
            id[HVal]   = id[Val];   id[Val] = ++i;      // gyxu 函数体内局部变量序号 + 函数参数序号
            next();
            if (tk == ',') next();
          }
          next();
        }
        // gyxu 进入函数体内语句分析
        *++e = ENT; *++e = i - loc;   // gyxu 函数体内局部变量个数  总个数-函数参数个数  e指针指向的是汇编后的汇编代码  ENT进入子函数指令
        while (tk != '}') stmt();     // gyxu 句法分析
        // gyxu 离开子函数
        *++e = LEV;
        // gyxu unwind松开符号表局部变量覆盖的全局变量
        id = sym; // unwind symbol table locals
        while (id[Tk]) {
          if (id[Class] == Loc) {
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }
          id = id + Idsz;     // gyxu 符号表跳转到下一个数据结构
        }
      }
      else {
        id[Class] = Glo;    // gyxu Glo 全局变量
        id[Val] = (int)data;
        data = data + sizeof(int);
      }
      if (tk == ',') next();
    }
    next();
  }

  // gyxu pc程序计数器 初始指向被编译代码的main
  if (!(pc = (int *)idmain[Val])) { printf("main() not defined\n"); return -1; }
  if (src) return 0;     // gyxu 编译器选项 标志位 是否打印代码和汇编

  // gyxu bp 基址寄存器
  // setup stack
  bp = sp = (int *)((int)sp + poolsz);   // gyxu bp堆栈基址位置 压栈
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; t = sp;
  *--sp = argc;
  *--sp = (int)argv;
  *--sp = (int)t;

  // gyxu 虚拟机通过解释器方式实现
  // run...
  cycle = 0;
  while (1) {
    i = *pc++; ++cycle;
    if (debug) {
      printf("%d> %.4s", cycle,
        &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
         "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[i * 5]);
      if (i <= ADJ) printf(" %d\n", *pc); else printf("\n");
    }

    // gyxu 指令集 基于x86？ 顺序为了打印调试信息更方便 带参指令在前？
    // gyxu MOV指令Inter风格 MOV dest source 虚拟机只有一个寄存器a 因此将Mov拆分为5个指令：IMM LI LC SI SC

    // gyxu 基址+pc当前位置存的数值 放入a  为了获取函数参数 我们只能操作a寄存器 所以定义了LEA
    if      (i == LEA) a = (int)(bp + *pc++);                             // load local address
    // gyxu IMM <num> 将num放入寄存器a中
    else if (i == IMM) a = *pc++;                                         // load global address or immediate
    // gyxu JMP 无条件pc指向pc中存的地址 pc是指向下一条指令的地址 因此该地址里存的就是需要跳转到的目的地址
    else if (i == JMP) pc = (int *)*pc;                                   // jump
    // gyxu 跳转到子函数  下一条指令入栈
    else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; }        // jump to subroutine
    // gyxu a不为0 下一个指令 a为0 跳转到pc地址中存的地址数值
    else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc;                      // branch if zero
    // gyxu a为0 下一个指令 a不为0 跳转到pc地址中存的地址数值
    else if (i == BNZ) pc = a ? (int *)*pc : pc + 1;                      // branch if not zero
    // gyxu 进入子函数 bp基址指针 : 基址bp值入栈; sp当前值入bp; 栈sp进入pc指令地址处;pc内容是什么？栈减少若干个，入栈了若干个字节？保留一定的栈空间！
    else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; }     // enter subroutine
    // gyxu pc所指地址值赋给sp栈 是将压入栈的数据清除
    else if (i == ADJ) sp = sp + *pc++;                                   // stack adjust
    // gyxu 函数返回值：约定如果有返回值 返回值放在a中；
    // gyxu 函数参数，各种语言约定不同
    // gyxu C语言约定：1由调用者将参数入栈 2调用结束时由调用者出栈 3参数逆序入栈
    // gyxu 从子函数中返回：基址寄存器赋回给sp 栈顶地址存放的值是之前的bp，再存回bp中；ret？栈顶存的内容是pc
    else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // leave subroutine
    // gyxu LI 将对应地址中的整数载入 ax 中，要求 ax 中存放地址。
    else if (i == LI)  a = *(int *)a;                                     // load int
    // gyxu LC 将对应地址中的字符载入 ax 中，要求 ax 中存放地址。
    else if (i == LC)  a = *(char *)a;                                    // load char
    // gyxu SI 将 ax 中的数据作为整数存放入地址中，要求栈顶存放地址。
    else if (i == SI)  *(int *)*sp++ = a;                                 // store int
    // gyxu SC 将 ax 中的数据作为字符存放入地址中，要求栈顶存放地址。
    else if (i == SC)  a = *(char *)*sp++ = a;                            // store char
    // gyxu PUSH 将a入栈
    else if (i == PSH) *--sp = a;                                         // push

    // gyxu 运算符指令 两参数 一个放栈顶 一个放a 结果放a
    else if (i == OR)  a = *sp++ |  a;
    else if (i == XOR) a = *sp++ ^  a;
    else if (i == AND) a = *sp++ &  a;
    else if (i == EQ)  a = *sp++ == a;
    else if (i == NE)  a = *sp++ != a;
    else if (i == LT)  a = *sp++ <  a;
    else if (i == GT)  a = *sp++ >  a;
    else if (i == LE)  a = *sp++ <= a;
    else if (i == GE)  a = *sp++ >= a;
    else if (i == SHL) a = *sp++ << a;
    else if (i == SHR) a = *sp++ >> a;
    else if (i == ADD) a = *sp++ +  a;
    else if (i == SUB) a = *sp++ -  a;
    else if (i == MUL) a = *sp++ *  a;
    else if (i == DIV) a = *sp++ /  a;
    else if (i == MOD) a = *sp++ %  a;

    // gyxu 外部库函数

    else if (i == OPEN) a = open((char *)sp[1], *sp);
    else if (i == READ) a = read(sp[2], (char *)sp[1], *sp);
    else if (i == CLOS) a = close(*sp);
    else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); }
    else if (i == MALC) a = (int)malloc(*sp);
    else if (i == FREE) free((void *)*sp);
    else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp);
    else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp);
    else if (i == EXIT) { printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp; }
    else { printf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; }
  }
}





// /*
// C4致力于用最少的代码，实现一个可以自举的C编译器。它的整个实现只有4个函数组成，可想而知，它不可能完整的实现整个C语言的规范，它只实现了C语言的一个子集。

// 数据类型

//     char
//     int
//     指针
//     枚举（enum）
//     数组
//     字符串

// 不支持struct、typedef、union等数据类型。

// 语句结构

//     if-else控制语句
//     while循环语句
//     return语句
//     函数

// 不支持do-while、switch-case、for、continue、break、goto等语句结构。

// 运算符

// 它支持除+=、%=、<<=、&=等符合运算符之外的几乎所有运算符。包括：

//     算术运算符
//     关系运算符
//     逻辑运算符
//     位运算符
//     赋值运算符
//     杂项运算符（如三元运算符?:）

// 内建库函数

// C4编译器实现时用到了一些系统库函数，因此，为了实现自举，它也内建支持了几个库函数。包括：

// open、read、close、printf、malloc、free、memset、memcmp、exit

// 需要注意的是，它不支持以#开头的预处理命令，如#include、#define、#if等。

// 代码注释只支持“//”开头的单行注释，不支持“/* */”标记的多行注释形式。

// 与传统的C语言编译器相比，C4在实现上有其独到之处。

// 下面，先简单介绍一些传统编译器的实现过程。
// 传统典型编译器的实现

// 典型的编译器的实现，一般都会有下面几个过程：

//     词法分析
//     语法分析
//     语义分析
//     中间代码生成
//     代码优化
//     机器代码生成

// 如GCC、Clang、华为方舟编译器等均是如此。

// 这些阶段，会对代码进行多次扫描。这里的代码，包括文本形式的源代码、语法树、中间代码等表示形式。

// 典型的实现中，词法分析和语法分析通常会糅合在一起，在语法分析时，调用词法分析器逐个取得token。

// 因此，理论上讲，词法分析和语法分析阶段，只需要对源码扫描一遍即可，并生成语法树，有时也叫抽象语法树（Abstract Syntax Tree）。

// 语义分析阶段操作的主要对象就是这棵树，至少要对这棵树扫描一遍。有些实现中，在进行语义检查的同时也会直接生成中间代码。

// 在代码优化阶段，根据编译器优化的力度的不同，可能会对中间代码进行多次扫描。

// 这里所谓的“扫描一遍”，在编译器术语中一般称为pass。对LLVM有了解的朋友应该知道，LLVM中每一种类型的优化都是一个pass，要应用多种优化技术，就需要有多个pass。
// C4的独具特色之处

// 作为追求极简主义的C4编译器来说，它在实现上有很多独具特色之处。

// 对C源码解释执行

// 传统的C语言编译器，最终都把C语言源码编译成可执行文件，也就是二进制的机器码。

// 而C4则是把C语言源码先编译成其专门设计的字节码（bytecode），然后直接在虚拟机中解释执行。

// C4设计了39个字节码指令，其中大部分与汇编语言中的指令有些类似，主要是内存加载指令，算术运算指令等，此外，还包含了为支持内建的库函数而专门设计的9条特殊的库函数调用指令。

// 它的虚拟机是典型的栈式虚拟机（Java虚拟机也是典型的栈式虚拟机，早期的Lua也是栈式虚拟机，但最新的Lua 5.x采用寄存器虚拟机）。

// 我们可以使用-d命令，把生成的字节码dump出来。下图是hello.c的字节码：

//////////////////////////////
// ENT 0
// IMM 140292790341648
// PSH
// PRTF
// hellow, world
// ADJ 1
// IMM 0
// LEV
// PSH
// EXIT
// exit(0)  cycle = 9
//////////////////////////////

// 对源码只扫描一遍

// 与传统的编译器实现不同，C4它把词法分析、语法分析、语义分析、代码生成这几个步骤巧妙的结合在一起，在把C语言源码编译成字节码的整个过程中，只扫描了一遍源码。

// Lua的解释器也是采用对源码扫描一遍的方式，因此，C4和Lua的性能都相当不错。
// C4源码的可读性

// 对于C4的实现，网上也有一些讨论。有人认为C4的实现非常简洁、易读，也有人认为C4的实现稍显晦涩。

// 我个人认为，C4的实现确实非常简洁，毕竟只有4个函数，500多行代码。但是要真正完全理解，需要有一定的编译原理基础知识。

// 比如C4的语法分析过程中，就是典型的递归下降和算符优先算法相结合的实现方式。只要了解这些编译器的经典算法原理，C4的实现逻辑理解起来，还是比较轻松的。

// 此外，C4为了追求以最少的代码实现自举，在实现上采用了一些技巧。

// 比如，我们前面提到，C4不支持struct类型。这也意味着C4的源码中不能使用struct类型。为此，它选择使用数组来模拟struct结构。这样乍看起来，可能会产生一些困惑。

// 个人认为，C4的一个槽点，就是它变量的命名上，过于简洁。比如标记字节码的位置的变量用e表示，其实如果用emit的话，就会清晰许多，也会更容易理解。
// C4的衍生实现

// 除了C4的原生实现外，网上也有很多基于C4的衍生实现。

// 比如有人给C4额外增加了80多行代码，却给C4添加了JIT功能，使得执行速度得到明显提升。

// 也有人对C4做了简单修改，使得它可以直接产生真实的机器码，并最终生成ELF可执行文件。

// 这些都是非常有趣的项目，都很值得研究。
// C4的项目地址

// 在github上，找到用户名为rswier的大牛，就可以看到C4的项目了。

// */





