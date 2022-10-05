---
title: The Tiger-0 Language
author: Pedro Vasconcelos, DCC/FCUP
date: September 2021
papersize: a4
numbersections: true
...

# Overview

*Tiger* is a small language defined in the book *Modern Compiler
Implementation in ML* by Andrew Appel (Cambridge University Press,
1998).  This document defines a subset of Tiger, called *Tiger-0*,
suitable as a target for implementing a small compiler
in the context of an introductory compilers course.

Tiger-0 is a small imperative language with integers, strings, arrays,
basic control flow structures and functions. The most notable
differences to the Tiger language defined in Appel's book are: Tiger-0
disallows nested function definitions and record types, and supports only
integer arrays.

The syntax of Tiger-0 has some similarities to Pascal
and Standard ML. Readers more familiar with languages from the C
family should pay special attention to different conventions regarding
terminators and  operators. Studying the example programs at the end of 
this document can also help clarify the differences.


# Lexical Aspects

Whitespace characters (spaces, newlines or tabulations) may
appear between any *tokens* and are ignored. Comments are delimited
by `/*` and `*/` and may also occur between any tokens. Multi-line
comments are allowed, but nested comments are not.

An *identifier* is a sequence of letters (`a` to `z` or `A` to `Z`),
digits (`0` to `9`) or underscores (`_`) begining with a letter or
underscore.

An *integer constant* is a sequence of one or more decimal digits
(`0` to `9`). Constants do not have a sign; negative numbers can
be obtained by applying the unary operator `-`.

A *string constant* is a sequence of zero or more printable characters
between double quotes (`"`). Some escape sequences 
are treated to mean special characters:

`\n`

:   Newline

`\t`

:   Tabulation

`\"`

:   Double quotes

`\\`

:   Backslash (`\`)

The following sequences are *reserved keywords*:  `break` `do` `else` 
`end` `for` `function` `if` `in` `let` `of` `then` `var` `while`.

The following charaters are punctuation signs: `, : ; ( ) [ ]`.

The language operators are: `+ - * / % = <> < <= > >= & | :=`.

# Expressions

$$
\begin{array}{ll}
expr: \\
& \textit{integer-constant} \\
& \textit{string-constant} \\
& lvalue \\
& expr ~ \textit{binary-operato}r~ expr \\
& \texttt{-} expr \\
& lvalue ~\texttt{:=}~ expr \\
& id\texttt{(} \textit{expr-list}_{opt} \texttt{)} \\
& \texttt{(} \textit{expr-seq}_{opt} \texttt{)} \\
& \texttt{if}~ expr ~\texttt{then}~ expr \\
& \texttt{if}~ expr ~\texttt{then}~ expr ~\texttt{else}~expr \\
& \texttt{while}~expr~\texttt{do}~ expr \\
& \texttt{for}~ id ~\texttt{:=}~ expr~ \texttt{to}~ expr~ \texttt{do}~ expr \\
& \texttt{break}\\
& \texttt{let}~ \textit{var-decl-list}~ \texttt{in}~ \textit{expr-seq}~ \texttt{end} \\
& \\
lvalue: \\
& id \\
\\
\textit{expr-seq}: \\
& expr \\
& \textit{expr-seq} ~\texttt{;}~ expr\\
\\
\textit{expr-list}: \\
& expr \\
& \textit{expr-list} ~\texttt{,}~ expr
\end{array}
$$ 


Tiger-0 is an expression-based language, meaning that expressions
are the fundamental syntactical construct used to build
programs. Expressions may compute a value (e.g. `2*a+1`) or may be
used just for side-effects, such as assigning to a variable
(e.g. `a:=1`).

Note that, unlike languages from the C family, Tiger uses `:=` for
assignment and `=` for comparing equality.


## Return Values

Procedure calls, assignments, if-then, while, break, and sometimes
if-then-else produce no value and may not appear where a value is
expected (e.g., `(a:=b)+c` is illegal). A let expression with nothing
between the `in` and `end` returns no value.  A sequence of zero or
more expressions in parenthesis (e.g., `(a:=3; b:=a)`) separated by
semicolons are evaluated in order and returns the value produced by
the final expression, if any.  An empty pair of parenthesis `()` is
legal and returns no value

Note also that, unlike C, semicolons `;` are used as
separators between expressions rather than after each expression.

## Assignments and Lvalues

The assignment expression $lvalue$ `:=` $expr$ evaluates the
expression then binds its value to the contents of the $lvalue$. 
Assignment expressions do not produce values, so something like 
`a := b := 1` is illegal.

The valid left-hand targets of assignments are called *lvalues*; in the base
Tiger-0 language these can only be simple variables (e.g. `a:=2*b`).
Section 6 discusses an extension to allows arrays.

## Function Calls

A function application is an expression `id(e1,e2,...)` with zero or
more comma-separated expression parameters. When a function is called,
the values of these actual parameters are evaluated from left to
right and bound to the function’s formal parameters using
conventional static scoping rules.

## Operators

The binary operators are `+ - * % / = <> < > <= >= & |`.
Parentheses group expressions in the usual way.
A leading minus sign negates an integer expression.

The binary operators `+`, `-`, `*`, `/` and `%` require integer operands
and return an integer result.

The binary operators `>`, `<`, `>=`, and `<=` compare their operands,
which may be either both integer or both string and produce
the integer 1 if the comparison holds, and 0 otherwise. String
comparison is done using normal ASCII lexicographic order.
The binary operators `=` and `<>` can compare any two operands
of the same (non-valueless) type and return either integer 0 or 1.
Integers are the same if they have the same value. Strings are the
same if they contain the same characters. 

The logical operators `&` and `|` are lazy logical operators on
integers. They do not evaluate their right argument if evaluating the
left determines the result. Logical negation is expressed by the
library function `not`.  Zero is considered false; everything else is
considered true.

Unary minus has the highest precedence followed by `*`,`/` and `%`,
then `+` and `-`, then `=`, `<>`, `>`, `<`, `>=`, and `<=`, then 
`&`, then `|`, then finally `:=`.

The `+`, `-`, `*`, `/` and `%` operators are left associative. The
comparison operators do not associate, e.g., `a=b=c` is erroneous, but
`a=(b=c)` is legal.

## Flow Control

The if-then-else expression, written `if` $expr$ `then` $expr$ `else`
$expr$ evaluates the first expression, which must return an integer.
If the result is non-zero, the second expression is evaluated and
becomes the result, otherwise the third expression is evaluated
and becomes the result. Thus, the second and third expressions
must be of the same type or both not return a value.

The if-then expression, `if` $expr$ `then` $expr$ evaluates its first
expression, which must be an integer. If the result is non-zero, it
evaluates the second expression, which must not return a value.
The if-then expression does not return a value.

The while-do expression, `while` $expr$ `do` $expr$ evaluates its
first expression, which must return an integer. If it is non-zero,
the second expression is evaluated, which must not return a
value, and the while-do expression is evaluated again.

The for expression, `for` $id$ `:=` $expr$ `to` $expr$ `do` $expr$,
evaluates the first and second expressions, which are loop bounds.
Then, for each integer value between the values of these two
expressions (inclusive), the third expression is evaluated with the
integer variable named by $id$ bound to the loop index. The scope of
this variable is limited to the third expression, and may not be
assigned to. This expression may not produce a result and is not
executed if the loop’s upper bound is less than the lower bound.

The `break` expression terminates the innermost enclosing `while` or `for`
expression that is enclosed in the same function/procedure. The break
is illegal outside this.



## Variable Declarations

$$
\begin{array}{ll} 
\textit{var-decl-list}: \\
& \textit{var-decl}~ \\
& \textit{var-decl-list} ~ \textit{var-decl} \\
\\
\textit{var-decl}: \\
& \texttt{var}~ id ~\texttt{:=}~ expr \\
\end{array}
$$ 

The expression `let` $\textit{var-decl-list}$ `in`
$\textit{expr-seq}$ `end` evaluates the declarations, binding
variables to the scope of the expression sequence, which
is a sequence of one or more semicolon-separated expressions. The
result is that of the last expression.

A variable declaration defines a new variable and its initial value. The variable’s
type comes from the expression.  In `let`
\ldots$\textit{var-decl}$\ldots `in`
$\textit{expr-seq}$ `end`, the scope of the variable declaration
begins just after the declaration and closes at the `end`. A variable
lasts throughout its scope.  Variables and functions share the same
name space.

Usually, variables must be declared and initialized before use.  The
only exceptions are loop variables in the `for` construct e.g. the
following expression is valid with no explicit declaration of
variable `i`:

~~~
for i:=1 to 10 do print(i)
~~~

Note that it is not possible to use a loop variable outside of the loop
because the scope of the loop variable is limited to the  body of the loop.


# Programs

$$
\begin{array}{ll}
\textit{program}: \\
& \texttt{let} ~\textit{decl-list}~ \texttt{in}~ \textit{expr-seq}
\\
\\
\textit{decl-list}: \\
& \textit{decl} \\
& \textit{decl-list}~ \textit{decl} \\
\\
\textit{decl}: \\
& \textit{var-decl}\\
& \textit{fun-decl}\\
\\
\textit{fun-decl}: \\
& \texttt{function}~ id \texttt{(} \textit{type-fields}_{opt} \texttt{) =}~ expr\\
& \texttt{function}~ id \texttt{(} \textit{type-fields}_{opt} \texttt{):} \textit{type-id} ~\texttt{=}~ expr\\
\\
\textit{type-fields}: \\
& \textit{type-field} \\
& \textit{type-fields} ~\texttt{,}~ \textit{type-field} \\
\\
\textit{type-field}: \\
& id ~\texttt{:}~ \textit{type-id}
\end{array}
$$

A *program* is list of global variable or function definitions
followed by an expression sequence.

Note that Tiger-0 does not allow nested function definitions, that is,
functions can only be declared at the outermost `let` expression of a
program.

## Function Declarations

There are two forms of function declarations.
The first form is a procedure declaration; the second is a function.
Functions return a value of the specified type; procedures are only
called for their side-effects. Both forms allow the specification of a
list of zero or more typed arguments, which are passed by value. The
scope of these arguments is the $expr$.

The $expr$ is the body of the function or procedure.

A sequence of function declarations (i.e., with no intervening
variable or type declarations) may be mutually recursive. No two
functions in such a sequence can have the same name.

## Types

$$
\begin{array}{ll}
\textit{type-id}: \\
& \texttt{int} \\
& \texttt{string} \\
& \texttt{intArray} 
\end{array}
$$


Tiger-0 has only two basic types, `int` and `string`, and one structured
type `intArray` for arrays of integers (see Section 6).


# Standard Library

`function print(s: string)`

:  Print the string on the standard output.

`function printi(i: int)`

:  Print the integer on the standard output.

`function scani() : int`

:  Read an integer from the standard input.

# Arrays

$$ \begin{array}{ll}
expr: \\
& \vdots \\
& \texttt{intArray} ~\texttt{[}~ expr ~\texttt{]}~ \texttt{of}~ expr 
\\ \\
lvalue: \\
&  id \\
& id ~\texttt{[}~ expr ~\texttt{]}
\end{array}
$$

This section describes an extension to Tiger-0 for simple integer
arrays.

The expression `intArray [` $expr$ `] of` $expr$ creates a new array
of integers whose size is given by the expression in
brackets. Initially, the array is filled with elements whose values
are given by the expression after the `of`. These two expressions are
evaluated in the order they appear.  For example, `intArray [10] of 0`
creates a 10 element array with each element initialized to 0.

Array indexing is expressed by $id$ `[` $expr$ `]` where $id$ is 
an array identifier and $expr$ is an (integer) index.
Valid indices are from 0 to $N-1$ for an $N$-element array.

Array assignment is by reference, not value. 
Assigning an array to a variable creates an alias, meaning
later updates of the variable or the value will be reflected in both
places. Passing an array or record as an actual argument to a
function behaves similarly.


# Example Programs

## Sum of squares

~~~{.numberLines}
/* Compute the sum of squares from 1 to 10 */
let 
  var s := 0
  var n := 1
in
  while n <= 10 do
     (s := s + n*n;
      n := n + 1);
  printi(n)
end
~~~

## Recursive Factorial

~~~{.numberLines}
let 
  function fact(n: int): int = 
     if n>0 then n*fact(n-1) 
     else 1
in 
  printi(fact(10)) 
~~~

## Naive prime number test

~~~{.numberLines}
/* Test prime numbers */
let 
  function is_prime(n:int): int = 
    let 
        var d := 2
    in 
        while d<n & n%d<>0 do
            d := d+1;
        n>1 & d=n
    end
in
  let 
    var i := scani()
  in if is_prime(i) then 
    print("prime")
  else
    print("not prime")
end
~~~


## Fibonnaci numbers

~~~{.numberLines}
/* Tabulate some Fibonnaci numbers */
let 
   var N := 20
   var fib := intArray [ N ] of 0
in
   fib[1] := 1;
   for i := 2 to N-1 
   do fib[i] := fib[i-1] + fib[i-2];
   for i := 0 to N-1
   do printi(fib[i])
end
~~~

## 8-Queens problem

~~~{.numberLines}
/* Solver for the 8 queens problem by Andrew Appel */
let
  var N := 8
  var row := intArray [ N ] of 0
  var col := intArray [ N ] of 0
  var diag1 := intArray [ N+N-1 ] of 0
  var diag2 := intArray [ N+N-1 ] of 0
  
  function printboard() =
     (for i := 0 to N-1
      do (for j := 0 to N-1
          do print(if col[i]=j then " O" else " .");
          print("\n"));
      print("\n"))

  function try(c:int) =
     if c=N then printboard()
     else for r := 0 to N-1
          do if row[r]=0 &
                diag1[r+c]=0 & diag2[r+7-c]=0
             then (row[r] := 1; diag1[r+c] := 1;
                   diag2[r+7-c] := 1; col[c] := r;
                   try(c+1);
                   row[r] := 0; diag1[r+c] := 0;
                   diag2[r+7-c] := 0)
in try(0) end
~~~

