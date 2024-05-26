Beware
======

This project is no longer updated. If you need to parse lambda lists, consider a few alternatives:

* [Alexandria](https://gitlab.common-lisp.net/alexandria/alexandria)'s `parse-ordinary-lambda-list`
* [Ecclesia](https://github.com/s-expressionists/Ecclesia) has various parsers
* [CST](https://github.com/s-expressionists/Concrete-syntax-tree) can parse lambda lists while keeping track of source locations

Original documentation
======================

This is a library for manipulating and using general Common Lisp lambda lists. Unlike other utilities, such as ALEXANDRIA:PARSE-ORDINARY-LAMBDA-LIST, this library is general and beefy enough to handle all the different types of lambda lists listed in section 3.4 of the standard. It also allows defining your own grammars, clause types, etc.

The two basic components of this library are _parsing_ and _binding generation_. Parsing turns a lambda list, like (a b &key c d), and a grammar for lambda lists into an object of class LAMBDA-LIST. Binding generation takes such an object and a series of forms, and returns a series of bindings and declarations suitable for CL:LET* that implement the lambda list binding to the values of those forms.

The two main entry points to these components are PARSE-LAMBDA-LIST and GENERATE-BINDINGS. The GENERATE-LET* function takes care of the common use of GENERATE-BINDINGS to generate a LET* form. As an example of their use, this is how you would transform a lambda form (3.1.2.1.2.4) into a LET* form:

    (destructuring-bind ((lambda lambda-list &body body)
                         &rest arguments)
        form
      (declare (ignore lambda))
      (generate-let* (parse-lambda-list lambda-list *ordinary-lambda-list*)
		     body
		     arguments))

More complete documentation below and in docstrings.

Basic use
=========

_Function_ **PARSE-LAMBDA-LIST**

**Syntax:**

**parse-lambda-list** lambda-list grammar-spec &rest initargs &key safe

=> lambda-list-object

lambda-list is something parseable (i.e., that corresponds to a grammar). grammar-specs are explained below in "Grammars", or you can just use one of the pre-defined ones. Initargs are to the lambda list object as well as clause objects; currently the only one defined is :SAFE, which determines whether the binding form for a lambda list follows the restrictions on "safe code" (3.5). Note to self: move that out of parsing.

lambda-list-object is an object of class LAMBDA-LIST suitable for generating bindings.

_Function_ **GENERATE-BINDINGS**

**Syntax:**

**generate-bindings** lambda-list &rest forms

=> bindings, declarations

lambda-list is an object of class LAMBDA-LIST. forms are any lisp forms. bindings is a list of let* bindings, and declarations is a list of declarations intended to be bound in a let* form.

This function does the basic work of generating bindings from a lambda list.

The forms argument takes some explanation. An ordinary lambda list conceptually binds to one thing, a list of arguments. If all lambda lists were like this, forms would be one form, that list. Some lambda lists, however - the macro-related ones - actually bind to two different things, a list of arguments and a separate "environment" argument - that necessitates some complication.

This is to say, if ll is an ordinary lambda list, you would generate bindings with `(generate-bindings ll args)`. But if ll is a macro lambda list, you would generate bindings with `(generate-bindings ll form env)`.

Things are also complicated by the fact that some lambda lists do not bind the same conceptual things. Above, the ordinary lambda list binds to "args", but the macro one binds to "form", because a macro function receives the entire macro form as an argument, not just the arguments to a macro form.

_Function_ **GENERATE-LET**

**Syntax:**

**generate-let** lambda-list body &rest forms

=> form

A convenience function around generate-bindings, for the common case that you just want to wrap a body in the appropriate let. Returns `(let* ,bindings ,@declarations ,@body) where bindings and declarations are the result of generate-bindings.

Standard grammars
-----------------

All the lambda list types specified in CLHS 3.4 are available, except for generic function and boa lambda lists, which are not used for binding. (Could add if wanted; file an issue.) Just pass these as arguments to parse-lambda-list:

* `*ordinary-lambda-list*`
* `*specialized-lambda-list*` as in DEFMETHOD
* `*macro-lambda-list*` as in DEFMACRO
* `*compiler-macro-lambda-list*` as in DEFINE-COMPILER-MACRO
* `*destructuring-lambda-list*` as in DESTRUCTURING-BIND
* `*defsetf-lambda-list*` as in DEFSETF
* `*deftype-lambda-list*` as in DEFTYPE
* `*define-modify-macro-lambda-list*` as in DEFINE-MODIFY-MACRO
* `*define-method-combination-lambda-list*` as in DEFINE-METHOD-COMBINATION

Defining your own lambda list grammars
======================================

Past here is for customization only. If you are fine with being able to parse the lambda lists defined in the standard, don't worry about this.

Clauses
-------

If you want your own lambda lists, you want to deal with clauses. Both parsing and binding generation protocols are oriented around clauses.

Lambda lists are composed of "clauses". Each clause is zero or more specifications of variables which are all bound similarly, and usually correspond to a lambda list keyword. For example, in "(a &optional (b 3) &rest x &key c (d a))", there are four clauses: a required variable clause binding a, an &optional clause binding b, a &rest clause binding x, and a &key clause binding c and d.

Clauses exist both in the parsing and binding generation stages of this library, and the same classes pull double duty.

The abstract overlording class is CLAUSE. All clauses have a list of keywords (initarg :keywords), used for parsing, e.g. in a macro lambda list, the &rest clause has keywords &rest and &body.

Note that if your lambda lists have a clause that doesn't have an associated lambda list keyword - like the required parameters in an ordinary lambda list - the list of keywords should include nil. (If you want nil as a lambda list keyword, too bad I guess. File an issue.)

Two more abstract classes under this exist, SINGLETON-CLAUSE and MULTIPLE-CLAUSE. A singleton-clause is one that allows only one variable, like &rest. A multiple-clause is one that allows multiple variables, like &key. A few functions have multiple- versions for running through multiple-clauses.

singleton-clauses have a 'spec' slot, while multiple-clauses have a 'specs' slot, which is unsurprisingly a list of specs. A spec is just a variable specification - for a required variable it's just the symbol name, for an &optional it's (var default var-p), etc. The specs should be normalized in some way, but that's up to you, since you'll be the one turning them into bindings.

If the lambda list is destructuring, specs may be whole other LAMBDA-LIST objects.

Furthermore there are two mixins, DEFAULTING-CLAUSE and DESTRUCTURING-CLAUSE. A defaulting-clause allows there to be no argument associated to the variable, in which case a default value is used - &optional. A destructuring-clause allows destructuring, i.e. nested lambda lists, as described in CLHS 3.4.4.1.

The concrete clause classes are regular-clause, optional-clause, etc. Some of distinct destructuring versions which are basically the same but for the mixin.

If you want to define your own clause, you might want to specify your own :initform for the keywords.

Additional initargs used by the predefined clauses are as follows.

&whole clauses have :map, which specify how the rest of the lambda list should be derived from the &whole argument; for a macro lambda list this is CDR, and for a compiler macro lambda list it is the special function COMPILER-MACRO-ARGUMENTS.

defaulting-clauses have :default-default, which can be used to specify what the default value for a variable is if the lambda list itself does not specify a default. The default default default is nil. This is used in deftype lambda lists, which have a :default-default \*.

destructuring-clauses have :destructure. :destructure specifies a grammar that is used to parse recursive lambda lists. In most cases, this necessitates a separate *destructuring-whatever-lambda-list*, since the sub-lambda-lists have different grammars (e.g. sub macro lambda lists do not allow &environment).

Grammars
--------

Frankly the grammar creation is sort of a mess. I'm sorry! Lambda lists have weird grammars. In the future I will probably rip out all the crap I wrote and use a proper generated recursive descent parser if this library gets any use.

Anyway. Grammars are specified by lists. The elements of these lists are clause specifiers. A clause specifier is either a class specifier or a list (class-specifier ...plist...).

When a lambda list is parsed, the grammar is first turned into a prototype lambda list by instantiating clause objects as by make-instance. For example, if your grammar was (regular-clause aux-clause), the prototype would be (#<REGULAR-CLAUSE> #<AUX-CLAUSE>). If the clause specifier includes a plist, that plist is used as initargs to make-instance, with two exceptions: :data-destructure and :anywhere are treated specially by the parser and scrubbed from the initargs.

:data-destructure in a clause specifier indicates that if a lambda list is dotted, the variable specification after the dot is an instance of this clause. For example a &rest clause has this in a macro-lambda-list.

:anywhere indicates that the clause can occur out of order and occur anywhere after the first clause. This is for &environment. The restriction to not being the first clause is not great.

Not counting :anywhere, the grammar indicates the clauses in order. A clause can be elided but cannot appear in the wrong place.

Parsing clauses
---------------

There are two generic functions you can customize to control parsing: CLAUSE-PARSE and MULTIPLE-CLAUSE-PARSE.

CLAUSE-PARSE receives a prototype clause, the current portion of the lambda list being parsed, and the whole prototype LAMBDA-LIST object. It should alter the clause object to match the lambda list, and return the unparsed remainder of the lambda list.

If your clause is a singleton-clause, you don't need to specialize this.

If you have a multiple-clause, you probably want to use the default multiple-clause parser on the whole clause except the lambda list keyword, so you do

    (defmethod clause-parse ((clause my-clause) list lambda-list)
      (call-next-method clause (rest list) lambda-list))

The default multiple-clause parser essentially calls MULTIPLE-CLAUSE-PARSE repeatedly. For an example of something more complicated, check the method on key-clause.

MULTIPLE-CLAUSE-PARSE receives a prototype clause, a single possible spec, and the lambda list as a whole. It should return the normalized spec, or, if the spec turns out _not_ to be a spec (i.e. it's the start of the next clause) it should return NIL. Check the built-in methods.

Making bindings from clauses
----------------------------

TODO
