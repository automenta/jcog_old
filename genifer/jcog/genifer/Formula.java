/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.genifer;

/**
 *
 * Formulas = formulas.  This allows for self-reference, ie, to talk about formulas in formulas.
 * At the top level is the formula (or proposition, or sentence, or logic statement, all synonyms).
 * A formula can be a rule or a fact.  A rule is of the form
     A <- X, Y, Z, ...
   And a fact does not contain the "<-" operator.
 * Each formula consists of a number of literals joined by logical operators (aka connectives) such as AND and OR, and operations can be nested.  Note that NOT is a unary operator.
 *
   The abstract base class of all further Formula classes including constants, functor applications
   and operator applications.

   Possible Metadata: creation date, URI (serial num)

 to-do: implement unify.
	rwallace says: It's a standard algorithm so there are no decisions to be made there, it gives your data definitions a reasonable workout, and it is something of a Rosetta stone
	http://norvig.com/unify-bug.pdf
	http://en.wikipedia.org/wiki/Unification_(computing)
	http://www.google.com/search?hl=en&q=unify+algorithm

== References

`   2010-07-07 through 09 #Genifer IRC chat

    http://en.wikipedia.org/wiki/Prolog

    http://en.wikibooks.org/wiki/Prolog/Putting_it_Together

    http://www.dai.ed.ac.uk/groups/ssp/bookpages/quickprolog/node5.html



 * @author SEH
 */
abstract public class Formula {
    
    public static class BoolVal extends Constant {   }
    
    /** Number: both an integer or double */
    public static class ScalarVal extends Constant {   }

    public static class StringVal extends Atom {
        public StringVal(String name) {
            super(name);
        }            
    }

    public class Variable extends Atom {
        public Variable(String name) {
            super(name);
        }       
    }

/** FunctorApp is a functor application consisting of a name and arguments.
    A functor application must have one or more arguments.
    Otherwise, you should create an Atom instead. */
    public class ApplyFunc extends Formula {
        final public String function;
        final public Formula[] arguments;

        public ApplyFunc(String function, Formula[] arguments) {
            this.function = function;
            this.arguments = arguments;
        }


        //hashcode
        //isEquals
        //tostring
    }

    abstract public static class ApplyOp extends Formula {
//        all,
//        and,
//        eq,
//        eqv,
//        exists,
//        none,
//        not,
//        or,
    }


    /**
     UnaryOpApp is the application of a unary operator to one Formula.
     This is the abstract base class of all unary operators such as NotOpApp.
     */
    abstract public static class ApplyUnaryOp<X extends Formula> extends ApplyOp {
        public final X x;

        public ApplyUnaryOp(X x) {
            this.x = x;
        }

        //toString
        
//	def toString as String is override
//		return '[.typeOf.name]([.x])'
//

//	def variables as Variable* is override
//		for variable in .x.variables, yield variable

    }

/**
    BinaryOpApp is the application of a binary operator to two Formulas.
    This is the abstract base class of all binary operators such as AndOpApp and OrOpApp.
 */
    abstract public static class ApplyBinaryOp<A extends Formula, B extends Formula> extends ApplyOp {
       public final A left;
       public final B right;

        public ApplyBinaryOp(A left, B right) {
            super();
            this.left = left;
            this.right = right;
        }

        //tostring

//	def variables as Variable* is override
//		for variable in .left.variables, yield variable
//		for variable in .right.variables, yield variable
    }
    
    public static class ImpliesOp extends ApplyBinaryOp {

        public ImpliesOp(Formula condition, Formula consequent) {
            super(condition, consequent);
        }
        public Formula getCondition() { return left; }
        public Formula getConsequent() { return right; }
    }


//
//class AndOpApp inherits BinaryOpApp
//
//	cue init(left as Formula, right as Formula)
//		base.init(left, right)
//
//
//class OrOpApp inherits BinaryOpApp
//
//	cue init(left as Formula, right as Formula)
//		base.init(left, right)
//
//
//class UnaryOpApp inherits Formula is abstract
//
//
//class NotOpApp inherits UnaryOpApp
//
//	cue init(x as Formula)
//		base.init(x)
//
//

    
}
