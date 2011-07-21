package jcog.genifer;

/**
 * Constants are... constants.
 * Constant is the abstract base class of Atom, Number and StringConstant.
 * Subclasses must override Object.equals and Object.getHashCode.
 * Constants can be treated as "value types". If two constants have the same value,
 *    they are the same constant.
 */
public class Constant extends Formula {
}
