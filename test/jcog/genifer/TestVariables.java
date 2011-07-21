/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.genifer;

import junit.framework.TestCase;

/**
 *
 * @author SEH
 */
public class TestVariables extends TestCase {

    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    public void testAtom() {
//		t = Atom('exists')
//		vars = t.variables.toList
//		assert vars.count == 0
    }

    public void testExists() {
//		t = ImpliesOpApp(Variable('x'), Atom('exists')) to Term
//		vars = t.variables.toList
//		assert vars.count == 1 and vars[0].name == 'x'
    }

    public void testNotOp() {
//		t = ImpliesOpApp(NotOpApp(NotOpApp(Variable('x'))), Variable('x'))
//		vars = t.variables.toList
//		assert vars.count == 2 and vars[0].name == 'x' and vars[1].name == 'x'
    }

    @Override
    protected void tearDown() throws Exception {
        super.tearDown();
    }

}
