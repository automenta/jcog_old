package jcog.nars.reason;

/** "Non-Axiomatic Reasoner", a specific instance of a NARS implementation, with self-contained memory and executive control.
    
   	Specific implementations of "NAR" utilize specific memory and inference components 
*/
public interface NAR {

	//result of refactoring the original Open-NARS "Memory", "NARS", "Parameters", and "Center" classes

    public void start();
	public void reset();
	public void cycle();
	public long getNow();
	
}
