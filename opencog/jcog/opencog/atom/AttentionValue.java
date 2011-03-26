/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.opencog.atom;

/**
 *
 * @author seh
 */
public class AttentionValue {
//static const int 	DISPOSABLE = 0
//static const int 	NONDISPOSABLE = 1
//static const sti_t 	DEFAULTATOMSTI = 0
//static const lti_t 	DEFAULTATOMLTI = 0
//static const vlti_t 	DEFAULTATOMVLTI = DISPOSABLE
//static const sti_t 	MAXSTI = SHRT_MAX
//static const lti_t 	MAXLTI = SHRT_MAX
//static const sti_t 	MINSTI = SHRT_MIN
//static const lti_t 	MINLTI = SHRT_MIN

    private short sti;
    private short lti;
    private int vlti; //is unsigned short in C++, increased to int to cover the range

    public static final int Disposable = 0;
    public static final int NonDisposable = 1;
    
    public static final short DefaultSTI = 0;
    public static final short DefaultLTI = 0;
    public static final int DefaultVLTI = Disposable;
    
    public static final short MinSTI = Short.MIN_VALUE;
    public static final short MinLTI = Short.MIN_VALUE;
    public static final short MaxSTI = Short.MAX_VALUE;
    public static final short MaxLTI = Short.MAX_VALUE;
    
    public AttentionValue(boolean disposable) {
        super();
        sti = DefaultSTI;
        lti = DefaultLTI;
        vlti = disposable ? Disposable : NonDisposable;
    }
    
    public AttentionValue() {
        this(true);        
    }

    
//virtual 	~AttentionValue ()
// 	AttentionValue (sti_t STI=DEFAULTATOMSTI, lti_t LTI=DEFAULTATOMLTI, vlti_t VLTI=DEFAULTATOMVLTI)
//virtual sti_t 	getSTI () const
//virtual float 	getScaledSTI () const
//virtual lti_t 	getLTI () const
//virtual vlti_t 	getVLTI () const
//void 	decaySTI ()
//virtual std::string 	toString () const
//virtual AttentionValue * 	clone () const
//virtual bool 	operator== (const AttentionValue &av) const
//bool 	operator!= (const AttentionValue &rhs) const

    public short getSTI() {
        return sti;
    }

    public short getLTI() {
        return lti;
    }

    public int getVLTI() {
        return vlti;
    }

    public void setVLTI(int newVLTI) {
        this.vlti = newVLTI;
    }

    public short addSTI(short deltaSTI) {
        this.sti += deltaSTI;
        return sti;
    }

    public void setSTI(short newSti) {
        this.sti = newSti;
    }

    public short addLTI(short deltaLTI) {
        this.lti += deltaLTI;
        return lti;
    }
    

}
