/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.opencog;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import jcog.atom.OrderedSetHypergraph;
import org.apache.log4j.Logger;

/**
 *
 * @author seh
 */
public class MemoryAtomSpace implements ReadableAtomSpace, EditableAtomSpace {

    final static Logger logger = Logger.getLogger(MemoryAtomSpace.class);
    
    public final OrderedSetHypergraph<Atom, Atom> graph;
    Map<Atom, OCType> atomToType;
    Multimap<OCType, Atom> typesToAtom;
    BiMap<Atom, String> names;

    public MemoryAtomSpace() {
        super();
        graph = new OrderedSetHypergraph<Atom, Atom>();
        typesToAtom = HashMultimap.create();
        atomToType = new HashMap();
        names = HashBiMap.create();
    }
    
    protected boolean indexAtom(Atom a, OCType type, String name) {
        typesToAtom.put(type, a);        //TODO check if this preserves previously-added types
        atomToType.put(a, type);
        
        String targetName = null;
        if (name!=null) {
            targetName = name;
        }
        
        if (targetName!=null) {
            if (names.containsValue(name)) {
                logger.error(this + " can not add atom with existing name: " + name);
                return false;
            }
            
            names.put(a, targetName);  
        }
        else {
            names.remove(a);
        }
        
        return true;
    }
    
    @Override
    public boolean addVertex(OCType type, Atom a)  {
        return addVertex(type, a, null);
    }
    
    @Override
    public boolean addVertex(OCType type, Atom a, String name)  {
        if (indexAtom(a, type, name)) {
            graph.addVertex(a);
            return true;        
        }
        return false;
    }
    
    
    @Override
    public Atom addVertex(OCType type, String name) {        
        Atom a = new Atom();
        if (addVertex(type, a, name)) {
            return a;
        }
        else {
            return null;
        }
    }
    
    protected void unindexAtom(Atom a) {
        OCType t = getType(a);
        typesToAtom.remove(t, a);      //TODO check if this preserves previously-added types
        atomToType.remove(a);        
        names.remove(a);                
    }
    
    @Override
    public boolean removeVertex(Atom a) {
        if (!graph.containsVertex(a)) {
            logger.error(this + " can not remove non-existent atom: " + a.toString());
            return false;
        }
     
        unindexAtom(a);
        
        graph.removeVertex(a);
        return true;
    }

    @Override
    public Collection<Atom> getIncidentEdges(Atom vertex) {
        return graph.getIncidentEdges(vertex);
    }
    
    @Override
    public Collection<Atom> getIncidentVertices(Atom edge) {
        return graph.getIncidentVertices(edge);
    }
    
    public static boolean isEqual(Collection<Atom> a, Atom[] b) {
        if (a.size()!=b.length)
            return false;
        
        int i = 0;
        for (Atom x : a) {
            if (x != b[i++])
                return false;
        }
        return true;
    }
    
    @Override
    public Atom getEdge(OCType type, Atom... members) {
        if (members.length == 0)
            return null;
        
        Collection<Atom> incidentEdges = getIncidentEdges(members[0]);
        for (Atom e : incidentEdges) {
            if (getType(e).equals(type)) {
                Collection<Atom> iv = getIncidentVertices(e);
                if (isEqual(iv, members))
                    return e;
            }
        }
        
        return null;
    }
        
    
    public Collection<Atom> getAtoms(OCType type, boolean includeSubtypes) {
        if (!includeSubtypes) {
            //TODO unmodifiable
            return typesToAtom.get(type);
        }
        else {
            logger.error("subtype type fetch not implemented yet");
            return null;
        }
    }
    
    public OCType getType(Atom a) {
        return atomToType.get(a);
    }
    
    public String getName(Atom a) {
        return names.get(a);
    }
    
    public Atom addEdge(OCType t, String name, Atom... members) {
        
        final List<Atom> memberList = Arrays.asList(members);
        
        //preventing duplicate edges (type, members). allows updating the name if already exists
        
        for (final Atom eaa : typesToAtom.get(t)) {
            if (graph.getIncidentVertices(eaa).equals(memberList)) {
                logger.info("retrieving duplicate edge " + eaa + "(" + t + ")");                    

                String oldName = getName(eaa);
                if (name!=null) {
                    if (oldName == null) oldName = "";
                    if (!oldName.equals(name)) {
                        logger.error("overwriting " + eaa + "{name=" + oldName + ", type=" + t + "} TO name=" + name);
                    }
                }
                else {
                    if (oldName != null) {
                        logger.error("overwriting " + eaa + "{name=" + oldName +", type=" + t + "} TO name NULL");
                    }

                }
                return eaa;
            }
        }

        Atom e = new Atom();
        indexAtom(e, t, name);
        
        graph.addEdge(e, memberList);
        
        return e;        
    }
    
    /**
     * TODO If a Link with the same type and outgoing set of a previously inserted Link is inserted in the AtomSpace, they are merged.
     */
    @Override
    public Atom addEdge(OCType t, Atom... members) {        
        return addEdge(t, null, members);
    }
    
    @Override
    public boolean removeEdge(Atom e) {
        unindexAtom(e);
        return graph.removeEdge(e);
    }
    
    public int getArity(Atom e) {
        return graph.getIncidentCount(e);
    }
    
    public boolean hasAtom(Atom a) {
        if (graph.containsVertex(a))
            return true;
        if (graph.containsEdge(a))
            return true;
        return false;
    }
    
    public boolean isVertex(Atom a) {
        return getArity(a) == 0;
    }
    
    @Override
    public void clear() { 
        logger.error("clear() not implemented yet");
    }
    
    
    public void addType(OCType type, OCType... supertypes) {
        addVertex(Atom.Type, type);
    }

    @Override
    public boolean visitEdges(Predicate<Atom> predicate, Operation<ReadableAtomSpace, Atom> op) {
        for (Atom e : getEdges()) {
            if (predicate.isTrue(e)) {
                boolean result = op.operate(this, e);
                if (!result)
                    return false;
            }
        }
        return true;
    }
    
    @Override
    public boolean visitVertices(Predicate<Atom> predicate, Operation<ReadableAtomSpace, Atom> op) {
        for (Atom e : getVertices()) {
            if (predicate.isTrue(e)) {
                boolean result = op.operate(this, e);
                if (!result)
                    return false;
            }
        }
        return true;
    }

    public Iterator<Atom> iterateVertices() {
        return graph.getVertices().iterator();
    }
    public Iterator<Atom> iterateEdges() {
        return graph.getEdges().iterator();
    }
    
    @Override
    public Collection<Atom> getVertices() {
        return graph.getVertices();
    }
    
    @Override
    public Collection<Atom> getEdges() {
        return graph.getEdges();
    }
    
    public double getMeanEdgeArity() {
        Collection<Atom> edges = getEdges();
        if (edges.size() == 0)
            return 0;
        double sum = 0;
        int num = 0;
        for (Atom a : edges) {
            sum += getArity(a);
            num++;
        }
        return sum / ((double)num);
    }
    
    public Multimap<OCType, Atom> getTypeIndex() {
        //TODO return unmodifiable
        return typesToAtom;
    }
    
    public BiMap<Atom, String> getNameIndex() {
        //TODO return unmodifiable
        return names;
    }
    
    //public Atom removeEdge(String t, Atom... members) { }
    
// 	AtomSpace (void)
// 	AtomSpace (AtomSpaceAsync &a)
// 	~AtomSpace ()
//void 	storeAtom (Handle h)
//Handle 	fetchAtom (Handle h)
//Handle 	fetchIncomingSet (Handle h, bool recursive)
//TimeServer & 	getTimeServer () const
//SpaceServer & 	getSpaceServer () const
//AttentionBank & 	getAttentionBank ()
//int 	getSize () const
//Handle 	addRealAtom (const Atom &atom, const TruthValue &tvn=TruthValue::NULL_TV())
//void 	print (std::ostream &output=std::cout, Type type=ATOM, bool subclass=true) const
//Handle 	addNode (Type t, const std::string &name="", const TruthValue &tvn=TruthValue::DEFAULT_TV())
//Handle 	addPrefixedNode (Type t, const std::string &prefix="", const TruthValue &tvn=TruthValue::DEFAULT_TV())
//Handle 	addLink (Type t, const HandleSeq &outgoing, const TruthValue &tvn=TruthValue::DEFAULT_TV())
//Handle 	addLink (Type t, Handle h, const TruthValue &tvn=TruthValue::DEFAULT_TV())
//Handle 	addLink (Type t, Handle ha, Handle hb, const TruthValue &tvn=TruthValue::DEFAULT_TV())
//Handle 	addLink (Type t, Handle ha, Handle hb, Handle hc, const TruthValue &tvn=TruthValue::DEFAULT_TV())
//Handle 	addLink (Type t, Handle ha, Handle hb, Handle hc, Handle hd, const TruthValue &tvn=TruthValue::DEFAULT_TV())
//Handle 	addLink (Type t, Handle ha, Handle hb, Handle hc, Handle hd, Handle he, const TruthValue &tvn=TruthValue::DEFAULT_TV())
//Handle 	addLink (Type t, Handle ha, Handle hb, Handle hc, Handle hd, Handle he, Handle hf, const TruthValue &tvn=TruthValue::DEFAULT_TV())
//Handle 	addLink (Type t, Handle ha, Handle hb, Handle hc, Handle hd, Handle he, Handle hf, Handle hg, const TruthValue &tvn=TruthValue::DEFAULT_TV())
//Handle 	addLink (Type t, Handle ha, Handle hb, Handle hc, Handle hd, Handle he, Handle hf, Handle hg, Handle hh, const TruthValue &tvn=TruthValue::DEFAULT_TV())
//Handle 	addLink (Type t, Handle ha, Handle hb, Handle hc, Handle hd, Handle he, Handle hf, Handle hg, Handle hh, Handle hi, const TruthValue &tvn=TruthValue::DEFAULT_TV())
//bool 	removeAtom (Handle h, bool recursive=false)
//Handle 	getHandle (Type t, const std::string &str) const
//Handle 	getHandle (Type t, const HandleSeq &outgoing) const
//std::string 	atomAsString (Handle h, bool terse=true) const
//std::string 	getName (Handle h) const
//void 	setSTI (Handle h, AttentionValue::sti_t stiValue)
//void 	setLTI (Handle h, AttentionValue::lti_t ltiValue)
//void 	setVLTI (Handle h, AttentionValue::vlti_t vltiValue)
//AttentionValue::sti_t 	getSTI (Handle h) const
//AttentionValue::lti_t 	getLTI (Handle h) const
//AttentionValue::vlti_t 	getVLTI (Handle h) const
//HandleSeq 	getOutgoing (Handle h) const
//Handle 	getOutgoing (Handle h, int idx) const
//int 	getArity (Handle h) const
//bool 	isSource (Handle source, Handle link) const
//AttentionValue 	getAV (Handle h) const
//void 	setAV (Handle h, const AttentionValue &av)
//Type 	getType (Handle h) const
//const TruthValue * 	getTV (Handle h, VersionHandle vh=NULL_VERSION_HANDLE) const
//void 	setTV (Handle h, const TruthValue &tv, VersionHandle vh=NULL_VERSION_HANDLE)
//void 	setMean (Handle h, float mean)
//boost::shared_ptr< Atom > 	cloneAtom (const Handle h) const
//bool 	commitAtom (const Atom &a)
//bool 	isValidHandle (const Handle h) const
//float 	getNormalisedSTI (Handle h, bool average=true, bool clip=false) const
//float 	getNormalisedZeroToOneSTI (Handle h, bool average=true, bool clip=false) const
//size_t 	getAtomHash (const Handle h) const
//HandleSeq 	getNeighbors (const Handle h, bool fanin, bool fanout, Type linkType=LINK, bool subClasses=true) const
//HandleSeq 	getIncoming (Handle h)
//bool 	isNode (const Type t) const
//bool 	isLink (const Type t) const
//bool 	inheritsType (Type t1, Type t2) const
//std::string 	getName (Type t) const
//bool 	isNode (const Handle &h) const
//bool 	isLink (const Handle &h) const
//template<typename OutputIterator >
//OutputIterator 	getHandleSet (OutputIterator result, Type type, const std::string &name, bool subclass=true, VersionHandle vh=NULL_VERSION_HANDLE) const
//template<typename OutputIterator >
//OutputIterator 	getHandleSet (OutputIterator result, const char *name, Type type, bool subclass=true, VersionHandle vh=NULL_VERSION_HANDLE) const
//template<typename OutputIterator >
//OutputIterator 	getHandleSet (OutputIterator result, Type type, bool subclass=false, VersionHandle vh=NULL_VERSION_HANDLE) const
//template<typename OutputIterator >
//OutputIterator 	getHandleSet (OutputIterator result, Type type, Type targetType, bool subclass, bool targetSubclass, VersionHandle vh=NULL_VERSION_HANDLE, VersionHandle targetVh=NULL_VERSION_HANDLE) const
//template<typename OutputIterator >
//OutputIterator 	getHandleSet (OutputIterator result, Handle handle, Type type, bool subclass, VersionHandle vh=NULL_VERSION_HANDLE) const
//template<typename OutputIterator >
//OutputIterator 	getHandleSet (OutputIterator result, const HandleSeq &handles, Type *types, bool *subclasses, Arity arity, Type type, bool subclass, VersionHandle vh=NULL_VERSION_HANDLE) const
//template<typename OutputIterator >
//OutputIterator 	getHandleSet (OutputIterator result, const char *targetName, Type targetType, Type type, bool subclass, VersionHandle vh=NULL_VERSION_HANDLE, VersionHandle targetVh=NULL_VERSION_HANDLE) const
//template<typename OutputIterator >
//OutputIterator 	getHandleSet (OutputIterator result, const char **names, Type *types, bool *subclasses, Arity arity, Type type, bool subclass, VersionHandle vh=NULL_VERSION_HANDLE) const
//template<typename OutputIterator >
//OutputIterator 	getHandleSet (OutputIterator result, Type *types, bool *subclasses, Arity arity, Type type, bool subclass, VersionHandle vh=NULL_VERSION_HANDLE) const
//template<typename OutputIterator >
//OutputIterator 	getHandleSetInAttentionalFocus (OutputIterator result, Type type, bool subclass, VersionHandle vh=NULL_VERSION_HANDLE) const
//template<typename OutputIterator >
//OutputIterator 	getHandleSetFiltered (OutputIterator result, Type type, bool subclass, AtomPredicate *compare, VersionHandle vh=NULL_VERSION_HANDLE) const
//template<typename OutputIterator , typename Compare >
//OutputIterator 	getSortedHandleSet (OutputIterator result, Type type, bool subclass, Compare compare, VersionHandle vh=NULL_VERSION_HANDLE) const
//template<class T >
//bool 	foreach_handle_of_type (Type atype, bool(T::*cb)(Handle), T *data, bool subclass=false)
//template<class T >
//bool 	foreach_handle_of_type (const char *atypename, bool(T::*cb)(Handle), T *data, bool subclass=false)
//void 	decayShortTermImportance ()
//long 	getTotalSTI () const
//long 	getTotalLTI () const
//AttentionValue::sti_t 	getAttentionalFocusBoundary () const
//AttentionValue::sti_t 	setAttentionalFocusBoundary (AttentionValue::sti_t s)
//AttentionValue::sti_t 	getMaxSTI (bool average=true)
//AttentionValue::sti_t 	getMinSTI (bool average=true)
//void 	updateMinSTI (AttentionValue::sti_t m)
//void 	updateMaxSTI (AttentionValue::sti_t m)
//int 	Nodes (VersionHandle=NULL_VERSION_HANDLE) const
//int 	Links (VersionHandle=NULL_VERSION_HANDLE) const
//void 	clear ()
// 	Clear the atomspace, remove all atoms. 
//HandleSeq 	filter (AtomPredicate *compare, VersionHandle vh=NULL_VERSION_HANDLE)
//template<typename OutputIterator >
//OutputIterator 	filter (OutputIterator it, AtomPredicate *compare, VersionHandle vh=NULL_VERSION_HANDLE)
//template<typename InputIterator >
//HandleSeq 	filter (InputIterator begin, InputIterator end, AtomPredicate *compare) const
//template<typename InputIterator , typename OutputIterator >
//OutputIterator 	filter (InputIterator begin, InputIterator end, OutputIterator it, AtomPredicate *compare) const
//template<typename InputIterator >
//HandleSeq 	filter_InAttentionalFocus (InputIterator begin, InputIterator end) const
//AtomSpace & 	operator= (const AtomSpace &)
// 	AtomSpace (const AtomSpace &)

    public Collection<Atom> getAtoms() {
        return atomToType.keySet();
    }


}
