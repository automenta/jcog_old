/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.opencog.atom;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.syncleus.dann.graph.AbstractHyperEdge;
import com.syncleus.dann.graph.MutableDirectedAdjacencyGraph;
import com.syncleus.dann.graph.MutableHyperAdjacencyGraph;
import edu.uci.ics.jung.graph.util.Pair;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.TreeMap;
import jcog.atom.OrderedSetHypergraphDANN;
import jcog.opencog.Atom;
import jcog.opencog.AtomType;
import jcog.opencog.Operation;
import jcog.opencog.swing.graph.HyperedgeSegment;
import org.apache.commons.collections15.IteratorUtils;
import org.apache.commons.collections15.Predicate;
import org.apache.log4j.Logger;

/**
 *
 * @author seh
 */
public class MemoryAtomSpaceDANN implements ReadableAtomSpace, EditableAtomSpace {

    public static class AtomEdge extends AbstractHyperEdge<Atom> {
        public final Atom atom;

        public AtomEdge(Atom e, List<Atom> nodes) {
            super(nodes);
            this.atom = e;
        }

        public AtomEdge(Atom e, Atom... nodes) {
            super(nodes);
            this.atom = e;
        }

        public Atom getAtom() {
            return atom;
        }
                
    }
    
    public static class AtomHypergraph extends MutableHyperAdjacencyGraph<Atom, AtomEdge> {
        
    }
    
    final static Logger logger = Logger.getLogger(MemoryAtomSpaceDANN.class);
    
    //private final OrderedSetHypergraph<Atom, Atom> graph;
    private final AtomHypergraph graph;
    
    private final Map<Atom, AtomData> atomData;
    private Multimap<Class<? extends AtomType>, Atom> typesToAtom;
    protected NavigableMap<Atom, AttentionValue> attentionSortedBySTI;
    private short minSTISeen = 0, maxSTISeen = 0;    
    
//    private Map<Atom, Class<? extends AtomType>> atomToType;
//    private Map<Atom, String> names;
//    private Map<Atom, TruthValue> truth;
//    private Map<Atom, AttentionValue> attention;
    

    public MemoryAtomSpaceDANN() {
        super();
        graph = new AtomHypergraph();
        atomData = new HashMap();                
        typesToAtom = HashMultimap.create();
        
        clear();
    }
    
    @Override
    public void clear() { 
        graph.clear();
        atomData.clear();
        typesToAtom.clear();
    
        updateIndexes();
    }
    
    
    public AtomData getData(final Atom a) {
        return atomData.get(a);
    }
    
    protected void indexAtom(final Atom a, final Class<? extends AtomType> type, final String name) {       
        final AtomData ad = new AtomData(type, name, newDefaultTruthValue(a), newDefaultAttentionValue(a));
        atomData.put(a, ad);
        
        typesToAtom.put(type, a);        //TODO check if this preserves previously-added types
        
    }
    
    protected void unindexAtom(final Atom a) {
        Class<? extends AtomType> t = getType(a);
        typesToAtom.remove(t, a);      //TODO check if this preserves previously-added types
        atomData.remove(a);
    }
    
    public Atom addEdge(Class<? extends AtomType> t, List<Atom> members) {
        Atom[] ma = new Atom[members.size()];
        members.toArray(ma);    
        return addEdge(t, ma);
    }

    public Atom addEdge(Class<? extends AtomType> t, Atom... members) {
        return addEdge(t, null, members);
    }


    public Atom addVertex(Class<? extends AtomType> type) {
        final Atom a = new Atom();
        if (addVertex(type, a, a.toString())) {
            return a;
        }
        return null;
    }

    public Atom addVertex(Class<? extends AtomType> type, String name) {
        final Atom a = new Atom();
        if (addVertex(type, a, name)) {
            return a;
        }
        return null;
    }

    @Override
    public boolean addVertex(Class<? extends AtomType> type, Atom a, String name)  {
        graph.add(a);
        indexAtom(a, type, name);
        return true;        
    }
    
    @Override
    public Atom addEdge(Class<? extends AtomType> t, String name, Atom... members) {        
        
        //Unmodifiable list
        //final List<Atom> memberList = Arrays.asList(members); 
        
        final List<Atom> memberList = new ArrayList(members.length);
        for (int i = 0; i < members.length; i++)
            memberList.add(members[i]); 
        
        //preventing duplicate edges (type, members). allows updating the name if already exists
        
        for (final Atom eaa : typesToAtom.get(t)) {
            boolean allowNameChange = false;
            if (graph.getIncidentVertices(eaa) == null) {
                if (members.length == 0) {
                    allowNameChange = true;
                }
            }
            else if (graph.getIncidentVertices(eaa).equals(memberList)) {
                allowNameChange = true;
            }
            
            if (allowNameChange) {
                String oldName = getName(eaa);
                if (name!=null) {
                    if (oldName == null) oldName = "";
                    if (!oldName.equals(name)) {
                        logger.info("Renaming " + eaa + "{name=" + oldName + ", type=" + t + "} to: " + name);
                    }
                }
                else {
                    if (oldName != null) {
                        logger.info("Renaming " + eaa + "{name=" + oldName +", type=" + t + "} to: NULL");
                    }

                }
                return eaa;
            }
        }

        Atom e = new Atom();

        graph.addEdge(e, memberList);
        
        indexAtom(e, t, name);
        
        
        return e;        
    }
    
//    protected boolean removeVertex(Atom a) {
//        if (graph.containsVertex(a)) {
//            unindexAtom(a);       
//            graph.removeVertex(a);
//            return true;        
//        }
//     
//        return false;
//    }
//    
//    protected boolean removeEdge(Atom e) {
//        if (graph.containsEdge(e)) {
//            unindexAtom(e);
//            graph.removeEdge(e);
//            return true;
//        }
//        
//        return false;        
//    }

    public TruthValue getTruth(Atom a) {
        return getData(a).truth;
    }

    public AttentionValue getAttention(Atom a) {
        return getData(a).attention;
    }

    /** called by UpdateImportance -- indicates the min and max STI seen by update importance as it iterates across all atoms utilized by Agents */
    public void setSTIRange(short minSTISeen, short maxSTISeen) {
        this.minSTISeen = minSTISeen;
        this.maxSTISeen = maxSTISeen;
    }

    public TruthValue newDefaultTruthValue(Atom a) {
        return new SimpleTruthValue();
    }

    public AttentionValue newDefaultAttentionValue(Atom a) {
        return new AttentionValue(true);
    }
    
    final static Collection<Atom> emptyAtomsList = Collections.unmodifiableCollection(new LinkedList());
    
    @Override
    public Collection<Atom> getIncidentEdges(final Atom vertex) {
        //Collection<Atom> gg = graph.getIncidentEdges(vertex);
        Collection<AtomEdge> gg = graph.getAdjacentEdges(vertex);
        
        return (gg!=null) ? Collections.unmodifiableCollection(gg): emptyAtomsList;
    }
    
    @Override
    public Collection<Atom> getIncidentVertices(final Atom edge) {
        Collection<Atom> gg = graph.getIncidentVertices(edge);
        return (gg!=null) ? Collections.unmodifiableCollection(graph.getIncidentVertices(edge)) : emptyAtomsList;
    }
    
    public static boolean isEqual(final Collection<Atom> a, final Atom[] b) {
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
    public Atom getEdge(Class<? extends AtomType> type, Atom... members) {
        if (members.length == 0)
            return null;
        
        Collection<Atom> incidentEdges = getIncidentEdges(members[0]);
        if (incidentEdges == null)
            return null;
        
        for (Atom e : incidentEdges) {
            if (getType(e).equals(type)) {
                Collection<Atom> iv = getIncidentVertices(e);
                if (isEqual(iv, members))
                    return e;
            }
        }
        
        return null;
    }
        
    
    public Collection<Atom> getAtoms(Class<? extends AtomType> type, boolean includeSubtypes) {
        if (!includeSubtypes) {
            //TODO unmodifiable
            return Collections.unmodifiableCollection(typesToAtom.get(type));
        }
        else {
            List<Atom> la = new LinkedList();
            for (Class<? extends AtomType> ca : typesToAtom.keySet()) {
                if (type.isAssignableFrom(ca)) {
                    la.addAll(typesToAtom.get(ca));
                }
            }
            return Collections.unmodifiableCollection(la);
        }
    }
    
    public Class<? extends AtomType> getType(final Atom a) {
        return getData(a).type;
    }
    
    public String getName(final Atom a) {
        return getData(a).name;
    }           
    
    public int getArity(final Atom e) {        
        return graph.getIncidentCount(e);
    }
    
    public boolean containsAtom(final Atom a) {
        if (graph.containsVertex(a))
            return true;
        if (graph.containsEdge(a))
            return true;
        return false;
    }
    
    public boolean containsVertex(Atom a) {
        return graph.containsVertex(a);
    }
    public boolean containsEdge(Atom a) {
        return graph.containsEdge(a);
    }
    

    @Override
    public boolean visitEdges(Predicate<Atom> predicate, Operation<ReadableAtomSpace, Atom> op) {
        for (Atom e : getEdges()) {
            if (predicate.evaluate(e)) {
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
            if (predicate.evaluate(e)) {
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
        return Collections.unmodifiableCollection(graph.getVertices());
    }
    
    @Override
    public Collection<Atom> getEdges() {
        return Collections.unmodifiableCollection(graph.getEdges());
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
    
    
    public Iterator<Atom> iterateAtoms() {
        return IteratorUtils.chainedIterator(iterateVertices(), iterateEdges());
    }

    /** removes either an edge or a vertex from the hypergraph.  if it is not a vertex, it tries to remove it as an edge. */
    @Override
    public boolean remove(Atom a) {
//        if (!removeVertex(a)) {
//            if (!removeEdge(a)) {
//                return false;    
//            }
//        }
        graph.removeVertex(a);
        graph.removeEdge(a);
        unindexAtom(a);
        return true;
    }


    public int getVertexCount() {
        return getVertices().size();
    }

    public int getEdgeCount() {
        return getEdges().size();
    }

    //TODO debug this
    public int getOrderInIncidentEdge(Atom a, Atom edge) {
        int i = 0;
        if (getIncidentEdges(edge) == null) {
            return -1;
        }
        for (Atom p : getIncidentEdges(edge)) {
            if (p.equals(a)) {
                return i;
            }
            i++;
        }
        return -1;
    }
    
    public String getTypeName(final Atom a) {
        return getType(a).getSimpleName();
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

    public int getIncidentEdgeDegree(Atom a) {
        Collection<Atom> ie = getIncidentEdges(a);
        if (ie == null)
            return 0;
        return ie.size();
    }

    public int getIncidentVertexDegree(Atom a) {
        Collection<Atom> ie = getIncidentVertices(a);
        if (ie == null)
            return 0;
        return ie.size();
    }

    public boolean setName(Atom a, String newName) {
        if (containsAtom(a)) {
            getData(a).name = newName;
            return true;
        }
        
        return false;        
    }
    
    protected void updateIndexes() {
        updateSTISort();
    }

    protected void updateSTISort() {
        attentionSortedBySTI = new TreeMap<Atom, AttentionValue>(new Comparator<Atom>() {

            @Override
            public int compare(Atom a, Atom b) {
                short sa = getSTI(a);
                short sb = getSTI(b);

                if (sa == sb) {
                    return -1;
                }
                return (sa > sb) ? -1 : 1;
            }
        });
        
        for (final Entry<Atom, AtomData> ea : atomData.entrySet()) {
            attentionSortedBySTI.put(ea.getKey(), ea.getValue().attention);            
        }
    }
    
    /** returns an atom's STI normalized to -1..+1 range */
    public double getNormalizedSTI(Atom a) {
        return getNormalizedSTI(a, maxSTISeen, minSTISeen);
    }

    public double getNormalizedSTI(Atom a, double maxSTI, double minSTI) {
        double sti = getSTI(a);

        if (maxSTI != minSTI) {
            return (sti - ((double) minSTI)) / (double) (maxSTI - minSTI);
        } else {
            return 0;
        }
    }

    public void setVLTI(Atom a, int newVLTI) {
        getAttention(a).setVLTI(newVLTI);
    }

    public short getSTI(Atom a) {
        return getAttention(a).getSTI();
    }

    public short getLTI(Atom a) {
        return getAttention(a).getLTI();
    }
    
    public Pair<Short> getSTIRange(Collection<Atom> atoms) {
        boolean first = true;

        short minSTI = 0, maxSTI = 0;

        for (Atom x : atoms) {
            if (first) {
                minSTI = maxSTI = getSTI(x);
                first = false;
            } else {
                short s = getSTI(x);
                if (s < minSTI) {
                    minSTI = s;
                }
                if (s > maxSTI) {
                    maxSTI = s;
                }
            }
        }

        return new Pair<Short>(minSTI, maxSTI);
    }
   
    public short getMaxSeenSTI() {
        return maxSTISeen;
    }

    public short getMinSeenSTI() {
        return minSTISeen;
    }

    public AtomHypergraph getHypergraph() {
        return graph;
    }

    /**
     * Creates a <code>Graph</code> which is an edge-folded version of <code>h</code>, where
     * hyperedges are replaced by k-cliques in the output graph.
     *
     * <p>The vertices of the new graph are the same objects as the vertices of
     * <code>h</code>, and <code>a</code>
     * is connected to <code>b</code> in the new graph if the corresponding vertices
     * in <code>h</code> are connected by a hyperedge.  Thus, each hyperedge with
     * <i>k</i> vertices in <code>h</code> induces a <i>k</i>-clique in the new graph.</p>
     *
     * <p>The edges of the new graph are generated by the specified edge factory.</p>
     *
     * @param <V> vertex type
     * @param <E> input edge type
     * @param h hypergraph to be folded
     * @param graph_factory factory used to generate the output graph
     * @param edge_factory factory used to create the new edges
     * @return a copy of the input graph where hyperedges are replaced by cliques
     */
    public MutableDirectedAdjacencyGraph<Atom, HyperedgeSegment> foldHypergraphEdges(final Collection<Atom> vertices, final MutableDirectedAdjacencyGraph<Atom, HyperedgeSegment> target, final boolean linkEdgeToMembers) {
        AtomHypergraph h = getHypergraph();
        
        for (final Atom v : vertices) {
            target.add(v);
        }
        for (final AtomEdge e : h.getEdges()) {
            final Atom ea = e.getAtom();
            
            boolean contained = true;
            for (final Atom iv : e.getNodes()) {
                if (!vertices.contains(iv)) {
                    contained = false;
                    break;
                }
            }
            if (!contained) {
                continue;
            }
            
            ArrayList<Atom> incident = new ArrayList(e.getNodes());
            if (incident.size() == 0) {
                target.add(ea);
                continue;
            }
            else if (incident.size() == 2) {
                Atom a = incident.get(0);
                Atom b = incident.get(1);
                target.add(new HyperedgeSegment(a, b, ea, ""));
            }
            else {
                target.add(ea);
                if (linkEdgeToMembers) {
                    for (int i = 0; i < incident.size(); i++) {
                        Atom i1 = incident.get(i);
                        if (i == 0) {
                            target.add(new HyperedgeSegment(ea, i1, ea, "("));
                        } else {
                            target.add(new HyperedgeSegment(incident.get(i - 1), i1, ea, ""));
                        }
                    }
                } else {
                    final String typeString = getType(ea).toString();
                    //Just link the edge to the first element
                    for (int i = 0; i < incident.size(); i++) {
                        if (i > 0) {
                            target.add(new HyperedgeSegment(incident.get(i - 1), incident.get(i), ea, Integer.toString(i)));
                        } else {
                            target.add(new HyperedgeSegment(ea, incident.get(i), ea, "(" + typeString));
                        }
                    }
                }
                
            }
        }
        return target;
    }
    
    public MutableDirectedAdjacencyGraph<Atom, HyperedgeSegment> foldHypergraphEdges() {
        return foldHypergraphEdges(getVertices(), new MutableDirectedAdjacencyGraph<Atom, HyperedgeSegment>(), true);
    }
    
    
}
