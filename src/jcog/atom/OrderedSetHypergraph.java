package jcog.atom;

/*
 * Adapted from Jung's "SetHypergraph.java"
 */
import java.io.Serializable;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import edu.uci.ics.jung.graph.Hypergraph;
import edu.uci.ics.jung.graph.MultiGraph;
import edu.uci.ics.jung.graph.util.EdgeType;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.concurrent.ConcurrentHashMap;

/** implementation of JUNG's hypergraph that maintains ordering of the vertices pointed to by an edge(=link) */
public class OrderedSetHypergraph<V, H> implements Hypergraph<V, H>, MultiGraph<V, H>, /*Graph<V,H>*/ Serializable {

    protected final Map<V, Set<H>> vertices; // Map of vertices to incident hyperedge sets
    protected final Map<H, List<V>> edges;    // Map of hyperedges to incident vertex sets

    public OrderedSetHypergraph() {
        this(false);
    }

    /**
     * Creates a <code>SetHypergraph</code> and initializes the internal data structures.
     */
    public OrderedSetHypergraph(boolean concurrent /*, boolean weak*/) {
        super();
        if (concurrent) {
            vertices = new ConcurrentHashMap();
            edges = new ConcurrentHashMap();
        }
        else {
            vertices = new HashMap();
            edges = new HashMap();
        }
    }

    public void clear() {
        vertices.clear();
        edges.clear();
    }


    @Deprecated
    public boolean addEdge(H h, java.util.Collection<? extends V> v) {
        assert false;
        //TODO add as a List<V>...
        return false;
    }

    @Deprecated
    public boolean addEdge(H h, java.util.Collection<? extends V> v, EdgeType arg2) {
        return addEdge(h, v);
    }

    /**
     * Adds <code>hyperedge</code> to this graph and connects them to the vertex collection <code>to_attach</code>.
     * Any vertices in <code>to_attach</code> that appear more than once will only appear once in the
     * incident vertex collection for <code>hyperedge</code>, that is, duplicates will be ignored.
     * 
     * @see Hypergraph#addEdge(Object, Collection)
     */
    public boolean addEdge(final H hyperedge, final List<V> vList) {
        if (hyperedge == null) {
            throw new IllegalArgumentException("input hyperedge may not be null");
        }

//        Set<V> new_endpoints = new HashSet<V>(to_attach);

        //TODO check for duplicate edge
        if (edges.containsKey(hyperedge)) {
            List<V> attached = edges.get(hyperedge);
            if (!attached.equals(vList)) {
                throw new IllegalArgumentException("Edge " + hyperedge
                        + " already exists in this graph with endpoints " + attached);
            } else {
                return false;
            }
        }

        edges.put(hyperedge, vList);

        for (V v : vList) {
            // add v if it's not already in the graph
            addVertex(v);

            // associate v with hyperedge
            this.vertices.get(v).add(hyperedge);
        }
        return true;
    }

    /**
     * @see Hypergraph#getEdgeType(Object)
     */
    public EdgeType getEdgeType(H edge) {
        if (containsEdge(edge)) {
            return EdgeType.UNDIRECTED;
        } else {
            return null;
        }
    }

    public boolean containsVertex(V vertex) {
        return vertices.keySet().contains(vertex);
    }

    public boolean containsEdge(H edge) {
        return edges.keySet().contains(edge);
    }

    public Collection<H> getEdges() {
        return Collections.unmodifiableCollection(edges.keySet());
    }

    public Collection<V> getVertices() {
        return Collections.unmodifiableCollection(vertices.keySet());
    }

    public int getEdgeCount() {
        return edges.size();
    }

    public int getVertexCount() {
        return vertices.size();
    }

    public Collection<V> getNeighbors(V vertex) {
        if (!containsVertex(vertex)) {
            return null;
        }

        Set<V> neighbors = new HashSet<V>();
        for (H hyperedge : vertices.get(vertex)) {
            neighbors.addAll(edges.get(hyperedge));
        }
        return Collections.unmodifiableSet(neighbors);
    }

    public Collection<H> getIncidentEdges(V vertex) {
        final Collection<H> lh = vertices.get(vertex);
        if (lh == null)
            return null;
        return Collections.unmodifiableCollection(lh);
    }

    public List<V> getIncidentVertices(H edge) {
        final List<V> lv = edges.get(edge);
        if (lv == null)
            return null;
        return Collections.unmodifiableList(lv);
    }

    public H findEdge(V v1, V v2) {
        if (!containsVertex(v1) || !containsVertex(v2)) {
            return null;
        }

        for (H h : getIncidentEdges(v1)) {
            if (isIncident(v2, h)) {
                return h;
            }
        }
        return null;
    }

    public Collection<H> findEdgeSet(V v1, V v2) {
        if (!containsVertex(v1) || !containsVertex(v2)) {
            return null;
        }

        Collection<H> edges = new LinkedList();
        for (final H h : getIncidentEdges(v1)) {
            if (isIncident(v2, h)) {
                edges.add(h);
            }
        }
        return Collections.unmodifiableCollection(edges);
    }

    public boolean addVertex(final V vertex) {
        if (containsVertex(vertex)) {
            return false;
        }
        vertices.put(vertex, new HashSet<H>());
        return true;
    }
    
    
    public boolean removeVertex(final V vertex) {
        if (!containsVertex(vertex)) {
            return false;
        }

        final List<H> toRemove = new ArrayList(getIncidentEdges(vertex));
        for (final H edge : toRemove) {
            removeEdge(edge);
        }
     
        vertices.remove(vertex);
        return true;
    }


    public boolean removeEdge(final H hyperedge) {
        if (!containsEdge(hyperedge)) {
            return false;
        }
        for (final V vertex : edges.get(hyperedge)) {
            final Set<H> l = vertices.get(vertex);
            if (l!=null)
                l.remove(hyperedge);
        }
        edges.remove(hyperedge);
        return true;
    }

    public boolean isNeighbor(final V v1, final V v2) {
        if (!containsVertex(v1) || !containsVertex(v2)) {
            return false;
        }

        if (vertices.get(v2).isEmpty()) {
            return false;
        }
        for (final H hyperedge : vertices.get(v1)) {
            if (edges.get(hyperedge).contains(v2)) {
                return true;
            }
        }
        return false;
    }

    public boolean isIncident(final V vertex, final H edge) {
        if (!containsVertex(vertex) || !containsEdge(edge)) {
            return false;
        }

        return vertices.get(vertex).contains(edge);
    }

    public int degree(final V vertex) {
        if (!containsVertex(vertex)) {
            return 0;
        }

        return vertices.get(vertex).size();
    }

    public int getNeighborCount(final V vertex) {
        if (!containsVertex(vertex)) {
            return 0;
        }

        return getNeighbors(vertex).size();
    }

    public int getIncidentCount(final H edge) {
        if (!containsEdge(edge)) {
            return 0;
        }

        return edges.get(edge).size();
    }

    public int getEdgeCount(final EdgeType edge_type) {
        if (edge_type == EdgeType.DIRECTED) {
            return edges.size();
        }
        return 0;
    }

    public Collection<H> getEdges(EdgeType edge_type) {
        if (edge_type == EdgeType.DIRECTED) {
            return Collections.unmodifiableSet(edges.keySet());
        }
        return null;
    }

    public EdgeType getDefaultEdgeType() {
        return EdgeType.DIRECTED;
    }

//    public Collection<H> getInEdges(V vertex) {
//        return getIncidentEdges(vertex);
//    }
//
//    public Collection<H> getOutEdges(V vertex) {
//        return getIncidentEdges(vertex);
//    }

//    public int inDegree(V vertex) {
//        return degree(vertex);
//    }
//
//    public int outDegree(V vertex) {
//        return degree(vertex);
//    }

    public V getDest(H directed_edge) {
        return null;
    }

    public V getSource(H directed_edge) {
        return null;
    }

    public Collection<V> getPredecessors(final V vertex) {
        return getNeighbors(vertex);
    }

    public Collection<V> getSuccessors(V vertex) {
        return getNeighbors(vertex);
    }

//    @Override
//    public boolean isPredecessor(V v, V v1) {
//        throw new UnsupportedOperationException("Not supported yet.");
//    }
//
//    @Override
//    public boolean isSuccessor(V v, V v1) {
//        throw new UnsupportedOperationException("Not supported yet.");
//    }
//
//    @Override
//    public int getPredecessorCount(V v) {
//        return getIncidentEdges(v).size();
//    }
//
//    @Override
//    public int getSuccessorCount(V v) {
//        return getIncidentVertices((H)v).size();
//    }
//
//    @Override
//    public boolean isSource(V v, H e) {
//        return getIncidentVertices(e).contains(v);
//    }
//
//    @Override
//    public boolean isDest(V v, H e) {
//        return getIncidentVertices(e).contains(v);
//    }
//
//    @Override
//    public boolean addEdge(H e, V v, V v1) {
//        throw new UnsupportedOperationException("Not supported yet.");
//    }
//
//    @Override
//    public boolean addEdge(H e, V v, V v1, EdgeType et) {
//        throw new UnsupportedOperationException("Not supported yet.");
//    }
//
//    @Override
//    public Pair<V> getEndpoints(H e) {
//        return new Pair<V>((V)e, getIncidentVertices(e).get(0));
//    }
//
//    @Override
//    public V getOpposite(V v, H e) {
//        List<V> ll = getIncidentVertices(e);
//        return ll.get(ll.size() - 1);
//    }
}