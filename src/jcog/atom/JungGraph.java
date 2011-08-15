/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.atom;

import com.syncleus.dann.graph.DirectedEdge;
import com.syncleus.dann.graph.Edge;
import com.syncleus.dann.graph.Graph;
import com.syncleus.dann.graph.topological.Topography;
import edu.uci.ics.jung.graph.util.EdgeType;
import edu.uci.ics.jung.graph.util.Pair;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

/**
 * Adapts a dANN Graph to a JUNG graph interface.  Useful for JUNG's visualization and graph algorithm implementations.
 * @author seh
 * @param <N> Node type
 * @param <E> Edge type
 */
public class JungGraph<N, E extends Edge<N>> implements edu.uci.ics.jung.graph.Graph<N, E> {

    final Graph<N, E> dannGraph;
	private EdgeType defaultEdgeType;

    public JungGraph(Graph<N, E> dannGraph) {
        super();
        this.dannGraph = dannGraph;
        this.defaultEdgeType = null;
    }

    public void setDefaultEdgeType(EdgeType defaultEdgeType) {
		this.defaultEdgeType = defaultEdgeType;
    }

    public EdgeType getDefaultEdgeType() {
		if (defaultEdgeType == null) {
			return (getEdgeCount(EdgeType.DIRECTED) > getEdgeCount(EdgeType.UNDIRECTED)) ? EdgeType.DIRECTED : EdgeType.UNDIRECTED;
		} else {
			return defaultEdgeType;
		}
    }

    public Collection<E> getIncidentEdges(N v) {
        return dannGraph.getAdjacentEdges(v);
    }

    public Collection<N> getIncidentVertices(E e) {
        return e.getNodes();
//        if (e instanceof DirectedEdge) {
//            List<N> ll = new LinkedList();
//            DirectedEdge d = (DirectedEdge)e;
//            ll.add(d.getSourceNode());
//            ll.add(d.getDestinationNode());
//            return ll;
//        }
//        else {
//            return e.getNodes();
//        }
    }

    public E findEdge(N v, N v1) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public Collection<E> findEdgeSet(N v, N v1) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public Collection<E> getInEdges(N v) {
        List<E> inEdges = new LinkedList();
        Set<E> edges = dannGraph.getAdjacentEdges(v);
        for (E e : edges) {
            if (e instanceof DirectedEdge) {
                if (((DirectedEdge) e).getDestinationNode() == v) {
                    inEdges.add(e);
                }
            }
        }
        return inEdges;
    }

    public Collection<E> getOutEdges(N v) {
        List<E> outEdges = new LinkedList();
        Set<E> edges = dannGraph.getAdjacentEdges(v);
        for (E e : edges) {
            if (e instanceof DirectedEdge) {
                if (((DirectedEdge) e).getSourceNode() == v) {
                    outEdges.add(e);
                }
            }
        }
        return outEdges;
    }

    public Collection<N> getPredecessors(N v) {
        Collection<E> inEdges = getInEdges(v);
        Set<N> p = new HashSet<N>(inEdges.size());
        for (E e : inEdges) {
            if (e instanceof DirectedEdge) {
                p.add(((DirectedEdge<N>) e).getSourceNode());
            }
        }
        return p;
    }

    public Collection<N> getSuccessors(N v) {
        Collection<E> outEdges = getOutEdges(v);
        Set<N> p = new HashSet<N>(outEdges.size());
        for (E e : outEdges) {
            if (e instanceof DirectedEdge) {
                p.add(((DirectedEdge<N>) e).getDestinationNode());
            }
        }
        return p;
    }

    public boolean isPredecessor(N a, N b) {
        return getPredecessors(a).contains(b);
    }

    public boolean isSuccessor(N a, N b) {
        return getSuccessors(a).contains(b);
    }

    public int getPredecessorCount(N v) {
        return getPredecessors(v).size();
    }

    public int getSuccessorCount(N v) {
        return getSuccessors(v).size();
    }

    public int inDegree(N v) {
        //TODO this may be sped up by not needing to create a list in getInEdges
        return getInEdges(v).size();
    }

    public int outDegree(N v) {
        //TODO this may be sped up by not needing to create a list in getOutEdges
        return getOutEdges(v).size();
    }

    public N getSource(E e) {
        if (e instanceof DirectedEdge) {
            return ((DirectedEdge<N>) e).getSourceNode();
        }
        return null;
    }

    public N getDest(E e) {
        if (e instanceof DirectedEdge) {
            return ((DirectedEdge<N>) e).getDestinationNode();
        }
        return null;
    }

    public boolean isSource(N v, E e) {
        if (e instanceof DirectedEdge) {
            return ((DirectedEdge<N>) e).getSourceNode() == v;
        }
        return false;
    }

    public boolean isDest(N v, E e) {
        if (e instanceof DirectedEdge) {
            return ((DirectedEdge<N>) e).getDestinationNode() == v;
        }
        return false;
    }

    public Pair<N> getEndpoints(E e) {
        if (e instanceof DirectedEdge) {
            DirectedEdge<N> d = (DirectedEdge<N>) e;
            return new Pair<N>(d.getSourceNode(), d.getDestinationNode());
        }
        return new Pair<N>(e.getNodes().get(0), e.getNodes().get(1));
    }

    public N getOpposite(N v, E e) {
        if (e instanceof DirectedEdge) {
            DirectedEdge<N> d = (DirectedEdge<N>) e;
            if (d.getSourceNode() == v) {
                return d.getDestinationNode();
            } else {
                return d.getSourceNode();
            }
        } else {
            if (e.getNodes().size() == 2) {
                N a = e.getNodes().get(0);
                N b = e.getNodes().get(1);
                if (v == a) {
                    return b;
                } else {
                    return a;
                }
            }
            else /* if nodes size == 1 */{
                return v;
            }
        }
    }

    public Collection<E> getEdges() {
        return dannGraph.getEdges();
    }

    public Collection<N> getVertices() {
        return dannGraph.getNodes();
    }

    public boolean containsVertex(N v) {
        return getVertices().contains(v);
    }

    public boolean containsEdge(E e) {
        return getEdges().contains(e);
    }

    public int getEdgeCount() {
        return getEdges().size();
    }

    public int getVertexCount() {
        return getVertices().size();
    }

    public Collection<N> getNeighbors(N v) {
        return dannGraph.getAdjacentNodes(v);
    }

    public boolean isNeighbor(N a, N b) {
        return dannGraph.getAdjacentNodes(a).contains(b);
    }

    public boolean isIncident(N v, E e) {
        return dannGraph.getAdjacentEdges(v).contains(e);
    }

    public int degree(N v) {
        return Topography.getDegree(dannGraph, v);
    }

    public int getNeighborCount(N v) {
        return getNeighbors(v).size();
    }

    public int getIncidentCount(E e) {
        return getIncidentVertices(e).size();
    }

    public EdgeType getEdgeType(E e) {
        if (e instanceof DirectedEdge) {
            return EdgeType.DIRECTED;
        } else {
            return EdgeType.UNDIRECTED;
        }
    }

    public Collection<E> getEdges(EdgeType et) {
        List<E> le = new LinkedList();
        for (E e : getEdges()) {
            if (et == EdgeType.DIRECTED) {
                if (e instanceof DirectedEdge) {
                    le.add(e);
                }
            }
            else {
                if (!(e instanceof DirectedEdge)) {
                    le.add(e);
                }
            }
        }
        return le;
    }

    public int getEdgeCount(EdgeType et) {
        return getEdges(et).size();
    }

    public boolean addEdge(E e, N v, N v1) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public boolean addEdge(E e, N v, N v1, EdgeType et) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public boolean addVertex(N v) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public boolean addEdge(E e, Collection<? extends N> clctn) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public boolean addEdge(E e, Collection<? extends N> clctn, EdgeType et) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public boolean removeVertex(N v) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public boolean removeEdge(E e) {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}