package jcog.opencog;

import com.syncleus.dann.graph.DirectedEdge;
import com.syncleus.dann.graph.ImmutableDirectedEdge;
import com.syncleus.dann.graph.MutableDirectedAdjacencyGraph;
import java.util.HashSet;
import java.util.Set;
import org.apache.log4j.Logger;

/**
 * Default built-in Atom types
 * 
 * @see http://opencog.org/wiki/Category:Atom_Types
 * @see http://opencog.org/wiki/Node_types
 * @see http://opencog.org/wiki/Link_types
 * @see http://wiki.opencog.org/w/MindOntology:Self-Modifying_Evolving_Probabilistic_Hypergraph
 * TODO add text descriptions for each type as metadata in the atomspace
 */
public class AtomTypes {
    private static final Logger logger = Logger.getLogger(AtomTypes.class);
    
    public static final OCType NoType = new OCType("NoType");
    
    public static final OCType AtomType = new OCType("AtomType");
    
    //Vertex Only
    public static final OCType NodeType = new OCType("NodeType");
    
    //Edge Only
    public static final OCType LinkType = new OCType("LinkType");        
    
    public static final OCType ConceptNode = new OCType("ConceptNode");
    public static final OCType NumberNode = new OCType("NumberNode");
    
    
    public static final OCType OrderedLink = new OCType("OrderedLink");
    public static final OCType UnorderedLink = new OCType("UnorderedLink");
    
    public static final OCType PROCEDURE_NODE = new OCType("ProcedureNode");
    public static final OCType GROUNDED_PROCEDURE_NODE = new OCType("GroundedProcedureNode");
    public static final OCType SCHEMA_NODE = new OCType("SchemaNode");
    public static final OCType GROUNDED_SCHEMA_NODE = new OCType("GroundedSchemaNode");
    public static final OCType SymmetricHebbianLink = new OCType("SymmetricHebbianLink");
    
    public static final OCType InheritsLink = new OCType("InheritsLink");
    
    
    public static final OCType SIMILARITY_LINK = new OCType("SimilarityLink");
    public static final OCType INTENSIONAL_INHERITANCE_LINK = new OCType("IntensionInheritLink");
    public static final OCType EXTENSIONAL_INHERITANCE_LINK = new OCType("ExtensionInheritLink");
    public static final OCType EXTENSIONAL_SIMILARITY_LINK = new OCType("ExtensionSimilarityLink");

    MutableDirectedAdjacencyGraph<OCType, ImmutableDirectedEdge<OCType>> inheritance = new MutableDirectedAdjacencyGraph<OCType, ImmutableDirectedEdge<OCType>>();
    
    protected void addType(OCType type, OCType... supertypes) {
        inheritance.add(type);
        
        for (OCType s : supertypes) {
            if (!inheritance.getNodes().contains(s)) {
                logger.error("supertype " + s + " not present when defining type " + type);
            }
            else {
                inheritance.add(new ImmutableDirectedEdge<OCType>(s, type));
            }            
        }
        
    }
    
    protected void addToAtomSpace(MemoryAtomSpace c) {
        //System.out.println(inheritance.getNodes());
        //System.out.println(inheritance.getEdges());
        
//        if (Cycles.getCycleCount(inheritance) > 0) {
//            ExhaustiveDepthFirstSearchCycleFinder<OCType, ImmutableDirectedEdge<OCType>> f = new ExhaustiveDepthFirstSearchCycleFinder();
//            logger.error("inheritance tree has cycles: " + f.findCycles(inheritance));
//            return;
//        }
        
        for (OCType t : inheritance.getNodes()) {
            c.addVertex(Atom.Type, t);
        }
        
        for (OCType t : inheritance.getNodes()) {
            //System.out.println(t + " has children: " + inheritance.getTraversableNodes(t));
            Set<OCType> parents = new HashSet();
            for (DirectedEdge<OCType> e : inheritance.getAdjacentEdges(t)) {
                if (e.getDestinationNode() == t)
                    parents.add(e.getSourceNode());
            }
            //System.out.println(t + " has parents: " + parents);
            
            if (parents.size() > 0) {
                OCType[] i = new OCType[parents.size() + 1];
                i[0] = t;
                int j = 1;
                for (OCType p : parents) {
                    i[j++] = p;
                }
                c.addEdge(InheritsLink, i);
            }
            
        }
        
    }
    
    public AtomTypes(MemoryAtomSpace c) {

        // Special type designating that no atom type has been assigned
        addType(NoType);

        // Base of hierarchy
        addType(AtomType);

        //NODE <- ATOM
        addType(NodeType, AtomType);

        //LINK <- ATOM
        addType(LinkType, AtomType);

        //CONCEPT_NODE <- NODE
        addType(ConceptNode, NodeType);

        //NUMBER_NODE <- NODE
        addType(NumberNode, NodeType);


        // Basic Links
        //		ORDERED_LINK <- LINK
        addType(OrderedLink, LinkType);

        //		UNORDERED_LINK <- LINK
        addType(UnorderedLink, LinkType);


//		// Basic sets
//		SET_LINK <- UNORDERED_LINK
//		SUBSET_LINK <- ORDERED_LINK
//		LIST_LINK <- ORDERED_LINK
//		MEMBER_LINK <- ORDERED_LINK
//
//		// Boolean set operations, logical constants
//		AND_LINK <- ORDERED_LINK
//		OR_LINK <- UNORDERED_LINK
//		NOT_LINK <- UNORDERED_LINK
//		FALSE_LINK <- ORDERED_LINK
//		TRUE_LINK <- ORDERED_LINK
//
//		// Universe of discourse
//		CONTEXT_LINK <- ORDERED_LINK
//
//		// Basic first-order logic ungrounded terms and term types
//		VARIABLE_NODE <- NODE
//		VARIABLE_TYPE_NODE <- NODE
//		TYPED_VARIABLE_LINK <- ORDERED_LINK
//
//		// Scoping and Logical quantifiers
//		VARIABLE_SCOPE_LINK <- ORDERED_LINK
//		FORALL_LINK <- VARIABLE_SCOPE_LINK
//		EXIST_LINK <- VARIABLE_SCOPE_LINK
//		SCHOLEM_LINK <- ORDERED_LINK
//
//		// Basic first-order logic operations
//		IMPLICATION_LINK <- ORDERED_LINK
//		EVALUATION_LINK <- ORDERED_LINK
//
//		// ??? symbol link not used anywhere. What is it's semantics?
//		// SYMBOL_LINK <- ORDERED_LINK
//
//		// Generic association
//		ASSOCIATIVE_LINK <- ORDERED_LINK
//
//		// Inheritence and association links
//		INHERITANCE_LINK <- ORDERED_LINK
        addType(InheritsLink, OrderedLink);

//		SIMILARITY_LINK <- UNORDERED_LINK
        addType(SIMILARITY_LINK, UnorderedLink);

//		EXTENSIONAL_INHERITANCE_LINK <- ORDERED_LINK
        addType(EXTENSIONAL_INHERITANCE_LINK, OrderedLink);

//		EXTENSIONAL_SIMILARITY_LINK <- ORDERED_LINK
        addType(EXTENSIONAL_SIMILARITY_LINK, OrderedLink);
        
//		INTENSIONAL_INHERITANCE_LINK <- ORDERED_LINK
        addType(INTENSIONAL_INHERITANCE_LINK, OrderedLink);

//		INTENSIONAL_SIMILARITY_LINK <- ORDERED_LINK
//
//		// Procedure, schema and predicate nodes.
//		PROCEDURE_NODE <- NODE
        addType(PROCEDURE_NODE, NodeType);

//		GROUNDED_PROCEDURE_NODE <- PROCEDURE_NODE
        addType(GROUNDED_PROCEDURE_NODE, PROCEDURE_NODE);

//		SCHEMA_NODE <- PROCEDURE_NODE
        addType(SCHEMA_NODE, PROCEDURE_NODE);

//		GROUNDED_SCHEMA_NODE <- SCHEMA_NODE,GROUNDED_PROCEDURE_NODE		
        addType(GROUNDED_SCHEMA_NODE, SCHEMA_NODE, GROUNDED_PROCEDURE_NODE);

//
//		PREDICATE_NODE <- SCHEMA_NODE
//		GROUNDED_PREDICATE_NODE <- PREDICATE_NODE,GROUNDED_PROCEDURE_NODE
//
//		SATISFYING_SET_LINK <- ORDERED_LINK
//
//		SCHEMA_EXECUTION_LINK <- INHERITANCE_LINK
//		SCHEMA_EVALUATION_LINK <- SCHEMA_EXECUTION_LINK
//
//		EXECUTION_LINK <- ORDERED_LINK
//		EXECUTION_OUTPUT_LINK <- ORDERED_LINK
//
//		// Temporal Links
//
//		PREDICTIVE_IMPLICATION <- ORDERED_LINK
//		TAIL_PREDICTIVE_IMPLICATION <- ORDERED_LINK
//		SEQUENTIAL_AND_LINK <- AND_LINK
//		SIMULTANEOUS_AND_LINK <- AND_LINK
//
//		EVENTUAL_SEQUENTIAL_AND_LINK <- ORDERED_LINK "EventualSequentialAND"
//		EVENTUAL_PREDICTIVE_IMPLICATION_LINK <- ORDERED_LINK "EventualSequentialImplication"
//
//		/// PLN types:
//		// These are used in PLN currently, but can perhaps be replaced/removed
//		HYPOTHETICAL_LINK <- ORDERED_LINK
//		MIXED_IMPLICATION_LINK <- ORDERED_LINK
//		EXTENSIONAL_IMPLICATION_LINK <- ORDERED_LINK
//
//		// Should be replaced by entensional and intensional equivalence
//		EQUIVALENCE_LINK <- UNORDERED_LINK
//		EXTENSIONAL_EQUIVALENCE_LINK <- UNORDERED_LINK
//
//
//		// Older parsing and language handling nodes;
//		// Commented out, as these are currently unused,
//		// and are being replaced by newer RelEx forms.
//		// Some of these may be revived. See below.
//		//
//		// PHRASE_NODE <- NODE
//		// COLLOCATION_NODE <- WORD_NODE
//		// SEQUENCE_LINK <- ORDERED_LINK
//		// 
//		// TENSE_LINK <- FEATURE_LINK
//		// QUANTITY_LINK <- FEATURE_LINK
//		// NOUN_NUMBER_LINK <- FEATURE_LINK
//		// WORD_GENDER_LINK <- FEATURE_LINK
//		//
//		// TENSE_NODE <- FEATURE_NODE
//		// NOUN_NUMBER_NODE <- FEATURE_NODE
//		// WORD_GENDER_NODE <- FEATURE_NODE
//		// FLAG_NODE <- NODE
//		//
//		// QUERY_VARIABLE_LINK <- ORDERED_LINK
//		//
//		// New RelEx nodes, added by Linas per names used by Ben
//		// These are subject to change or deletion pending review.
//		// Some of the link types may go away.
//
//		// Synchronization primitive, used to mark stages of a processing
//		// pipeline; used as a fence or guard barrier, make sure that earlier
//		// stages have completed processing before later ones start.
//		// Basically, hypergraphs may be attached to the anchor, with a 
//		// ListLink, to indicate that they are either done, or awaiting
//		// processing.
//		ANCHOR_NODE <- NODE
//
//		// Holder of statistical truth values. 
//		// XXX not clear if this is used anywhere ... maybe should remove this.
//		COUNT_LINK <- ORDERED_LINK
//
//		WORD_NODE <- NODE
//		REFERENCE_LINK <- ASSOCIATIVE_LINK
//
//		// Custom atoms to identify documents, sentences, word instances
//		// These are emitted by RelEx and are used to input text into OpenCog.
//		DOCUMENT_NODE <- CONCEPT_NODE
//		SENTENCE_NODE <- CONCEPT_NODE
//		PARSE_NODE <- CONCEPT_NODE
//		PARSE_LINK <- ASSOCIATIVE_LINK
//		WORD_INSTANCE_NODE <- CONCEPT_NODE
//		WORD_INSTANCE_LINK <- ASSOCIATIVE_LINK
//
//		// Custom atoms to identify features, relations
//		// Note that the feature links could (should ??) be replaced by
//		// predicate links, where the feature type is encodeed in the 
//		// predicate name, rather than in the link type.  However, there
//		// is no immediate need for this, and custom link-types use less RAM.
//		// So we define a bunch of custom link types.
//		FEATURE_NODE <- CONCEPT_NODE
//		FEATURE_LINK <- ORDERED_LINK
//		LINK_GRAMMAR_RELATIONSHIP_NODE <- PREDICATE_NODE   // e.g. "MX+"
//		LINK_GRAMMAR_DISJUNCT_NODE <- PREDICATE_NODE       // e.g. "S- O+ "
//
//		// Used to represent RelEx dependency relations
//		DEFINED_LINGUISTIC_CONCEPT_NODE <- FEATURE_NODE
//		DEFINED_LINGUISTIC_RELATIONSHIP_NODE <- PREDICATE_NODE // e.g. _subj, _obj
//		PREPOSITIONAL_RELATIONSHIP_NODE <- PREDICATE_NODE  // e.g. of, next, to
//
//		// Frame related stuff, almost certainly will be changed soon
//		// These are semi-obsolete, probably need to be redesigned.
//		// These are emitted by RelEx.
//		DEFINED_FRAME_NODE <- CONCEPT_NODE
//		DEFINED_FRAME_ELEMENT_NODE <- PREDICATE_NODE
//		FRAME_ELEMENT_LINK <- ORDERED_LINK
//
//		// New wordnet-import nodes, added by Linas.
//		// These are subject to change, pending review.
//		// See nlp/wordnet-import/README and nlp/wsd/README for details.
//		// LEMMA_NODE appears to be currently unused.
//		WORD_SENSE_NODE <- CONCEPT_NODE
//		WORD_SENSE_LINK <- ASSOCIATIVE_LINK
//		PART_OF_SPEECH_NODE <- FEATURE_NODE
//		PART_OF_SPEECH_LINK <- FEATURE_LINK
//		LEMMA_NODE <- CONCEPT_NODE
//		LEMMA_LINK <- ORDERED_LINK
//		HOLONYM_LINK <- INHERITANCE_LINK
//
//		// New word-sense-disambiguation nodes, added by Linas
//		// See nlp/wsd/README for details.
//		COSENSE_LINK <- UNORDERED_LINK
//
//		// Concept-formation anchors.
//		// See nlp/seme/README for details.
//		SEME_NODE <- CONCEPT_NODE
//		SEMANTIC_RELATION_NODE <- SEME_NODE
//		CONTEXT_NODE <- CONCEPT_NODE
//
//		// AGISim related links and nodes
//		AGISIM_SOUND_NODE <- NODE "AGISIMSoundNode"
//		AGISIM_TASTE_NODE <- NODE "AGISIMTasteNode"
//		AGISIM_SMELL_NODE <- NODE "AGISIMSmellNode"
//		AGISIM_SELF_NODE <- NODE "AGISIMSelfNode"
//		AGISIM_PERCEPT_NODE <- NODE "AGISIMPerceptNode"
//		AGISIM_VISUAL_PERCEPT_NODE <- NODE "AGIMSIMVisualPerceptNode" // note
//		AGISIM_PIXEL_PERCEPT_NODE <- NODE "AGISIMPixelPerceptNode"
//		AGISIM_POLYGON_PERCEPT_NODE <- NODE "AGISIMPolygonPerceptNode"
//		AGISIM_OBJECT_PERCEPT_NODE <- NODE "AGISIMObjectPerceptNode"
//
//		CW_PIXEL_PERCEPT_NODE <- NODE "CWPixelPerceptNode"
//		CW_COLOR_NODE <- NODE "CWColorNode"
//
//		FW_VARIABLE_NODE <- NODE "FWVariableNode"
//
//		// Embodiment atoms
//
//		LATEST_LINK <- ORDERED_LINK
//
//		OBJECT_NODE <- NODE
//		PET_NODE <- OBJECT_NODE
//		AVATAR_NODE <- OBJECT_NODE
//		STRUCTURE_NODE <- OBJECT_NODE
//		ACCESSORY_NODE <- OBJECT_NODE
//		HUMANOID_NODE <- OBJECT_NODE
//
//		// Time-related atoms:
//
//		AT_TIME_LINK <- ORDERED_LINK
//		TIME_NODE <- NODE
//
//		// Perception specific terms
//		IS_ACCEPTABLE_SECOND_ARG_LINK <- ORDERED_LINK
//
//		// Attention allocation atoms:
//
//		HEBBIAN_LINK <- LINK
//		ASYMMETRIC_HEBBIAN_LINK <- ORDERED_LINK,HEBBIAN_LINK
//		SYMMETRIC_HEBBIAN_LINK <- UNORDERED_LINK,HEBBIAN_LINK
//		INVERSE_HEBBIAN_LINK <- ORDERED_LINK,HEBBIAN_LINK
//		SYMMETRIC_INVERSE_HEBBIAN_LINK <- UNORDERED_LINK,HEBBIAN_LINK


//		// =====================================================================
//		// EMBODIMENT-RELATED ATOMS: 
//		// TODO: move all related definitions from 
//		// opencog/atomspace/atom_types.script to here when the atom types 
//		// initialization issue is definitely solved.
//		// =====================================================================
//
//		WR_LINK <- ASSOCIATIVE_LINK "WRLink"
//		FREQUENCY_LINK <- INHERITANCE_LINK
//
//		FEELING_NODE <- GROUNDED_PREDICATE_NODE
//		PHRASE_NODE <- NODE

        addToAtomSpace(c);
    }


}
