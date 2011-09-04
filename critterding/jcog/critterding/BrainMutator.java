/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package jcog.critterding;

/**
 *
 * @author seh
 */
public class BrainMutator {
    boolean mutate_percentChanceConsistentSynapses;
    boolean mutate_percentChanceInhibitorySynapses;
    boolean mutate_percentChanceMotorNeuron;
    boolean mutate_percentChancePlasticNeuron;
    boolean mutate_PlasticityFactors;
    boolean mutate_minFiringThreshold;
    boolean mutate_maxFiringThreshold;
    boolean mutate_maxDendridicBranches;
    boolean mutate_percentChanceSensorySynapse;
    boolean mutate_percentMutation;
    boolean mutate_percentChanceInhibitoryNeuron;

    int percentMutateEffectAddNeuron;
    int percentMutateEffectRemoveNeuron;
    int percentMutateEffectAlterNeuron;
    int percentMutateEffectAddSynapse;
    int percentMutateEffectRemoveSynapse;
    int percentMutateEffectAlterMutable;
    boolean mutate_MutateEffects;

    public BrainMutator() {
        super();
        
        mutate_percentChanceConsistentSynapses = false;
        mutate_percentChanceInhibitorySynapses = false;
        mutate_percentChanceMotorNeuron = false;
        mutate_percentChancePlasticNeuron = false;
        mutate_PlasticityFactors = false;
        mutate_percentChanceSensorySynapse = false;
        mutate_minFiringThreshold = false;
        mutate_maxFiringThreshold = false;
        mutate_maxDendridicBranches = false;
        mutate_percentMutation = false;
        mutate_percentChanceInhibitoryNeuron = false;

        // 90 % > 10 for mutatables
        percentMutateEffectAddNeuron = 5;
        percentMutateEffectRemoveNeuron = 5;
        percentMutateEffectAlterNeuron = 5;
        percentMutateEffectAddSynapse = 40;
        percentMutateEffectRemoveSynapse = 40;
        percentMutateEffectAlterMutable = 5;
        mutate_MutateEffects = false;

    }

    public CritterdingBrain mutate(CritterdingBrain b, int runs) {
//		// have to do count cuz wireArch not done yet
//		totalNeurons		= ArchNeurons.size();
//		totalSynapses		= 0;
//		for ( unsigned int i = 0; i < totalNeurons; i++ ) totalSynapses += ArchNeurons[i].ArchSynapses.size();
//
//		if ( runs == 0 )
//		{
//			runs = randgen->Instance()->get(1, (int)(totalSynapses/(100/percentMutation)));
//		}
//
//		for ( unsigned int i=0; i < runs; i++ )
//		{
//			unsigned int mode = randgen->Instance()->get(1,100);
//
//		// add a new neuron
//			if ( mode <= percentMutateEffectAddNeuron )
//			{
//				if ( ArchNeurons.size() < maxNeurons )
//				{
//					addRandomArchNeuron();
//					unsigned int nid = ArchNeurons.size()-1;
//					//cerr << "\t+N " << nid << endl;
//
//					// random amount of connections, at mutation time we take the average syns / neuron *2 as a maximum
//					unsigned int tNeurons = totalNeurons;
//					if ( tNeurons == 0 )
//						tNeurons = 1;
//
//					int maxSynapsesAtMutation = (2*(totalSynapses/tNeurons)) + 1;
//
//					unsigned int cAmount = randgen->Instance()->get( minSynapsesAtBuildtime, maxSynapsesAtMutation );
//					for ( unsigned j = 0; j < cAmount; j++ )
//						addRandomArchSynapse(nid);
//				}
//				else runs++;
//			}
//
//		// remove a neuron
//			else if ( mode <= percentMutateEffectAddNeuron + percentMutateEffectRemoveNeuron )
//			{
//				if ( ArchNeurons.size() > 0 )
//				{
//					// pick a random neuron
//					unsigned int nid = randgen->Instance()->get( 0, ArchNeurons.size()-1 );
//					//cerr << "\t-N " << nid << endl;
//
//					// first remove all connections to this neuron
//					for ( unsigned int i=0; i < ArchNeurons.size(); i++ )
//					{
//						for ( unsigned int j=0; j < ArchNeurons[i].ArchSynapses.size(); j++ )
//						{
//							if ( !ArchNeurons[i].ArchSynapses[j].isSensorNeuron )
//							{
//								if ( ArchNeurons[i].ArchSynapses[j].neuronID == nid )
//								{
//									//delete ArchNeurons[i].ArchSynapses[j];
//									ArchNeurons[i].ArchSynapses.erase(ArchNeurons[i].ArchSynapses.begin()+j);
//									j--;
//								}
//								// if higher id drop one
//								else if ( ArchNeurons[i].ArchSynapses[j].neuronID > nid )
//								{
//									ArchNeurons[i].ArchSynapses[j].neuronID--;
//								}
//							}
//						}
//					}
//					ArchNeurons.erase(ArchNeurons.begin()+nid);
//				}
//				// make sure we mutate
//				else runs++;
//			}
//
//		// alter a neuron
//			else if ( mode <= percentMutateEffectAddNeuron + percentMutateEffectRemoveNeuron + percentMutateEffectAlterNeuron )
//			{
//				if ( ArchNeurons.size() > 0 )
//				{
//
//					// pick a random neuron
//						unsigned int nid = randgen->Instance()->get( 0, ArchNeurons.size()-1 );
//
//					// decide what to alter
//						unsigned int jmode = randgen->Instance()->get(1,6);
//
//					// inhibitory function
//						if ( jmode == 1 )
//						{
//							// backup old
//							bool old = ArchNeurons[nid].isInhibitory;
//
//							// reset
//							ArchNeurons[nid].isInhibitory = false;
//
//							// redetermine
//							if ( randgen->Instance()->get(1,100) <= percentChanceInhibitoryNeuron )
//								ArchNeurons[nid].isInhibitory = true;
//
//							// make sure we mutate
//							if ( old == ArchNeurons[nid].isInhibitory ) runs++;
//						}
//					// motor neuron
//						else if ( jmode == 2 )
//						{
//							// backup old
//							bool old = ArchNeurons[nid].isMotor;
//							unsigned int oldfunc = ArchNeurons[nid].motorID;
//
//							// reset
//							ArchNeurons[nid].isMotor = false;
//							ArchNeurons[nid].motorID = 0;
//
//							// redetermine
//							if ( !ArchNeurons[nid].isInhibitory && randgen->Instance()->get(1,100) <= percentChanceMotorNeuron )
//							{
//								unsigned int motorID = Outputs[ randgen->Instance()->get( 0, numberOfOutputs-1 ) ].id;
//
//								bool proceed = true;
//								for ( unsigned int i=0; i < ArchNeurons.size() && proceed; i++ )
//									if ( ArchNeurons[i].isMotor && ArchNeurons[i].motorID == motorID )
//										proceed = false;
//
//								if ( proceed )
//								{
//									ArchNeurons[nid].isMotor = true;
//									ArchNeurons[nid].motorID = motorID;
//								}
//							}
//
//							// make sure we mutate
//							if ( old == ArchNeurons[nid].isMotor && oldfunc == ArchNeurons[nid].motorID )
//								runs++;
//						}
//					// synaptic plasticity
//						else if ( jmode == 3 )
//						{
//							// backup old
//							bool old = ArchNeurons[nid].isPlastic;
//							unsigned int olds = ArchNeurons[nid].plasticityStrengthen;
//							unsigned int oldw = ArchNeurons[nid].plasticityWeaken;
//
//							// reset
//							ArchNeurons[nid].isPlastic = false;
//							ArchNeurons[nid].plasticityStrengthen = 1;
//							ArchNeurons[nid].plasticityWeaken = 1;
//
//							// redetermine
//							if ( randgen->Instance()->get(1,100) <= percentChancePlasticNeuron )
//							{
//								ArchNeurons[nid].isPlastic = true;
//								ArchNeurons[nid].plasticityStrengthen = randgen->Instance()->get( minPlasticityStrengthen, maxPlasticityStrengthen );
//								ArchNeurons[nid].plasticityWeaken = randgen->Instance()->get( minPlasticityWeaken, maxPlasticityWeaken );
//							}
//
//							// make sure we mutate
//							if ( old == ArchNeurons[nid].isPlastic && olds == ArchNeurons[nid].plasticityStrengthen && oldw == ArchNeurons[nid].plasticityWeaken ) runs++;
//						}
//
//					// consistent synapses
//						else if ( jmode == 4 )
//						{
//							// backup old
//							bool old = ArchNeurons[nid].hasConsistentSynapses;
//							bool oldi = ArchNeurons[nid].hasInhibitorySynapses;
//
//							// reset
//							ArchNeurons[nid].hasConsistentSynapses = false;
//
//							// redetermine
//							if ( randgen->Instance()->get(1,100) <= percentChanceConsistentSynapses )
//							{
//								ArchNeurons[nid].hasConsistentSynapses = true;
//
//								// if so, does it have inhibitory synapses ?
//									if ( randgen->Instance()->get(1,100) <= percentChanceInhibitorySynapses )
//										ArchNeurons[nid].hasInhibitorySynapses = true;
//							}
//
//							// make sure we mutate
//							if ( old == ArchNeurons[nid].hasConsistentSynapses && oldi == ArchNeurons[nid].hasInhibitorySynapses ) runs++;
//						}
//
//					// firing threshold
//						else if ( jmode == 5 )
//						{
//							// backup old
//							unsigned int old = ArchNeurons[nid].firingThreshold;
//
//							ArchNeurons[nid].firingThreshold = randgen->Instance()->get( minFiringThreshold, maxFiringThreshold );
//
//							// make sure we mutate
//							if ( old == ArchNeurons[nid].firingThreshold ) runs++;
//						}
//
//					// dendritic branches
//						else if ( jmode == 6 )
//						{
//							// backup old
//							unsigned int old = ArchNeurons[nid].dendridicBranches;
//
//							ArchNeurons[nid].dendridicBranches = randgen->Instance()->get( 1, maxDendridicBranches );
//
//							// make sure we mutate
//							if ( old == ArchNeurons[nid].dendridicBranches ) runs++;
//						}
//
//				}
//			}
//
//		// add a new synapse
//			else if ( mode <= percentMutateEffectAddNeuron + percentMutateEffectRemoveNeuron + percentMutateEffectAlterNeuron + percentMutateEffectAddSynapse )
//			{
//				if ( ArchNeurons.size() > 0 )
//				{
//					// pick a random neuron
//					unsigned int nid = randgen->Instance()->get( 0, ArchNeurons.size()-1 );
//
//					// don't go over maximum connections
//					if ( ArchNeurons[nid].ArchSynapses.size() < maxSynapses )
//					{
//						//cerr << "\t+C " << nid << endl;
//						addRandomArchSynapse(nid);
//					}
//					else runs++;
//				}
//				else runs++;
//			}
//
//		// remove a synapse
//			else if ( mode <= percentMutateEffectAddNeuron + percentMutateEffectRemoveNeuron + percentMutateEffectAlterNeuron + percentMutateEffectAddSynapse + percentMutateEffectRemoveSynapse )
//			{
//				if ( ArchNeurons.size() > 0 )
//				{
//					// pick a random neuron
//					int nid = randgen->Instance()->get( 0, ArchNeurons.size()-1 );
//
//					// don't go under minimum connections
//					if ( ArchNeurons[nid].ArchSynapses.size() > minSynapses )
//					{
//						//cerr << "\t-C " << nid << endl;
//						unsigned int sID = randgen->Instance()->get(0, ArchNeurons[nid].ArchSynapses.size()-1);
//						//delete ArchNeurons[nid].ArchSynapses[connid];
//						ArchNeurons[nid].ArchSynapses.erase(ArchNeurons[nid].ArchSynapses.begin()+sID);
//					}
//					else runs++;
//				}
//				else runs++;
//			}
//
//		// change a mutatable
//			else if (  mode <= percentMutateEffectAddNeuron + percentMutateEffectRemoveNeuron + percentMutateEffectAlterNeuron + percentMutateEffectAddSynapse + percentMutateEffectRemoveSynapse + percentMutateEffectAlterMutable )
//			{
//				unsigned int imode = randgen->Instance()->get(1,12);
//
//				if ( imode == 1 && mutate_percentChanceInhibitoryNeuron )
//				{
//					unsigned int jmode = randgen->Instance()->get(1,2);
//					unsigned int factor = randgen->Instance()->get(1,5);
//
//					if ( jmode == 1 && percentChanceInhibitoryNeuron < 100-factor )	percentChanceInhibitoryNeuron+=factor;
//					else if ( percentChanceInhibitoryNeuron > 0+factor )		percentChanceInhibitoryNeuron-=factor;
//				}
//
//				else if ( imode == 2 && mutate_percentChanceConsistentSynapses )
//				{
//					unsigned int jmode = randgen->Instance()->get(1,2);
//					unsigned int factor = randgen->Instance()->get(1,5);
//
//					if ( jmode == 1 && percentChanceConsistentSynapses < 100-factor )	percentChanceConsistentSynapses+=factor;
//					else if ( percentChanceConsistentSynapses > 0+factor )			percentChanceConsistentSynapses-=factor;
//				}
//
//				else if ( imode == 3 && mutate_percentChanceInhibitorySynapses )
//				{
//					unsigned int jmode = randgen->Instance()->get(1,2);
//					unsigned int factor = randgen->Instance()->get(1,5);
//
//					if ( jmode == 1 && percentChanceInhibitorySynapses < 100-factor )	percentChanceInhibitorySynapses+=factor;
//					else if ( percentChanceInhibitorySynapses > 0+factor )			percentChanceInhibitorySynapses-=factor;
//				}
//
//				else if ( imode == 4 && mutate_percentChanceMotorNeuron )
//				{
//					unsigned int jmode = randgen->Instance()->get(1,2);
//					unsigned int factor = randgen->Instance()->get(1,5);
//
//					if ( jmode == 1 && percentChanceMotorNeuron < 100-factor )	percentChanceMotorNeuron+=factor;
//					else if ( percentChanceMotorNeuron > 0+factor )			percentChanceMotorNeuron-=factor; // !!! > 1
//				}
//
//				else if ( imode == 5 && mutate_percentChancePlasticNeuron )
//				{
//					unsigned int jmode = randgen->Instance()->get(1,2);
//					unsigned int factor = randgen->Instance()->get(1,5);
//
//					if ( jmode == 1 && percentChancePlasticNeuron < 100-factor )	percentChancePlasticNeuron+=factor;
//					else if ( percentChancePlasticNeuron > 0+factor )		percentChancePlasticNeuron-=factor;
//				}
//
//				else if ( imode == 6 && mutate_percentChanceSensorySynapse )
//				{
//					unsigned int jmode = randgen->Instance()->get(1,2);
//					unsigned int factor = randgen->Instance()->get(1,5);
//
//					if ( jmode == 1 && percentChanceSensorySynapse < 100-factor )	percentChanceSensorySynapse+=factor;
//					else if ( percentChanceSensorySynapse > minSynapses+factor )	percentChanceSensorySynapse-=factor;
//				}
//
//				else if ( imode == 7 && mutate_minFiringThreshold )
//				{
//					unsigned int jmode = randgen->Instance()->get(1,2);
//					unsigned int factor = randgen->Instance()->get(1,5);
//
//					if ( jmode == 1 && minFiringThreshold < maxFiringThreshold-factor )	minFiringThreshold+=factor; // watch out idd
//					else if ( minFiringThreshold > 1+factor )				minFiringThreshold-=factor; // !!! > 1
//				}
//
//				else if ( imode == 8 && mutate_maxFiringThreshold )
//				{
//					unsigned int jmode = randgen->Instance()->get(1,2);
//					unsigned int factor = randgen->Instance()->get(1,5);
//
//					if ( jmode == 1 && maxFiringThreshold < 1000-factor )		maxFiringThreshold+=factor;
//					else if ( maxFiringThreshold > minFiringThreshold+factor )	maxFiringThreshold-=factor; // !!! > minFiringThreshold
//				}
//
//				else if ( imode == 9 && mutate_maxDendridicBranches )
//				{
//					unsigned int jmode = randgen->Instance()->get(1,2);
//					unsigned int factor = randgen->Instance()->get(1,5);
//
//					if ( jmode == 1 && maxDendridicBranches < 100-factor )	maxDendridicBranches+=factor;
//					else if ( maxDendridicBranches > 1+factor )		maxDendridicBranches-=factor; // !!! > 1
//				}
//
//				else if ( imode == 10 && mutate_percentMutation )
//				{
//					unsigned int jmode = randgen->Instance()->get(1,2);
//					unsigned int factor = randgen->Instance()->get(1,5);
//
//					if ( jmode == 1 && percentMutation < 100-factor )	percentMutation+=factor;
//					else if ( percentMutation > 1+factor )			percentMutation-=factor; // !!! > 1 or no more mutation at all
//				}
//
//				else if ( imode == 11 && mutate_MutateEffects )
//				{
//					// up or down
//					unsigned int jmode = randgen->Instance()->get(1,2);
//
//					// which of 5
//					unsigned int kmode = randgen->Instance()->get(1,6);
//
//					if ( jmode == 1 && percentMutateEffectAddNeuron + percentMutateEffectRemoveNeuron + percentMutateEffectAlterNeuron + percentMutateEffectAddSynapse + percentMutateEffectRemoveSynapse + percentMutateEffectAlterMutable <= 100 )
//					{
//						if ( kmode == 1 )	percentMutateEffectAddNeuron++;
//						else if ( kmode == 2 )	percentMutateEffectRemoveNeuron++;
//						else if ( kmode == 3 )	percentMutateEffectAlterNeuron++;
//						else if ( kmode == 4 )	percentMutateEffectAddSynapse++;
//						else if ( kmode == 5 )	percentMutateEffectRemoveSynapse++;
//						else if ( kmode == 6 )	percentMutateEffectAlterMutable++;
//					}
//					else if ( jmode == 2 )
//					{
//						if ( kmode == 1 && percentMutateEffectAddNeuron > 0 )		percentMutateEffectAddNeuron--;
//						else if ( kmode == 2 && percentMutateEffectRemoveNeuron > 0 )	percentMutateEffectRemoveNeuron--;
//						else if ( kmode == 3 && percentMutateEffectAlterNeuron > 0 )	percentMutateEffectAlterNeuron--;
//						else if ( kmode == 4 && percentMutateEffectAddSynapse > 0 )	percentMutateEffectAddSynapse--;
//						else if ( kmode == 5 && percentMutateEffectRemoveSynapse > 0 )	percentMutateEffectRemoveSynapse--;
//						else if ( kmode == 6 && percentMutateEffectAlterMutable > 0 )	percentMutateEffectAlterMutable--;
//					}
//				}
//
//				else if ( imode == 12 && mutate_PlasticityFactors )
//				{
//					// up or down
//					unsigned int jmode = randgen->Instance()->get(1,2);
//
//					// which of 4
//					unsigned int kmode = randgen->Instance()->get(1,4);
//
//					// factor
//					unsigned int factor = randgen->Instance()->get(10,1000);
//
//					if ( jmode == 1 )
//					{
//						if ( kmode == 1 && minPlasticityStrengthen+factor < maxPlasticityStrengthen )	minPlasticityStrengthen+=factor;
//						else if ( kmode == 2 && maxPlasticityStrengthen+factor < 1000000 )		maxPlasticityStrengthen+=factor;
//						else if ( kmode == 3 && minPlasticityWeaken+factor < maxPlasticityWeaken )	minPlasticityWeaken+=factor;
//						else if ( kmode == 4 && maxPlasticityWeaken+factor < 1000000 )			maxPlasticityWeaken+=factor;
//					}
//					else
//					{
//						if ( kmode == 1 && minPlasticityStrengthen > 10+factor  )				minPlasticityStrengthen-=factor;
//						else if ( kmode == 2 && maxPlasticityStrengthen > minPlasticityStrengthen+factor )	maxPlasticityStrengthen-=factor;
//						else if ( kmode == 3 && minPlasticityWeaken > 100+factor )				minPlasticityWeaken-=factor;
//						else if ( kmode == 4 && maxPlasticityWeaken > minPlasticityWeaken+factor )		maxPlasticityWeaken-=factor;
//					}
//				}
//
//				else runs++;
//			}
//			else runs++;
//		}
//
        return b;
    }
}
