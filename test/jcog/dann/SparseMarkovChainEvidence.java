/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.dann;

/**
 *
 * @author seh
 */
/******************************************************************************
 *                                                                             *
 *  Copyright: (c) Syncleus, Inc.                                              *
 *                                                                             *
 *  You may redistribute and modify this source code under the terms and       *
 *  conditions of the Open Source Community License - Type C version 1.0       *
 *  or any later version as published by Syncleus, Inc. at www.syncleus.com.   *
 *  There should be a copy of the license included with this file. If a copy   *
 *  of the license is not included you are granted no right to distribute or   *
 *  otherwise use this file except through a legal and valid license. You      *
 *  should also contact Syncleus, Inc. at the information below if you cannot  *
 *  find a license:                                                            *
 *                                                                             *
 *  Syncleus, Inc.                                                             *
 *  2604 South 12th Street                                                     *
 *  Philadelphia, PA 19148                                                     *
 *                                                                             *
 ******************************************************************************/

import com.syncleus.dann.math.statistics.MarkovChain;
import com.syncleus.dann.math.statistics.MarkovChainEvidence;


import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * only difference between this and SimpleMarkovChainEvidence is 'getMarkovChain' which I could not simply override because 'evidence' was private
 * @author me
 * @param <S> 
 */
public class SparseMarkovChainEvidence<S> implements MarkovChainEvidence<S>
{
	private final int order;
	private final ArrayDeque<S> history;
	private final boolean arbitraryStart;
	private final Map<List<S>, StateCounter<S>> evidence;
	private final Set<S> observedStates;

	public SparseMarkovChainEvidence(final boolean arbitraryStart, final int order)
	{
		this.history = new ArrayDeque<S>(order);
		this.order = order;
		this.arbitraryStart = arbitraryStart;
		this.evidence = new LinkedHashMap<List<S>, StateCounter<S>>();
		this.observedStates = new HashSet<S>();
	}

	@Override
	public void newChain()
	{
		this.history.clear();
	}

	private void learnFromMemory(final Collection<S> stateMemoryCollection, final S nextState)
	{
		final List<S> stateMemory = Collections.unmodifiableList(new ArrayList<S>(stateMemoryCollection));

		//get the current evidence for this state
		StateCounter<S> transitions = this.evidence.get(stateMemory);
		//if there is no transitions then create a blank one
		if( transitions == null )
		{
			transitions = new StateCounter<S>();
			this.evidence.put(stateMemory, transitions);
		}

		//update the transitions evidence for the new state
		transitions.increment(nextState);
	}

	@Override
	public void learnStep(final S state)
	{
		//update the evidence
		learnFromMemory(this.history, state);

		//if there is an arbitrary starting place update the evidence for the
		//various sub-states of shorter order
		if( (this.arbitraryStart) && (this.order > 1) )
		{
			final ArrayDeque<S> trainingMemory = new ArrayDeque<S>(this.history);
			while( trainingMemory.size() > 1 )
			{
				trainingMemory.poll();
				learnFromMemory(trainingMemory, state);
			}
		}

		//update the history
		this.history.add(state);
		while( this.history.size() > this.order )
			this.history.poll();

		//update the set of observed states
		this.observedStates.add(state);
	}

	@Override
	public Set<S> getObservedStates()
	{
		return Collections.unmodifiableSet(this.observedStates);
	}

	@Override
	public int getOrder()
	{
		return this.order;
	}

	public boolean isArbitraryStart()
	{
		return this.arbitraryStart;
	}

	@Override
	public MarkovChain<S> getMarkovChain()
	{
		final Map<List<S>, Map<S, Double>> transitionProbabilities = new LinkedHashMap<List<S>, Map<S, Double>>(this.evidence.size());
		for(Map.Entry<List<S>, StateCounter<S>> countEntry : this.evidence.entrySet())
			transitionProbabilities.put(countEntry.getKey(), countEntry.getValue().probabilities());

		return new SparseMarkovChain<S>(transitionProbabilities, this.order, this.observedStates);
	}

	private static class StateCounter<S>
	{
		private final Map<S, Integer> stateCount = new HashMap<S, Integer>();
		private long totalEvidence;

		public void increment(final S state)
		{
			Integer count = this.stateCount.get(state);
			if(count == null)
				count = 1;
			else
				count++;
			this.stateCount.put(state, count);
			this.totalEvidence++;
		}

		public double probability(final S state)
		{
			Integer count = this.stateCount.get(state);
			if(count == null)
				count = 0;
			return count.doubleValue() / ((double)totalEvidence);
		}

		public Map<S, Double> probabilities()
		{
			final Map<S, Double> prob = new HashMap<S, Double>(this.stateCount.size());
			for(Map.Entry<S, Integer> countEntry : this.stateCount.entrySet())
				prob.put(countEntry.getKey(), countEntry.getValue().doubleValue() / ((double)this.totalEvidence));
			return prob;
		}
	}
}