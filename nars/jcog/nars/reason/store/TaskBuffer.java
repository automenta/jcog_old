/*
 * TaskBuffer.java
 *
 * Copyright (C) 2008  Pei Wang
 *
 * This file is part of Open-NARS.
 *
 * Open-NARS is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * Open-NARS is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Open-NARS.  If not, see <http://www.gnu.org/licenses/>.
 */

package jcog.nars.reason.store;

import org.opencog.atom.nars.Task;
import org.opencog.reason.nars.NARParams;




/**
 * New tasks that contain new Term.
 */
public class TaskBuffer extends Bag<Task> {
    
	private NARParams params;

	/**
	 * 
	 * @param bufferSize	//Parameters.TASK_BUFFER_SIZE
	 * @param forgetRate	//Parameters.NEW_TASK_FORGETTING_CYCLE
	 * @param loadFactor
	 * @param threshold
	 * @param totalLevel
	 */
	public TaskBuffer(NARParams params) {
		super(params.getBagLoadFactor(), params.getBagThreshold(), params.getBagLevel(), params.getTaskBufferSize());
		this.params = params;
	}
	
	@Override public int forgetRate() {
		return params.getNewTaskForgetRate();
	}
	
	
    
//    /**
//     * Sepecial treatment: the display also include Tasks in the NewTask list
//     * @return New Tasks in the buffer and list for display
//     */
//    @Override public String toString() {
//        return Memory.newTasksToString() + super.toString();
//    }
    
}

