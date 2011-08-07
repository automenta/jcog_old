/*
 * Open.java
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

package jcog.nars.reason.operation;

import java.util.ArrayList;
import jcog.nars.Task;


/**
 * A class used in testing only.
 */
public class Open extends Operator {
    public Open(String name) {
        super(name);
    }

    public ArrayList<Task> execute(Task task) {
//        System.out.println("EXECUTE in " + name + " " + task);
        return null;
    }
}


