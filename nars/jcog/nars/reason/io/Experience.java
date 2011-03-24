/*
 * ExperienceIO.java
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
package jcog.nars.reason.io;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;

import org.opencog.reason.nars.Memory;


/**
 * To read and write experience as Task streams (formerly named "ExperienceIO" )
 */
public class Experience {

    /** Input experience from a window */
    //private InputWindow inputWindow;
    
    /** Input experience from a file */
    private BufferedReader inExp;
    
    /** Output experience into a file */
    private PrintWriter outExp;

	private Memory memory;

	private StringParser stringParser;

    /** 
     * Default constructor
     * @param memory 
     */
    public Experience(Memory memory) {
    	super();
    	
    	this.memory = memory;
    	this.stringParser = new StringParser(memory);
    	
        //inputWindow = MainWindow.inputWindow;
        inExp = null;
        outExp = null;
    }

//    /** 
//     * Open an input experience file
//     */
//    public void openLoadFile() {
//        FileDialog dialog = new FileDialog((FileDialog) null, "Load experience", FileDialog.LOAD);
//        dialog.setVisible(true);
//        String directoryName = dialog.getDirectory();
//        String fileName = dialog.getFile();
//        try {
//            inExp = new BufferedReader(new FileReader(directoryName + fileName));
//        } catch (IOException ex) {
//            System.out.println("i/o error: " + ex.getMessage());
//        }
//    }

//    /** 
//     * Open an output experience file
//     */
//    public void openSaveFile() {
//        FileDialog dialog = new FileDialog((FileDialog) null, "Save experience", FileDialog.SAVE);
//        dialog.setVisible(true);
//        String directoryName = dialog.getDirectory();
//        String fileName = dialog.getFile();
//        try {
//            outExp = new PrintWriter(new FileWriter(directoryName + fileName));
//        } catch (IOException ex) {
//            System.out.println("i/o error: " + ex.getMessage());
//        }
//    }

    public void eval(String line) {
        stringParser.parseExperience(new StringBuffer(line));
    }
    
    /**
     * Read a line from input, and send running instruction to Memory
     * @return Nember of running steps, if any
     */
    public long loadLine() {
        String line = null;
        long cycle = 0;
        if (inExp != null) {
            try {
                line = inExp.readLine();
                if (line == null) {
                    inExp.close();
                    inExp = null;
                }
            } catch (IOException ex) {
                System.out.println("i/o error: " + ex.getMessage());
            }
        } else {
            //line = inputWindow.getLine();
        	
        	line = "";
        	
        }
        if (line != null) {
            line = line.trim();
            if (line.length() > 0) {
                try {
                    cycle = new Long(line);
                } catch (NumberFormatException e) {
                    if (line.charAt(0) == Symbols.RESET_MARK) {
                        //Center.reset();
                        saveLine(line);
                    } else if (line.charAt(0) == Symbols.COMMENT_MARK) {
                        saveLine(line);
                    } else {
                    	eval(line);
                    }
                }
            }
        }
        return cycle;
    }

    /** 
     * Write a line into the output experience file
     * @param line The output line
     */
    public void saveLine(String line) {
        if (outExp != null) {
            outExp.println(line);
        }
    }

    /** 
     * Close an input experience file
     */
    public void closeLoadFile() {
        try {
            inExp.close();
        } catch (IOException ex) {
            System.out.println("i/o error: " + ex.getMessage());
        }
    }

    /** 
     * Close an output experience file
     */
    public void closeSaveFile() {
        outExp.close();
    }
}
