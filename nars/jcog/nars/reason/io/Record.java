/*
 * Record.java
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


/**
 * Inference log, which record input/output of each inference step
 * Deprecated - use Java logging, which can achieve all functionality Record was previously responsible for
 */
@Deprecated public class Record {

//    /** the display window */
//    //private InferenceWindow window = new InferenceWindow();
//    
//    /** whether to display */
//    private boolean isReporting = false;
//    
//    /** the log file */
//    private PrintWriter logFile = null;
//
//    public Record() {
//    	super();
//    	
//    	init();
//    }
//    
//    /** 
//     * Initialize the window and the file
//     */
//    public void init() {
//        //window.clear();
//        if (logFile != null) {
//            closeLogFile();
//        }
//    }
//
//
//    /** 
//     * Begin the display
//     */
//    public void play() {
//        isReporting = true;
//    }
//
//    /**
//     * Stop the display
//     */
//    public void stop() {
//        isReporting = false;
//    }
//
//    /** 
//     * Add new text to display
//     * @param s The line to be displayed
//     */
//    public void append(String s) {
//        if (isReporting) {
//            window.append(s);
//        }
//        if (logFile != null) {
//            logFile.println(s);
//        }
//    }
//
//    /** 
//     * Open the log file
//     */
//    public void openLogFile() {
//        FileDialog dialog = new FileDialog((FileDialog) null, "Inference Log", FileDialog.SAVE);
//        dialog.setVisible(true);
//        String directoryName = dialog.getDirectory();
//        String fileName = dialog.getFile();
//        try {
//            logFile = new PrintWriter(new FileWriter(directoryName + fileName));
//        } catch (IOException ex) {
//            System.out.println("i/o error: " + ex.getMessage());
//        }
//        window.switchBackground();
//        window.setVisible(true);
//    }
//
//    /** 
//     * Close the log file
//     */
//    public void closeLogFile() {
//        logFile.close();
//        logFile = null;
//        window.resetBackground();
//    }
//
//    /** 
//     * Check file logging
//     * @return If the file logging is going on
//     */
//    public boolean isLogging() {
//        return (logFile != null);
//    }
}

