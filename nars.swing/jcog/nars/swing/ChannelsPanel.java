package jcog.nars.swing;

/**
 * Communication control: The administrator can approve or decline the requests for establishing a communication channel, or terminate an existing channel. Some of the decisions will be made by the system itself.
 * 
 	The system will support multiple types of communication channels, as explained in Modules. Each communication channel provides experience (input) to the system, and realize the system's behaviors (output).
	The manager of a channel will need a communication GUI to request the establishing of a channel, as well as to manage it when approved. For different types of channels, there are different management tasks.

    * For a native channel between NARS implementations, no special support is needed, except the default functions, such as to display the communication in plain-text Narsese, and to save the communication history into a file. 

    * For a text-based channel between NARS and a human user or another computer, the tasks include to specify the adapter/parser that translate between Narsese and another language or format, to read input from a static knowledge source (file or database) at a specified speed, to support manually typed tasks with format checking and prompt, 

    * For a sensorimotor channel between NARS and a computerized tool, the GUI should support ways to register the commands of the tool as Narsese operators, with restrictions on valid arguments, and initial knowledge about the preconditions and consequences of each operation.
    *  
 * @see  http://code.google.com/p/open-nars/wiki/GUIRequest
 */
public class ChannelsPanel {

}
