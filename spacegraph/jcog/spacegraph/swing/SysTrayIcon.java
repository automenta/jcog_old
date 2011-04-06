/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package jcog.spacegraph.swing;

import java.awt.AWTException;
import java.awt.Image;
import java.awt.MenuItem;
import java.awt.PopupMenu;
import java.awt.SystemTray;
import java.awt.Toolkit;
import java.awt.TrayIcon;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

/**
@see http://java.sun.com/developer/technicalArticles/J2SE/Desktop/javase6/systemtray/
@author seh
 */
public class SysTrayIcon extends TrayIcon implements MouseListener {

    public SysTrayIcon(Image image, String tooltip, PopupMenu menu) {
        super(image, tooltip, menu);

        if (SystemTray.isSupported()) {

            SystemTray tray = SystemTray.getSystemTray();


//            ActionListener actionListener = new ActionListener() {
//                public void actionPerformed(ActionEvent e) {
//                    trayIcon.displayMessage("Action Event",
//                        "An Action Event Has Been Performed!",
//                        TrayIcon.MessageType.INFO);
//
//                }
//            };

            setImageAutoSize(true);
            //addActionListener(actionListener);
            addMouseListener(this);

            try {
                tray.add(this);
            } catch (AWTException e) {
                System.err.println("System Tray Icon could not be added.");
            }

        } else {
            //  System Tray is not supported
            System.err.println("SysTray not supported");
        }
    }

    @Override
    public void mouseClicked(MouseEvent e) {
    }

    @Override
    public void mousePressed(MouseEvent e) {
    }

    @Override
    public void mouseReleased(MouseEvent e) {
    }

    @Override
    public void mouseEntered(MouseEvent e) {
    }

    @Override
    public void mouseExited(MouseEvent e) {
    }

    public static void main(String[] args) {
        ActionListener exitListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                System.out.println("Exiting...");
                System.exit(0);
            }
        };

        PopupMenu popup = new PopupMenu();
        MenuItem defaultItem = new MenuItem("Exit");
        defaultItem.addActionListener(exitListener);
        popup.add(defaultItem);

        new SysTrayIcon(Toolkit.getDefaultToolkit().getImage("tray.gif"), "SysTray", popup);
    }

}
