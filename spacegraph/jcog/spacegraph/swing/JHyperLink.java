package jcog.spacegraph.swing;

import java.awt.Cursor;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JPopupMenu;

public class JHyperLink extends JButton {
    public JHyperLink(String toString, String tooltip, float fontScale) {
        super("<html><u>" + toString + "</u></html>");
        setOpaque(false);
        setBorderPainted(false);
        float nextSize = getFont().getSize2D() * fontScale;
        setFont(getFont().deriveFont(Font.BOLD).deriveFont(nextSize));
        setCursor(new Cursor(Cursor.HAND_CURSOR));
        setToolTipText(tooltip);
    }

    public JHyperLink(String toString, String tooltip) {
        this(toString, tooltip, 1.0f);
    }

    public void addPopup(final JPopupMenu menu) {
        addActionListener(new ActionListener() {
            @Override public void actionPerformed(ActionEvent e) {
                menu.show(JHyperLink.this, getWidth()/2, getHeight()/2);
            }
        });
    }
}
