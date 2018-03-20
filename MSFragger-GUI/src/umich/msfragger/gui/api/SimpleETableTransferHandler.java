/* 
 * Copyright (C) 2018 Dmitry Avtonomov
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package umich.msfragger.gui.api;

import java.awt.Component;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import org.netbeans.swing.etable.ETable;
import org.netbeans.swing.etable.ETableTransferHandler;

/**
 *
 * @author Dmitry Avtonomov
 */
public class SimpleETableTransferHandler extends ETableTransferHandler {
    
    private static final long serialVersionUID = 1L;

    public SimpleETableTransferHandler() {
    }

    @Override
    public int getSourceActions(JComponent c) {
        return super.getSourceActions(c); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    protected Transferable createTransferable(JComponent c) {
        return super.createTransferable(c); //To change body of generated methods, choose Tools | Templates.
    }

    
    
    @Override
    public boolean canImport(TransferSupport s) {
        Component c = s.getComponent();
        DataFlavor[] dataFlavors = s.getDataFlavors();
        StringBuilder dfsb = new StringBuilder("Flavors: ");
        for (int i = 0; i < dataFlavors.length; i++) {
            DataFlavor dataFlavor = dataFlavors[i];
            String humanPresentableName = dataFlavor.getHumanPresentableName();
            dfsb.append(humanPresentableName).append(", ");
        }
        System.out.printf("canImport(): %s", dfsb.toString());
        if (!(c instanceof ETable)) {
            return false;
        }
        if (!s.isDataFlavorSupported(DataFlavor.stringFlavor)) {
            return false;
        }
        return true;
    }

    @Override
    public boolean importData(TransferSupport s) {
        JOptionPane.showMessageDialog(null, "Transfer!");
        return false;
        //return super.importData(support);
    }
    
}
