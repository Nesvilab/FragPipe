/*
 * Copyright 2017 Dmitry Avtonomov.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
