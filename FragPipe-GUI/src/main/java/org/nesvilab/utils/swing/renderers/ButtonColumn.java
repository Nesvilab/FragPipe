/*
 * This file is part of FragPipe.
 *
 * FragPipe is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * FragPipe is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with FragPipe. If not, see <https://www.gnu.org/licenses/>.
 */

package org.nesvilab.utils.swing.renderers;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.table.*;

/**
 *  The ButtonColumn class provides a renderer and an editor that looks like a
 *  JButton. The renderer and editor will then be used for a specified column
 *  in the table. The TableModel will contain the String to be displayed on
 *  the button.
 *
 *  The button can be invoked by a mouse click or by pressing the space bar
 *  when the cell has focus. Optionally a mnemonic can be set to invoke the
 *  button. When the button is invoked the provided Action is invoked. The
 *  source of the Action will be the table. The action command will contain
 *  the model row number of the button that was clicked.
 *
 */
public class ButtonColumn extends AbstractCellEditor
    implements TableCellRenderer, TableCellEditor, ActionListener, MouseListener
{
  private JTable table;
  private Action action;
  private int mnemonic;
  private Border originalBorder;
  private Border focusBorder;

  private JButton renderButton;
  private JButton editButton;
  private Object editorValue;
  private boolean isButtonColumnEditor;

  /**
   *  Create the ButtonColumn to be used as a renderer and editor. The
   *  renderer and editor will automatically be installed on the TableColumn
   *  of the specified column.
   *
   *  @param table the table containing the button renderer/editor
   *  @param action the Action to be invoked when the button is invoked
   *  @param column the column to which the button renderer/editor is added
   */
  public ButtonColumn(JTable table, Action action, int column)
  {
    this.table = table;
    this.action = action;

    renderButton = new JButton();
    editButton = new JButton();
    editButton.setFocusPainted( false );
    editButton.addActionListener( this );
    originalBorder = editButton.getBorder();
    setFocusBorder( new LineBorder(Color.BLUE) );

    TableColumnModel columnModel = table.getColumnModel();
    columnModel.getColumn(column).setCellRenderer( this );
    columnModel.getColumn(column).setCellEditor( this );
    table.addMouseListener( this );
  }


  /**
   *  Get foreground color of the button when the cell has focus
   *
   *  @return the foreground color
   */
  public Border getFocusBorder()
  {
    return focusBorder;
  }

  /**
   *  The foreground color of the button when the cell has focus
   *
   *  @param focusBorder the foreground color
   */
  public void setFocusBorder(Border focusBorder)
  {
    this.focusBorder = focusBorder;
    editButton.setBorder( focusBorder );
  }

  public int getMnemonic()
  {
    return mnemonic;
  }

  /**
   *  The mnemonic to activate the button when the cell has focus
   *
   *  @param mnemonic the mnemonic
   */
  public void setMnemonic(int mnemonic)
  {
    this.mnemonic = mnemonic;
    renderButton.setMnemonic(mnemonic);
    editButton.setMnemonic(mnemonic);
  }

  @Override
  public Component getTableCellEditorComponent(
      JTable table, Object value, boolean isSelected, int row, int column)
  {
    if (value == null)
    {
      editButton.setText( "" );
      editButton.setIcon( null );
    }
    else if (value instanceof Icon)
    {
      editButton.setText( "" );
      editButton.setIcon( (Icon)value );
    }
    else
    {
      editButton.setText( value.toString() );
      editButton.setIcon( null );
    }

    this.editorValue = value;
    return editButton;
  }

  @Override
  public Object getCellEditorValue()
  {
    return editorValue;
  }

  //
//  Implement TableCellRenderer interface
//
  public Component getTableCellRendererComponent(
      JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column)
  {
    if (isSelected)
    {
      renderButton.setForeground(table.getSelectionForeground());
      renderButton.setBackground(table.getSelectionBackground());
    }
    else
    {
      renderButton.setForeground(table.getForeground());
      renderButton.setBackground(UIManager.getColor("Button.background"));
    }

    if (hasFocus)
    {
      renderButton.setBorder( focusBorder );
    }
    else
    {
      renderButton.setBorder( originalBorder );
    }

//		renderButton.setText( (value == null) ? "" : value.toString() );
    if (value == null)
    {
      renderButton.setText( "" );
      renderButton.setIcon( null );
    }
    else if (value instanceof Icon)
    {
      renderButton.setText( "" );
      renderButton.setIcon( (Icon)value );
    }
    else
    {
      renderButton.setText( value.toString() );
      renderButton.setIcon( null );
    }

    return renderButton;
  }

  //
//  Implement ActionListener interface
//
  /*
   *	The button has been pressed. Stop editing and invoke the custom Action
   */
  public void actionPerformed(ActionEvent e)
  {
    int row = table.convertRowIndexToModel( table.getEditingRow() );
    fireEditingStopped();

    //  Invoke the Action

    ActionEvent event = new ActionEvent(
        table,
        ActionEvent.ACTION_PERFORMED,
        "" + row);
    action.actionPerformed(event);
  }

  //
//  Implement MouseListener interface
//
  /*
   *  When the mouse is pressed the editor is invoked. If you then then drag
   *  the mouse to another cell before releasing it, the editor is still
   *  active. Make sure editing is stopped when the mouse is released.
   */
  public void mousePressed(MouseEvent e)
  {
    if (table.isEditing()
        &&  table.getCellEditor() == this)
      isButtonColumnEditor = true;
  }

  public void mouseReleased(MouseEvent e)
  {
    if (isButtonColumnEditor
        &&  table.isEditing())
      table.getCellEditor().stopCellEditing();

    isButtonColumnEditor = false;
  }

  public void mouseClicked(MouseEvent e) {}
  public void mouseEntered(MouseEvent e) {}
  public void mouseExited(MouseEvent e) {}
}
