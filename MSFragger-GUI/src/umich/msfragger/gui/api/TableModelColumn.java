/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package umich.msfragger.gui.api;

/**
 *
 * @author Dmitry Avtonomov
 */
public class TableModelColumn<I, O> {
    public final String name;
    public final Class<O> clazz;
    public final boolean isEditable;
    public final DataConverter<I, O> converter;

    public TableModelColumn(String name, Class<O> clazz, boolean isEditable, DataConverter<I, O> converter) {
        this.name = name;
        this.clazz = clazz;
        this.isEditable = isEditable;
        this.converter = converter;
    }
}
