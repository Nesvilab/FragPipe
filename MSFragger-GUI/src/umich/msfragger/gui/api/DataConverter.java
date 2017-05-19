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
public interface DataConverter<I, O> {
    O convert(I data);
}
