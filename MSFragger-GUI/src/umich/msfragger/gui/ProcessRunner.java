/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package umich.msfragger.gui;

/**
 *
 * @author dmitriya
 */
public class ProcessRunner {
    Appendable[] outs;

    public ProcessRunner(Appendable... outs) {
        this.outs = outs;
    }
}
