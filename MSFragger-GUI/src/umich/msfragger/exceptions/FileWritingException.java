/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package umich.msfragger.exceptions;

/**
 *
 * @author dmitriya
 */
public class FileWritingException extends Exception {
    
    public FileWritingException() {
    }

    public FileWritingException(String message) {
        super(message);
    }

    public FileWritingException(String message, Throwable cause) {
        super(message, cause);
    }

    public FileWritingException(Throwable cause) {
        super(cause);
    }

    public FileWritingException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
   
}
