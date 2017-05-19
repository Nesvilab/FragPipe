package umich.msfragger.exceptions;

/**
 * Created by Dmitry Avtonomov on 2016-04-27.
 */
public class ParsingException extends Exception {
    public ParsingException() {
    }

    public ParsingException(String message) {
        super(message);
    }

    public ParsingException(String message, Throwable cause) {
        super(message, cause);
    }

    public ParsingException(Throwable cause) {
        super(cause);
    }

    public ParsingException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
