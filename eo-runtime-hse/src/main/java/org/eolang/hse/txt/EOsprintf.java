package org.eolang.hse.txt;

import org.eolang.hse.EOarray;
import org.eolang.hse.core.EOObject;
import org.eolang.hse.core.data.EOData;

import java.util.Arrays;
import java.util.Objects;

/**
 * Объект осуществляющий создание форматированых строк.
 */
public class EOsprintf extends EOObject {
    private EOObject format;
    private EOObject[] data;

    public EOsprintf(EOObject format, EOObject... data) {
        this.format = format;
        this.data = data;
    }

    @Override
    public EOData _getData() {
        String sFormat = format._getData().toString();
        Object[] objects = Arrays.stream(data).map(obj -> obj._getData().toObject()).toArray();
        return new EOData(
                String.format(
                        sFormat,
                        objects
                )
        );
    }

    /**
     * !!!For testing purposes only!!!
     *
     * Determines if this object is equal to the {@code o} object.
     * To do it, this method checks that the {@code o} object is
     * of the {@code EOObject} type and its dataization result is the same
     * as the result of dataization of this object. This is a simplified
     * equality check sufficient for checking equality of runtime object
     * for testing purposes.
     *
     * This method can be called only in the testing environment
     * since all methods within the EO environment have the 'EO' prefix.
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || !(o instanceof EOObject)) return false;
        EOObject eoObject = (EOObject) o;
        return _getData().toString().equals(eoObject._getData().toString());
    }

    /**
     * !!!For testing purposes only!!!
     *
     * Produces a string that represents this object.
     * The resulting string has the following form:
     * sprintf(format:"format string", data:array([arg1, arg2, arg3])),
     * where each argN is converted to a string, too.
     *
     * Example:
     * sprintf(format:"The winner is %s, and he got %d points!",
     * data:array(["Mike", int(100)])).
     *
     * This method can be called only in the testing environment
     * since all methods within the EO environment have the 'EO' prefix.
     */
    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("sprintf(");
        sb.append("format:").append(format);
        sb.append(", data:");
        sb.append(new EOarray(data).toString());
        sb.append(')');
        return sb.toString();
    }

    @Override
    public int hashCode() {
        int result = Objects.hash(format);
        result = 31 * result + Arrays.hashCode(data);
        return result;
    }
}
