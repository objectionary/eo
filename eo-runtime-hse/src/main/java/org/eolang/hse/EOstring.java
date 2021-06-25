package org.eolang.hse;

import org.eolang.hse.core.EOObject;
import org.eolang.hse.core.data.EOData;

import java.util.Objects;

/***
 * Represents a string type
 * @version %I%, %G%
 */
public class EOstring extends EOObject {
    private final String stringValue;

    public EOstring() {
        stringValue = "";
    }

    public EOstring(String stringValue) {
        this.stringValue = stringValue;
    }

    @Override
    public EOData _getData() {
        return new EOData(stringValue);
    }

    /***
     * Trims this string on both sides
     * @return An object representing the trimmed value of this string
     */
    public EOstring EOtrim() {
        return new EOstring(stringValue.trim());
    }

    /***
     * Makes an integer type of this string
     * @return An object representing the integer value of this string
     */
    public EOint EOtoInt() {
        return new EOint(Long.parseLong(stringValue));
    }

    /***
     * Parses a floating-point number from this string
     * @return An object representing the float value of this string
     */
    public EOfloat EOtoFloat() {
        return new EOfloat(Double.parseDouble(stringValue));
    }

    /***
     * Compares this string to the {@code rightString} free attribute
     * @param rightString a string to compare with
     * @return An object representing the truth value of the comparison of this string with the {@code rightString} free attribute
     */
    public EObool EOeq(EOObject rightString) {
        return new EObool(stringValue.equals(rightString._getData().toString()));
    }

    /**
     * !!!For testing purposes only!!!
     *
     * Determines if this object is equal to the {@code o} object.
     * To do it, this method checks that the {@code o} object is
     * of the {@code EOObject} type and its dataization result is the same
     * as the result of dataization of this object by delegating the check
     * to the standard {@code string.eq} attribute. This is a simplified
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
        return this.EOeq(eoObject)._getData().toBoolean();
    }

    /**
     * !!!For testing purposes only!!!
     *
     * Produces a string that represents this object.
     * The resulting string has the following form:
     * "value".
     *
     * Example:
     * "this is an example string".
     *
     * This method can be called only in the testing environment
     * since all methods within the EO environment have the 'EO' prefix.
     */
    @Override
    public String toString() {
        return "\""+stringValue+"\"";
    }

    @Override
    public int hashCode() {
        return Objects.hash(stringValue);
    }
}
