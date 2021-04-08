package org.eolang;

import org.eolang.core.EOObject;
import org.eolang.core.data.EOData;

/***
 * Represents a string type
 * @version %I%, %G%
 */
public class EOstring extends EOObject {
    private final String stringValue;

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
     * Compares this string to the {@code rightString} free attribute
     * @param rightString a string to compare with
     * @return An object representing the truth value of the comparison of this string with the {@code rightString} free attribute
     */
    public EObool EOeq(EOObject rightString) {
        return new EObool(stringValue.equals(rightString._getData().toString()));
    }
}
