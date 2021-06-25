package org.eolang.hse;

import org.eolang.hse.core.EOObject;
import org.eolang.hse.core.data.EOData;

/***
 * Represents a character type
 * @version %I%, %G%
 */
public class EOchar extends EOObject {
    private char characterValue;

    public EOchar(char characterValue) {
        this.characterValue = characterValue;
    }

    @Override
    public EOData _getData() {
        return new EOData(characterValue);
    }

    /***
     * Converts this character to a string
     * @return An object representing the string type of this character
     */
    public EOstring EOtoString() {
        return new EOstring(Character.toString(characterValue));
    }
}
