package org.eolang.hse;

import org.eolang.hse.core.EOObject;
import org.eolang.hse.core.data.EOData;

import java.util.Objects;

/***
 * Represents a boolean type
 * @version %I%, %G%
 */
public class EObool extends EOObject {

    private final Boolean baseBool;

    public EObool(Boolean baseBool) {
        this.baseBool = baseBool;
    }

    @Override
    public EOData _getData() {
        return new EOData(baseBool);
    }

    /***
     * Used for value substitution based on the condition of this baseBool
     * @param trueObject an object to be substituted with if this baseBool object is true.
     * @param falseObject an object to be substituted with if this baseBool object is false.
     * @return An object being either the trueObject or the falseObject
     */
    public EOObject EOif(EOObject trueObject, EOObject falseObject) {
        EOObject result;

        if (this.baseBool) {
            result = trueObject;
        } else {
            result = falseObject;
        }
        return result;
    }

    /***
     *Inverts this bool
     * @return An object representing the inverse value of this bool
     */
    public EObool EOnot() {
        return new EObool(!this.baseBool);
    }

    /***
     * Checks for the logical AND between this baseBool and the {@code eoObjects} free attributes
     * @param eoObjects an array of objects
     * @return An object representing the logical AND of this baseBool object and all the objects passed to this method
     */
    public EObool EOand(EOObject... eoObjects) {
        Boolean eoBool = this.baseBool;
        for (EOObject eoObject : eoObjects) {
            if (!eoBool) {
                break;
            }
            eoBool &= eoObject._getData().toBoolean();
        }
        return new EObool(eoBool);
    }

    /***
     * Checks for the logical OR between this baseBool and the {@code eoObjects} free attributes
     * @param eoObjects an array of objects
     * @return An object representing the logical OR of this baseBool object and all the objects passed to this method
     */
    public EObool EOor(EOObject... eoObjects) {
        Boolean eoBool = this.baseBool;
        for (EOObject eoObject : eoObjects) {
            if (eoBool) {
                break;
            }
            eoBool |= eoObject._getData().toBoolean();
        }
        return new EObool(eoBool);
    }

    /**
     * !!!For testing purposes only!!!
     *
     * Determines if this object is equal to the {@code o} object.
     * To do it, this method checks that the {@code o} object is
     * of the {@code EOObject} type and its dataization result is the same
     * as the result of dataization of this object by delegating the check
     * to the standard {@code int.eq} attribute. This is a simplified
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
        return eoObject._getData().toBoolean().equals(baseBool);
    }

    @Override
    public int hashCode() {
        return Objects.hash(baseBool);
    }
}
