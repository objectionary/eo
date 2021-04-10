package org.eolang;

import org.eolang.core.EOObject;
import org.eolang.core.data.EODataObject;

/***
 * Represents an array
 * @version %I%, %G%
 */
public class EOarray {
    private final EOObject[] _array;

    public EOarray(){
        this._array = new EOObject[]{};
    }

    public EOarray(EOObject... objects) {
        _array = objects;
    }

    /***
     * Determines if the base array is empty
     * @return A boolean object, true if empty and false if not empty
     */
    public EObool EOisEmpty() {
        return new EObool(_array.length == 0);
    }

    /***
     * Gets the length of the array
     * @return An object representing the length of the array
     */
    public EOint EOlength() {
        return new EOint(_array.length);
    }

    /***
     * Get the object at {@code position} free attribute of the array
     * @param position an index of the array to fetch from
     * @return An object representing the value at the {@code position} free attribute index of the array
     */
    public EOObject EOget(EOObject position) {
        if (_array.length <= position._getData().toInt().intValue()) {
            throw new IllegalArgumentException(
                    String.format(
                            "Can't get() the %dth element of the array, there are just %d of them",
                            position._getData().toInt().intValue(), _array.length
                    )
            );
        }
        return _array[position._getData().toInt().intValue()];
    }

    /***
     * Appends the object of the free attribute {@code eoObject} to the array
     * @param eoObject An object to append to this array
     * @return An object representing the new array with the appended object of the free attribute {@code eoObject}
     */
    public EOarray EOappend(EOObject eoObject) {
        EOObject[] newArray = new EOObject[this._array.length + 1];
        System.arraycopy(_array, 0, newArray, 0, _array.length);
        newArray[this._array.length] = eoObject;
        return new EOarray(newArray);
    }

    /***
     * Performs the reduction operation of the base array object
     * @param accumulator a partial/subtotal result
     * @param reduceFunction represents the reduction function
     * @return An object representing the final accumulated value of the reduce operation
     */
    public EOObject EOreduce(EOObject accumulator, EOObject reduceFunction) {
        EOObject out = accumulator;
        for (EOObject eoObject: this._array) {
            out = reduceFunction._getAttribute("reduce", out, eoObject);
        }
        return out;
    }

    /***
     * Performs a map operation on the base array object
     * @param mapFunction represents the map function
     * @return An {@code EOarray} object containing mapped elements
     */
    public EOarray EOmap(EOObject mapFunction) {
        int length = this._array.length;
        EOObject[] mappedArray = new EOObject[length];
        for (int i=0;i<length;i++) {
            mappedArray[i] = mapFunction._getAttribute("map", _array[i]);
        }
        return new EOarray(mappedArray);
    }

}
